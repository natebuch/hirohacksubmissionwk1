;; title: keys
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1000))
(define-constant ERR_CANT_SELL_KEYS_THAT_DONT_EXIST (err u1001))
(define-constant ERR_TOTAL_PERCENT_EXCEEDS_100 (err u1002))
(define-constant ERR_PERCENT_EXCEEDS_100 (err u1003))

;; data vars
;;
(define-data-var subjectFeePercent uint u1)
(define-data-var protocolFeePercent uint u1)

;; data maps
;;
(define-map keysBalance { subject: principal, holder: principal } uint)
(define-map keysSupply { subject: principal } uint)

;; public functions
;;
(define-public (buy-keys (subject principal) (amount uint))
  (let
    (
      (supply (default-to u0 (map-get? keysSupply { subject: subject })))
      (price (get-price supply amount))
      
    )
    (if (or (> supply u0) (is-eq tx-sender subject))
      (begin
        (match (stx-transfer? price tx-sender (as-contract tx-sender))
          success
          (begin
            (map-set keysBalance { subject: subject, holder: tx-sender }
              (+ (default-to u0 (map-get? keysBalance { subject: subject, holder: tx-sender })) amount)
            )
            (map-set keysSupply { subject: subject } (+ supply amount))
            (ok true)
          )
          error
          (err u2)
        )
      )
      (err u1)
    )
  )
)

;; known issue with selling keys and calculated pricing.  
;; Keys can be bought in different increments which affects the overall pricing.
;; This can leave the protocol STX pool insufficient to pay out 
;; EX buy u100 keys, then u100 keys again.  Attempt to sell u200 keys will not work as contract doesnt not have enough to payout


(define-public (sell-keys (subject principal) (amount uint))
  (begin
    (asserts! (>= (default-to u0 (map-get? keysSupply { subject: subject })) amount) ERR_CANT_SELL_KEYS_THAT_DONT_EXIST)
    (let
      (
        (balance (default-to u0 (map-get? keysBalance { subject: subject, holder: tx-sender })))
        (supply (default-to u0 (map-get? keysSupply { subject: subject })))
        (sellerPrice  (get-price (- supply amount) amount))
        (subjectFee (/ (* (var-get subjectFeePercent) sellerPrice) u100))
        (sellerFee (- sellerPrice  subjectFee))
        (recipient tx-sender)
      )
      (if (and (>= balance amount) (or (> supply u0) (is-eq tx-sender subject)))
        (begin
          (match
            (begin 
               (try! (as-contract (stx-transfer? sellerFee tx-sender recipient)))
               (as-contract (stx-transfer? subjectFee tx-sender subject))
            )
            success
            (begin
              (map-set keysBalance { subject: subject, holder: tx-sender } (- balance amount))
              (map-set keysSupply { subject: subject } (- supply amount))
              (ok true)
            )
            error
            (err u2)
          )
        )
        (err u1)
      )
    )
  )
)

(define-public (set-protocol-percentage (amount uint))
  (begin
    (asserts! (is-eq contract-caller contract-owner) ERR_UNAUTHORIZED)
    (asserts! (<= u100 amount) ERR_PERCENT_EXCEEDS_100)
    (asserts! (<= u100 (+ (var-get subjectFeePercent) amount)) ERR_TOTAL_PERCENT_EXCEEDS_100)
    (ok (var-set protocolFeePercent amount))
  )
)

(define-public (set-subject-percentage (amount uint))
  (begin
    (asserts! (is-eq contract-caller contract-owner) ERR_UNAUTHORIZED)
    (asserts! (<= u100 amount) ERR_PERCENT_EXCEEDS_100)
    (asserts! (<= u100 (+ (var-get protocolFeePercent) amount)) ERR_TOTAL_PERCENT_EXCEEDS_100)
    (ok (var-set subjectFeePercent amount))
  )
)

;; read only functions
;;
(define-read-only (get-price (supply uint) (amount uint))
  (let
    (
      (base-price u10)
      (price-change-factor u100)
      (adjusted-supply (+ supply amount))
    )
    (+ base-price (* amount (/ (* adjusted-supply adjusted-supply) price-change-factor)))
  )
)

(define-read-only (is-keyholder (subject principal) (holder principal))
  (>= (default-to u0 (map-get? keysBalance { subject: subject, holder: holder })) u1)
)

(define-read-only (get-keys-supply (subject principal))
  (default-to u0 (map-get? keysSupply { subject: subject }))
)

(define-read-only (get-keys-balance (subject principal) (holder principal))
  (default-to u0 (map-get? keysBalance { subject: subject, holder: holder}))
)

(define-read-only (get-total-sell-price (subject principal) (amount uint))
  (let
    (
      (supply (default-to u0 (map-get? keysSupply { subject: subject })))
      (totalSellPrice (get-price (- supply amount) amount))
    )
    totalSellPrice
  )
)

(define-read-only (get-subject-fee (subject principal) (amount uint))
  (let
    (
      (totalSellPrice (get-total-sell-price subject amount))
      (subjectFee (/ (* (var-get subjectFeePercent) totalSellPrice) u100))
    )
    subjectFee
  )
)

(define-read-only (get-protocol-fee (subject principal) (amount uint))
  (let
    (
      (totalSellPrice (get-total-sell-price subject amount))
      (protocolFee (/ (* (var-get protocolFeePercent) totalSellPrice) u100))
    )
    protocolFee
  )
)

(define-read-only (get-sell-price-with-fee (subject principal) (amount uint))
  (let
    (
      (totalSellPrice (get-total-sell-price subject amount))
      (subjectFee (get-subject-fee subject amount))
      (protocolFee (get-protocol-fee subject amount))
      (paidToSeller (- totalSellPrice (+ subjectFee protocolFee)))
    )
    paidToSeller
  )
)


;; private functions
;;


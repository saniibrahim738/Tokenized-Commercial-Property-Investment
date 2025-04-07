;; Rental Income Distribution Contract
;; This contract manages payments to token holders

(define-data-var owner principal tx-sender)
(define-map properties
  { property-id: uint }
  {
    total-rent: uint,
    last-distribution: uint,
    distribution-frequency: uint
  }
)

(define-map token-holders
  { property-id: uint, holder: principal }
  { tokens-owned: uint }
)

(define-public (register-property (property-id uint) (distribution-frequency uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err u100))
    (ok (map-insert properties { property-id: property-id } {
      total-rent: u0,
      last-distribution: u0,
      distribution-frequency: distribution-frequency
    }))
  )
)

(define-public (deposit-rent (property-id uint) (amount uint))
  (let ((property (unwrap-panic (map-get? properties { property-id: property-id }))))
    (begin
      (asserts! (is-eq tx-sender (var-get owner)) (err u101))
      (map-set properties { property-id: property-id }
        (merge property { total-rent: (+ (get total-rent property) amount) })
      )
      (ok true)
    )
  )
)

(define-public (register-token-holder (property-id uint) (holder principal) (tokens uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err u102))
    (ok (map-insert token-holders { property-id: property-id, holder: holder } { tokens-owned: tokens }))
  )
)

(define-public (update-token-holder (property-id uint) (holder principal) (tokens uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err u103))
    (ok (map-set token-holders { property-id: property-id, holder: holder } { tokens-owned: tokens }))
  )
)

(define-public (distribute-rental-income (property-id uint) (total-tokens uint))
  (let ((property (unwrap-panic (map-get? properties { property-id: property-id }))))
    (begin
      (asserts! (is-eq tx-sender (var-get owner)) (err u104))
      (asserts! (>= (- block-height (get last-distribution property)) (get distribution-frequency property)) (err u105))
      (map-set properties { property-id: property-id }
        (merge property {
          total-rent: u0,
          last-distribution: block-height
        })
      )
      (ok true)
    )
  )
)

(define-read-only (get-property-rent (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-token-holder-info (property-id uint) (holder principal))
  (map-get? token-holders { property-id: property-id, holder: holder })
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err u106))
    (var-set owner new-owner)
    (ok true)
  )
)

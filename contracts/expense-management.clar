;; Expense Management Contract
;; This contract handles maintenance and operational costs

(define-data-var owner principal tx-sender)
(define-map properties
  { property-id: uint }
  {
    maintenance-fund: uint,
    operational-fund: uint
  }
)

(define-map expenses
  { expense-id: uint }
  {
    property-id: uint,
    amount: uint,
    expense-type: (string-ascii 20),
    paid: bool,
    timestamp: uint
  }
)

(define-data-var expense-counter uint u0)

(define-public (register-property (property-id uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err u100))
    (ok (map-insert properties { property-id: property-id } {
      maintenance-fund: u0,
      operational-fund: u0
    }))
  )
)

(define-public (deposit-funds (property-id uint) (maintenance-amount uint) (operational-amount uint))
  (let ((property (unwrap-panic (map-get? properties { property-id: property-id }))))
    (begin
      (asserts! (is-eq tx-sender (var-get owner)) (err u101))
      (map-set properties { property-id: property-id }
        {
          maintenance-fund: (+ (get maintenance-fund property) maintenance-amount),
          operational-fund: (+ (get operational-fund property) operational-amount)
        }
      )
      (ok true)
    )
  )
)

(define-public (add-expense (property-id uint) (amount uint) (expense-type (string-ascii 20)))
  (let ((expense-id (var-get expense-counter)))
    (begin
      (asserts! (is-eq tx-sender (var-get owner)) (err u102))
      (var-set expense-counter (+ expense-id u1))
      (map-insert expenses { expense-id: expense-id } {
        property-id: property-id,
        amount: amount,
        expense-type: expense-type,
        paid: false,
        timestamp: block-height
      })
      (ok expense-id)
    )
  )
)

(define-public (pay-expense (expense-id uint))
  (let ((expense (unwrap-panic (map-get? expenses { expense-id: expense-id })))
        (property (unwrap-panic (map-get? properties { property-id: (get property-id expense) }))))
    (begin
      (asserts! (is-eq tx-sender (var-get owner)) (err u103))
      (asserts! (not (get paid expense)) (err u104))

      ;; Decide which fund to use based on expense type
      (if (is-eq (get expense-type expense) "maintenance")
        (begin
          (asserts! (>= (get maintenance-fund property) (get amount expense)) (err u105))
          (map-set properties { property-id: (get property-id expense) }
            (merge property { maintenance-fund: (- (get maintenance-fund property) (get amount expense)) })
          )
        )
        (begin
          (asserts! (>= (get operational-fund property) (get amount expense)) (err u106))
          (map-set properties { property-id: (get property-id expense) }
            (merge property { operational-fund: (- (get operational-fund property) (get amount expense)) })
          )
        )
      )

      ;; Mark expense as paid
      (map-set expenses { expense-id: expense-id }
        (merge expense { paid: true })
      )
      (ok true)
    )
  )
)

(define-read-only (get-property-funds (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-expense (expense-id uint))
  (map-get? expenses { expense-id: expense-id })
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err u107))
    (var-set owner new-owner)
    (ok true)
  )
)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_POOL_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_NO_OVERDRAFT_NEEDED (err u105))
(define-constant ERR_OVERDRAFT_LIMIT_EXCEEDED (err u106))
(define-constant ERR_REPAYMENT_FAILED (err u107))

(define-data-var total-pools uint u0)
(define-data-var protocol-fee-rate uint u250)
(define-data-var base-interest-rate uint u500)

(define-map liquidity-pools
  { pool-id: uint }
  {
    owner: principal,
    total-liquidity: uint,
    available-liquidity: uint,
    base-rate: uint,
    utilization-multiplier: uint,
    created-at: uint
  }
)

(define-map user-deposits
  { user: principal, pool-id: uint }
  {
    amount: uint,
    deposited-at: uint,
    last-claim: uint
  }
)

(define-map overdraft-positions
  { user: principal }
  {
    borrowed-amount: uint,
    pool-id: uint,
    borrowed-at: uint,
    interest-rate: uint,
    collateral-amount: uint
  }
)

(define-map pool-stats
  { pool-id: uint }
  {
    total-borrowed: uint,
    total-repaid: uint,
    active-loans: uint
  }
)

(define-public (create-liquidity-pool (base-rate uint) (utilization-multiplier uint))
  (let
    (
      (pool-id (+ (var-get total-pools) u1))
    )
    (asserts! (> base-rate u0) ERR_INVALID_AMOUNT)
    (asserts! (> utilization-multiplier u0) ERR_INVALID_AMOUNT)
    (map-set liquidity-pools
      { pool-id: pool-id }
      {
        owner: tx-sender,
        total-liquidity: u0,
        available-liquidity: u0,
        base-rate: base-rate,
        utilization-multiplier: utilization-multiplier,
        created-at: stacks-block-height
      }
    )
    (map-set pool-stats
      { pool-id: pool-id }
      {
        total-borrowed: u0,
        total-repaid: u0,
        active-loans: u0
      }
    )
    (var-set total-pools pool-id)
    (ok pool-id)
  )
)

(define-public (deposit-liquidity (pool-id uint) (amount uint))
  (let
    (
      (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (existing-deposit (default-to 
        { amount: u0, deposited-at: u0, last-claim: u0 }
        (map-get? user-deposits { user: tx-sender, pool-id: pool-id })
      ))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool {
        total-liquidity: (+ (get total-liquidity pool) amount),
        available-liquidity: (+ (get available-liquidity pool) amount)
      })
    )
    (map-set user-deposits
      { user: tx-sender, pool-id: pool-id }
      {
        amount: (+ (get amount existing-deposit) amount),
        deposited-at: (if (is-eq (get amount existing-deposit) u0) stacks-block-height (get deposited-at existing-deposit)),
        last-claim: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (withdraw-liquidity (pool-id uint) (amount uint))
  (let
    (
      (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (user-deposit (unwrap! (map-get? user-deposits { user: tx-sender, pool-id: pool-id }) ERR_INSUFFICIENT_FUNDS))
    )
    (asserts! (>= (get amount user-deposit) amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= (get available-liquidity pool) amount) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool {
        total-liquidity: (- (get total-liquidity pool) amount),
        available-liquidity: (- (get available-liquidity pool) amount)
      })
    )
    (if (is-eq (get amount user-deposit) amount)
      (map-delete user-deposits { user: tx-sender, pool-id: pool-id })
      (map-set user-deposits
        { user: tx-sender, pool-id: pool-id }
        (merge user-deposit { amount: (- (get amount user-deposit) amount) })
      )
    )
    (ok true)
  )
)

(define-public (request-overdraft (pool-id uint) (amount uint) (collateral-amount uint))
  (let
    (
      (pool (unwrap! (map-get? liquidity-pools { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      (current-rate (calculate-interest-rate pool-id))
      (max-overdraft (/ (* collateral-amount u150) u100))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> collateral-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount max-overdraft) ERR_OVERDRAFT_LIMIT_EXCEEDED)
    (asserts! (>= (get available-liquidity pool) amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (is-none (map-get? overdraft-positions { user: tx-sender })) ERR_ALREADY_EXISTS)
    (try! (stx-transfer? collateral-amount tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set overdraft-positions
      { user: tx-sender }
      {
        borrowed-amount: amount,
        pool-id: pool-id,
        borrowed-at: stacks-block-height,
        interest-rate: current-rate,
        collateral-amount: collateral-amount
      }
    )
    (map-set liquidity-pools
      { pool-id: pool-id }
      (merge pool { available-liquidity: (- (get available-liquidity pool) amount) })
    )
    (let
      (
        (stats (unwrap! (map-get? pool-stats { pool-id: pool-id }) ERR_POOL_NOT_FOUND))
      )
      (map-set pool-stats
        { pool-id: pool-id }
        (merge stats {
          total-borrowed: (+ (get total-borrowed stats) amount),
          active-loans: (+ (get active-loans stats) u1)
        })
      )
    )
    (ok true)
  )
)

(define-public (repay-overdraft)
  (let
    (
      (position (unwrap! (map-get? overdraft-positions { user: tx-sender }) ERR_POOL_NOT_FOUND))
      (total-debt (calculate-total-debt tx-sender))
      (pool (unwrap! (map-get? liquidity-pools { pool-id: (get pool-id position) }) ERR_POOL_NOT_FOUND))
    )
    (try! (stx-transfer? total-debt tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? (get collateral-amount position) tx-sender tx-sender)))
    (map-set liquidity-pools
      { pool-id: (get pool-id position) }
      (merge pool { 
        available-liquidity: (+ (get available-liquidity pool) (get borrowed-amount position))
      })
    )
    (let
      (
        (stats (unwrap! (map-get? pool-stats { pool-id: (get pool-id position) }) ERR_POOL_NOT_FOUND))
      )
      (map-set pool-stats
        { pool-id: (get pool-id position) }
        (merge stats {
          total-repaid: (+ (get total-repaid stats) total-debt),
          active-loans: (- (get active-loans stats) u1)
        })
      )
    )
    (map-delete overdraft-positions { user: tx-sender })
    (ok true)
  )
)

(define-public (liquidate-position (user principal))
  (let
    (
      (position (unwrap! (map-get? overdraft-positions { user: user }) ERR_POOL_NOT_FOUND))
      (total-debt (calculate-total-debt user))
      (collateral-value (get collateral-amount position))
      (pool (unwrap! (map-get? liquidity-pools { pool-id: (get pool-id position) }) ERR_POOL_NOT_FOUND))
    )
    (asserts! (> total-debt (/ (* collateral-value u120) u100)) ERR_NO_OVERDRAFT_NEEDED)
    (let
      (
        (liquidator-reward (/ (* collateral-value u10) u100))
        (remaining-collateral (- collateral-value liquidator-reward))
      )
      (try! (as-contract (stx-transfer? liquidator-reward tx-sender tx-sender)))
      (if (> remaining-collateral total-debt)
        (begin
          (try! (as-contract (stx-transfer? (- remaining-collateral total-debt) tx-sender user)))
          (map-set liquidity-pools
            { pool-id: (get pool-id position) }
            (merge pool { 
              available-liquidity: (+ (get available-liquidity pool) total-debt)
            })
          )
        )
        (map-set liquidity-pools
          { pool-id: (get pool-id position) }
          (merge pool { 
            available-liquidity: (+ (get available-liquidity pool) remaining-collateral)
          })
        )
      )
      (let
        (
          (stats (unwrap! (map-get? pool-stats { pool-id: (get pool-id position) }) ERR_POOL_NOT_FOUND))
        )
        (map-set pool-stats
          { pool-id: (get pool-id position) }
          (merge stats {
            total-repaid: (+ (get total-repaid stats) (if (< total-debt remaining-collateral) total-debt remaining-collateral)),
            active-loans: (- (get active-loans stats) u1)
          })
        )
      )
      (map-delete overdraft-positions { user: user })
      (ok true)
    )
  )
)

(define-read-only (get-pool-info (pool-id uint))
  (map-get? liquidity-pools { pool-id: pool-id })
)

(define-read-only (get-user-deposit (user principal) (pool-id uint))
  (map-get? user-deposits { user: user, pool-id: pool-id })
)

(define-read-only (get-overdraft-position (user principal))
  (map-get? overdraft-positions { user: user })
)

(define-read-only (get-pool-stats (pool-id uint))
  (map-get? pool-stats { pool-id: pool-id })
)

(define-read-only (calculate-interest-rate (pool-id uint))
  (match (map-get? liquidity-pools { pool-id: pool-id })
    pool
    (let
      (
        (utilization (if (> (get total-liquidity pool) u0)
          (/ (* (- (get total-liquidity pool) (get available-liquidity pool)) u10000) (get total-liquidity pool))
          u0
        ))
        (dynamic-rate (/ (* utilization (get utilization-multiplier pool)) u10000))
      )
      (+ (get base-rate pool) dynamic-rate)
    )
    u0
  )
)

(define-read-only (calculate-total-debt (user principal))
  (match (map-get? overdraft-positions { user: user })
    position
    (let
      (
        (blocks-elapsed (- stacks-block-height (get borrowed-at position)))
        (interest (/ (* (get borrowed-amount position) (get interest-rate position) blocks-elapsed) u1000000))
      )
      (+ (get borrowed-amount position) interest)
    )
    u0
  )
)

(define-read-only (get-total-pools)
  (var-get total-pools)
)

(define-read-only (calculate-utilization-rate (pool-id uint))
  (match (map-get? liquidity-pools { pool-id: pool-id })
    pool
    (if (> (get total-liquidity pool) u0)
      (/ (* (- (get total-liquidity pool) (get available-liquidity pool)) u10000) (get total-liquidity pool))
      u0
    )
    u0
  )
)
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

(define-constant ERR_REWARD_CLAIM_FAILED (err u108))
(define-constant ERR_INSUFFICIENT_REWARDS (err u109))
(define-constant ERR_STAKING_PERIOD_NOT_MET (err u110))
(define-constant ERR_INVALID_MULTIPLIER (err u111))
(define-constant ERR_REWARD_POOL_NOT_FOUND (err u112))
(define-constant ERR_ALREADY_STAKING (err u113))
(define-constant ERR_NOT_STAKING (err u114))
(define-constant ERR_EMISSION_SCHEDULE_EXISTS (err u115))

(define-data-var total-reward-pools uint u0)
(define-data-var protocol-token-supply uint u1000000000000)
(define-data-var base-emission-rate uint u1000)
(define-data-var reward-multiplier-threshold uint u5184000)

(define-map reward-pools
  { pool-id: uint }
  {
    total-rewards: uint,
    rewards-per-block: uint,
    last-update-block: uint,
    accumulated-rewards-per-share: uint,
    total-staked: uint,
    pool-type: (string-ascii 20),
    emission-end-block: uint,
    min-stake-duration: uint
  }
)

(define-map user-rewards
  { user: principal, reward-pool-id: uint }
  {
    staked-amount: uint,
    reward-debt: uint,
    pending-rewards: uint,
    last-claim-block: uint,
    stake-start-block: uint,
    multiplier-tier: uint
  }
)

(define-map staking-multipliers
  { tier: uint }
  {
    duration-blocks: uint,
    multiplier: uint,
    bonus-rate: uint
  }
)

(define-map emission-schedules
  { schedule-id: uint }
  {
    start-block: uint,
    end-block: uint,
    initial-rate: uint,
    final-rate: uint,
    target-pools: (list 10 uint)
  }
)

(define-map user-stake-locks
  { user: principal }
  {
    locked-amount: uint,
    unlock-block: uint,
    lock-tier: uint,
    governance-weight: uint
  }
)

(define-data-var total-emission-schedules uint u0)
(define-data-var governance-token-balance uint u0)

(define-public (initialize-reward-system)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set staking-multipliers { tier: u1 } { duration-blocks: u1008, multiplier: u11000, bonus-rate: u100 })
    (map-set staking-multipliers { tier: u2 } { duration-blocks: u4032, multiplier: u12500, bonus-rate: u250 })
    (map-set staking-multipliers { tier: u3 } { duration-blocks: u8064, multiplier: u15000, bonus-rate: u500 })
    (map-set staking-multipliers { tier: u4 } { duration-blocks: u16128, multiplier: u20000, bonus-rate: u1000 })
    (ok true)
  )
)

(define-public (create-reward-pool (pool-type (string-ascii 20)) (rewards-per-block uint) (emission-duration uint) (min-stake-duration uint))
  (let
    (
      (reward-pool-id (+ (var-get total-reward-pools) u1))
      (emission-end (+ stacks-block-height emission-duration))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> rewards-per-block u0) ERR_INVALID_AMOUNT)
    (asserts! (> emission-duration u0) ERR_INVALID_AMOUNT)
    (map-set reward-pools
      { pool-id: reward-pool-id }
      {
        total-rewards: u0,
        rewards-per-block: rewards-per-block,
        last-update-block: stacks-block-height,
        accumulated-rewards-per-share: u0,
        total-staked: u0,
        pool-type: pool-type,
        emission-end-block: emission-end,
        min-stake-duration: min-stake-duration
      }
    )
    (var-set total-reward-pools reward-pool-id)
    (ok reward-pool-id)
  )
)

(define-public (stake-for-rewards (reward-pool-id uint) (amount uint) (lock-tier uint))
  (let
    (
      (reward-pool (unwrap! (map-get? reward-pools { pool-id: reward-pool-id }) ERR_REWARD_POOL_NOT_FOUND))
      (existing-stake (default-to 
        { staked-amount: u0, reward-debt: u0, pending-rewards: u0, last-claim-block: u0, stake-start-block: u0, multiplier-tier: u0 }
        (map-get? user-rewards { user: tx-sender, reward-pool-id: reward-pool-id })
      ))
      (multiplier-config (unwrap! (map-get? staking-multipliers { tier: lock-tier }) ERR_INVALID_MULTIPLIER))
      (updated-pool (update-reward-pool reward-pool-id))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= lock-tier u4) ERR_INVALID_MULTIPLIER)
    (asserts! (is-eq (get staked-amount existing-stake) u0) ERR_ALREADY_STAKING)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let
      (
        (new-total-staked (+ (get total-staked reward-pool) amount))
        (reward-debt (/ (* amount (get accumulated-rewards-per-share updated-pool)) u1000000))
      )
      (map-set reward-pools
        { pool-id: reward-pool-id }
        (merge updated-pool { total-staked: new-total-staked })
      )
      (map-set user-rewards
        { user: tx-sender, reward-pool-id: reward-pool-id }
        {
          staked-amount: amount,
          reward-debt: reward-debt,
          pending-rewards: u0,
          last-claim-block: stacks-block-height,
          stake-start-block: stacks-block-height,
          multiplier-tier: lock-tier
        }
      )
      (map-set user-stake-locks
        { user: tx-sender }
        {
          locked-amount: amount,
          unlock-block: (+ stacks-block-height (get duration-blocks multiplier-config)),
          lock-tier: lock-tier,
          governance-weight: (/ (* amount (get multiplier multiplier-config)) u10000)
        }
      )
      (ok true)
    )
  )
)

(define-public (claim-rewards (reward-pool-id uint))
  (let
    (
      (reward-pool (unwrap! (map-get? reward-pools { pool-id: reward-pool-id }) ERR_REWARD_POOL_NOT_FOUND))
      (user-stake (unwrap! (map-get? user-rewards { user: tx-sender, reward-pool-id: reward-pool-id }) ERR_NOT_STAKING))
      (updated-pool (update-reward-pool reward-pool-id))
      (multiplier-config (unwrap! (map-get? staking-multipliers { tier: (get multiplier-tier user-stake) }) ERR_INVALID_MULTIPLIER))
    )
    (let
      (
        (pending-base-rewards (/ (* (get staked-amount user-stake) (get accumulated-rewards-per-share updated-pool)) u1000000))
        (total-pending (- pending-base-rewards (get reward-debt user-stake)))
        (time-multiplier (if (>= (- stacks-block-height (get stake-start-block user-stake)) (get duration-blocks multiplier-config))
          (get multiplier multiplier-config)
          u10000
        ))
        (bonus-rewards (/ (* total-pending (get bonus-rate multiplier-config)) u10000))
        (final-rewards (+ (/ (* total-pending time-multiplier) u10000) bonus-rewards))
      )
      (asserts! (> final-rewards u0) ERR_INSUFFICIENT_REWARDS)
      (asserts! (<= final-rewards (var-get protocol-token-supply)) ERR_INSUFFICIENT_REWARDS)
      (var-set protocol-token-supply (- (var-get protocol-token-supply) final-rewards))
      (map-set user-rewards
        { user: tx-sender, reward-pool-id: reward-pool-id }
        (merge user-stake {
          reward-debt: pending-base-rewards,
          last-claim-block: stacks-block-height,
          pending-rewards: u0
        })
      )
      (ok final-rewards)
    )
  )
)

(define-public (unstake-rewards (reward-pool-id uint))
  (let
    (
      (reward-pool (unwrap! (map-get? reward-pools { pool-id: reward-pool-id }) ERR_REWARD_POOL_NOT_FOUND))
      (user-stake (unwrap! (map-get? user-rewards { user: tx-sender, reward-pool-id: reward-pool-id }) ERR_NOT_STAKING))
      (stake-lock (unwrap! (map-get? user-stake-locks { user: tx-sender }) ERR_NOT_STAKING))
      (updated-pool (update-reward-pool reward-pool-id))
    )
    (asserts! (>= stacks-block-height (get unlock-block stake-lock)) ERR_STAKING_PERIOD_NOT_MET)
    (let
      (
        (stake-amount (get staked-amount user-stake))
        (new-total-staked (- (get total-staked reward-pool) stake-amount))
      )
      (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
      (map-set reward-pools
        { pool-id: reward-pool-id }
        (merge updated-pool { total-staked: new-total-staked })
      )
      (map-delete user-rewards { user: tx-sender, reward-pool-id: reward-pool-id })
      (map-delete user-stake-locks { user: tx-sender })
      (ok stake-amount)
    )
  )
)

(define-public (create-emission-schedule (start-block uint) (end-block uint) (initial-rate uint) (final-rate uint) (target-pools (list 10 uint)))
  (let
    (
      (schedule-id (+ (var-get total-emission-schedules) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> end-block start-block) ERR_INVALID_AMOUNT)
    (asserts! (> initial-rate u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? emission-schedules { schedule-id: schedule-id })) ERR_EMISSION_SCHEDULE_EXISTS)
    (map-set emission-schedules
      { schedule-id: schedule-id }
      {
        start-block: start-block,
        end-block: end-block,
        initial-rate: initial-rate,
        final-rate: final-rate,
        target-pools: target-pools
      }
    )
    (var-set total-emission-schedules schedule-id)
    (ok schedule-id)
  )
)

(define-public (update-emission-rates (schedule-id uint))
  (let
    (
      (schedule (unwrap! (map-get? emission-schedules { schedule-id: schedule-id }) ERR_REWARD_POOL_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (and (>= current-block (get start-block schedule)) (<= current-block (get end-block schedule))) ERR_INVALID_AMOUNT)
    (let
      (
        (total-duration (- (get end-block schedule) (get start-block schedule)))
        (elapsed-duration (- current-block (get start-block schedule)))
        (rate-difference (if (> (get initial-rate schedule) (get final-rate schedule))
          (- (get initial-rate schedule) (get final-rate schedule))
          (- (get final-rate schedule) (get initial-rate schedule))
        ))
        (rate-adjustment (/ (* rate-difference elapsed-duration) total-duration))
        (current-rate (if (> (get initial-rate schedule) (get final-rate schedule))
          (- (get initial-rate schedule) rate-adjustment)
          (+ (get initial-rate schedule) rate-adjustment)
        ))
      )
      (var-set base-emission-rate current-rate)
      (ok current-rate)
    )
  )
)

(define-private (update-reward-pool (pool-id uint))
  (match (map-get? reward-pools { pool-id: pool-id })
    pool
    (let
      (
        (blocks-elapsed (- stacks-block-height (get last-update-block pool)))
        (total-rewards (if (> (get total-staked pool) u0)
          (* blocks-elapsed (get rewards-per-block pool))
          u0
        ))
        (rewards-per-share-increase (if (> (get total-staked pool) u0)
          (/ (* total-rewards u1000000) (get total-staked pool))
          u0
        ))
        (new-accumulated-rewards (+ (get accumulated-rewards-per-share pool) rewards-per-share-increase))
      )
      (map-set reward-pools
        { pool-id: pool-id }
        (merge pool {
          last-update-block: stacks-block-height,
          accumulated-rewards-per-share: new-accumulated-rewards,
          total-rewards: (+ (get total-rewards pool) total-rewards)
        })
      )
      (merge pool {
        last-update-block: stacks-block-height,
        accumulated-rewards-per-share: new-accumulated-rewards,
        total-rewards: (+ (get total-rewards pool) total-rewards)
      })
    )
    { total-rewards: u0, rewards-per-block: u0, last-update-block: u0, accumulated-rewards-per-share: u0, total-staked: u0, pool-type: "", emission-end-block: u0, min-stake-duration: u0 }
  )
)

(define-read-only (get-reward-pool-info (pool-id uint))
  (map-get? reward-pools { pool-id: pool-id })
)

(define-read-only (get-user-rewards-info (user principal) (reward-pool-id uint))
  (map-get? user-rewards { user: user, reward-pool-id: reward-pool-id })
)

(define-read-only (get-staking-multiplier (tier uint))
  (map-get? staking-multipliers { tier: tier })
)

(define-read-only (get-emission-schedule (schedule-id uint))
  (map-get? emission-schedules { schedule-id: schedule-id })
)

(define-read-only (get-user-stake-lock (user principal))
  (map-get? user-stake-locks { user: user })
)

(define-read-only (calculate-pending-rewards (user principal) (reward-pool-id uint))
  (match (map-get? user-rewards { user: user, reward-pool-id: reward-pool-id })
    user-stake
    (match (map-get? reward-pools { pool-id: reward-pool-id })
      pool
      (let
        (
          (blocks-elapsed (- stacks-block-height (get last-update-block pool)))
          (total-new-rewards (* blocks-elapsed (get rewards-per-block pool)))
          (rewards-per-share-increase (if (> (get total-staked pool) u0)
            (/ (* total-new-rewards u1000000) (get total-staked pool))
            u0
          ))
          (updated-accumulated-rewards (+ (get accumulated-rewards-per-share pool) rewards-per-share-increase))
          (pending-base-rewards (/ (* (get staked-amount user-stake) updated-accumulated-rewards) u1000000))
          (pending-rewards (- pending-base-rewards (get reward-debt user-stake)))
        )
        pending-rewards
      )
      u0
    )
    u0
  )
)

(define-read-only (get-total-reward-pools)
  (var-get total-reward-pools)
)

(define-read-only (get-protocol-token-supply)
  (var-get protocol-token-supply)
)

(define-read-only (get-current-emission-rate)
  (var-get base-emission-rate)
)

(define-constant ERR_INSURANCE_NOT_FOUND (err u116))
(define-constant ERR_INSUFFICIENT_COVERAGE (err u117))
(define-constant ERR_CLAIM_NOT_VALID (err u118))
(define-constant ERR_PREMIUM_CALCULATION_FAILED (err u119))
(define-constant ERR_INSURANCE_EXPIRED (err u120))
(define-constant ERR_ALREADY_INSURED (err u121))
(define-constant ERR_INSUFFICIENT_INSURANCE_FUNDS (err u122))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u123))

(define-data-var total-insurance-pools uint u0)
(define-data-var insurance-protocol-fee uint u500)
(define-data-var base-premium-rate uint u200)
(define-data-var claim-processing-delay uint u144)

(define-map insurance-pools
  { insurance-pool-id: uint }
  {
    pool-owner: principal,
    coverage-type: (string-ascii 30),
    total-coverage-capacity: uint,
    available-coverage: uint,
    premium-rate: uint,
    min-coverage-amount: uint,
    max-coverage-amount: uint,
    pool-utilization: uint,
    created-at: uint,
    active-policies: uint
  }
)

(define-map insurance-policies
  { policy-id: uint }
  {
    insured-user: principal,
    insurance-pool-id: uint,
    coverage-amount: uint,
    premium-paid: uint,
    policy-start: uint,
    policy-duration: uint,
    insured-position-type: (string-ascii 20),
    insured-pool-id: uint,
    risk-score: uint,
    policy-status: (string-ascii 10)
  }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    claim-amount: uint,
    claim-type: (string-ascii 30),
    claim-timestamp: uint,
    evidence-hash: (buff 32),
    claim-status: (string-ascii 15),
    processed-at: uint,
    payout-amount: uint
  }
)

(define-map insurance-providers
  { provider: principal, insurance-pool-id: uint }
  {
    provided-coverage: uint,
    earned-premiums: uint,
    coverage-share: uint,
    last-premium-claim: uint,
    provider-since: uint
  }
)

(define-map risk-parameters
  { risk-type: (string-ascii 20) }
  {
    base-multiplier: uint,
    utilization-factor: uint,
    volatility-adjustment: uint,
    historical-loss-rate: uint
  }
)

(define-data-var total-policies uint u0)
(define-data-var total-claims uint u0)
(define-data-var insurance-treasury uint u0)

(define-public (create-insurance-pool (coverage-type (string-ascii 30)) (initial-coverage uint) (premium-rate uint) (min-coverage uint) (max-coverage uint))
  (let
    (
      (insurance-pool-id (+ (var-get total-insurance-pools) u1))
    )
    (asserts! (> initial-coverage u0) ERR_INVALID_AMOUNT)
    (asserts! (> premium-rate u0) ERR_INVALID_AMOUNT)
    (asserts! (> max-coverage min-coverage) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? initial-coverage tx-sender (as-contract tx-sender)))
    (map-set insurance-pools
      { insurance-pool-id: insurance-pool-id }
      {
        pool-owner: tx-sender,
        coverage-type: coverage-type,
        total-coverage-capacity: initial-coverage,
        available-coverage: initial-coverage,
        premium-rate: premium-rate,
        min-coverage-amount: min-coverage,
        max-coverage-amount: max-coverage,
        pool-utilization: u0,
        created-at: stacks-block-height,
        active-policies: u0
      }
    )
    (map-set insurance-providers
      { provider: tx-sender, insurance-pool-id: insurance-pool-id }
      {
        provided-coverage: initial-coverage,
        earned-premiums: u0,
        coverage-share: u10000,
        last-premium-claim: stacks-block-height,
        provider-since: stacks-block-height
      }
    )
    (var-set total-insurance-pools insurance-pool-id)
    (ok insurance-pool-id)
  )
)

(define-public (provide-insurance-coverage (insurance-pool-id uint) (coverage-amount uint))
  (let
    (
      (insurance-pool (unwrap! (map-get? insurance-pools { insurance-pool-id: insurance-pool-id }) ERR_INSURANCE_NOT_FOUND))
      (existing-provider (default-to
        { provided-coverage: u0, earned-premiums: u0, coverage-share: u0, last-premium-claim: u0, provider-since: u0 }
        (map-get? insurance-providers { provider: tx-sender, insurance-pool-id: insurance-pool-id })
      ))
    )
    (asserts! (> coverage-amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? coverage-amount tx-sender (as-contract tx-sender)))
    (let
      (
        (new-total-capacity (+ (get total-coverage-capacity insurance-pool) coverage-amount))
        (new-available-coverage (+ (get available-coverage insurance-pool) coverage-amount))
        (new-provided-coverage (+ (get provided-coverage existing-provider) coverage-amount))
        (new-coverage-share (/ (* new-provided-coverage u10000) new-total-capacity))
      )
      (map-set insurance-pools
        { insurance-pool-id: insurance-pool-id }
        (merge insurance-pool {
          total-coverage-capacity: new-total-capacity,
          available-coverage: new-available-coverage
        })
      )
      (map-set insurance-providers
        { provider: tx-sender, insurance-pool-id: insurance-pool-id }
        (merge existing-provider {
          provided-coverage: new-provided-coverage,
          coverage-share: new-coverage-share,
          provider-since: (if (is-eq (get provided-coverage existing-provider) u0) stacks-block-height (get provider-since existing-provider))
        })
      )
      (ok true)
    )
  )
)

(define-public (purchase-insurance-policy (insurance-pool-id uint) (coverage-amount uint) (duration-blocks uint) (insured-position-type (string-ascii 20)) (insured-pool-id uint))
  (let
    (
      (insurance-pool (unwrap! (map-get? insurance-pools { insurance-pool-id: insurance-pool-id }) ERR_INSURANCE_NOT_FOUND))
      (policy-id (+ (var-get total-policies) u1))
      (risk-score (calculate-risk-score insured-position-type insured-pool-id coverage-amount))
      (premium-amount (calculate-premium insurance-pool-id coverage-amount duration-blocks risk-score))
    )
    (asserts! (>= coverage-amount (get min-coverage-amount insurance-pool)) ERR_INVALID_AMOUNT)
    (asserts! (<= coverage-amount (get max-coverage-amount insurance-pool)) ERR_INVALID_AMOUNT)
    (asserts! (<= coverage-amount (get available-coverage insurance-pool)) ERR_INSUFFICIENT_COVERAGE)
    (asserts! (> duration-blocks u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    (map-set insurance-policies
      { policy-id: policy-id }
      {
        insured-user: tx-sender,
        insurance-pool-id: insurance-pool-id,
        coverage-amount: coverage-amount,
        premium-paid: premium-amount,
        policy-start: stacks-block-height,
        policy-duration: duration-blocks,
        insured-position-type: insured-position-type,
        insured-pool-id: insured-pool-id,
        risk-score: risk-score,
        policy-status: "active"
      }
    )
    (map-set insurance-pools
      { insurance-pool-id: insurance-pool-id }
      (merge insurance-pool {
        available-coverage: (- (get available-coverage insurance-pool) coverage-amount),
        pool-utilization: (/ (* (- (get total-coverage-capacity insurance-pool) (- (get available-coverage insurance-pool) coverage-amount)) u10000) (get total-coverage-capacity insurance-pool)),
        active-policies: (+ (get active-policies insurance-pool) u1)
      })
    )
    (var-set total-policies policy-id)
    (var-set insurance-treasury (+ (var-get insurance-treasury) (/ (* premium-amount (var-get insurance-protocol-fee)) u10000)))
    (ok policy-id)
  )
)

(define-public (file-insurance-claim (policy-id uint) (claim-amount uint) (claim-type (string-ascii 30)) (evidence-hash (buff 32)))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR_INSURANCE_NOT_FOUND))
      (claim-id (+ (var-get total-claims) u1))
    )
    (asserts! (is-eq (get insured-user policy) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get policy-status policy) "active") ERR_INSURANCE_EXPIRED)
    (asserts! (<= (+ (get policy-start policy) (get policy-duration policy)) stacks-block-height) ERR_INSURANCE_EXPIRED)
    (asserts! (<= claim-amount (get coverage-amount policy)) ERR_INVALID_AMOUNT)
    (asserts! (> claim-amount u0) ERR_INVALID_AMOUNT)
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        claim-amount: claim-amount,
        claim-type: claim-type,
        claim-timestamp: stacks-block-height,
        evidence-hash: evidence-hash,
        claim-status: "pending",
        processed-at: u0,
        payout-amount: u0
      }
    )
    (var-set total-claims claim-id)
    (ok claim-id)
  )
)

(define-public (process-insurance-claim (claim-id uint) (approved bool))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR_INSURANCE_NOT_FOUND))
      (policy (unwrap! (map-get? insurance-policies { policy-id: (get policy-id claim) }) ERR_INSURANCE_NOT_FOUND))
      (insurance-pool (unwrap! (map-get? insurance-pools { insurance-pool-id: (get insurance-pool-id policy) }) ERR_INSURANCE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get claim-status claim) "pending") ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (>= stacks-block-height (+ (get claim-timestamp claim) (var-get claim-processing-delay))) ERR_INVALID_AMOUNT)
    (let
      (
        (payout-amount (if approved (get claim-amount claim) u0))
        (new-status (if approved "approved" "rejected"))
      )
      (map-set insurance-claims
        { claim-id: claim-id }
        (merge claim {
          claim-status: new-status,
          processed-at: stacks-block-height,
          payout-amount: payout-amount
        })
      )
      (if approved
        (begin
          (try! (as-contract (stx-transfer? payout-amount tx-sender (get claimant claim))))
          (map-set insurance-pools
            { insurance-pool-id: (get insurance-pool-id policy) }
            (merge insurance-pool {
              total-coverage-capacity: (- (get total-coverage-capacity insurance-pool) payout-amount),
              active-policies: (- (get active-policies insurance-pool) u1)
            })
          )
          (map-set insurance-policies
            { policy-id: (get policy-id claim) }
            (merge policy { policy-status: "claimed" })
          )
        )
        (map-set insurance-policies
          { policy-id: (get policy-id claim) }
          (merge policy { policy-status: "expired" })
        )
      )
      (ok approved)
    )
  )
)

(define-public (claim-provider-premiums (insurance-pool-id uint))
  (let
    (
      (provider-info (unwrap! (map-get? insurance-providers { provider: tx-sender, insurance-pool-id: insurance-pool-id }) ERR_INSURANCE_NOT_FOUND))
      (insurance-pool (unwrap! (map-get? insurance-pools { insurance-pool-id: insurance-pool-id }) ERR_INSURANCE_NOT_FOUND))
      (blocks-since-last-claim (- stacks-block-height (get last-premium-claim provider-info)))
      (total-premiums-earned (calculate-provider-premiums insurance-pool-id tx-sender blocks-since-last-claim))
    )
    (asserts! (> total-premiums-earned u0) ERR_INSUFFICIENT_REWARDS)
    (asserts! (<= total-premiums-earned (var-get insurance-treasury)) ERR_INSUFFICIENT_INSURANCE_FUNDS)
    (try! (as-contract (stx-transfer? total-premiums-earned tx-sender tx-sender)))
    (map-set insurance-providers
      { provider: tx-sender, insurance-pool-id: insurance-pool-id }
      (merge provider-info {
        earned-premiums: (+ (get earned-premiums provider-info) total-premiums-earned),
        last-premium-claim: stacks-block-height
      })
    )
    (var-set insurance-treasury (- (var-get insurance-treasury) total-premiums-earned))
    (ok total-premiums-earned)
  )
)

(define-public (initialize-risk-parameters)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set risk-parameters { risk-type: "deposit" } { base-multiplier: u8000, utilization-factor: u1000, volatility-adjustment: u500, historical-loss-rate: u50 })
    (map-set risk-parameters { risk-type: "loan" } { base-multiplier: u12000, utilization-factor: u1500, volatility-adjustment: u800, historical-loss-rate: u150 })
    (map-set risk-parameters { risk-type: "liquidation" } { base-multiplier: u15000, utilization-factor: u2000, volatility-adjustment: u1200, historical-loss-rate: u300 })
    (ok true)
  )
)

(define-private (calculate-risk-score (position-type (string-ascii 20)) (pool-id uint) (amount uint))
  (let
    (
      (pool-utilization (calculate-utilization-rate pool-id))
      (base-risk (if (is-eq position-type "deposit") u5000
                    (if (is-eq position-type "loan") u8000 u12000)))
      (utilization-risk (/ (* pool-utilization u2000) u10000))
      (amount-risk (if (> amount u1000000) u1000 u500))
    )
    (+ base-risk utilization-risk amount-risk)
  )
)

(define-private (calculate-premium (insurance-pool-id uint) (coverage-amount uint) (duration-blocks uint) (risk-score uint))
  (match (map-get? insurance-pools { insurance-pool-id: insurance-pool-id })
    insurance-pool
    (let
      (
        (base-premium (/ (* coverage-amount (get premium-rate insurance-pool) duration-blocks) u1000000))
        (risk-adjustment (/ (* base-premium risk-score) u10000))
        (utilization-adjustment (/ (* base-premium (get pool-utilization insurance-pool)) u10000))
      )
      (+ base-premium risk-adjustment utilization-adjustment)
    )
    u0
  )
)

(define-private (calculate-provider-premiums (insurance-pool-id uint) (provider principal) (blocks-elapsed uint))
  (match (map-get? insurance-providers { provider: provider, insurance-pool-id: insurance-pool-id })
    provider-info
    (let
      (
        (base-premium-share (/ (* (var-get insurance-treasury) (get coverage-share provider-info)) u10000))
        (time-factor (if (> blocks-elapsed u1440) u10000 (/ (* blocks-elapsed u10000) u1440)))
      )
      (/ (* base-premium-share time-factor) u10000)
    )
    u0
  )
)

(define-read-only (get-insurance-pool-info (insurance-pool-id uint))
  (map-get? insurance-pools { insurance-pool-id: insurance-pool-id })
)

(define-read-only (get-insurance-policy-info (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)

(define-read-only (get-insurance-claim-info (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-provider-info (provider principal) (insurance-pool-id uint))
  (map-get? insurance-providers { provider: provider, insurance-pool-id: insurance-pool-id })
)

(define-read-only (get-risk-parameters (risk-type (string-ascii 20)))
  (map-get? risk-parameters { risk-type: risk-type })
)

(define-read-only (get-total-insurance-pools)
  (var-get total-insurance-pools)
)

(define-read-only (get-total-insurance-policies)
  (var-get total-policies)
)

(define-read-only (get-insurance-treasury-balance)
  (var-get insurance-treasury)
)



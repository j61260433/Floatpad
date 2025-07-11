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
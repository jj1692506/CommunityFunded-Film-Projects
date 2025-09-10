;; Film Backer Rewards System
;; Enables creators to offer tiered rewards and perks to film backers

;; Error constants
(define-constant err-reward-not-found (err u500))
(define-constant err-tier-not-found (err u501))
(define-constant err-reward-already-exists (err u502))
(define-constant err-not-film-creator (err u503))
(define-constant err-not-eligible-backer (err u504))
(define-constant err-reward-already-claimed (err u505))
(define-constant err-film-not-funded (err u506))
(define-constant err-invalid-tier-threshold (err u507))
(define-constant err-reward-expired (err u508))
(define-constant err-insufficient-backing (err u509))

;; Data variables
(define-data-var next-reward-id uint u1)
(define-data-var next-tier-id uint u1)

;; Reward tiers for films
(define-map film-reward-tiers
  { film-id: uint, tier-id: uint }
  {
    tier-name: (string-ascii 50),
    min-contribution: uint,
    max-backers: uint,
    current-backers: uint,
    tier-description: (string-ascii 200),
    created-at: uint,
    is-active: bool
  }
)

;; Individual rewards within tiers
(define-map tier-rewards
  { film-id: uint, tier-id: uint, reward-id: uint }
  {
    reward-type: (string-ascii 30),
    reward-description: (string-ascii 150),
    digital-asset-url: (optional (string-ascii 200)),
    is-limited: bool,
    max-quantity: uint,
    claimed-quantity: uint,
    expiry-block: (optional uint),
    reward-value: uint
  }
)

;; Backer tier eligibility and claims
(define-map backer-tier-status
  { film-id: uint, backer: principal }
  {
    qualifying-tier: uint,
    total-contribution: uint,
    tier-assigned-at: uint,
    rewards-claimed-count: uint
  }
)

;; Individual reward claims tracking
(define-map reward-claims
  { film-id: uint, reward-id: uint, backer: principal }
  {
    claimed-at: uint,
    claim-transaction-id: (optional (string-ascii 64))
  }
)

;; Film reward statistics
(define-map film-reward-stats
  { film-id: uint }
  {
    total-tiers: uint,
    total-rewards: uint,
    total-claims: uint,
    most-popular-tier: uint
  }
)

;; Public functions

;; Create a reward tier for a film
(define-public (create-reward-tier 
  (film-id uint) 
  (tier-name (string-ascii 50))
  (min-contribution uint)
  (max-backers uint)
  (tier-description (string-ascii 200)))
  
  (let (
    (tier-id (var-get next-tier-id))
    (film (unwrap! (contract-call? .film get-film film-id) err-reward-not-found))
    (caller tx-sender)
  )
    ;; Only film creator can create tiers
    (asserts! (is-eq caller (get creator film)) err-not-film-creator)
    (asserts! (> min-contribution u0) err-invalid-tier-threshold)
    
    (map-set film-reward-tiers
      { film-id: film-id, tier-id: tier-id }
      {
        tier-name: tier-name,
        min-contribution: min-contribution,
        max-backers: max-backers,
        current-backers: u0,
        tier-description: tier-description,
        created-at: stacks-block-height,
        is-active: true
      }
    )
    
    ;; Update film stats
    (update-film-reward-stats film-id "tier-created")
    
    (var-set next-tier-id (+ tier-id u1))
    (ok tier-id)
  )
)

;; Add a reward to an existing tier
(define-public (add-tier-reward
  (film-id uint)
  (tier-id uint)
  (reward-type (string-ascii 30))
  (reward-description (string-ascii 150))
  (digital-asset-url (optional (string-ascii 200)))
  (is-limited bool)
  (max-quantity uint)
  (expiry-blocks (optional uint))
  (reward-value uint))
  
  (let (
    (reward-id (var-get next-reward-id))
    (film (unwrap! (contract-call? .film get-film film-id) err-reward-not-found))
    (tier (unwrap! (map-get? film-reward-tiers { film-id: film-id, tier-id: tier-id }) err-tier-not-found))
    (caller tx-sender)
    (expiry-block (match expiry-blocks
      blocks (some (+ stacks-block-height blocks))
      none))
  )
    ;; Only film creator can add rewards
    (asserts! (is-eq caller (get creator film)) err-not-film-creator)
    (asserts! (get is-active tier) err-tier-not-found)
    
    (map-set tier-rewards
      { film-id: film-id, tier-id: tier-id, reward-id: reward-id }
      {
        reward-type: reward-type,
        reward-description: reward-description,
        digital-asset-url: digital-asset-url,
        is-limited: is-limited,
        max-quantity: max-quantity,
        claimed-quantity: u0,
        expiry-block: expiry-block,
        reward-value: reward-value
      }
    )
    
    ;; Update film stats
    (update-film-reward-stats film-id "reward-created")
    
    (var-set next-reward-id (+ reward-id u1))
    (ok reward-id)
  )
)



;; Claim a specific reward
(define-public (claim-reward (film-id uint) (tier-id uint) (reward-id uint))
  (let (
    (film (unwrap! (contract-call? .film get-film film-id) err-reward-not-found))
    (tier (unwrap! (map-get? film-reward-tiers { film-id: film-id, tier-id: tier-id }) err-tier-not-found))
    (reward (unwrap! (map-get? tier-rewards { film-id: film-id, tier-id: tier-id, reward-id: reward-id }) err-reward-not-found))
    (backer-status (unwrap! (map-get? backer-tier-status { film-id: film-id, backer: tx-sender }) err-not-eligible-backer))
    (existing-claim (map-get? reward-claims { film-id: film-id, reward-id: reward-id, backer: tx-sender }))
  )
    ;; Validations
    (asserts! (is-none existing-claim) err-reward-already-claimed)
    (asserts! (>= (get qualifying-tier backer-status) tier-id) err-not-eligible-backer)
    (asserts! (get is-funded film) err-film-not-funded)
    
    ;; Check reward availability
    (asserts! (or (not (get is-limited reward)) 
                  (< (get claimed-quantity reward) (get max-quantity reward))) 
              err-reward-not-found)
    
    ;; Check expiry
    (match (get expiry-block reward)
      expiry-block (asserts! (<= stacks-block-height expiry-block) err-reward-expired)
      true)
    
    ;; Record claim
    (map-set reward-claims
      { film-id: film-id, reward-id: reward-id, backer: tx-sender }
      {
        claimed-at: stacks-block-height,
        claim-transaction-id: none
      }
    )
    
    ;; Update reward claimed quantity
    (map-set tier-rewards
      { film-id: film-id, tier-id: tier-id, reward-id: reward-id }
      (merge reward { claimed-quantity: (+ (get claimed-quantity reward) u1) })
    )
    
    ;; Update backer status
    (map-set backer-tier-status
      { film-id: film-id, backer: tx-sender }
      (merge backer-status { rewards-claimed-count: (+ (get rewards-claimed-count backer-status) u1) })
    )
    
    ;; Update film stats
    (update-film-reward-stats film-id "reward-claimed")
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-reward-tier (film-id uint) (tier-id uint))
  (map-get? film-reward-tiers { film-id: film-id, tier-id: tier-id })
)

(define-read-only (get-tier-reward (film-id uint) (tier-id uint) (reward-id uint))
  (map-get? tier-rewards { film-id: film-id, tier-id: tier-id, reward-id: reward-id })
)

(define-read-only (get-backer-tier-status (film-id uint) (backer principal))
  (map-get? backer-tier-status { film-id: film-id, backer: backer })
)

(define-read-only (get-reward-claim (film-id uint) (reward-id uint) (backer principal))
  (map-get? reward-claims { film-id: film-id, reward-id: reward-id, backer: backer })
)

(define-read-only (get-film-reward-stats (film-id uint))
  (default-to 
    { total-tiers: u0, total-rewards: u0, total-claims: u0, most-popular-tier: u0 }
    (map-get? film-reward-stats { film-id: film-id })
  )
)

(define-read-only (is-reward-available (film-id uint) (tier-id uint) (reward-id uint))
  (match (map-get? tier-rewards { film-id: film-id, tier-id: tier-id, reward-id: reward-id })
    reward
      (and 
        (or (not (get is-limited reward)) (< (get claimed-quantity reward) (get max-quantity reward)))
        (match (get expiry-block reward)
          expiry-block (<= stacks-block-height expiry-block)
          true))
    false
  )
)

;; Private functions

(define-private (find-qualifying-tier (film-id uint) (contribution uint))
  (let ((tier-1 (map-get? film-reward-tiers { film-id: film-id, tier-id: u1 }))
        (tier-2 (map-get? film-reward-tiers { film-id: film-id, tier-id: u2 }))
        (tier-3 (map-get? film-reward-tiers { film-id: film-id, tier-id: u3 })))
    
    (if (and (is-some tier-3) (>= contribution (get min-contribution (unwrap-panic tier-3))))
      (some u3)
      (if (and (is-some tier-2) (>= contribution (get min-contribution (unwrap-panic tier-2))))
        (some u2)
        (if (and (is-some tier-1) (>= contribution (get min-contribution (unwrap-panic tier-1))))
          (some u1)
          none
        )
      )
    )
  )
)

(define-private (update-film-reward-stats (film-id uint) (action (string-ascii 20)))
  (let ((current-stats (get-film-reward-stats film-id)))
    (map-set film-reward-stats
      { film-id: film-id }
      (if (is-eq action "tier-created")
        (merge current-stats { total-tiers: (+ (get total-tiers current-stats) u1) })
        (if (is-eq action "reward-created")
          (merge current-stats { total-rewards: (+ (get total-rewards current-stats) u1) })
          (merge current-stats { total-claims: (+ (get total-claims current-stats) u1) })
        )
      )
    )
  )
)

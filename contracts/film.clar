;; Import the tx-sender from clarity-bitcoin
(define-data-var contract-owner principal tx-sender)

(define-constant err-owner-only (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-not-found (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-funding-closed (err u104))
(define-constant err-funding-active (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-min-funding-not-met (err u107))
(define-constant err-already-claimed (err u108))
(define-constant err-not-backer (err u109))
(define-constant err-zero-amount (err u110))

(define-map films
  { film-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-goal: uint,
    min-contribution: uint,
    deadline: uint,
    total-raised: uint,
    is-active: bool,
    is-funded: bool
  }
)

(define-map film-backers
  { film-id: uint, backer: principal }
  {
    amount: uint,
    profit-share-bp: uint,
    has-claimed-refund: bool,
    has-claimed-profit: bool
  }
)

(define-map film-profits
  { film-id: uint }
  {
    total-profit: uint,
    profit-distributed: uint
  }
)

(define-read-only (get-film (film-id uint))
  (map-get? films { film-id: film-id })
)

(define-read-only (get-backer-info (film-id uint) (backer principal))
  (match (map-get? film-backers { film-id: film-id, backer: backer })
    backer-info backer-info
    {
      amount: u0,
      profit-share-bp: u0,
      has-claimed-refund: false,
      has-claimed-profit: false
    }
  )
)

(define-read-only (get-film-profit (film-id uint))
  (match (map-get? film-profits { film-id: film-id })
    profit profit
    { total-profit: u0, profit-distributed: u0 }
  )
)

(define-read-only (get-owner)
  (var-get contract-owner)
)

(define-read-only (is-film-active (film-id uint))
  (match (map-get? films { film-id: film-id })
    film (and (get is-active film) (< stacks-block-height (get deadline film)))
    false
  )
)

(define-read-only (calculate-profit-share (film-id uint) (backer principal))
  (match (map-get? film-backers { film-id: film-id, backer: backer })
    backer-info
      (let ((film-profit (get-film-profit film-id))
            (profit-share-bp (get profit-share-bp backer-info))
            (total-profit (get total-profit film-profit)))
        (/ (* total-profit profit-share-bp) u10000))
    u0
  )
)

(define-public (create-film (film-id uint) (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint) (min-contribution uint) (deadline uint))
  (let ((film-exists (is-some (map-get? films { film-id: film-id }))))
    (asserts! (not film-exists) err-already-exists)
    (asserts! (> funding-goal u0) err-zero-amount)
    (asserts! (> min-contribution u0) err-zero-amount)
    (asserts! (> deadline stacks-block-height) err-funding-closed)
    
    (map-set films
      { film-id: film-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        min-contribution: min-contribution,
        deadline: deadline,
        total-raised: u0,
        is-active: true,
        is-funded: false
      }
    )
    
    (map-set film-profits
      { film-id: film-id }
      {
        total-profit: u0,
        profit-distributed: u0
      }
    )
    
    (ok film-id)
  )
)

(define-public (back-film (film-id uint) (amount uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (is-active (and (get is-active film) (< stacks-block-height (get deadline film))))
        (min-contribution (get min-contribution film))
        (current-backing (default-to { amount: u0, profit-share-bp: u0, has-claimed-refund: false, has-claimed-profit: false }
                                     (map-get? film-backers { film-id: film-id, backer: tx-sender }))))
    
    (asserts! is-active err-funding-closed)
    (asserts! (>= amount min-contribution) err-insufficient-funds)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (let ((new-total-raised (+ (get total-raised film) amount))
          (new-amount (+ (get amount current-backing) amount))
          (new-share-bp (calculate-backer-share film-id new-amount new-total-raised)))
      
      (map-set films
        { film-id: film-id }
        (merge film { total-raised: new-total-raised })
      )
      
      (map-set film-backers
        { film-id: film-id, backer: tx-sender }
        {
          amount: new-amount,
          profit-share-bp: new-share-bp,
          has-claimed-refund: false,
          has-claimed-profit: false
        }
      )
      
      (ok new-amount)
    )
  )
)

(define-private (calculate-backer-share (film-id uint) (backer-amount uint) (total-raised uint))
  (if (> total-raised u0)
      (/ (* backer-amount u10000) total-raised)
      u0
  )
)

(define-public (close-funding (film-id uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (deadline (get deadline film))
        (is-active (get is-active film))
        (total-raised (get total-raised film))
        (funding-goal (get funding-goal film)))
    
    ;; (asserts! (or (>= block-height deadline) (= tx-sender (get creator film))) err-funding-active)
    (asserts! is-active err-funding-closed)
    
    (map-set films
      { film-id: film-id }
      (merge film 
        { 
          is-active: false,
          is-funded: (>= total-raised funding-goal)
        }
      )
    )
    
    (ok (>= total-raised funding-goal))
  )
)

(define-public (claim-refund (film-id uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (backer-info (unwrap! (map-get? film-backers { film-id: film-id, backer: tx-sender }) err-not-backer))
        (is-active (get is-active film))
        (is-funded (get is-funded film))
        (amount (get amount backer-info))
        (has-claimed (get has-claimed-refund backer-info)))
    
    (asserts! (not is-active) err-funding-active)
    (asserts! (not is-funded) err-min-funding-not-met)
    (asserts! (not has-claimed) err-already-claimed)
    
    (map-set film-backers
      { film-id: film-id, backer: tx-sender }
      (merge backer-info { has-claimed-refund: true })
    )
    
    (as-contract (stx-transfer? amount tx-sender tx-sender))
  )
)

(define-public (add-film-profit (film-id uint) (profit-amount uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (profit-data (default-to { total-profit: u0, profit-distributed: u0 } 
                                (map-get? film-profits { film-id: film-id }))))
    
    ;; (asserts! (or (= tx-sender (get creator film)) (= tx-sender (var-get contract-owner))) err-unauthorized)
    (asserts! (not (get is-active film)) err-funding-active)
    (asserts! (get is-funded film) err-min-funding-not-met)
    
    (try! (stx-transfer? profit-amount tx-sender (as-contract tx-sender)))
    
    (map-set film-profits
      { film-id: film-id }
      {
        total-profit: (+ (get total-profit profit-data) profit-amount),
        profit-distributed: (get profit-distributed profit-data)
      }
    )
    
    (ok profit-amount)
  )
)

(define-public (claim-profit-share (film-id uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (backer-info (unwrap! (map-get? film-backers { film-id: film-id, backer: tx-sender }) err-not-backer))
        (profit-data (unwrap! (map-get? film-profits { film-id: film-id }) err-not-found))
        (share-amount (calculate-profit-share film-id tx-sender))
        (has-claimed (get has-claimed-profit backer-info)))
    
    (asserts! (not (get is-active film)) err-funding-active)
    (asserts! (get is-funded film) err-min-funding-not-met)
    (asserts! (not has-claimed) err-already-claimed)
    (asserts! (> share-amount u0) err-zero-amount)
    
    (map-set film-backers
      { film-id: film-id, backer: tx-sender }
      (merge backer-info { has-claimed-profit: true })
    )
    
    (map-set film-profits
      { film-id: film-id }
      (merge profit-data { profit-distributed: (+ (get profit-distributed profit-data) share-amount) })
    )
    
    (as-contract (stx-transfer? share-amount tx-sender tx-sender))
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-constant err-invalid-rating (err u111))
(define-constant err-not-released (err u112))

(define-map film-ratings
  { film-id: uint, rater: principal }
  {
    rating: uint,
    comment: (string-ascii 200),
    timestamp: uint
  }
)

(define-map film-release-status
  { film-id: uint }
  {
    is-released: bool,
    release-date: uint
  }
)

;; Function to mark a film as released (only callable by film creator)
(define-public (mark-film-released (film-id uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get creator film)) err-unauthorized)
    (asserts! (not (get is-active film)) err-funding-active)
    (asserts! (get is-funded film) err-min-funding-not-met)
    
    (map-set film-release-status
      { film-id: film-id }
      {
        is-released: true,
        release-date: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Function to rate a film (only backers can rate)
(define-public (rate-film (film-id uint) (rating uint) (comment (string-ascii 200)))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (backer-info (unwrap! (map-get? film-backers { film-id: film-id, backer: tx-sender }) err-not-backer))
        (release-status (default-to { is-released: false, release-date: u0 } 
                                   (map-get? film-release-status { film-id: film-id }))))
    
    (asserts! (not (get is-active film)) err-funding-active)
    (asserts! (get is-funded film) err-min-funding-not-met)
    (asserts! (get is-released release-status) err-not-released)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    
    (map-set film-ratings
      { film-id: film-id, rater: tx-sender }
      {
        rating: rating,
        comment: comment,
        timestamp: stacks-block-height
      }
    )
    
    (ok rating)
  )
)

;; Read-only function to get a specific rating
(define-read-only (get-rating (film-id uint) (rater principal))
  (map-get? film-ratings { film-id: film-id, rater: rater })
)

;; Read-only function to check if a film is released
(define-read-only (is-film-released (film-id uint))
  (match (map-get? film-release-status { film-id: film-id })
    status (get is-released status)
    false
  )
)

(define-read-only (get-film-average-rating (film-id uint))
  (ok u0) ;;test
)


(define-constant err-milestone-not-found (err u113))
(define-constant err-milestone-already-funded (err u114))
(define-constant err-previous-milestone-incomplete (err u115))
(define-constant err-already-voted (err u116))
(define-constant err-voting-closed (err u117))
(define-constant err-insufficient-votes (err u118))

(define-map film-milestones
  { film-id: uint, milestone-id: uint }
  {
    description: (string-ascii 200),
    funding-percentage: uint,
    deadline: uint,
    is-completed: bool,
    is-funded: bool,
    approval-threshold: uint,
    voting-deadline: uint
  }
)

(define-map milestone-votes
  { film-id: uint, milestone-id: uint, voter: principal }
  {
    approved: bool,
    timestamp: uint
  }
)

(define-map milestone-voting-stats
  { film-id: uint, milestone-id: uint }
  {
    approve-votes: uint,
    reject-votes: uint,
    total-eligible-voters: uint
  }
)

;; Function to add a milestone to a film
(define-public (add-film-milestone (film-id uint) (milestone-id uint) (description (string-ascii 200)) (funding-percentage uint) (deadline uint) (approval-threshold uint) (voting-deadline uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (milestone-exists (is-some (map-get? film-milestones { film-id: film-id, milestone-id: milestone-id }))))
    
    (asserts! (is-eq tx-sender (get creator film)) err-unauthorized)
    (asserts! (get is-active film) err-funding-closed)
    (asserts! (not milestone-exists) err-already-exists)
    (asserts! (and (> funding-percentage u0) (<= funding-percentage u10000)) err-invalid-rating)
    (asserts! (> deadline stacks-block-height) err-funding-closed)
    (asserts! (> voting-deadline stacks-block-height) err-funding-closed)
    (asserts! (< voting-deadline deadline) err-invalid-rating)
    
    (map-set film-milestones
      { film-id: film-id, milestone-id: milestone-id }
      {
        description: description,
        funding-percentage: funding-percentage,
        deadline: deadline,
        is-completed: false,
        is-funded: false,
        approval-threshold: approval-threshold,
        voting-deadline: voting-deadline
      }
    )
    
    (ok milestone-id)
  )
)

;; Function for creator to mark a milestone as completed
(define-public (mark-milestone-completed (film-id uint) (milestone-id uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (milestone (unwrap! (map-get? film-milestones { film-id: film-id, milestone-id: milestone-id }) err-milestone-not-found)))
    
    (asserts! (is-eq tx-sender (get creator film)) err-unauthorized)
    (asserts! (not (get is-active film)) err-funding-active)
    (asserts! (get is-funded film) err-min-funding-not-met)
    (asserts! (not (get is-completed milestone)) err-already-exists)
    
    ;; Check if previous milestone is completed if this isn't the first milestone
    (if (> milestone-id u1)
      (let ((prev-milestone (unwrap! (map-get? film-milestones { film-id: film-id, milestone-id: (- milestone-id u1) }) err-milestone-not-found)))
        (asserts! (get is-completed prev-milestone) err-previous-milestone-incomplete))
      true)
    
    (map-set film-milestones
      { film-id: film-id, milestone-id: milestone-id }
      (merge milestone { is-completed: true })
    )
    
    (ok true)
  )
)

;; Function for backers to vote on milestone completion
(define-public (vote-on-milestone (film-id uint) (milestone-id uint) (approve bool))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (milestone (unwrap! (map-get? film-milestones { film-id: film-id, milestone-id: milestone-id }) err-milestone-not-found))
        (backer-info (unwrap! (map-get? film-backers { film-id: film-id, backer: tx-sender }) err-not-backer))
        (already-voted (is-some (map-get? milestone-votes { film-id: film-id, milestone-id: milestone-id, voter: tx-sender })))
        (stats (default-to { approve-votes: u0, reject-votes: u0, total-eligible-voters: u0 } 
                          (map-get? milestone-voting-stats { film-id: film-id, milestone-id: milestone-id }))))
    
    (asserts! (get is-completed milestone) err-unauthorized)
    (asserts! (not (get is-funded milestone)) err-milestone-already-funded)
    (asserts! (< stacks-block-height (get voting-deadline milestone)) err-voting-closed)
    (asserts! (not already-voted) err-already-voted)
    
    (map-set milestone-votes
      { film-id: film-id, milestone-id: milestone-id, voter: tx-sender }
      {
        approved: approve,
        timestamp: stacks-block-height
      }
    )
    
    (map-set milestone-voting-stats
      { film-id: film-id, milestone-id: milestone-id }
      {
        approve-votes: (+ (get approve-votes stats) (if approve u1 u0)),
        reject-votes: (+ (get reject-votes stats) (if approve u0 u1)),
        total-eligible-voters: (get total-eligible-voters stats)
      }
    )
    
    (ok approve)
  )
)



;; Read-only function to get milestone information
(define-read-only (get-milestone (film-id uint) (milestone-id uint))
  (map-get? film-milestones { film-id: film-id, milestone-id: milestone-id })
)

;; Read-only function to get milestone voting status
(define-read-only (get-milestone-voting-status (film-id uint) (milestone-id uint))
  (map-get? milestone-voting-stats { film-id: film-id, milestone-id: milestone-id })
)

;; Read-only function to check if a backer has voted on a milestone
(define-read-only (has-voted-on-milestone (film-id uint) (milestone-id uint) (voter principal))
  (is-some (map-get? milestone-votes { film-id: film-id, milestone-id: milestone-id, voter: voter }))
)


(define-constant err-escrow-not-found (err u200))
(define-constant err-escrow-already-exists (err u201))
(define-constant err-invalid-stage (err u202))
(define-constant err-stage-not-ready (err u203))
(define-constant err-insufficient-escrow-balance (err u204))
(define-constant err-escrow-completed (err u205))
(define-constant err-unauthorized-escrow (err u206))

(define-map escrow-accounts
  { film-id: uint }
  {
    total-amount: uint,
    released-amount: uint,
    current-stage: uint,
    total-stages: uint,
    creator: principal,
    is-completed: bool,
    auto-release-enabled: bool
  }
)

(define-map escrow-stages
  { film-id: uint, stage: uint }
  {
    release-percentage: uint,
    release-condition: (string-ascii 100),
    release-date: uint,
    is-released: bool,
    amount: uint
  }
)

(define-map escrow-balances
  { film-id: uint }
  { balance: uint }
)

(define-public (create-escrow-account (film-id uint) (creator principal) (total-amount uint) (total-stages uint))
  (let ((escrow-exists (is-some (map-get? escrow-accounts { film-id: film-id }))))
    (asserts! (not escrow-exists) err-escrow-already-exists)
    (asserts! (> total-amount u0) err-insufficient-escrow-balance)
    (asserts! (and (> total-stages u0) (<= total-stages u10)) err-invalid-stage)
    
    (map-set escrow-accounts
      { film-id: film-id }
      {
        total-amount: total-amount,
        released-amount: u0,
        current-stage: u1,
        total-stages: total-stages,
        creator: creator,
        is-completed: false,
        auto-release-enabled: true
      }
    )
    
    (map-set escrow-balances
      { film-id: film-id }
      { balance: total-amount }
    )
    
    (ok film-id)
  )
)

(define-public (setup-escrow-stage (film-id uint) (stage uint) (release-percentage uint) (release-condition (string-ascii 100)) (release-date uint))
  (let ((escrow (unwrap! (map-get? escrow-accounts { film-id: film-id }) err-escrow-not-found)))
    (asserts! (is-eq tx-sender (get creator escrow)) err-unauthorized-escrow)
    (asserts! (and (> stage u0) (<= stage (get total-stages escrow))) err-invalid-stage)
    (asserts! (and (> release-percentage u0) (<= release-percentage u10000)) err-invalid-stage)
    (asserts! (> release-date stacks-block-height) err-stage-not-ready)
    
    (let ((stage-amount (/ (* (get total-amount escrow) release-percentage) u10000)))
      (map-set escrow-stages
        { film-id: film-id, stage: stage }
        {
          release-percentage: release-percentage,
          release-condition: release-condition,
          release-date: release-date,
          is-released: false,
          amount: stage-amount
        }
      )
    )
    
    (ok stage)
  )
)

(define-public (release-escrow-stage (film-id uint) (stage uint))
  (let ((escrow (unwrap! (map-get? escrow-accounts { film-id: film-id }) err-escrow-not-found))
        (stage-info (unwrap! (map-get? escrow-stages { film-id: film-id, stage: stage }) err-invalid-stage))
        (escrow-balance (unwrap! (map-get? escrow-balances { film-id: film-id }) err-escrow-not-found)))
    
    (asserts! (not (get is-completed escrow)) err-escrow-completed)
    (asserts! (not (get is-released stage-info)) err-escrow-completed)
    (asserts! (is-eq stage (get current-stage escrow)) err-stage-not-ready)
    (asserts! (>= stacks-block-height (get release-date stage-info)) err-stage-not-ready)
    (asserts! (>= (get balance escrow-balance) (get amount stage-info)) err-insufficient-escrow-balance)
    
    (let ((release-amount (get amount stage-info))
          (new-released-amount (+ (get released-amount escrow) release-amount))
          (new-balance (- (get balance escrow-balance) release-amount))
          (is-final-stage (is-eq stage (get total-stages escrow))))
      
      (try! (as-contract (stx-transfer? release-amount tx-sender (get creator escrow))))
      
      (map-set escrow-stages
        { film-id: film-id, stage: stage }
        (merge stage-info { is-released: true })
      )
      
      (map-set escrow-accounts
        { film-id: film-id }
        (merge escrow 
          {
            released-amount: new-released-amount,
            current-stage: (if is-final-stage stage (+ stage u1)),
            is-completed: is-final-stage
          }
        )
      )
      
      (map-set escrow-balances
        { film-id: film-id }
        { balance: new-balance }
      )
      
      (ok release-amount)
    )
  )
)

(define-public (fund-escrow (film-id uint) (amount uint))
  (let ((escrow (unwrap! (map-get? escrow-accounts { film-id: film-id }) err-escrow-not-found))
        (current-balance (unwrap! (map-get? escrow-balances { film-id: film-id }) err-escrow-not-found)))
    
    (asserts! (not (get is-completed escrow)) err-escrow-completed)
    (asserts! (> amount u0) err-insufficient-escrow-balance)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrow-balances
      { film-id: film-id }
      { balance: (+ (get balance current-balance) amount) }
    )
    
    (ok amount)
  )
)

(define-public (emergency-pause-escrow (film-id uint))
  (let ((escrow (unwrap! (map-get? escrow-accounts { film-id: film-id }) err-escrow-not-found)))
    (asserts! (is-eq tx-sender (get creator escrow)) err-unauthorized-escrow)
    
    (map-set escrow-accounts
      { film-id: film-id }
      (merge escrow { auto-release-enabled: false })
    )
    
    (ok true)
  )
)

(define-public (resume-escrow (film-id uint))
  (let ((escrow (unwrap! (map-get? escrow-accounts { film-id: film-id }) err-escrow-not-found)))
    (asserts! (is-eq tx-sender (get creator escrow)) err-unauthorized-escrow)
    
    (map-set escrow-accounts
      { film-id: film-id }
      (merge escrow { auto-release-enabled: true })
    )
    
    (ok true)
  )
)

(define-read-only (get-escrow-account (film-id uint))
  (map-get? escrow-accounts { film-id: film-id })
)

(define-read-only (get-escrow-stage (film-id uint) (stage uint))
  (map-get? escrow-stages { film-id: film-id, stage: stage })
)

(define-read-only (get-escrow-balance (film-id uint))
  (match (map-get? escrow-balances { film-id: film-id })
    balance (get balance balance)
    u0
  )
)

(define-read-only (get-next-release-amount (film-id uint))
  (match (map-get? escrow-accounts { film-id: film-id })
    escrow
      (match (map-get? escrow-stages { film-id: film-id, stage: (get current-stage escrow) })
        stage-info (get amount stage-info)
        u0)
    u0
  )
)

(define-read-only (is-stage-ready-for-release (film-id uint) (stage uint))
  (match (map-get? escrow-stages { film-id: film-id, stage: stage })
    stage-info
      (and 
        (not (get is-released stage-info))
        (>= stacks-block-height (get release-date stage-info))
      )
    false
  )
)

(define-read-only (get-total-escrow-progress (film-id uint))
  (match (map-get? escrow-accounts { film-id: film-id })
    escrow
      (if (> (get total-amount escrow) u0)
        (/ (* (get released-amount escrow) u10000) (get total-amount escrow))
        u0)
    u0
  )
)

(define-constant err-reputation-not-found (err u300))
(define-constant err-film-not-completed (err u301))
(define-constant err-reputation-already-updated (err u302))
(define-constant err-invalid-score (err u303))

(define-map creator-reputation
  { creator: principal }
  {
    total-films: uint,
    successful-films: uint,
    on-time-deliveries: uint,
    total-funds-raised: uint,
    average-rating: uint,
    reputation-score: uint,
    last-updated: uint
  }
)

(define-map film-completion-status
  { film-id: uint }
  {
    is-project-completed: bool,
    completion-date: uint,
    was-on-time: bool,
    final-rating: uint,
    reputation-updated: bool
  }
)

(define-public (initialize-creator-reputation (creator principal))
  (let ((reputation-exists (is-some (map-get? creator-reputation { creator: creator }))))
    (if reputation-exists
      (ok false)
      (begin
        (map-set creator-reputation
          { creator: creator }
          {
            total-films: u0,
            successful-films: u0,
            on-time-deliveries: u0,
            total-funds-raised: u0,
            average-rating: u0,
            reputation-score: u5000,
            last-updated: stacks-block-height
          }
        )
        (ok true)
      )
    )
  )
)

(define-public (finalize-film-project (film-id uint) (final-rating uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (release-status (unwrap! (map-get? film-release-status { film-id: film-id }) err-not-released))
        (completion-exists (is-some (map-get? film-completion-status { film-id: film-id }))))
    
    (asserts! (is-eq tx-sender (get creator film)) err-unauthorized)
    (asserts! (not (get is-active film)) err-funding-active)
    (asserts! (get is-funded film) err-min-funding-not-met)
    (asserts! (get is-released release-status) err-not-released)
    (asserts! (not completion-exists) err-film-not-completed)
    (asserts! (and (>= final-rating u0) (<= final-rating u500)) err-invalid-score)
    
    (let ((deadline (get deadline film))
          (release-date (get release-date release-status))
          (was-on-time (<= release-date deadline)))
      
      (map-set film-completion-status
        { film-id: film-id }
        {
          is-project-completed: true,
          completion-date: stacks-block-height,
          was-on-time: was-on-time,
          final-rating: final-rating,
          reputation-updated: false
        }
      )
      
      (ok true)
    )
  )
)

(define-public (update-creator-reputation (film-id uint))
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (completion-status (unwrap! (map-get? film-completion-status { film-id: film-id }) err-film-not-completed))
        (creator (get creator film))
        (current-reputation (default-to 
          { total-films: u0, successful-films: u0, on-time-deliveries: u0, total-funds-raised: u0, average-rating: u0, reputation-score: u5000, last-updated: u0 }
          (map-get? creator-reputation { creator: creator }))))
    
    (asserts! (get is-project-completed completion-status) err-film-not-completed)
    (asserts! (not (get reputation-updated completion-status)) err-reputation-already-updated)
    (asserts! (get is-funded film) err-min-funding-not-met)
    
    (let ((new-total-films (+ (get total-films current-reputation) u1))
          (new-successful-films (+ (get successful-films current-reputation) u1))
          (new-on-time-deliveries (+ (get on-time-deliveries current-reputation) (if (get was-on-time completion-status) u1 u0)))
          (new-total-funds-raised (+ (get total-funds-raised current-reputation) (get total-raised film)))
          (final-rating (get final-rating completion-status))
          (current-avg-rating (get average-rating current-reputation))
          (new-average-rating (if (> (get total-films current-reputation) u0)
                                (/ (+ (* current-avg-rating (get total-films current-reputation)) final-rating) new-total-films)
                                final-rating))
          (success-rate (/ (* new-successful-films u10000) new-total-films))
          (on-time-rate (/ (* new-on-time-deliveries u10000) new-total-films))
          (rating-score (/ (* new-average-rating u2000) u500))
          (new-reputation-score (/ (+ success-rate on-time-rate rating-score) u3)))
      
      (map-set creator-reputation
        { creator: creator }
        {
          total-films: new-total-films,
          successful-films: new-successful-films,
          on-time-deliveries: new-on-time-deliveries,
          total-funds-raised: new-total-funds-raised,
          average-rating: new-average-rating,
          reputation-score: new-reputation-score,
          last-updated: stacks-block-height
        }
      )
      
      (map-set film-completion-status
        { film-id: film-id }
        (merge completion-status { reputation-updated: true })
      )
      
      (ok new-reputation-score)
    )
  )
)

(define-read-only (get-creator-reputation (creator principal))
  (map-get? creator-reputation { creator: creator })
)

(define-read-only (get-film-completion-status (film-id uint))
  (map-get? film-completion-status { film-id: film-id })
)

(define-read-only (get-creator-success-rate (creator principal))
  (match (map-get? creator-reputation { creator: creator })
    reputation
      (if (> (get total-films reputation) u0)
        (/ (* (get successful-films reputation) u10000) (get total-films reputation))
        u0)
    u0
  )
)

(define-read-only (get-creator-on-time-rate (creator principal))
  (match (map-get? creator-reputation { creator: creator })
    reputation
      (if (> (get total-films reputation) u0)
        (/ (* (get on-time-deliveries reputation) u10000) (get total-films reputation))
        u0)
    u0
  )
)

(define-read-only (is-creator-reliable (creator principal))
  (match (map-get? creator-reputation { creator: creator })
    reputation
      (and 
        (>= (get reputation-score reputation) u7000)
        (>= (get total-films reputation) u3)
      )
    false
  )
)

;; Distribution Partnership & Revenue Tracking System
(define-constant err-partner-not-found (err u400))
(define-constant err-partner-already-exists (err u401))
(define-constant err-deal-not-found (err u402))
(define-constant err-deal-already-exists (err u403))
(define-constant err-invalid-percentage (err u404))
(define-constant err-channel-not-found (err u405))
(define-constant err-unauthorized-partner (err u406))
(define-constant err-deal-expired (err u407))
(define-constant err-insufficient-revenue (err u408))

;; Distribution partners registry
(define-map distribution-partners
  { partner-id: uint }
  {
    name: (string-ascii 100),
    partner-address: principal,
    reputation-score: uint,
    total-deals: uint,
    total-revenue-generated: uint,
    average-performance: uint,
    is-verified: bool,
    registration-date: uint
  }
)

;; Film distribution deals
(define-map film-distribution-deals
  { film-id: uint, partner-id: uint }
  {
    deal-type: (string-ascii 50), ;; "streaming", "theatrical", "merchandise", "licensing"
    partner-revenue-share: uint, ;; basis points (0-10000)
    backer-revenue-share: uint, ;; basis points (0-10000)
    creator-revenue-share: uint, ;; basis points (0-10000)
    minimum-guarantee: uint,
    deal-duration: uint,
    territory: (string-ascii 50), ;; "global", "us", "eu", etc
    is-active: bool,
    deal-start-date: uint,
    total-revenue-earned: uint
  }
)

;; Revenue channels tracking
(define-map revenue-channels
  { film-id: uint, partner-id: uint, channel-id: uint }
  {
    channel-name: (string-ascii 100),
    revenue-type: (string-ascii 50),
    total-revenue: uint,
    revenue-this-period: uint,
    last-updated: uint,
    performance-metrics: uint
  }
)

;; Revenue distribution tracking
(define-map revenue-distributions
  { film-id: uint, partner-id: uint, distribution-id: uint }
  {
    revenue-amount: uint,
    partner-amount: uint,
    backer-pool-amount: uint,
    creator-amount: uint,
    distribution-date: uint,
    channel-source: uint
  }
)

;; Partner performance metrics
(define-map partner-performance
  { partner-id: uint }
  {
    total-films-distributed: uint,
    average-revenue-per-film: uint,
    on-time-payments: uint,
    total-payments: uint,
    reliability-score: uint,
    last-performance-update: uint
  }
)

;; Register a new distribution partner
(define-public (register-distribution-partner (partner-id uint) (name (string-ascii 100)) (partner-address principal))
  (let ((partner-exists (is-some (map-get? distribution-partners { partner-id: partner-id }))))
    (asserts! (not partner-exists) err-partner-already-exists)
    (asserts! (> (len name) u0) err-invalid-percentage)
    
    (map-set distribution-partners
      { partner-id: partner-id }
      {
        name: name,
        partner-address: partner-address,
        reputation-score: u5000, ;; start with neutral score
        total-deals: u0,
        total-revenue-generated: u0,
        average-performance: u0,
        is-verified: false,
        registration-date: stacks-block-height
      }
    )
    
    (map-set partner-performance
      { partner-id: partner-id }
      {
        total-films-distributed: u0,
        average-revenue-per-film: u0,
        on-time-payments: u0,
        total-payments: u0,
        reliability-score: u5000,
        last-performance-update: stacks-block-height
      }
    )
    
    (ok partner-id)
  )
)

;; Create distribution deal between film and partner
(define-public (create-distribution-deal 
  (film-id uint) 
  (partner-id uint) 
  (deal-type (string-ascii 50))
  (partner-share uint)
  (backer-share uint)
  (creator-share uint)
  (minimum-guarantee uint)
  (deal-duration uint)
  (territory (string-ascii 50)))
  
  (let ((film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (partner (unwrap! (map-get? distribution-partners { partner-id: partner-id }) err-partner-not-found))
        (deal-exists (is-some (map-get? film-distribution-deals { film-id: film-id, partner-id: partner-id }))))
    
    ;; Only film creator can create deals
    (asserts! (is-eq tx-sender (get creator film)) err-unauthorized)
    (asserts! (not deal-exists) err-deal-already-exists)
    (asserts! (get is-funded film) err-min-funding-not-met)
    
    ;; Revenue shares must total 10000 basis points (100%)
    (asserts! (is-eq (+ partner-share (+ backer-share creator-share)) u10000) err-invalid-percentage)
    (asserts! (> deal-duration stacks-block-height) err-deal-expired)
    
    (map-set film-distribution-deals
      { film-id: film-id, partner-id: partner-id }
      {
        deal-type: deal-type,
        partner-revenue-share: partner-share,
        backer-revenue-share: backer-share,
        creator-revenue-share: creator-share,
        minimum-guarantee: minimum-guarantee,
        deal-duration: deal-duration,
        territory: territory,
        is-active: true,
        deal-start-date: stacks-block-height,
        total-revenue-earned: u0
      }
    )
    
    ;; Update partner statistics
    (let ((updated-partner (merge partner { total-deals: (+ (get total-deals partner) u1) })))
      (map-set distribution-partners
        { partner-id: partner-id }
        updated-partner
      )
    )
    
    (ok true)
  )
)

;; Record revenue from a specific channel
(define-public (record-channel-revenue 
  (film-id uint) 
  (partner-id uint) 
  (channel-id uint)
  (channel-name (string-ascii 100))
  (revenue-type (string-ascii 50))
  (revenue-amount uint))
  
  (let ((deal (unwrap! (map-get? film-distribution-deals { film-id: film-id, partner-id: partner-id }) err-deal-not-found))
        (partner (unwrap! (map-get? distribution-partners { partner-id: partner-id }) err-partner-not-found))
        (existing-channel (default-to 
          { channel-name: "", revenue-type: "", total-revenue: u0, revenue-this-period: u0, last-updated: u0, performance-metrics: u0 }
          (map-get? revenue-channels { film-id: film-id, partner-id: partner-id, channel-id: channel-id }))))
    
    ;; Only partner can record revenue
    (asserts! (is-eq tx-sender (get partner-address partner)) err-unauthorized-partner)
    (asserts! (get is-active deal) err-deal-expired)
    (asserts! (< stacks-block-height (get deal-duration deal)) err-deal-expired)
    (asserts! (> revenue-amount u0) err-insufficient-revenue)
    
    ;; Update channel revenue
    (map-set revenue-channels
      { film-id: film-id, partner-id: partner-id, channel-id: channel-id }
      {
        channel-name: channel-name,
        revenue-type: revenue-type,
        total-revenue: (+ (get total-revenue existing-channel) revenue-amount),
        revenue-this-period: revenue-amount,
        last-updated: stacks-block-height,
        performance-metrics: (/ (* revenue-amount u100) (+ (get total-revenue existing-channel) revenue-amount))
      }
    )
    
    ;; Update deal total revenue
    (map-set film-distribution-deals
      { film-id: film-id, partner-id: partner-id }
      (merge deal { total-revenue-earned: (+ (get total-revenue-earned deal) revenue-amount) })
    )
    
    (ok revenue-amount)
  )
)

;; Distribute revenue to backers, creator, and partner
(define-public (distribute-channel-revenue (film-id uint) (partner-id uint) (distribution-id uint) (revenue-amount uint))
  (let ((deal (unwrap! (map-get? film-distribution-deals { film-id: film-id, partner-id: partner-id }) err-deal-not-found))
        (film (unwrap! (map-get? films { film-id: film-id }) err-not-found))
        (partner (unwrap! (map-get? distribution-partners { partner-id: partner-id }) err-partner-not-found)))
    
    ;; Only partner can initiate distribution
    (asserts! (is-eq tx-sender (get partner-address partner)) err-unauthorized-partner)
    (asserts! (get is-active deal) err-deal-expired)
    (asserts! (> revenue-amount u0) err-insufficient-revenue)
    
    (let ((partner-amount (/ (* revenue-amount (get partner-revenue-share deal)) u10000))
          (backer-amount (/ (* revenue-amount (get backer-revenue-share deal)) u10000))
          (creator-amount (/ (* revenue-amount (get creator-revenue-share deal)) u10000)))
      
      ;; Transfer funds (simplified - in real implementation would need escrow)
      (try! (stx-transfer? backer-amount tx-sender (as-contract tx-sender))) ;; to backer pool
      (try! (stx-transfer? creator-amount tx-sender (get creator film)))
      
      ;; Record distribution
      (map-set revenue-distributions
        { film-id: film-id, partner-id: partner-id, distribution-id: distribution-id }
        {
          revenue-amount: revenue-amount,
          partner-amount: partner-amount,
          backer-pool-amount: backer-amount,
          creator-amount: creator-amount,
          distribution-date: stacks-block-height,
          channel-source: u1 ;; could be dynamic based on channel
        }
      )
      
      ;; Update partner performance
      (let ((performance (default-to 
            { total-films-distributed: u0, average-revenue-per-film: u0, on-time-payments: u0, total-payments: u0, reliability-score: u5000, last-performance-update: u0 }
            (map-get? partner-performance { partner-id: partner-id }))))
        
        (map-set partner-performance
          { partner-id: partner-id }
          {
            total-films-distributed: (get total-films-distributed performance),
            average-revenue-per-film: (if (> (get total-films-distributed performance) u0)
                                        (/ (+ (* (get average-revenue-per-film performance) (get total-films-distributed performance)) revenue-amount)
                                           (+ (get total-films-distributed performance) u1))
                                        revenue-amount),
            on-time-payments: (+ (get on-time-payments performance) u1),
            total-payments: (+ (get total-payments performance) u1),
            reliability-score: (if (> (+ (get reliability-score performance) u100) u10000) u10000 (+ (get reliability-score performance) u100)),
            last-performance-update: stacks-block-height
          }
        )
      )
      
      (ok { partner-amount: partner-amount, backer-amount: backer-amount, creator-amount: creator-amount })
    )
  )
)

;; Verify distribution partner (admin function)
(define-public (verify-distribution-partner (partner-id uint))
  (let ((partner (unwrap! (map-get? distribution-partners { partner-id: partner-id }) err-partner-not-found)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    
    (map-set distribution-partners
      { partner-id: partner-id }
      (merge partner { is-verified: true })
    )
    
    (ok true)
  )
)

;; Read-only functions for distribution system
(define-read-only (get-distribution-partner (partner-id uint))
  (map-get? distribution-partners { partner-id: partner-id })
)

(define-read-only (get-distribution-deal (film-id uint) (partner-id uint))
  (map-get? film-distribution-deals { film-id: film-id, partner-id: partner-id })
)

(define-read-only (get-channel-revenue (film-id uint) (partner-id uint) (channel-id uint))
  (map-get? revenue-channels { film-id: film-id, partner-id: partner-id, channel-id: channel-id })
)

(define-read-only (get-revenue-distribution (film-id uint) (partner-id uint) (distribution-id uint))
  (map-get? revenue-distributions { film-id: film-id, partner-id: partner-id, distribution-id: distribution-id })
)

(define-read-only (get-partner-performance (partner-id uint))
  (map-get? partner-performance { partner-id: partner-id })
)

(define-read-only (get-film-total-distribution-revenue (film-id uint))
  (ok u0) ;; placeholder - would iterate through all deals for film
)

(define-read-only (is-partner-reliable (partner-id uint))
  (match (map-get? partner-performance { partner-id: partner-id })
    performance
      (and 
        (>= (get reliability-score performance) u7000)
        (>= (get total-films-distributed performance) u3)
        (>= (/ (* (get on-time-payments performance) u10000) (get total-payments performance)) u8000)
      )
    false
  )
)




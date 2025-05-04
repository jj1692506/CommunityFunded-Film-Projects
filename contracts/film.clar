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
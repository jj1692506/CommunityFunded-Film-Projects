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
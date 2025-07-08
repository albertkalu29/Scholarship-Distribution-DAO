(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_VOTING_CLOSED (err u105))
(define-constant ERR_ALREADY_VOTED (err u106))
(define-constant ERR_NOT_COMMITTEE (err u107))
(define-constant ERR_INVALID_STATUS (err u108))

(define-data-var next-application-id uint u1)
(define-data-var committee-size uint u0)
(define-data-var voting-period uint u1440)
(define-data-var min-votes-required uint u3)


(define-constant ERR_APPEAL_WINDOW_CLOSED (err u109))
(define-constant ERR_APPEAL_EXISTS (err u110))
(define-constant ERR_NOT_REJECTED (err u111))
(define-constant ERR_INVALID_APPEAL (err u112))

(define-data-var appeal-window uint u720)
(define-data-var appeal-threshold-multiplier uint u2)

(define-map committee-members principal bool)
(define-map applications uint {
    applicant: principal,
    amount: uint,
    description: (string-ascii 500),
    status: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    voting-ends-at: uint
})
(define-map votes {application-id: uint, voter: principal} bool)
(define-map applicant-history principal (list 10 uint))

(define-public (initialize-dao (initial-committee (list 10 principal)) (min-votes uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set min-votes-required min-votes)
        (var-set committee-size (len initial-committee))
        (ok (map add-committee-member initial-committee))
    )
)

(define-private (add-committee-member (member principal))
    (map-set committee-members member true)
)

(define-public (add-committee-member-public (member principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set committee-members member true)
        (var-set committee-size (+ (var-get committee-size) u1))
        (ok true)
    )
)

(define-public (remove-committee-member (member principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-delete committee-members member)
        (var-set committee-size (- (var-get committee-size) u1))
        (ok true)
    )
)

(define-public (submit-application (amount uint) (description (string-ascii 500)))
    (let (
        (application-id (var-get next-application-id))
        (current-block stacks-block-height)
        (voting-end (+ current-block (var-get voting-period)))
    )
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (map-set applications application-id {
            applicant: tx-sender,
            amount: amount,
            description: description,
            status: "pending",
            votes-for: u0,
            votes-against: u0,
            created-at: current-block,
            voting-ends-at: voting-end
        })
        (match (map-get? applicant-history tx-sender)
            existing-history (map-set applicant-history tx-sender (unwrap! (as-max-len? (append existing-history application-id) u10) ERR_INVALID_AMOUNT))
            (map-set applicant-history tx-sender (list application-id))
        )
        (var-set next-application-id (+ application-id u1))
        (ok application-id)
    )
)

(define-public (vote-on-application (application-id uint) (vote-for bool))
    (let (
        (application (unwrap! (map-get? applications application-id) ERR_NOT_FOUND))
        (voter-key {application-id: application-id, voter: tx-sender})
        (current-block stacks-block-height)
    )
        (asserts! (default-to false (map-get? committee-members tx-sender)) ERR_NOT_COMMITTEE)
        (asserts! (is-eq (get status application) "pending") ERR_VOTING_CLOSED)
        (asserts! (< current-block (get voting-ends-at application)) ERR_VOTING_CLOSED)
        (asserts! (is-none (map-get? votes voter-key)) ERR_ALREADY_VOTED)
        
        (map-set votes voter-key vote-for)
        
        (if vote-for
            (map-set applications application-id (merge application {votes-for: (+ (get votes-for application) u1)}))
            (map-set applications application-id (merge application {votes-against: (+ (get votes-against application) u1)}))
        )
        (ok true)
    )
)

(define-public (finalize-application (application-id uint))
    (let (
        (application (unwrap! (map-get? applications application-id) ERR_NOT_FOUND))
        (current-block stacks-block-height)
        (total-votes (+ (get votes-for application) (get votes-against application)))
    )
        (asserts! (is-eq (get status application) "pending") ERR_INVALID_STATUS)
        (asserts! (>= current-block (get voting-ends-at application)) ERR_VOTING_CLOSED)
        (asserts! (>= total-votes (var-get min-votes-required)) ERR_INSUFFICIENT_FUNDS)
        
        (if (> (get votes-for application) (get votes-against application))
            (begin
                (map-set applications application-id (merge application {status: "approved"}))
                (ok "approved")
            )
            (begin
                (map-set applications application-id (merge application {status: "rejected"}))
                (ok "rejected")
            )
        )
    )
)

(define-public (distribute-scholarship (application-id uint))
    (let (
        (application (unwrap! (map-get? applications application-id) ERR_NOT_FOUND))
        (contract-balance (stx-get-balance (as-contract tx-sender)))
    )
        (asserts! (default-to false (map-get? committee-members tx-sender)) ERR_NOT_COMMITTEE)
        (asserts! (is-eq (get status application) "approved") ERR_INVALID_STATUS)
        (asserts! (>= contract-balance (get amount application)) ERR_INSUFFICIENT_FUNDS)
        
        (try! (as-contract (stx-transfer? (get amount application) tx-sender (get applicant application))))
        (map-set applications application-id (merge application {status: "distributed"}))
        (ok true)
    )
)

(define-public (fund-dao)
    (let (
        (amount (stx-get-balance tx-sender))
    )
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (stx-transfer? amount tx-sender (as-contract tx-sender))
    )
)

(define-public (emergency-withdraw (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER))
    )
)

(define-read-only (get-application (application-id uint))
    (map-get? applications application-id)
)

(define-read-only (get-applicant-history (applicant principal))
    (default-to (list) (map-get? applicant-history applicant))
)

(define-read-only (is-committee-member (member principal))
    (default-to false (map-get? committee-members member))
)

(define-read-only (get-vote (application-id uint) (voter principal))
    (map-get? votes {application-id: application-id, voter: voter})
)

(define-read-only (get-dao-balance)
    (stx-get-balance (as-contract tx-sender))
)

(define-read-only (get-dao-info)
    {
        committee-size: (var-get committee-size),
        voting-period: (var-get voting-period),
        min-votes-required: (var-get min-votes-required),
        next-application-id: (var-get next-application-id),
        dao-balance: (stx-get-balance (as-contract tx-sender))
    }
)

(define-public (update-voting-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set voting-period new-period)
        (ok true)
    )
)

(define-public (update-min-votes (new-min uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set min-votes-required new-min)
        (ok true)
    )
)

(define-read-only (can-vote (application-id uint) (voter principal))
    (let (
        (application (map-get? applications application-id))
        (current-block stacks-block-height)
    )
        (match application
            app (and 
                (default-to false (map-get? committee-members voter))
                (is-eq (get status app) "pending")
                (< current-block (get voting-ends-at app))
                (is-none (map-get? votes {application-id: application-id, voter: voter}))
            )
            false
        )
    )
)

(define-read-only (get-application-status (application-id uint))
    (match (map-get? applications application-id)
        application (get status application)
        "not-found"
    )
)


(define-map appeals uint {
    application-id: uint,
    appellant: principal,
    reason: (string-ascii 300),
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    voting-ends-at: uint,
    status: (string-ascii 20)
})

(define-map appeal-votes {appeal-id: uint, voter: principal} bool)

(define-public (submit-appeal (application-id uint) (reason (string-ascii 300)))
    (let (
        (application (unwrap! (map-get? applications application-id) ERR_NOT_FOUND))
        (current-block stacks-block-height)
        (appeal-deadline (+ (get voting-ends-at application) (var-get appeal-window)))
    )
        (asserts! (is-eq (get status application) "rejected") ERR_NOT_REJECTED)
        (asserts! (is-eq (get applicant application) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (< current-block appeal-deadline) ERR_APPEAL_WINDOW_CLOSED)
        (asserts! (is-none (map-get? appeals application-id)) ERR_APPEAL_EXISTS)
        
        (map-set appeals application-id {
            application-id: application-id,
            appellant: tx-sender,
            reason: reason,
            votes-for: u0,
            votes-against: u0,
            created-at: current-block,
            voting-ends-at: (+ current-block (var-get voting-period)),
            status: "pending"
        })
        (ok true)
    )
)

(define-public (vote-on-appeal (application-id uint) (vote-for bool))
    (let (
        (appeal (unwrap! (map-get? appeals application-id) ERR_NOT_FOUND))
        (voter-key {appeal-id: application-id, voter: tx-sender})
        (current-block stacks-block-height)
    )
        (asserts! (default-to false (map-get? committee-members tx-sender)) ERR_NOT_COMMITTEE)
        (asserts! (is-eq (get status appeal) "pending") ERR_VOTING_CLOSED)
        (asserts! (< current-block (get voting-ends-at appeal)) ERR_VOTING_CLOSED)
        (asserts! (is-none (map-get? appeal-votes voter-key)) ERR_ALREADY_VOTED)
        
        (map-set appeal-votes voter-key vote-for)
        
        (if vote-for
            (map-set appeals application-id (merge appeal {votes-for: (+ (get votes-for appeal) u1)}))
            (map-set appeals application-id (merge appeal {votes-against: (+ (get votes-against appeal) u1)}))
        )
        (ok true)
    )
)

(define-public (finalize-appeal (application-id uint))
    (let (
        (appeal (unwrap! (map-get? appeals application-id) ERR_NOT_FOUND))
        (application (unwrap! (map-get? applications application-id) ERR_NOT_FOUND))
        (current-block stacks-block-height)
        (required-votes (* (var-get min-votes-required) (var-get appeal-threshold-multiplier)))
    )
        (asserts! (is-eq (get status appeal) "pending") ERR_INVALID_STATUS)
        (asserts! (>= current-block (get voting-ends-at appeal)) ERR_VOTING_CLOSED)
        (asserts! (>= (get votes-for appeal) required-votes) ERR_INSUFFICIENT_FUNDS)
        
        (if (> (get votes-for appeal) (get votes-against appeal))
            (begin
                (map-set appeals application-id (merge appeal {status: "approved"}))
                (map-set applications application-id (merge application {status: "approved"}))
                (ok "appeal-approved")
            )
            (begin
                (map-set appeals application-id (merge appeal {status: "rejected"}))
                (ok "appeal-rejected")
            )
        )
    )
)

(define-read-only (get-appeal (application-id uint))
    (map-get? appeals application-id)
)

(define-read-only (can-appeal (application-id uint) (appellant principal))
    (match (map-get? applications application-id)
        application 
        (and 
            (is-eq (get status application) "rejected")
            (is-eq (get applicant application) appellant)
            (< stacks-block-height (+ (get voting-ends-at application) (var-get appeal-window)))
            (is-none (map-get? appeals application-id))
        )
        false
    )
)

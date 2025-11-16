(define-constant ERR_MILESTONE_NOT_FOUND (err u200))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u201))
(define-constant ERR_INVALID_MILESTONE_ORDER (err u202))
(define-constant ERR_APPLICATION_NOT_APPROVED (err u203))
(define-constant ERR_INSUFFICIENT_MILESTONE_VOTES (err u204))

(define-map milestone-plans uint {
    application-id: uint,
    total-milestones: uint,
    milestone-amount: uint,
    current-milestone: uint,
    total-disbursed: uint
})

(define-map milestones {application-id: uint, milestone-id: uint} {
    description: (string-ascii 200),
    amount: uint,
    completed: bool,
    votes-for: uint,
    votes-against: uint,
    created-at: uint
})

(define-map milestone-votes {application-id: uint, milestone-id: uint, voter: principal} bool)

(define-public (create-milestone-plan (application-id uint) (milestone-count uint) (milestone-descriptions (list 5 (string-ascii 200))))
    (let (
        (application (unwrap! (contract-call? .Scholarship-Distribution get-application application-id) ERR_APPLICATION_NOT_APPROVED))
        (total-amount (get amount application))
        (milestone-amount (/ total-amount milestone-count))
    )
        (asserts! (is-eq (get status application) "approved") ERR_APPLICATION_NOT_APPROVED)
        (asserts! (is-eq (get applicant application) tx-sender) (err u100))
        (asserts! (<= milestone-count u5) (err u103))
        
        (map-set milestone-plans application-id {
            application-id: application-id,
            total-milestones: milestone-count,
            milestone-amount: milestone-amount,
            current-milestone: u1,
            total-disbursed: u0
        })
        
        (ok (fold create-milestone-entry milestone-descriptions {application-id: application-id, milestone-id: u1, milestone-amount: milestone-amount}))
    )
)

(define-private (create-milestone-entry (description (string-ascii 200)) (context {application-id: uint, milestone-id: uint, milestone-amount: uint}))
    (begin
        (map-set milestones 
            {application-id: (get application-id context), milestone-id: (get milestone-id context)}
            {
                description: description,
                amount: (get milestone-amount context),
                completed: false,
                votes-for: u0,
                votes-against: u0,
                created-at: stacks-block-height
            }
        )
        (merge context {milestone-id: (+ (get milestone-id context) u1)})
    )
)

(define-public (vote-milestone-completion (application-id uint) (milestone-id uint) (approve bool))
    (let (
        (milestone (unwrap! (map-get? milestones {application-id: application-id, milestone-id: milestone-id}) ERR_MILESTONE_NOT_FOUND))
        (voter-key {application-id: application-id, milestone-id: milestone-id, voter: tx-sender})
        (is-committee (contract-call? .Scholarship-Distribution is-committee-member tx-sender))
    )
        (asserts! is-committee (err u107))
        (asserts! (not (get completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
        (asserts! (is-none (map-get? milestone-votes voter-key)) (err u106))
        
        (map-set milestone-votes voter-key approve)
        
        (if approve
            (map-set milestones {application-id: application-id, milestone-id: milestone-id}
                (merge milestone {votes-for: (+ (get votes-for milestone) u1)}))
            (map-set milestones {application-id: application-id, milestone-id: milestone-id}
                (merge milestone {votes-against: (+ (get votes-against milestone) u1)}))
        )
        (ok true)
    )
)

(define-public (complete-milestone (application-id uint) (milestone-id uint))
    (let (
        (milestone (unwrap! (map-get? milestones {application-id: application-id, milestone-id: milestone-id}) ERR_MILESTONE_NOT_FOUND))
        (plan (unwrap! (map-get? milestone-plans application-id) ERR_MILESTONE_NOT_FOUND))
        (dao-info (contract-call? .Scholarship-Distribution get-dao-info))
    )
        (asserts! (not (get completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
        (asserts! (is-eq milestone-id (get current-milestone plan)) ERR_INVALID_MILESTONE_ORDER)
        (asserts! (>= (get votes-for milestone) (get min-votes-required dao-info)) ERR_INSUFFICIENT_MILESTONE_VOTES)
        (asserts! (> (get votes-for milestone) (get votes-against milestone)) ERR_INSUFFICIENT_MILESTONE_VOTES)
        
        (map-set milestones {application-id: application-id, milestone-id: milestone-id}
            (merge milestone {completed: true}))
        
        (map-set milestone-plans application-id
            (merge plan {
                current-milestone: (+ (get current-milestone plan) u1),
                total-disbursed: (+ (get total-disbursed plan) (get amount milestone))
            }))
        
        (ok true)
    )
)

(define-read-only (get-milestone-plan (application-id uint))
    (map-get? milestone-plans application-id)
)

(define-read-only (get-milestone (application-id uint) (milestone-id uint))
    (map-get? milestones {application-id: application-id, milestone-id: milestone-id})
)

(define-read-only (get-milestone-progress (application-id uint))
    (match (map-get? milestone-plans application-id)
        plan (some {
            total-milestones: (get total-milestones plan),
            current-milestone: (get current-milestone plan),
            completion-percentage: (/ (* (- (get current-milestone plan) u1) u100) (get total-milestones plan)),
            total-disbursed: (get total-disbursed plan)
        })
        none
    )
)
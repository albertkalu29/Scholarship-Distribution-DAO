(define-constant ERR_NO_PERFORMANCE_DATA (err u300))
(define-constant ERR_INVALID_SCORE (err u301))

(define-map student-performance principal {
    applications-count: uint,
    scholarships-received: uint,
    total-milestones: uint,
    completed-milestones: uint,
    on-time-completions: uint,
    late-completions: uint,
    average-completion-time: uint,
    reputation-score: uint,
    last-updated: uint
})

(define-map application-performance uint {
    student: principal,
    completion-rate: uint,
    avg-milestone-time: uint,
    quality-score: uint,
    total-amount: uint
})

(define-map milestone-analytics {application-id: uint, milestone-id: uint} {
    completion-time: uint,
    expected-time: uint,
    quality-rating: uint
})

(define-public (record-application-performance (application-id uint))
    (let (
        (application (unwrap! (contract-call? .Scholarship-Distribution get-application application-id) ERR_NO_PERFORMANCE_DATA))
        (student (get applicant application))
        (current-performance (default-to {
            applications-count: u0,
            scholarships-received: u0,
            total-milestones: u0,
            completed-milestones: u0,
            on-time-completions: u0,
            late-completions: u0,
            average-completion-time: u0,
            reputation-score: u500,
            last-updated: u0
        } (map-get? student-performance student)))
    )
        (asserts! (is-eq (get status application) "distributed") ERR_NO_PERFORMANCE_DATA)
        (map-set student-performance student (merge current-performance {
            applications-count: (+ (get applications-count current-performance) u1),
            scholarships-received: (+ (get scholarships-received current-performance) u1),
            last-updated: stacks-block-height
        }))
        (ok true)
    )
)

(define-public (update-milestone-performance (application-id uint) (milestone-id uint) (quality-rating uint))
    (let (
        (milestone (unwrap! (contract-call? .milestone-disbursement get-milestone application-id milestone-id) ERR_NO_PERFORMANCE_DATA))
        (application (unwrap! (contract-call? .Scholarship-Distribution get-application application-id) ERR_NO_PERFORMANCE_DATA))
        (student (get applicant application))
        (completion-time (- stacks-block-height (get created-at milestone)))
        (expected-time u1440)
        (is-on-time (<= completion-time expected-time))
    )
        (asserts! (<= quality-rating u1000) ERR_INVALID_SCORE)
        (asserts! (get completed milestone) ERR_NO_PERFORMANCE_DATA)
        
        (map-set milestone-analytics {application-id: application-id, milestone-id: milestone-id} {
            completion-time: completion-time,
            expected-time: expected-time,
            quality-rating: quality-rating
        })
        
        (match (map-get? student-performance student)
            performance (map-set student-performance student (merge performance {
                total-milestones: (+ (get total-milestones performance) u1),
                completed-milestones: (+ (get completed-milestones performance) u1),
                on-time-completions: (if is-on-time (+ (get on-time-completions performance) u1) (get on-time-completions performance)),
                late-completions: (if (not is-on-time) (+ (get late-completions performance) u1) (get late-completions performance)),
                reputation-score: (calculate-reputation-score performance quality-rating is-on-time),
                last-updated: stacks-block-height
            }))
            false
        )
        (ok true)
    )
)

(define-private (calculate-reputation-score (performance {applications-count: uint, scholarships-received: uint, total-milestones: uint, completed-milestones: uint, on-time-completions: uint, late-completions: uint, average-completion-time: uint, reputation-score: uint, last-updated: uint}) (quality uint) (on-time bool))
    (let (
        (completion-rate (if (> (get total-milestones performance) u0)
            (/ (* (get completed-milestones performance) u100) (get total-milestones performance))
            u100))
        (timeliness-score (if (> (+ (get on-time-completions performance) (get late-completions performance)) u0)
            (/ (* (get on-time-completions performance) u100) (+ (get on-time-completions performance) (get late-completions performance)))
            u100))
        (base-score (/ (+ (* completion-rate u4) (* timeliness-score u3) (* quality u3)) u10))
    )
        (if (> base-score u1000) u1000 (if (< base-score u0) u0 base-score))
    )
)

(define-read-only (get-student-performance (student principal))
    (map-get? student-performance student)
)

(define-read-only (get-reputation-score (student principal))
    (match (map-get? student-performance student)
        performance (get reputation-score performance)
        u500
    )
)

(define-read-only (get-application-analytics (application-id uint))
    (map-get? application-performance application-id)
)

(define-read-only (get-milestone-analytics (application-id uint) (milestone-id uint))
    (map-get? milestone-analytics {application-id: application-id, milestone-id: milestone-id})
)

(define-read-only (get-top-performers (limit uint))
    (ok "feature-requires-off-chain-indexing")
)

(define-read-only (calculate-success-rate (student principal))
    (match (map-get? student-performance student)
        performance (if (> (get applications-count performance) u0)
            (/ (* (get scholarships-received performance) u100) (get applications-count performance))
            u0)
        u0
    )
)

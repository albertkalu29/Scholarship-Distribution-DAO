(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_INVALID_SCORE (err u301))
(define-constant ERR_NO_DATA (err u302))

(define-data-var total-scholarships-tracked uint u0)
(define-data-var total-successful-outcomes uint u0)

(define-map recipient-reputation principal {
    total-received: uint,
    successful-completions: uint,
    reputation-score: uint,
    last-updated: uint
})

(define-map committee-performance principal {
    total-votes: uint,
    accurate-votes: uint,
    accuracy-score: uint,
    last-vote-block: uint
})

(define-map application-outcomes uint {
    final-outcome: (string-ascii 20),
    success-rating: uint,
    completion-time: uint,
    recorded-at: uint
})

(define-public (record-scholarship-outcome (application-id uint) (recipient principal) (success-rating uint))
    (let (
        (current-rep (default-to {total-received: u0, successful-completions: u0, reputation-score: u50, last-updated: u0} 
                                 (map-get? recipient-reputation recipient)))
        (is-successful (>= success-rating u70))
        (new-completions (if is-successful (+ (get successful-completions current-rep) u1) (get successful-completions current-rep)))
        (new-total (+ (get total-received current-rep) u1))
        (new-score (/ (* new-completions u100) new-total))
    )
        (asserts! (contract-call? .Scholarship-Distribution is-committee-member tx-sender) ERR_UNAUTHORIZED)
        (asserts! (<= success-rating u100) ERR_INVALID_SCORE)
        
        (map-set recipient-reputation recipient {
            total-received: new-total,
            successful-completions: new-completions,
            reputation-score: new-score,
            last-updated: stacks-block-height
        })
        
        (map-set application-outcomes application-id {
            final-outcome: (if is-successful "successful" "unsuccessful"),
            success-rating: success-rating,
            completion-time: stacks-block-height,
            recorded-at: stacks-block-height
        })
        
        (var-set total-scholarships-tracked (+ (var-get total-scholarships-tracked) u1))
        (if is-successful 
            (var-set total-successful-outcomes (+ (var-get total-successful-outcomes) u1))
            true)
        (ok true)
    )
)

(define-public (update-committee-accuracy (voter principal) (application-id uint) (was-accurate bool))
    (let (
        (current-perf (default-to {total-votes: u0, accurate-votes: u0, accuracy-score: u50, last-vote-block: u0} 
                                  (map-get? committee-performance voter)))
        (new-accurate (if was-accurate (+ (get accurate-votes current-perf) u1) (get accurate-votes current-perf)))
        (new-total (+ (get total-votes current-perf) u1))
        (new-accuracy (/ (* new-accurate u100) new-total))
    )
        (asserts! (contract-call? .Scholarship-Distribution is-committee-member tx-sender) ERR_UNAUTHORIZED)
        
        (map-set committee-performance voter {
            total-votes: new-total,
            accurate-votes: new-accurate,
            accuracy-score: new-accuracy,
            last-vote-block: stacks-block-height
        })
        (ok true)
    )
)

(define-read-only (get-recipient-reputation (recipient principal))
    (map-get? recipient-reputation recipient)
)

(define-read-only (get-committee-performance (member principal))
    (map-get? committee-performance member)
)

(define-read-only (get-application-outcome (application-id uint))
    (map-get? application-outcomes application-id)
)

(define-read-only (get-dao-analytics)
    (let (
        (total-tracked (var-get total-scholarships-tracked))
        (success-rate (if (> total-tracked u0) 
                         (/ (* (var-get total-successful-outcomes) u100) total-tracked) 
                         u0))
    )
        {
            total-scholarships: total-tracked,
            successful-outcomes: (var-get total-successful-outcomes),
            overall-success-rate: success-rate,
            last-updated: stacks-block-height
        }
    )
)

(define-read-only (calculate-application-risk-score (applicant principal))
    (match (map-get? recipient-reputation applicant)
        reputation 
        (let (
            (base-score (get reputation-score reputation))
            (raw-bonus (* (get total-received reputation) u5))
            (experience-bonus (if (> raw-bonus u20) u20 raw-bonus))
            (raw-final (+ base-score experience-bonus))
            (final-score (if (> raw-final u100) u100 raw-final))
        )
            final-score)
        u50
    )
)
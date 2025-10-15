(define-constant ERR_NOT_COMMITTEE (err u400))
(define-constant ERR_INVALID_REPUTATION (err u401))
(define-constant ERR_REPUTATION_LOCKED (err u402))

(define-data-var reputation-decay-rate uint u2)
(define-data-var max-reputation uint u1000)
(define-data-var min-reputation uint u10)

(define-map committee-reputation principal {
    reputation-score: uint,
    total-votes: uint,
    majority-votes: uint,
    minority-votes: uint,
    abstain-count: uint,
    last-vote-block: uint,
    streak-count: uint
})

(define-map weighted-votes {application-id: uint, voter: principal} uint)

(define-public (initialize-reputation (member principal))
    (let (
        (is-committee (contract-call? .Scholarship-Distribution is-committee-member member))
    )
        (asserts! is-committee ERR_NOT_COMMITTEE)
        (asserts! (is-none (map-get? committee-reputation member)) (err u102))
        (map-set committee-reputation member {
            reputation-score: u100,
            total-votes: u0,
            majority-votes: u0,
            minority-votes: u0,
            abstain-count: u0,
            last-vote-block: stacks-block-height,
            streak-count: u0
        })
        (ok true)
    )
)

(define-public (cast-weighted-vote (application-id uint) (vote-for bool))
    (let (
        (voter-rep (unwrap! (map-get? committee-reputation tx-sender) ERR_NOT_COMMITTEE))
        (voting-power (calculate-voting-power (get reputation-score voter-rep) (get streak-count voter-rep)))
        (blocks-since-last (- stacks-block-height (get last-vote-block voter-rep)))
    )
        (asserts! (contract-call? .Scholarship-Distribution is-committee-member tx-sender) ERR_NOT_COMMITTEE)
        (asserts! (> voting-power u0) ERR_INVALID_REPUTATION)
        (try! (contract-call? .Scholarship-Distribution vote-on-application application-id vote-for))
        (map-set weighted-votes {application-id: application-id, voter: tx-sender} voting-power)
        (map-set committee-reputation tx-sender (merge voter-rep {
            total-votes: (+ (get total-votes voter-rep) u1),
            last-vote-block: stacks-block-height
        }))
        (ok voting-power)
    )
)

(define-public (update-reputation-post-finalization (application-id uint))
    (let (
        (application (unwrap! (contract-call? .Scholarship-Distribution get-application application-id) (err u101)))
        (total-for (get votes-for application))
        (total-against (get votes-against application))
        (final-result (> total-for total-against))
    )
        (asserts! (or (is-eq (get status application) "approved") (is-eq (get status application) "rejected")) (err u108))
        (ok true)
    )
)

(define-private (calculate-voting-power (reputation uint) (streak uint))
    (let (
        (base-power (/ (* reputation u100) u100))
        (streak-bonus (/ (* streak u5) u1))
        (total-power (+ base-power streak-bonus))
    )
        (if (> total-power (var-get max-reputation))
            (var-get max-reputation)
            total-power
        )
    )
)

(define-read-only (get-voting-power (member principal))
    (match (map-get? committee-reputation member)
        rep (calculate-voting-power (get reputation-score rep) (get streak-count rep))
        u0
    )
)

(define-read-only (get-member-reputation (member principal))
    (map-get? committee-reputation member)
)

(define-read-only (get-weighted-vote (application-id uint) (voter principal))
    (map-get? weighted-votes {application-id: application-id, voter: voter})
)

(define-read-only (calculate-total-weighted-votes (application-id uint) (voters (list 10 principal)))
    (fold sum-weighted-vote voters {app-id: application-id, total: u0})
)

(define-private (sum-weighted-vote (voter principal) (context {app-id: uint, total: uint}))
    (let (
        (weight (default-to u0 (map-get? weighted-votes {application-id: (get app-id context), voter: voter})))
    )
        {app-id: (get app-id context), total: (+ (get total context) weight)}
    )
)

(define-read-only (get-reputation-leaderboard)
    (ok "requires-off-chain-indexing")
)

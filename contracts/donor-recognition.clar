(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_INVALID_AMOUNT (err u501))
(define-constant ERR_NO_ACTIVE_MATCHING (err u502))
(define-constant ERR_MATCHING_DEPLETED (err u503))

(define-data-var total-donations uint u0)
(define-data-var total-donors uint u0)
(define-data-var active-matching-pool uint u0)
(define-data-var matching-multiplier uint u2)
(define-data-var matching-enabled bool false)

(define-map donor-stats principal {
    lifetime-donated: uint,
    total-donations-count: uint,
    recognition-points: uint,
    tier: (string-ascii 10),
    first-donation-block: uint,
    last-donation-block: uint
})

(define-map matching-campaigns uint {
    creator: principal,
    pool-amount: uint,
    matched-amount: uint,
    multiplier: uint,
    start-block: uint,
    end-block: uint,
    active: bool
})

(define-data-var next-campaign-id uint u1)

(define-public (donate-to-dao (amount uint))
    (let (
        (donor tx-sender)
        (current-stats (get-or-create-donor-stats donor))
        (recognition-boost (calculate-recognition-points amount))
        (final-amount (if (var-get matching-enabled)
            (apply-matching amount)
            amount))
    )
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount donor (as-contract tx-sender)))
        (var-set total-donations (+ (var-get total-donations) final-amount))
        (map-set donor-stats donor {
            lifetime-donated: (+ (get lifetime-donated current-stats) final-amount),
            total-donations-count: (+ (get total-donations-count current-stats) u1),
            recognition-points: (+ (get recognition-points current-stats) recognition-boost),
            tier: (calculate-tier (+ (get lifetime-donated current-stats) final-amount)),
            first-donation-block: (get first-donation-block current-stats),
            last-donation-block: stacks-block-height
        })
        (ok final-amount)
    )
)

(define-public (create-matching-campaign (pool-amount uint) (multiplier uint) (duration uint))
    (let (
        (campaign-id (var-get next-campaign-id))
        (is-committee (contract-call? .Scholarship-Distribution is-committee-member tx-sender))
    )
        (asserts! is-committee ERR_UNAUTHORIZED)
        (asserts! (> pool-amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? pool-amount tx-sender (as-contract tx-sender)))
        (map-set matching-campaigns campaign-id {
            creator: tx-sender,
            pool-amount: pool-amount,
            matched-amount: u0,
            multiplier: multiplier,
            start-block: stacks-block-height,
            end-block: (+ stacks-block-height duration),
            active: true
        })
        (var-set matching-enabled true)
        (var-set active-matching-pool pool-amount)
        (var-set matching-multiplier multiplier)
        (var-set next-campaign-id (+ campaign-id u1))
        (ok campaign-id)
    )
)

(define-private (apply-matching (base-amount uint))
    (let (
        (match-amount (* base-amount (- (var-get matching-multiplier) u1)))
        (current-pool (var-get active-matching-pool))
    )
        (if (>= current-pool match-amount)
            (begin
                (var-set active-matching-pool (- current-pool match-amount))
                (+ base-amount match-amount)
            )
            base-amount
        )
    )
)

(define-private (calculate-recognition-points (amount uint))
    (/ amount u100)
)

(define-private (calculate-tier (lifetime-amount uint))
    (if (>= lifetime-amount u10000000)
        "Platinum"
        (if (>= lifetime-amount u5000000)
            "Gold"
            (if (>= lifetime-amount u1000000)
                "Silver"
                "Bronze"
            )
        )
    )
)

(define-private (get-or-create-donor-stats (donor principal))
    (default-to {
        lifetime-donated: u0,
        total-donations-count: u0,
        recognition-points: u0,
        tier: "Bronze",
        first-donation-block: stacks-block-height,
        last-donation-block: u0
    } (map-get? donor-stats donor))
)

(define-read-only (get-donor-stats (donor principal))
    (map-get? donor-stats donor)
)

(define-read-only (get-matching-campaign (campaign-id uint))
    (map-get? matching-campaigns campaign-id)
)

(define-read-only (get-dao-donation-stats)
    {
        total-donations: (var-get total-donations),
        total-donors: (var-get total-donors),
        active-matching-pool: (var-get active-matching-pool),
        matching-enabled: (var-get matching-enabled),
        matching-multiplier: (var-get matching-multiplier)
    }
)

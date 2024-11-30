
;; match
;; <add a description here>

;; constants
;;

;; data maps and vars
;;

;; private functions
;;

;; public functions
;;

;; dating-match.clar
;; A smart contract for handling match confirmation and interaction between users

;; Data Variables
(define-data-var minimum-tokens-for-interaction uint u100)

;; Data Maps
(define-map user-profiles 
    principal 
    {
        active: bool,
        total-matches: uint,
        tokens-balance: uint
    }
)

(define-map matches 
    {user1: principal, user2: principal} 
    {
        confirmed: bool,
        interaction-count: uint,
        last-interaction: uint,
        active: bool
    }
)

;; Error codes
(define-constant ERR-NOT-REGISTERED u1)
(define-constant ERR-ALREADY-MATCHED u2)
(define-constant ERR-INSUFFICIENT-TOKENS u3)
(define-constant ERR-MATCH-NOT-FOUND u4)
(define-constant ERR-UNAUTHORIZED u5)

;; Register user
(define-public (register-user)
    (let
        ((caller tx-sender))
        (asserts! (is-none (map-get? user-profiles caller)) (err ERR-NOT-REGISTERED))
        (ok (map-set user-profiles 
            caller
            {
                active: true,
                total-matches: u0,
                tokens-balance: u500  ;; Initial tokens granted
            }
        ))
    )
)

;; Propose match
(define-public (propose-match (potential-match principal))
    (let
        ((caller tx-sender)
         (caller-profile (unwrap! (map-get? user-profiles caller) (err ERR-NOT-REGISTERED)))
         (match-profile (unwrap! (map-get? user-profiles potential-match) (err ERR-NOT-REGISTERED))))
        
        ;; Check if users have enough tokens
        (asserts! (>= (get tokens-balance caller-profile) (var-get minimum-tokens-for-interaction))
            (err ERR-INSUFFICIENT-TOKENS))
        
        ;; Create new match entry
        (ok (map-set matches
            {user1: caller, user2: potential-match}
            {
                confirmed: false,
                interaction-count: u0,
                last-interaction: block-height,
                active: true
            }
        ))
    )
)

;; Confirm match
(define-public (confirm-match (matcher principal))
    (let
        ((caller tx-sender)
         (match-data (unwrap! (map-get? matches {user1: matcher, user2: caller}) 
            (err ERR-MATCH-NOT-FOUND))))
        
        ;; Update match status
        (ok (map-set matches
            {user1: matcher, user2: caller}
            (merge match-data {confirmed: true})
        ))
    )
)

;; Record interaction
(define-public (record-interaction (other-user principal))
    (let
        ((caller tx-sender)
         (match-data (try! (get-match caller other-user))))
        
        ;; Update interaction count and timestamp
        (ok (map-set matches
            {user1: caller, user2: other-user}
            (merge match-data 
                {
                    interaction-count: (+ (get interaction-count match-data) u1),
                    last-interaction: block-height
                }
            )
        ))
    )
)

;; Helper function to get match data
(define-private (get-match (user1 principal) (user2 principal))
    (match (map-get? matches {user1: user1, user2: user2})
        match-data (ok match-data)
        (match (map-get? matches {user1: user2, user2: user1})
            match-data (ok match-data)
            (err ERR-MATCH-NOT-FOUND)
        )
    )
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user)
)

;; Check if users are matched
(define-read-only (are-users-matched (user1 principal) (user2 principal))
    (match (get-match user1 user2)
        match-data (ok (get confirmed match-data))
        err-value (ok false)
    )
)
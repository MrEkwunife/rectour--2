
;; decentralized-id
;; <add a description here>

;; constants
;;

;; data maps and vars
;;

;; private functions
;;

;; public functions
;;

;; DID Loyalty Program Contract
;; Allows users to manage their decentralized identity and associated loyalty data

;; Define data vars and maps
(define-data-var contract-owner principal tx-sender)

;; Constants for tiers (using UTF-8)
(define-constant TIER-BRONZE u"BRONZE")
(define-constant TIER-SILVER u"SILVER")
(define-constant TIER-GOLD u"GOLD")
(define-constant TIER-PLATINUM u"PLATINUM")

;; Map to store DID documents
(define-map did-documents
    principal
    {
        did: (string-utf8 250),              ;; The DID identifier
        public-key: (string-utf8 250),       ;; Associated public key
        controller: principal,               ;; DID controller
        created: uint,                       ;; Creation timestamp
        updated: uint,                       ;; Last update timestamp
        active: bool                         ;; DID status
    }
)

;; Map to store loyalty points associated with DIDs
(define-map loyalty-points
    principal
    {
        points: uint,
        last-updated: uint,
        tier: (string-utf8 20)
    }
)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-DID-EXISTS (err u2))
(define-constant ERR-DID-NOT-FOUND (err u3))
(define-constant ERR-INVALID-DID (err u4))

;; Read-only functions

(define-read-only (get-did-document (user principal))
    (ok (map-get? did-documents user))
)

(define-read-only (get-loyalty-info (user principal))
    (ok (map-get? loyalty-points user))
)

;; Public functions

;; Register new DID
(define-public (register-did (did-string (string-utf8 250)) (public-key (string-utf8 250)))
    (let
        (
            (caller tx-sender)
            (existing-did (map-get? did-documents caller))
        )
        (asserts! (is-none existing-did) (err ERR-DID-EXISTS))
        (asserts! (> (len did-string) u0) (err ERR-INVALID-DID))
        
        (map-set did-documents caller {
            did: did-string,
            public-key: public-key,
            controller: caller,
            created: block-height,
            updated: block-height,
            active: true
        })
        
        ;; Initialize loyalty points
        (map-set loyalty-points caller {
            points: u0,
            last-updated: block-height,
            tier: TIER-BRONZE
        })
        
        (ok true)
    )
)

;; Update DID document
(define-public (update-did (did-string (string-utf8 250)) (public-key (string-utf8 250)))
    (let
        (
            (caller tx-sender)
            (existing-did (unwrap! (map-get? did-documents caller) (err ERR-DID-NOT-FOUND)))
        )
        (asserts! (is-eq (get controller existing-did) caller) (err ERR-NOT-AUTHORIZED))
        
        (map-set did-documents caller (merge existing-did {
            did: did-string,
            public-key: public-key,
            updated: block-height
        }))
        
        (ok true)
    )
)

;; Deactivate DID
(define-public (deactivate-did)
    (let
        (
            (caller tx-sender)
            (existing-did (unwrap! (map-get? did-documents caller) (err ERR-DID-NOT-FOUND)))
        )
        (asserts! (is-eq (get controller existing-did) caller) (err ERR-NOT-AUTHORIZED))
        
        (map-set did-documents caller (merge existing-did {
            active: false,
            updated: block-height
        }))
        
        (ok true)
    )
)

;; Add loyalty points (only authorized partners can call this)
(define-public (add-points (user principal) (amount uint))
    (let
        (
            (caller tx-sender)
            (existing-points (unwrap! (map-get? loyalty-points user) (err ERR-DID-NOT-FOUND)))
            (did-doc (unwrap! (map-get? did-documents user) (err ERR-DID-NOT-FOUND)))
        )
        ;; Check if the DID is active
        (asserts! (get active did-doc) (err ERR-DID-NOT-FOUND))
        ;; Add authorization check here based on your requirements
        
        (map-set loyalty-points user (merge existing-points {
            points: (+ (get points existing-points) amount),
            last-updated: block-height,
            tier: (calculate-tier (+ (get points existing-points) amount))
        }))
        
        (ok true)
    )
)

;; Private function to calculate loyalty tier
(define-private (calculate-tier (points uint))
    (if (>= points u10000)
        TIER-PLATINUM
        (if (>= points u5000)
            TIER-GOLD
            (if (>= points u1000)
                TIER-SILVER
                TIER-BRONZE
            )
        )
    )
)
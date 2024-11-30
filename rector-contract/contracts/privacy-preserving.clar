
;; privacy-preserving
;; <add a description here>

;; constants
;;

;; data maps and vars
;;

;; private functions
;;

;; public functions
;;

;; NFT Profile Badges Contract for Web3 Dating App
;; Allows users to mint and manage profile verification badges as NFTs

(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

;; Define variables
(define-non-fungible-token profile-badge uint)
(define-data-var last-token-id uint u0)
(define-map badge-metadata ((token-id uint)) {
    owner: principal,
    badge-type: (string-utf8 24),
    timestamp: uint,
    verified: bool
})

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-BADGE-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-VERIFIED (err u409))

;; Base URL constant for the metadata endpoint
(define-constant BASE_URI "https://your-dating-app.com/api/badge/")

;; SIP009 NFT Standard functions
(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok BASE_URI) ;; Returns the base URI; token ID will be appended in the application layer
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? profile-badge token-id))
)

;; Custom functions for badge management
(define-public (mint-profile-badge (badge-type (string-utf8 24)))
    (let
        ((token-id (+ (var-get last-token-id) u1)))
        (try! (nft-mint? profile-badge token-id tx-sender))
        (map-set badge-metadata { token-id: token-id } {
            owner: tx-sender,
            badge-type: badge-type,
            timestamp: block-height,
            verified: false
        })
        (var-set last-token-id token-id)
        (ok token-id)
    )
)

(define-read-only (get-badge-info (token-id uint))
    (match (map-get? badge-metadata { token-id: token-id })
        badge-data (ok badge-data)
        ERR-BADGE-NOT-FOUND
    )
)

(define-public (verify-badge (token-id uint))
    (let
        ((badge-data (unwrap! (map-get? badge-metadata { token-id: token-id }) ERR-BADGE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get verified badge-data)) ERR-ALREADY-VERIFIED)
        (map-set badge-metadata { token-id: token-id } 
            (merge badge-data { verified: true }))
        (ok true)
    )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
        (try! (nft-transfer? profile-badge token-id sender recipient))
        (let
            ((badge-data (unwrap! (map-get? badge-metadata { token-id: token-id }) ERR-BADGE-NOT-FOUND)))
            (map-set badge-metadata { token-id: token-id }
                (merge badge-data { owner: recipient }))
            (ok true)
        )
    )
)

;; Administrative functions
(define-data-var contract-owner principal tx-sender)

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

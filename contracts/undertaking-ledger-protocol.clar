;; Undertaking Ledger Protocol
;; ==================================================================
;; Blockchain-powered commitment orchestration enabling entities to create,
;; monitor, and validate personal or collaborative promises with time-based
;; constraints and importance hierarchies.

;; ==================================================================
;; PROTOCOL RESPONSE CONSTANTS
;; ==================================================================

(define-constant ERR_DUPLICATE_VOW (err u409))
(define-constant ERR_INVALID_INPUT (err u400))
(define-constant ERR_VOW_NOT_FOUND (err u404))

;; ==================================================================
;; CORE DATA STORAGE STRUCTURES
;; ==================================================================

(define-map vow-registry
    principal
    {
        vow-description: (string-ascii 100),
        completion-flag: bool
    }
)

(define-map priority-tracker
    principal
    {
        importance-level: uint
    }
)

(define-map deadline-manager
    principal
    {
        target-block: uint,
        alert-sent: bool
    }
)

;; ==================================================================
;; CONFIGURATION MANAGEMENT FUNCTIONS
;; ==================================================================

;; Time constraint configuration function
;; Sets blockchain height-based expiration for active vows
(define-public (configure-deadline (block-duration uint))
    (let
        (
            (caller-address tx-sender)
            (vow-exists (map-get? vow-registry caller-address))
            (expiry-block (+ block-height block-duration))
        )
        (if (is-some vow-exists)
            (if (> block-duration u0)
                (begin
                    (map-set deadline-manager caller-address
                        {
                            target-block: expiry-block,
                            alert-sent: false
                        }
                    )
                    (ok "Deadline configuration completed successfully")
                )
                ERR_INVALID_INPUT
            )
            ERR_VOW_NOT_FOUND
        )
    )
)

;; Priority level assignment function
;; Assigns importance ranking to existing vows (1=low, 2=medium, 3=high)
(define-public (assign-importance (priority-rank uint))
    (let
        (
            (caller-address tx-sender)
            (vow-exists (map-get? vow-registry caller-address))
        )
        (if (is-some vow-exists)
            (if (and (>= priority-rank u1) (<= priority-rank u3))
                (begin
                    (map-set priority-tracker caller-address
                        {
                            importance-level: priority-rank
                        }
                    )
                    (ok "Priority assignment executed successfully")
                )
                ERR_INVALID_INPUT
            )
            ERR_VOW_NOT_FOUND
        )
    )
)

;; ==================================================================
;; STATE VALIDATION UTILITIES
;; ==================================================================

;; Vow existence checker function
;; Returns detailed information about caller's vow without state changes
(define-public (check-vow-status)
    (let
        (
            (caller-address tx-sender)
            (vow-data (map-get? vow-registry caller-address))
        )
        (if (is-some vow-data)
            (let
                (
                    (vow-info (unwrap! vow-data ERR_VOW_NOT_FOUND))
                    (description-text (get vow-description vow-info))
                    (is-completed (get completion-flag vow-info))
                )
                (ok {
                    has-vow: true,
                    text-length: (len description-text),
                    is-fulfilled: is-completed
                })
            )
            (ok {
                has-vow: false,
                text-length: u0,
                is-fulfilled: false
            })
        )
    )
)

;; ==================================================================
;; CORE VOW OPERATIONS
;; ==================================================================

;; New vow creation function
;; Establishes fresh commitment record for caller
(define-public (create-new-vow (vow-text (string-ascii 100)))
    (let
        (
            (caller-address tx-sender)
            (existing-vow (map-get? vow-registry caller-address))
        )
        (if (is-none existing-vow)
            (if (is-eq vow-text "")
                ERR_INVALID_INPUT
                (begin
                    (map-set vow-registry caller-address
                        {
                            vow-description: vow-text,
                            completion-flag: false
                        }
                    )
                    (ok "New vow created and stored successfully")
                )
            )
            ERR_DUPLICATE_VOW
        )
    )
)

;; Vow modification function  
;; Updates existing commitment with new parameters
(define-public (update-existing-vow 
    (new-description (string-ascii 100))
    (completion-status bool))
    (let
        (
            (caller-address tx-sender)
            (vow-exists (map-get? vow-registry caller-address))
        )
        (if (is-some vow-exists)
            (if (is-eq new-description "")
                ERR_INVALID_INPUT
                (begin
                    (if (or (is-eq completion-status true) (is-eq completion-status false))
                        (begin
                            (map-set vow-registry caller-address
                                {
                                    vow-description: new-description,
                                    completion-flag: completion-status
                                }
                            )
                            (ok "Vow updated with new configuration")
                        )
                        ERR_INVALID_INPUT
                    )
                )
            )
            ERR_VOW_NOT_FOUND
        )
    )
)

;; ==================================================================
;; MULTI-ENTITY INTERACTION LAYER
;; ==================================================================

;; Cross-entity vow assignment function
;; Creates commitment on behalf of specified recipient
(define-public (assign-vow-to-entity
    (target-principal principal)
    (vow-content (string-ascii 100)))
    (let
        (
            (recipient-vow (map-get? vow-registry target-principal))
        )
        (if (is-none recipient-vow)
            (if (is-eq vow-content "")
                ERR_INVALID_INPUT
                (begin
                    (map-set vow-registry target-principal
                        {
                            vow-description: vow-content,
                            completion-flag: false
                        }
                    )
                    (ok "Vow successfully assigned to target entity")
                )
            )
            ERR_DUPLICATE_VOW
        )
    )
)

;; ==================================================================
;; SYSTEM ADMINISTRATION FUNCTIONS  
;; ==================================================================

;; Complete data purge function
;; Eliminates all vow-related records for caller
(define-public (execute-full-cleanup)
    (let
        (
            (caller-address tx-sender)
            (vow-exists (map-get? vow-registry caller-address))
        )
        (if (is-some vow-exists)
            (begin
                (map-delete vow-registry caller-address)
                (map-delete priority-tracker caller-address)
                (map-delete deadline-manager caller-address)
                (ok "Complete cleanup operation finished")
            )
            ERR_VOW_NOT_FOUND
        )
    )
)

;; Comprehensive status report function
;; Generates detailed overview of caller's vow ecosystem
(define-public (build-status-report)
    (let
        (
            (caller-address tx-sender)
            (main-vow-data (map-get? vow-registry caller-address))
            (priority-info (map-get? priority-tracker caller-address))
            (deadline-info (map-get? deadline-manager caller-address))
        )
        (if (is-some main-vow-data)
            (let
                (
                    (vow-details (unwrap! main-vow-data ERR_VOW_NOT_FOUND))
                    (priority-set (if (is-some priority-info) 
                                      (get importance-level (unwrap! priority-info ERR_VOW_NOT_FOUND))
                                      u0))
                    (deadline-configured (is-some deadline-info))
                )
                (ok {
                    vow-exists: true,
                    finished: (get completion-flag vow-details),
                    has-priority: (> priority-set u0),
                    has-deadline: deadline-configured
                })
            )
            (ok {
                vow-exists: false,
                finished: false,
                has-priority: false,
                has-deadline: false
            })
        )
    )
)



;; will
;; <add a description here>
;;Error in line number 89 and 92


;; constants
;;
(define-constant contract-owner tx-sender)

(define-constant FAILED-TO-MINT-ERROR (err u1))
(define-constant NOT-OWNER-ERROR (err u2))
(define-constant CALLER-DOES-NOT-OWN-TOKEN (err u3))
(define-constant ACCESS-DENIED (err u4))
(define-constant NOT-LISCENSED-ENTITY (err u5))
(define-constant NOT-BENEFICIARY (err u6))
(define-constant NFT-REPETITION (err u7))
(define-constant TRANSACTION-FAILED (err u8))
(define-constant NFT-NOT-BURNED (err u9))



;; data maps and vars
;;
(define-map tokens-count { owner: principal } { count: int })

(define-map liscensed-entity { liscensed-entity-principal: principal } {access-allowed: bool})
(define-map beneficiary { beneficiary-principal: principal } {access-allowed: bool})

(define-non-fungible-token will-token uint)

(define-data-var curr-token-index uint u100)

(define-map will {token-id: uint} {liscensed-entity-principal: principal, beneficiary-principal: principal, amount: uint } 
)

;; public functions
;;
(define-public (add-Liscensed-entity (adress principal))
  (begin
    (asserts! (is-eq contract-owner tx-sender) NOT-OWNER-ERROR)
    (ok (map-set liscensed-entity { liscensed-entity-principal: adress } { access-allowed: true }))
  )
)

(define-public (add-beneficiary (adress principal))
  (begin
    (asserts! (is-eq contract-owner tx-sender) NOT-OWNER-ERROR)
    (ok (map-set beneficiary { beneficiary-principal: adress } { access-allowed: true }))
  )
)



(define-public (mint!
  (owner principal)
  (sender principal)
  (amount uint)
  )
 
    (let ((token-id (+ (var-get curr-token-index) u1)))

      (asserts! (is-liscensed-entity tx-sender) NOT-LISCENSED-ENTITY)
      (asserts! (is-beneficiary owner) NOT-BENEFICIARY)

      ;; Check if the token is successfully minted
      (asserts! (register-token owner token-id) FAILED-TO-MINT-ERROR)
      ;; (asserts! (nft-repetition owner) NFT-REPETITION) 
      (asserts! (>= (stx-get-balance sender) amount ) TRANSACTION-FAILED)
      (map-set will 
        {token-id: token-id}
        {liscensed-entity-principal: tx-sender, beneficiary-principal: owner, amount: amount}
      )

      
      (asserts! (try! (stx-transfer? amount sender tx-sender)) TRANSACTION-FAILED)
      (var-set curr-token-index token-id)

      (ok token-id)
      
    )
)

(define-public (burn-nft (token-id uint) (beneficiary_1 principal))
 (begin
  
    (asserts! (is-liscensed-entity tx-sender) NOT-LISCENSED-ENTITY) 
    (asserts! (is-eq (owner-of? token-id) (some beneficiary_1)) NOT-BENEFICIARY)
    ;;(is-eq ( get liscensed-entity-principal (unwrap! (map-get? will {token-id: token-id}) (err NOT-LISCENSED-ENTITY))) tx-sender )  //recieving error in this

    (asserts! (try! (nft-burn? will-token token-id beneficiary_1)) NFT-NOT-BURNED)
   ;; (asserts! (try! (stx-transfer? (try! (get amount (map-get? will {token-id: token-id}) )) tx-sender beneficiary_1)) TRANSACTION-FAILED)    //recieving error in this
    (ok true)
 ) 
)



;; private functions
;;
(define-private (is-liscensed-entity (incoming-principal principal)) 
  (is-some (map-get? liscensed-entity (tuple (liscensed-entity-principal incoming-principal))))
)

(define-private (is-beneficiary (incoming-principal principal)) 
  (is-some (map-get? beneficiary (tuple (beneficiary-principal incoming-principal))))
)

;; (define-private (nft-repetition (incoming-principal principal) )
;;   (is-some (map-get? will {beneficiary-principal: incoming-principal}))
;; )

(define-private (balance-of (account principal))
  (default-to 0
    (get count
         (map-get? tokens-count (tuple (owner account))))))

(define-private (register-token (new-owner principal) (token-id uint))
    (begin
      (unwrap! (nft-mint? will-token token-id new-owner) false)
      (map-set tokens-count
          {owner: new-owner}
          {count: (+ 1 (balance-of new-owner))}
      )
    )
)

(define-read-only (owner-of? (token-id uint))
  (nft-get-owner? will-token token-id)
)

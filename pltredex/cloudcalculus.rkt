#lang racket
(require redex)

;; Cloud calculus grammar
(define-language CC
  (e    ::= xt v (spwn e) (e ♯ x) (e < e ... >) (par e ...)) 
  (v    ::= (srv r ...) (sinst n r  ...) ((sinst n r ...) ♯ x) (par))
  (r    ::= (p ... ▹ e))  
  (p    ::= (x < x ... >))
  (xt   ::= x this)  
  (x n   ::= (variable-except spwn srv sinst par : ▹  ♯ srv : this < >))
  (E    ::= hole (spwn E) (E ♯ x) (E <  e ... >) (e < e ... E e ... >) (par e ... E e ...))
)

;; match judgment
(define-judgment-form 
  CC 
  #:mode (match I I I O O) 
  #:contract (match v (p ...) (e ...  ) (e ... ) ((x v) ...))
  
  [------------------------------------------ "Match0"
   (match (sinst _ ...) () (e ...) (e ...) ())]
  
  
  [(match vs (p_1 ...) (e_1  ... e_2 ...) (e_r ...) ((x_r v_r) ...))    
   ----------------------------------------------------------------------------------------------------------------------------- "Match1"
   (match (name vs (sinst n r ... )) ((x < x_1 ..._1 >) p_1 ...) (e_1 ... (((sinst n r ...) ♯ x) < v_1 ..._1 >) e_2 ...) (e_r ...) ((x_1 v_1) ... (x_r v_r) ...))]
  )

;; reduction relation
(define red (reduction-relation CC #:domain e                               
  (--> (in-hole E (par e_1 ... (par e_2 ...) e_3 ...))
       (in-hole E (par e_1 ... e_2 ... e_3 ...)) "Par")
  
  (--> (in-hole E (spwn (srv r ...))) (in-hole E (sinst n r ...)) (fresh n) "Spwn")

  (--> (in-hole E (par e_1 ... (((name vs (sinst n r_1  ... (p_1 ... ▹ e_b) r_2 ...)) ♯ x) < v_1 ... >) e_2 ...)) 
       (in-hole E (par e_r ... (subst (x_r v_r) ... (subst (this vs) e_b))))
       (judgment-holds (match vs (p_1 ...) (e_1 ... ((vs ♯ x) < v_1 ... >) e_2 ...)  (e_r ...) ((x_r v_r) ...))) "Serve") 
))



;;capture-avoiding term substitution  

(define-metafunction CC subst : (xt v) ...  e -> e
  [(subst e) e]
  [(subst _ ... (xt v) _ ... xt) v]
  [(subst _ ... xt_1) xt_1]
  [(subst any ... (spwn e)) (spwn (subst any ... e))]
  [(subst any ... (e ♯ x)) ((subst any ... e) ♯ x)] 
  [(subst any ... (e < e_1 ... >)) ((subst any ... e) < (subst any ... e_1) ... >)]
  [(subst any ... (par e ...)) (par (subst any ... e) ...)]
  [(subst any ... (srv r ...)) (srv (subst-r any ... r) ...)]
  [(subst any ... (sinst n r ...)) (sinst n (subst-r any ... r) ...)]
)

(define-metafunction CC subst-r : (xt v) ... r  -> r      
  [(subst-r r) r]
  [(subst-r any ... (xt v) any_1 ... (name rule (p ... ▹ e))) 
   (subst-r any ... any_1 ... rule)
   (side-condition (or (redex-match? CC this (term xt)) 
                        (memq (term xt) (term (arg-vars p ...)))))]
  [(subst-r any ... (p ... ▹ e))  
   ((rename-p (x_bound x_new) ... p) ... ▹ (subst any ... (rename (x_bound x_new) ... e)))  
   (where ((x_bound ..._1) (x_new ..._1)) 
          ((arg-vars p ...) ,(variables-not-in (term (any ... e)) (term (arg-vars p ...)))))]
)          

;; blindly rename variables
(define-metafunction CC rename : (x x) ...  e -> e
  [(rename e) e]
  [(rename _ ... this) this]
  [(rename _ ... (x x_r) _ ... x) x_r]
  [(rename _ ... x) x] 
  [(rename any ... (spwn e)) (spwn (rename any ... e))]
  [(rename any ... (e ♯ x)) ((rename any ... e) ♯ x)] 
  [(rename any ... (e < e_1 ... >)) ((rename any ... e) < (rename any ... e_1) ... >)]
  [(rename any ... (par e ...)) (par (rename any ... e) ...)]
  [(rename any ... (srv r ...)) (srv (rename-r any ... r) ...)]
  [(rename any ... (sinst n r ...)) (sinst n (rename-r any ... r) ...)]
)

(define-metafunction CC rename-r : (x x) ... r -> r
  [(rename-r r) r]
  [(rename-r any ... (p ... ▹ e)) ((rename-p any ... p) ... ▹ (rename any ... e))]
)

(define-metafunction CC rename-p : (x x) ... p -> p
  [(rename-p p) p]
  [(rename-p any ... (x_1 < x ... >)) (x_1 < (rename any ... x) ... >)]
)

; argument variables of a pattern sequence
(define-metafunction CC arg-vars : p ... -> (x ...)
  [(arg-vars (x_0₁ < x ... >) ...) (x ... ...)]
 )


;some derived syntax

(define-metafunction CC Let : (x e) e -> e
  [(Let (x e_1) e_2)  (((spwn (srv (($let < x > ) ▹ e_2 ))) ♯ $let) < e_1 >)]  
)

(provide (all-defined-out))

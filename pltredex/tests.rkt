#lang racket
(require redex)
(require "cloudcalculus.rkt")



;;tests for rename
(define demo1 (term (par 
                       (Let (x this) 
                            ((this ♯ x) < (x ♯ x) >)) 
                       (Let (x (spwn (srv ((foo < x b c >) ▹ (x < b c (this ♯ foo) >))))) 
                            ((this ♯ x) < (x ♯ x) >))) ))

(test-equal (term (rename (x y) ,demo1))
             (term (par 
                       (Let (y this) 
                            ((this ♯ x) < (y ♯ x) >)) 
                       (Let (y (spwn (srv ((foo < y b c >) ▹ (y < b c (this ♯ foo) >))))) 
                            ((this ♯ x) < (y ♯ x) >))) ))

(define demo2 (term (par (Let (x (spwn (srv ( (x < x >) ▹ (x < (this ♯ x) x >)))) )
                                   ((x ♯ x) < (x ♯ x) >)
                    ))
                    ))

(define demo22 (term (par (Let (x  (spwn (srv ( (x < y z >) ▹ (y < (this ♯ x) z >)))) )
                                   ((x ♯ x) < y z >)
                    ) )
                    ))

(define demo23 (term (par (Let (server (spwn (srv ( (x < arg >) ▹ (arg < (this ♯ x) >)))) )
                                   ((server ♯ x) < (server ♯ x) >)
                    ))
                    ))

(test-equal (term (rename (x y) ,demo2))
            (term (par (Let (y  (spwn (srv ((x < y >) ▹ (y < (this ♯ x) y >)))))
                                  ((y ♯ x) < (y ♯ x) >)))))

(define demo3 (term 
               (Let (x1 (spwn (srv ((foo < x >) ▹ (x < >)))))
                 (Let (x2 (spwn (srv ((bar < x >) ▹ ((x ♯ foo) < x2 >)))))
                   (Let (x3 (spwn (srv ((baz < x >) ▹ ((x ♯ bar) < x1 >)))))
                     ((x1 ♯ baz) < (x2 ♯ baz) >)
                   )
                 )
               )
             ))


(test-equal (term (rename (x1 y) (x2 y) (x3 y) (x z) ,demo3))
            (term 
               (Let (y (spwn (srv ((foo < z >) ▹ (z < >)))))
                 (Let (y (spwn (srv ((bar < z >) ▹ ((z ♯ foo) < y >)))))
                   (Let (y (spwn (srv ((baz < z >) ▹ ((z ♯ bar) < y >)))))
                     ((y ♯ baz) < (y ♯ baz) >)
                   )
                 )
               )
             ))


(define demo-subst (term (subst (this (sinst n1 ( (y < >) ▹ (par ((x ♯ explode) < >) ((this ♯ y) < >) ) ))) ,demo1   )))


;; tests for subst

; test if capture of service arguments is avoided correctly by subst
(define demo-subst-svc-capture (term (par (Let (y (spwn (srv ((z < > ) ▹ (a < >)))))
                                               (Let (s (spwn (srv ((x < a >) ▹ ((y ♯ z) < >)))))
                                                 ((s ♯ x) <  ( (spwn (srv ( (f < > ) ▹ ((hijack ♯ svc) < >))))  ♯ f) >))))))

(test-->>E red demo-subst-svc-capture (term (par (a < >))))



(define demo-subst-svc-capture2 (term 
                                (par
                                (Let (x (sinst n1 ( (service < a b >) ▹  (Let (x (spwn (srv ((service2 < a b >) ▹ (a < b >))))) (a < b >)) )))                                   
                                   ((x ♯ service) < foo bar >))                                
                                )
                                )
  )




(define demo-nondeterm
  (term (par
   (Let (s (sinst n1 ((x < > ) ▹ (y < >) ) ( (x < > ) ▹ (z < >)  ) ))
          ((s ♯ x) < >)))))
   

(define (mkcell P)
  (term (par
   (Let (factory  (sinst n1 ( (mkCell < v k >) ▹ (Let (cell  (spwn (srv ((get < k >) (s < v >)   ▹ (par (k < v >) 
                                                                                       ((this ♯ s) < v >))) 
                                                             ((set < u k >) (s < v >) ▹ (par (k < >)  
                                                                                       ((this ♯ s) < u >))))))
                                               (par (k < (cell ♯ get) (cell ♯ set) >) 
                                                    ((cell ♯ s) < v >)))
                                            ))) 
                       ,P)) #:lang CC))
  


(define demo-cell1 (mkcell  
                   (term 
                    (Let (client  (spwn (srv  ((onResult < get set >) ▹ (par ((sinst echo) < get set >) ))))) 
                       (par ((factory ♯ mkCell) < (sinst n1337) (client ♯ onResult) >))
                    )
                   )))

(define demo-cell (mkcell  
                   (term 
                    (Let (client (spwn (srv  ((onResult < get set >) ▹ (par (get < (sinst echo) >) 
                                                                             (set < (sinst n1000) (this ♯ setOk) >)))
                                        
                                        ((setOk < > ) ▹ ((sinst echo) < (sinst success) >)) ))) 
                      (par ((factory ♯ mkCell) < (sinst n1337) (client ♯ onResult) >))
                    )
                   )))

(define demo-freshness   (term (par   (Let (x (spwn (srv ((x < x >) ▹ (x < (this ♯ x) >))))) ((x ♯ x) < (x ♯ x) >))                                      
                                      (Let (x (spwn (srv ((x < x >) ▹ (x < (this ♯ x) >))))) ((x ♯ x) < (x ♯ x) >))              )    )     )




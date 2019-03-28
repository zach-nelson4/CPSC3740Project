#lang racket

;CPSC 3740 Final Project
;Matthew Davison, Taranjot Kaur, Zachary Nelson
;February 26th 2019


(define (startEval2 list1 list2 list3)

  (cond
    [(and (pair? list1) (equal? (car list1) 'quote))
          (MyQuote(startEval2 (MyRemove (cadr list1)) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'lambda))
          (MyLambda(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'let))
          (MyLet(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'letrec))
          (MyLetrec(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '+))
          (MyAdd(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '-))
          (MySub(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '*))
          (MyMult(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '/))
          (MyDiv(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'equal?))
          (MyEqual(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '=))
          (MyEqualSign(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '<=))
          (MyLessThanEqual(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '>=))
          (MyGreaterThanEqual(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '>))
          (MyGreater(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) '<))
          (MyLesser(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'if))
          (MyIf(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'car))
          (MyCar(startEval2 (cadr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'cdr))
          (MyCdr(startEval2 (cadr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'cons))
          (MyCons(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
    [(and (pair? list1) (equal? (car list1) 'pair))
          (MyPair(startEval2 (cadr list1) list2 list3))]
    

    [(and (pair? list3) (and (pair? list1) (equal? (car list1) (car list3))))
     (car list2)]
    [(and (pair? list3) (and (not(pair? list1)) (equal? list1 (car list3))))
     (car list2)]

    [(and( not(pair? list1)) (not(pair? list3))) '()]
    [(and (pair? list3) (and (not(pair? list1)) (not(equal? list1 (car list3))))) (startEval2 list1 (cdr list2) (cdr list3))]

    [(and (not (pair? list3)) (and (not(pair? list1)) (equal? list1 list3)))
     list2]
    [(and (not (pair? list3)) (and (not(pair? list1)) (not(equal? list1 list3))))
     (startEval2 list1 list2 list3)]
    [(and (pair? list3)) (startEval2 list1 (cadr list2) (cadr list3))]
    [(equal? list1 list3)
     list2]
    [else 0]
    )
  
  )

;Start Eval Function
;Takes in a list which is to be evaluate

(define (startEval list1)
  (cond
    ;If there is nothing in the list, return the empty list
    [(null? list1) '()]
    [(not(pair? list1)) list1]

    [(equal? (car list1) 'quote)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyQuote(startEval(cadr list1)))]
    ;----------------------------------------------------------------------------------
    ;If the first item in the list is an addition sign, add the next two items together
    [(equal? (car list1) '+)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyAdd(startEval(cadr list1))(startEval(caddr list1)))]

    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a subtraction sign, subtract the next two items together
    [(equal? (car list1) '-)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MySub(startEval(cadr list1))(startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a multiplication sign, multiply the next two items together
    [(equal? (car list1) '*)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyMult(startEval(cadr list1))(startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a division sign, divide the next two items together
    [(equal? (car list1) '/)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyDiv(startEval(cadr list1))(startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a equal
    [(equal? (car list1) 'equal?)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyEqual (startEval(cadr list1)) (startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a =
    [(equal? (car list1) '=)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyEqualSign(startEval(cadr list1)) (startEval(caddr list1)))]   
    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a =
    [(equal? (car list1) '<=)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyLessThanEqual(startEval(cadr list1)) startEval(caddr list1))]  
    ;---------------------------------------------------------------------------------

    ;If the first item in the list is a =
    [(equal? (car list1) '>=)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyGreaterThanEqual(startEval(cadr list1)) (startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------

    ;If the first item in the list is a =
    [(equal? (car list1) '>)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyGreater(startEval(cadr list1)) (startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
     ;If the first item in the list is a =
    [(equal? (car list1) '<)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyLesser(startEval(cadr list1)) (startEval(caddr list1)))]
    ;--------------------------------------------------------------------------------
    ;If the first item in the list is a if function, check condition then evaluate
    [(equal? (car list1) 'if)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyIf(startEval(cadr list1)) (startEval(caddr list1)) (startEval(cadddr list1)))]
    ;---------------------------------------------------------------------------------
     [(equal? (car list1) 'car)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyCar(startEval(cadr list1)))]
    ;---------------------------------------------------------------------------------
     [(equal? (car list1) 'cdr)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyCdr(startEval(cadr list1)))]
    ;---------------------------------------------------------------------------------
     [(equal? (car list1) 'cons)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyCons(startEval(cadr list1)) (startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
     [(equal? (car list1) 'pair?)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyPair(startEval(cadr list1)))]
    ;---------------------------------------------------------------------------------

     [(equal? (caar list1) 'lambda) (MyLambda list1)];     
     
     [else list1]
    )
  )

;---------------------------------------------------------
;myAdd Function, given two numbers, add them together
(define (MyAdd num1 num2)
  (+ num1 num2)
  )
;----------------------------------------------------------
;mySub Function, given two numbers, subtract them
(define (MySub num1 num2)
  (- num1 num2)
  )
;---------------------------------------------------------
;myMult Function, given two numbers, multiply them
(define (MyMult num1 num2)
  (* num1 num2)
  )
;--------------------------------------------------------
;myDiv Function, given two numbers, divide them
(define (MyDiv num1 num2)
  (/ num1 num2)
  )
(define (MyIf cond1 action1 action2)
  (if cond1 action1 action2)
  )
(define (MyEqual action1 action2)
  (equal? action1 action2)
  )
(define (MyEqualSign action1 action2)
  (= action1 action2)
  )
(define (MyLessThanEqual action1 action2)
  (<= action1 action2)
  )
(define (MyGreaterThanEqual action1 action2)
  (>= action1 action2)
  )
(define (MyGreater action1 action2)
  (> action1 action2)
  )
(define (MyLesser action1 action2)
  (< action1 action2) 
  )
(define (MyQuote action1)
  (quote action1) 
  )
(define (MyCar list1)
  (cond [(pair? list1)
      (car list1)])
  )
(define (MyCdr list1)
  (cdr list1) 
  )
(define (MyCons list1 list2)
  (cons list1 list2) 
  )
(define (MyPair list1)
  (pair? list1) 
  )
(define (MyRemove lst)
  (if (null? lst)
      '()
      (my-append (car lst) (cdr lst))))

(define (my-append lhs rhs)
  (cond
    [(null? lhs)
     (MyRemove rhs)]
    [(pair? lhs)
     (cons (car lhs)
           (my-append (cdr lhs) rhs))]
    [else
     (cons lhs (MyRemove rhs))]))
;--------

(define (MyLambda list1)
  (startEval2 (caddr (car list1)) (cdr list1) (cadr (car list1)))
  )
(define (MyLet list1)
  (startEval2 (caddr (car list1)) (cdr list1) (cadr (car list1)))
  )
(define (MyLetrec list1)
  (startEval2 (caddr (car list1)) (cdr list1) (cadr (car list1)))
  )


;(MyLambda '((lambda (x y) (+ x y)) 10 5))
(startEval '((lambda (x y z) (car z)) (3 1) (7 3) (4 3)))
(startEval '((lambda (x y z a) (< a z)) 3 2 1 4))
;(startEval '((lambda (x y) (car x)) (3 1) (4 2)))
;(startEval '((lambda (x y) (+ (* y x) (+ x (+ x y)))) 5 2))
;(startEval '((lambda (x y) (+ (/ x y) (/ x y))) 6 2))

(startEval '((lambda (x y z) (+ (* y x) (+ x z))) 5 2 1))


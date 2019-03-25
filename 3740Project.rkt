#lang racket

;CPSC 3740 Final Project
;Matthew Davison, Taranjot Kaur, Zachary Nelson
;February 26th 2019


;Start Eval Function
;Takes in a list which is to be evaluated
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
     (myAdd(startEval(cadr list1))(startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a subtraction sign, subtract the next two items together
    [(equal? (car list1) '-)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (mySub(startEval(cadr list1))(startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a multiplication sign, multiply the next two items together
    [(equal? (car list1) '*)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (myMult(startEval(cadr list1))(startEval(caddr list1)))]
    ;---------------------------------------------------------------------------------
    ;If the first item in the list is a division sign, divide the next two items together
    [(equal? (car list1) '/)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (myDiv(startEval(cadr list1))(startEval(caddr list1)))]
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
     [else list1]
    )
  )

;---------------------------------------------------------
;myAdd Function, given two numbers, add them together
(define (myAdd num1 num2)
  (+ num1 num2)
  )
;----------------------------------------------------------
;mySub Function, given two numbers, subtract them
(define (mySub num1 num2)
  (- num1 num2)
  )
;---------------------------------------------------------
;myMult Function, given two numbers, multiply them
(define (myMult num1 num2)
  (* num1 num2)
  )
;--------------------------------------------------------
;myDiv Function, given two numbers, divide them
(define (myDiv num1 num2)
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
  (car list1) 
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


(startEval '(car (1 2 3)))
(startEval '(cdr (1 2 3)))
(startEval '(cons (2 3) 1))
(startEval '(pair? (1 2 3)))
(startEval '(if (= (+ 2 4) (* 2 3)) 7 0))

;lambda func for one expression x
(lambda (x) (+ x x))            

((lambda (x) (+ x x)) 3)                
;lambda func with let for x and y
(define subtract
  (lambda (x y)
    (- y x)))
(subtract 4 9)                 

(define adding
  (let ((x 3))
    (lambda (y) (+ x y))))
(adding 5)            
;defining letrec to for even odd numbers
(letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (even? 10)) 

#lang racket

;CPSC 3740 Final Project
;Zachary Nelson
;February 26th 2019


;Start Eval Function
;Takes in a list which is to be evaluated
(define (startEval list1)
  (cond
    ;If there is nothing in the list, return the empty list
    [(null? list1) '()]

    [(not(pair? list1)) list1]


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
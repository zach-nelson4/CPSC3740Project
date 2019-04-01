#lang racket

;---------------------------------
;CPSC 3740 Final Project
;---------------------------------

;This program is a Racket evaluator which takes in a list of racket parameters and evaluates them.
;To call it, type (startEval '(the list of racket parameters here)) into the console.

;Matthew Davison, Taranjot Kaur, Zachary Nelson
;February 26th 2019

;-------------------------------------------------------------
;Start Eval Function
;Takes in a list which is a racket program which is to be evaluated
;list1 is the list of racket parameters to evaluate
;-------------------------------------------------------------

(define (startEval list1)
  (cond
    ;If there is nothing in the list, return the empty list
    [(null? list1) '()]
    ;If it is not a pair return list
    [(not(pair? list1)) list1]

    [(equal? (car list1) 'quote)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyQuote(startEval(cadr list1)))]

    ;If the first item in the list is an addition sign, add the next two items together
    [(equal? (car list1) '+)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyAdd(startEval(cadr list1))(startEval(caddr list1)))]

    ;If the first item in the list is a subtraction sign, subtract the next two items together
    [(equal? (car list1) '-)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MySub(startEval(cadr list1))(startEval(caddr list1)))]

    ;If the first item in the list is a multiplication sign, multiply the next two items together
    [(equal? (car list1) '*)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyMult(startEval(cadr list1))(startEval(caddr list1)))]

    ;If the first item in the list is a division sign, divide the next two items together
    [(equal? (car list1) '/)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyDiv(startEval(cadr list1))(startEval(caddr list1)))]

    ;If the first item in the list is a equal
    [(equal? (car list1) 'equal?)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (MyEqual (startEval(cadr list1)) (startEval(caddr list1)))]

    ;If the first item in the list is a =
    [(equal? (car list1) '=)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyEqualSign(startEval(cadr list1)) (startEval(caddr list1)))]

    ;If the first item in the list is a =
    [(equal? (car list1) '<=)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyLessThanEqual(startEval(cadr list1)) startEval(caddr list1))]


    ;If the first item in the list is a =
    [(equal? (car list1) '>=)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyGreaterThanEqual(startEval(cadr list1)) (startEval(caddr list1)))]


    ;If the first item in the list is a >
    [(equal? (car list1) '>)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyGreater(startEval(cadr list1)) (startEval(caddr list1)))]

     ;If the first item in the list is a <
    [(equal? (car list1) '<)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
      (MyLesser(startEval(cadr list1)) (startEval(caddr list1)))]

    ;If the first item in the list is a if function, check condition then evaluate
    [(equal? (car list1) 'if)
     ;Evaluate the next element to see if it needs any further work done, then do the same for the one after
     (if (startEval (cadr list1) )
         (startEval (caddr list1) )
         (startEval (cadddr list1) ))]

    ;If the first item in the list is car
     [(equal? (car list1) 'car)
     ;Evaluate the next element to see if it needs any further work done
     (MyCar(startEval(cadr list1)))]

     ;If the first item in the list is cdr
     [(equal? (car list1) 'cdr)
     ;Evaluate the next element to see if it needs any further work done
     (MyCdr(startEval(cadr list1)))]

     ;If the first item in the list is cons
     [(equal? (car list1) 'cons)
     ;Evaluate the next element to see if it needs any further work done
     (MyCons(startEval(cadr list1)) (startEval(caddr list1)))]

     ;If the first item in the list is pair
     [(equal? (car list1) 'pair?)
     ;Evaluate the next element to see if it needs any further work done
     (MyPair(startEval(cadr list1)))]

     ;If the first item in the list is let, pass it into the MyLet function for further evaluation.
     [(equal? (car list1) 'let) (MyLet list1)];
     ;If the first item in the list is letrec, pass it into the MyLetrec function for further evaluation.
     [(equal? (car list1) 'letrec) (MyLetrec list1)];
     ;If the first item in the list is lambda, pass it into the MyLambda function for further evaluation.
     [(equal? (caar list1) 'lambda) (MyLambda list1 '() '())];

     [else list1]
    )
  )

;--------------------------------------------------
;startEval2 function
;If you are given a list with variables that need to be evaluated, this function is called
;list1 is the list of things that need to be evaluated
;list2 is the values of each of the variables
;list3 is the variable names
;--------------------------------------------------

(define (startEval2 list1 list2 list3)
  ;These statements check to see if the next part contains anything that is not known, such as function names, etc.
      (if (not(pair? list1))
          ;checks if the name and value lists are empty if so then give list
          ;else check if evaluation list (list1) is equal to a variale name
          
          (if (and (empty? list3)(empty? list2))
              list1
              ;Check if list1 matches a name in the list of variables
              ;eg. list1= x and list2= (1) list3 = (x)
              ;then list1 will become 1 because of substitution
              ;else if the list1 does not match the first element in list3
              ;then we check the rest of the names in list3 to find one that matches list1 
              (if (equal? list1 (car list3))
                  (car list2)
                  (startEval2 list1 (cdr list2) (cdr list3))
                  )
              )

          ;Check if length of list is one and remove redundant brackets
          ;eg. ((car x)) to (car x)
          ;else if the length is greater than 1 we check to see what the first item in the list is.
	  (if (equal? (length list1) 1)
                      (startEval2 (car list1) list2 list3)
		  (cond
                    ;If the first item in the list is a quote
		    [(and (pair? list1) (equal? (car list1) 'quote))
		     (MyQuote(startEval2 (MyRemove (cadr list1)) list2 list3))]
                    ;If the first item in the list is +
		    [(and (pair? list1) (equal? (car list1) '+))
		     (MyAdd(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is a -
                    [(and (pair? list1) (equal? (car list1) '-))
		     (MySub(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is a *
                    [(and (pair? list1) (equal? (car list1) '*))
		     (MyMult(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is a /
                    [(and (pair? list1) (equal? (car list1) '/))
		     (MyDiv(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is equal?
                    [(and (pair? list1) (equal? (car list1) 'equal?))
		     (MyEqual(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is a =
                    [(and (pair? list1) (equal? (car list1) '=))
		     (MyEqualSign(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is a <=
                    [(and (pair? list1) (equal? (car list1) '<=))
		     (MyLessThanEqual(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is a >=
                    [(and (pair? list1) (equal? (car list1) '>=))
		     (MyGreaterThanEqual(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is a >
                    [(and (pair? list1) (equal? (car list1) '>))
		     (MyGreater(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is a <
                    [(and (pair? list1) (equal? (car list1) '<))
		     (MyLesser(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is if
                    [(and (pair? list1) (equal? (car list1) 'if))
                     (if (startEval2 (cadr list1) list2 list3)
                         (startEval2 (caddr list1) list2 list3)
                         (startEval2 (cadddr list1) list2 list3))]
                    ;If the first item in the list is car
		    [(and (pair? list1) (equal? (car list1) 'car))
		     (MyCar(startEval2 (cadr list1) list2 list3))]
                    ;If the first item in the list is cdr
                    [(and (pair? list1) (equal? (car list1) 'cdr))
		     (MyCdr(startEval2 (cadr list1) list2 list3))]
                    ;If the first item in the list is cons
                    [(and (pair? list1) (equal? (car list1) 'cons))
		     (MyCons(startEval2 (cadr list1) list2 list3)(startEval2 (caddr list1) list2 list3))]
                    ;If the first item in the list is pair
                    [(and (pair? list1) (equal? (car list1) 'pair?))
		     (MyPair(startEval2 (cadr list1) list2 list3))]
                    ;If the first item in the list is lambda
                    [(and (pair? list1) (and (pair? (car list1)) (equal? (caar list1) 'lambda)))
		     (MyLambda list1 list2 list3)]
                    ;If the first item in the list is let
                    [(and (pair? list1) (equal? (car list1) 'let))
		     (MyLet list1)]
                    ;If the first item in the list is letrec
                    [(and (pair? list1) (equal? (car list1) 'letrec))
		     (MyLetrec list1)]
                    ;If it was not any of those, keep evaluating.
                    [else
                     ;if the names and values list is empty return list1
                     (if (and (empty? list3)(empty? list2))
                         ;(if (pair? (car list1)
                                    ;(startEval2 (append (list (startEval2 (car list1) list2 list3)) (list (startEval2 (cdr list1) list2 list3))) list2 list3))))
                                    list1
                                    ;)
                         ;else check for UNDEF from letrec
                         (if (and (and (pair? (car list1))(equal? (length (car list1)) 2)) (equal? (car(cdr(car list1))) "UNDEF"))
                             (startEval2 (cons (caar list1) (cdr list1)) list2 list3)
                             (if (and (and (pair? (car list2))(equal? (length (car list2)) 2)) (and (equal? (car(cdr(car list2))) "UNDEF") (equal? (car list1) (car list3))))
                                 (startEval2 (cons (caar list2) (cdr list1)) list2 list3)
                                 ;if it is not undef then it should be just a letrec function name and start that evaluation
                                 ;eg. (fact (+ 2 2)) then fact gets replaced if fact = (lambda (n) (* 2 n)) then the appended result is ((lambda (n) (* 2 n)) 4)
                                 (startEval2 (append (list (startEval2 (car list1) list2 list3)) (list (startEval2 (cdr list1) list2 list3))) list2 list3))))
                     ])
	       )
	)
  )

;---------------------------------------------------------
;---------------------------------------------------------
;Function Declarations
;The following functions are used to do the individual calculations based on the given symbol
;---------------------------------------------------------
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
;-------------------------------------------------------
;myEqual Function, given two numbers, return true if equal, false if not.
(define (MyEqual action1 action2)
  (equal? action1 action2)
  )
;------------------------------------------------------
;myEqualSign function. given two numbers, return true if equal, false if not
(define (MyEqualSign action1 action2)
  (= action1 action2)
  )
;------------------------------------------------------
;myLessThanEqual function. given two numbers, return true if action1 less than or equal to action2, false if not
(define (MyLessThanEqual action1 action2)
  (<= action1 action2)
  )
;------------------------------------------------------
;myGreaterThanEqual function. given two numbers, return true if action1 greater than or equal to action2, false if not
(define (MyGreaterThanEqual action1 action2)
  (>= action1 action2)
  )
;------------------------------------------------------
;myGreater function. given two numbers, return true if action1 greater than action2, false if not
(define (MyGreater action1 action2)
  (> action1 action2)
  )
;------------------------------------------------------
;myLesser function. given two numbers, return true if action1 lesser than action2, false if not
(define (MyLesser action1 action2)
  (< action1 action2)
  )
;------------------------------------------------------
;MyQuote function. given an action, quote it
(define (MyQuote action1)
  (quote action1)
  )
;------------------------------------------------------
;MyCar function. given a list, return the first element
(define (MyCar list1)
  (cond [(pair? list1)
      (car list1)])
  )
;------------------------------------------------------
;MyCdr function. Given a list, return the second element
(define (MyCdr list1)
  (cdr list1)
  )
;------------------------------------------------------
;myCons function. Construct a new list with list1 and list2
(define (MyCons list1 list2)
  (cons list1 list2)
  )
;------------------------------------------------------
;MyPair function. Checks if a given element is a pair or not
(define (MyPair list1)
  (pair? list1)
  )
;------------------------------------------------------
;MyRemove function. Removes the first element of the list
(define (MyRemove ele)
  (if (null? ele)
      '()
      (MyAppend (car ele) (cdr ele))))

;------------------------------------------------------
;MyAppend function. Appends the right to the left.
(define (MyAppend left right)
  (cond
    [(null? left)
     (MyRemove right)]
    [(pair? left)
     (cons (car left)
           (MyAppend (cdr left) right))]
    [else
     (cons left (MyRemove right))]))
;------------------------------------------------------
;MyLambda function. Given a lambda function, evaluate it by passing it into startEval2. See startEval2's comments for more information.

(define (MyLambda list1 list2 list3)
  ;check if pair
  (if (pair? (startEval2 (cdr list1) list2 list3))
  ;dont add to a list if it is a pair 
  (startEval2 (caddr (car list1)) (append (startEval2 (cdr list1) list2 list3) list2) (append (cadr (car list1)) list3))
  ;do add to a list if it is a not pair 
  (startEval2 (caddr (car list1)) (append (list (startEval2 (cdr list1) list2 list3)) list2) (append (cadr (car list1)) list3)))
  )

;------------------------------------------------------
;MyLet function. Given a let function, evaluate it by passing it into startEval2. See startEval2's comments for more information.

(define (MyLet list1)
  (startEval2 (caddr list1) (MyLetAttributesValues (cadr list1)) (MyLetAttributesNames (cadr list1)))
)

;------------------------------------------------------
;MyLetAttributeNames function.
(define (MyLetAttributesNames list1)
  ;extracting attribute names from let expression 
  (cond [(not(pair? list1)) list1]
        [(pair? list1) (cons (car(car list1)) (MyLetAttributesNames (cdr list1)))]
  )
)
(define (MyLetAttributesValues list1)
   ;extracting attribute values from let expression 
   (cond [(not(pair? list1)) list1]
        [(pair? list1) (cons (car(cdr(car list1))) (MyLetAttributesValues (cdr list1)))]
  )
)

;------------------------------------------------------
;MyLetrec function. Passes it into startEval2 for further evaluation.
(define (MyLetrec list1)
  (startEval2 (caddr list1) (MyLetrecAttributesValues (cadr list1)) (MyLetrecAttributesNames (cadr list1)))
)
(define (MyLetrecAttributesNames list1)
   ;extracting attribute names from letrec expression 
  (cond [(not(pair? list1)) list1]
        [(pair? list1) (cons (car(car list1)) (MyLetrecAttributesNames (cdr list1)))]
  )
)
(define (MyLetrecAttributesValues list1)
   ;extracting attribute values from letrec expression 
   (cond [(not(pair? list1)) list1]
        ;adds UNDEF to values in letrec 
        [(pair? list1) (cons (cons (car(cdr(car list1))) (list "UNDEF")) (MyLetrecAttributesValues (cdr list1)))]
  )
)

;--------------------------------------------------
;Test Cases
;--------------------------------------------------

;(startEval '((lambda (x y z) (car z)) (3 1) (7 3) (4 3)))
;(startEval '((lambda (x y z a) (* a z)) 3 2 5 4))
;(startEval '((lambda (x y z a) (> a z)) 3 2 1 4))
;(startEval '((lambda (x y) (car x)) (3 1) (4 2)))
;(startEval '((lambda (x y) (+ (* y x) (+ x (+ x y)))) 5 2))
;(startEval '(+ 3 3))

;((lambda (x) (+ x 1)) (* 2 ((lambda (x y) (+ x y)) 7 6)))
;(startEval '((lambda (x) (+ x 1)) (* 2 ((lambda (x y) (+ x y)) 7 6))))

(startEval '((lambda (x y) (+ x y)) (+ 3 3) (+ 2 2)))

;(startEval '(let ([x 3] [y 2] [z 2]) (+ x (+ y z))))
;(startEval '(let ((x (1 2 3)) (y (4 5 6))) (cons x y)))
;((lambda (n) (* 2 n)) 5)

;(startEval '(let ([x 3][y 2]) (+ x y)))
;(startEval '((lambda (x) (/ x x)) 2))
;(startEval '(letrec ([fact (lambda (n) (+ 2 n))])(fact 5)))
;(startEval '(letrec ([fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1)))))]) (fact 6)))
;(letrec ([fact (lambda (x y z) (if (= x 0) 1 (* x (fact (- x 1) y z))))]) (fact 6 7 8))
;(startEval '(letrec ([fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1)))))]) (fact 10)))
;(startEval '(letrec ([fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1)))))]) (fact ((lambda (n) (* 2 n)) 5))))
;((lambda (n) (* 2 n)) 5)
;((lambda (n) (* 2 n)) (/ 10 2))
;(let ((x '(1 2 3)) (y '(4 5 6))) (cons x y))

#lang racket

(define unicode_lambda (string->symbol "\u03BB")) ; defining lambda

(define (expr-compare x y) (expr-comp x y empty empty))

(define (expr-comp x y xdict ydict)
    (cond 
        [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) 
            (if x '% '(not %))
        ]
        [(not (and (list? x) (list? y)))  ;if one is not a list, no part of it can be equal, has to be before next condition to prevent contract violation
            (list 'if '% x y) ;list needed to concatenate different types
        ]
        [(not (equal? (length x) (length y))) ;if length not equal, has to be before next condition which assumes not empty
        (list 'if '% x y)
        ]
        [(or (equal? (first x) 'quote) (equal? (first y) 'quote)) ;if either or both are quotes no part of it can be equal
            (list 'if '% x y) 
        ]
        [(and (equal? (first x) (first y)) (equal? (first x) 'lambda)) ; both equal lambda
            (lambda-binding x y xdict ydict 'lambda) 
        ]
        [ (and (or (equal? (first x) unicode_lambda) (equal? (first x) 'lambda)) (or (equal? (first y) unicode_lambda) (equal? (first y) 'lambda))) ; both not lambda but other permutations of lambda
            (lambda-binding x y xdict ydict unicode_lambda) 
        ]
        [
            (and (or (equal? (first x) 'if) (equal? (first y) 'if) (equal? (first x) 'lambda) (equal? (first y) 'lambda) (equal? (first x) 'quote) (equal? (first y) 'quote)) (not (equal? (first x) (first y)))) ;head not equal and one is reserved keyword
            (list 'if '% x y) ; need to include ' and lambda????
        ]
        [
        else 
            (cons (expr-comp (first x) (first y) xdict ydict) (expr-comp (rest x) (rest y) xdict ydict)) 
         ;have to recursively pass in to evaluate without brackets
         ]
    )
)

(define (get-bindings argsx argsy xdict ydict) ;dict is a series of pairs
(cond
    [(equal? argsx '()) ;when u return
    (cons xdict ydict)
    ]
    [(and (equal? argsx argsy) (not (list? argsx))) ;either both are lists or both are not list, only need to check once
    (cons (cons (cons argsx argsx) xdict) (cons (cons argsy argsy) ydict))
    ]
    [(and (not (equal? argsx argsy)) (not (list? argsx)))
    (let [(bind 
        (let 
            create-binding [(x argsx) (y argsy)]     
            (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))    
        )
        )] 
        (cons (cons (cons argsx bind) xdict) (cons (cons argsy bind) ydict))
    ) 

    ]
    [(and (list? argsx) (equal? (first argsx) (first argsy))) ;no binding when equal
        (get-bindings (rest argsx) (rest argsy) (cons (cons (first argsx) (first argsx)) xdict) (cons (cons (first argsy) (first argsy)) ydict)) ;forms like a list of pairs
    ]
    [(list? argsx) ; if different arguments at same position
        (let [(bind 
            (let 
                create-binding [(x (first argsx)) (y (first argsy))]     
                (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))    
            )
            )] 
            (get-bindings (rest argsx) (rest argsy) (cons (cons (first argsx) bind) xdict) (cons (cons (first argsy) bind) ydict))
        )
    ]
)
)



(define (lambda-binding x y xdict ydict lambda-sign) ; check if equal and do lambda -- -----!!! and then if not a list and length is 1 then get bindings
    (cond 
        [(equal? (first (rest x)) (first (rest y)))
            (list lambda-sign 
                (first (rest x)) 
                (expr-comp (rest (rest x)) (rest (rest y)) xdict ydict) 
            )
        ]
        ;if length is same and arguments are different for lambda, add to dictionary
        [(or 
            (and (and (not (list? (first (rest x)))) (not (pair? (first (rest x)))) (not (pair? (first (rest y)))) (not (list? (first (rest y))))) (and (equal? (length (list (first (rest x)))) 1) (equal? (length (list (first (rest y)))) 1))) ;both are not a list and not pairs and length is 1
            (and (and (list? (first (rest x))) (list? (first (rest y)))) (equal? (length (first (rest x))) (length (first (rest y)))))
             ;comparing length of args ;if both are lists
        )
            (let 
            [
            (argsx (first (rest x)))
            (argsy (first (rest y)))
            (bodyx (first (rest (rest x))))
            (bodyy (first (rest (rest y))))
            (new-xdict (first (get-bindings (first (rest x)) (first (rest y)) empty empty)))
            (new-ydict (rest (get-bindings (first (rest x)) (first (rest y)) empty empty)))  ;call get-bindings with the arguments of x and arguments of y, initially empty dictionaries - function populates the dictionary
            ]
            (list
                    lambda-sign
                    (expr-comp (bind-with-dict argsx new-xdict) (bind-with-dict argsy new-ydict) (append new-xdict xdict) (append new-ydict ydict))
                    (expr-comp (bind-with-dict bodyx new-xdict) (bind-with-dict bodyy new-ydict) (append new-xdict xdict) (append new-ydict ydict)) ;to recursively check within lambda function
            )
            )
        ]
        [ else ; no binding for not same length
            (list 'if '% x y) 
        ])
) 


(define (bind-with-dict args dict)
    (cond 
    [(equal? args '()) 
        empty
    ]
    [(not (list? args)) ;one argument
    (dictionary args dict)
    ]
    [else
    (cons (dictionary (first args) dict) (bind-with-dict (rest args) dict)) ;concatenate all translated arguments
    ]
    )
)

(define (dictionary arg dict)
  (cond 
    [(equal? dict '()) ;dictionary is empty no more checking
        arg
    ]
    [(equal? arg (car (first dict))) ;car for pairs
        (cdr (first dict)) ;if find key return value else keep looking
    ]
    [else
        (dictionary arg (rest dict)) ; search rest of dict
    ]
  )
)

(define (test-expr-compare x y)

(and
    (equal? (eval `(let ((% #t)) ,(expr-compare x y))) (eval x)) (equal? (eval `(let ((% #f)) ,(expr-compare x y))) (eval y))
)
)

(define test-expr-x ;add more test cases
`(
    (12 20 "hello" "hello")
    ((lambda (a b) (list a b)) 1 2)
    (#f #f #t #t ; check different boolean
    ((lambda (a b) (list (list a b) b)) 1 2))
    ((lambda (a) (eq? a ((,unicode_lambda (a b) ((,unicode_lambda (a b) (a b)) b a)) a (lambda (a) a))))
    (lambda (b a) (b a)))
    (if x y z)
    (if x y y)
    (if x y y)
    (if x y y)
    (if x y y)
    ((,unicode_lambda (a) (if a a a)) #t)
    '(cons (1 2))
    '(cons (2 2))
    (f a)
    (lambda (a b) a) ;list vs pair
    (,unicode_lambda (a . b) a) ;equal pairs
    (lambda a a); single term
    (lambda a a) ; w/ & w/o brackets
    (lambda a a) ;same lambda

))

(define test-expr-y
`(
    (12 12 "hello" "hell")
    ((lambda (a b) (cons a b)) 1 2) ;same lambda different functions same numbers
    (#f #t #t #f ; check different boolean
    ((,unicode_lambda (a c) (list (list a c) c)) 3 4) ;one lambda, one short form, different variables - so binding, same nested functions different numbers
    )   
    ((,unicode_lambda (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a)) a (,unicode_lambda (b) a))))
    (lambda (a b) (a b)))
    (if x y y) ;if condition should combine
    (quote (x y y)) ;reserved keywords
    '(x y y)
    ((lambda (a) (f a)) 1)
    ((,unicode_lambda (a) (f a)) 1)
    ((,unicode_lambda (a c) (if a a c)) #t #f)    ; lambda with different lengths
    (quote (cons (1 2))) ;equal quote
    '(cons (1 2)) ;not equal quote
    (g b)
    (lambda (a . b) a)
    (lambda (a . b) a)
    (lambda b b)
    (lambda (a) a)
    (lambda a a)


))





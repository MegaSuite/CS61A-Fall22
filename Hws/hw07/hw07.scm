(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s)))

(define (caddr s) (car (cddr s)))

(define (ascending? asc-lst) 
    (or (null? asc-lst)
        (null? (cdr asc-lst)) 
        (and (ascending? (cdr asc-lst)) 
            (<= (car asc-lst) (cadr asc-lst)))))

(define (square n) (* n n))

(define (pow base exp)
    (if (= base 1) 1
            (cond
                ((= exp 0) 1)
                ((modulo exp 2) (* base (pow base (- exp 1))))
                (else (square (pow base (/ exp 2)))))))

(define (ascending? s) 
    (define (helper list last_label)
        (if (null? list)
            #t
            (if (>= (car list) last_label)
                (helper (cdr list) (car list))
                #f)))
    (helper s -1))

(define (my-filter pred s)
    (cond ((null? s) nil)
          ((pred (car s)) (cons (car s) (my-filter pred (cdr s))))
          (else (my-filter pred (cdr s)))))

            

(define (interleave lst1 lst2)
    (cond ((and (null? lst1) (null? lst2)) 
                nil)
          ((null? lst1)
                (cons (car lst2) (interleave lst1 (cdr lst2))))
          ((null? lst2)
                (cons (car lst1) (interleave (cdr lst1) lst2)))
          (else (cons (car lst1) (cons (car lst2) (interleave (cdr lst1) (cdr lst2)))))))

(define (no-repeats s)
  (define (contains? x lst)
    (cond ((null? lst) #f)
          ((equal? x (car lst)) #t)
          (else (contains? x (cdr lst)))))
  (define (helper s seen)
    (cond ((null? s) nil)
          ((contains? (car s) seen)
           (helper (cdr s) seen))
          (else
           (cons (car s)
                 (helper (cdr s) (cons (car s) seen))))))
  (helper s nil))

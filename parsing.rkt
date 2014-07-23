(module parsing racket/base
  ;;removes if strip-fn returns false
  (define (my-reverse-strip in strip-fn (out '()))
    (if (not (null? in))
	(if (strip-fn (car in))
	    (my-reverse-strip (cdr in) strip-fn (cons (car in) out))
	    (my-reverse-strip (cdr in) strip-fn out))
	out))

  (define (split-str str sep (len (string-length str)))
    (my-reverse-strip (let inner ((ind 0)
				  (last 0)
				  (strs '()))
			(cond [(= ind len)
			       (cons (substring str last ind) strs)]
			      [(eq? (string-ref str ind) sep)
			       (inner (add1 ind) (add1 ind)
				      (cons (substring str last ind) strs))]
			      [else
			       (inner (add1 ind)
				      last
				      strs)]))
		      (lambda (x)
			(not (or (equal? "" x)
				 (equal? "\n" x)
				 (equal? "\r\n"x))))))

  ;returns true if a list is longer than a certain length, O(len)
  (define (list-longer l len)
    (let inner ([i 0]
		[lis l])
      (cond [(null? lis)
	     #f]
	    [(> i len)
	     #t]
	    [else
	     (inner (add1 i) (cdr lis))])))

  ;makes hash of functions to apply in parse-string
  (define (make-environment . functions-and-names)
    (make-hash functions-and-names))

  (define (parse-string-list message environment . rest)
    (if (list-longer message -1) ;if the string is longer than 0 words
	(begin
	  (let ([verb (car message)]) ;let our "verb" be the first word
	    (let ([verb-proc (hash-ref environment verb #f)])
	      (if verb-proc
		  (apply verb-proc environment (cdr message) rest)
		  #f))))
	#f))

  (define (parse-string str environment . rest)
    (let ([message (split-str str #\space)]) ;split the string into words by space
      (apply parse-string-list message environment rest)))

  (provide (all-defined-out)))

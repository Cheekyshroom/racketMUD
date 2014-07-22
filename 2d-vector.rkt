(module 2d-vector racket/base
  (struct 2d-vector
    (data
     width
     height)
    #:mutable)

  ;makes a 2d-vector of width-height filled with value v
  (define (make-2d-vector width height (v #f))
    (2d-vector (build-vector 
		height
		(lambda (x)
		  (make-vector width v)))
	       width
	       height))

  (define (vector->2d-vector vec)
    (let ([h (vector-length vec)])
      (2d-vector vec
		 (if (zero? h)
		     0
		     (if (not (vector? (vector-ref vec 0)))
			 1
			 (vector-length (vector-ref vec 0))))
		 h)))

  (define (2d-vector-set! vec x y v)
    (vector-set! (vector-ref (2d-vector-data vec) y) x v))
 
  (define (2d-vector-get vec x y)
    (vector-ref (vector-ref (2d-vector-data vec) y) x))
 
  ;returns #f if outside of vector bounds instead of throwing exn
  (define (2d-vector-get-safe vec x y)
    (if (and (< x (2d-vector-width vec))
	     (< y (2d-vector-height vec))
	     (>= x 0)
	     (>= y 0))
	(2d-vector-get vec x y)
	#f))

  ;applies a procedure to each element of the vector, like 
  ;(fn (2d-vector-get vec x y) x y args)
  (define (2d-vector-apply! vec fn . args)
    (let loop ([y 0] [h (2d-vector-height vec)])
      (unless (= y h)
	(let loop ([x 0] [w (2d-vector-width vec)])
	  (unless (= x w)
	    (apply fn (2d-vector-get vec x y) x y args)
	    (loop (add1 x) w)))
	(loop (add1 y) h))))

  (provide (struct-out 2d-vector)
	   make-2d-vector
	   2d-vector
	   2d-vector-get
	   2d-vector-set!
	   2d-vector-get-safe
	   2d-vector-apply!
	   vector->2d-vector))

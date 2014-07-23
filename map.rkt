(module rackMUDmap racket/base
  (require "2d-vector.rkt")

  ;;;Node stuff
  (struct node
    (data
     n
     e
     w
     s)
    #:mutable)

  (define (make-node (data #f) (n #f) (e #f) (w #f) (s #f))
    (node data n e w s))

  ;returns a list of node exits in news order
  (define (node-exits node)
    (list (node-n node)
	  (node-e node)
	  (node-w node)
	  (node-s node)))

  ;connects two exits with setter functions
  (define (connect-exits! exit-setter exit neighbor-exit-setter neighbor-exit)
    (when exit
      (exit-setter exit neighbor-exit))
    (when neighbor-exit
      (neighbor-exit-setter neighbor-exit exit)))

  ;connect a node to 4 surrounding nodes
  (define (connect-nodes! node (n #f) (e #f) (w #f) (s #f))
    (connect-exits! set-node-n! s set-node-s! node)
    (connect-exits! set-node-e! w set-node-w! node)
    (connect-exits! set-node-w! e set-node-e! node)
    (connect-exits! set-node-s! n set-node-n! node)
    node)

  ;make a node and connect it to surrounding nodes
  (define (node-add! data (n #f) (e #f) (w #f) (s #f))
    (connect-nodes! (make-node data) n e w s))

  ;;;Room making functions
  (define (2d-vector->nodes vec (x 0) (y 0))
    ;;make a 2d-vector the size of our previous one, to hold the nodes
    (let ([nodes (make-2d-vector (2d-vector-width vec)
				 (2d-vector-height vec))])
      ;make a vector of nodes
      (2d-vector-apply! vec
			(lambda (v x y nodes)
			  (when v
			    (2d-vector-set! nodes x y
					    (make-node v))))
			nodes)
      ;now connect adjacent nodes to each other
      (2d-vector-apply!
       nodes
       (lambda (v x y)
	 (when v ;when we're on a node
	   (connect-nodes! v ;connect it to adjacent nodes, or if they don't exist #f
			   (2d-vector-get-safe nodes x (sub1 y))
			   (2d-vector-get-safe nodes (add1 x) y)
			   (2d-vector-get-safe nodes (sub1 x) y)
			   (2d-vector-get-safe nodes x (add1 y))))))
      (2d-vector-get-safe nodes x y)))

  ;(make-corridor 4 set-node-n! (lambda (i) 
  ;                               if (= i 2)
  ;                                  (make-corridor 6 set-node-e! 
  ;                                                 (lambda (i) (make-node i)))
  ;                                  (make-node i))))
  ;makes an L corridor
  (define (make-corridor length combinator-fn element-fn)
    (let loop ([i 0])
      (if (= i length)
	  (element-fn i)
	  (let ([n (element-fn i)])
	    (combinator-fn n (loop (add1 i)))
	    n))))

  (provide (all-defined-out)))

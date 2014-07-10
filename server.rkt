(module rMUD-server racket/base
  (require racket/tcp)
  (require racket/cmdline)

  (define outport (make-parameter 12345)) ;make this set by tcp-listen
  (define hostname (make-parameter "localhost"))
  (define args (current-command-line-arguments))
  (when (= 2 (vector-length args))
    (outport (vector-ref args 0))
    (hostname (vector-ref args 1)))

  (define (close-connections in out)
    (close-input-port in)
    (close-output-port out))

  (struct user
    ([parent]
     [log #:mutable]
     [in]
     [out]
     [kill #:mutable]
     [global-kill]
     [name #:mutable]))

  (define (add-log! user str)
    (set-user-log! user (cons str (user-log user))))

  (define (print-log user)
    (let inner ((log (user-log user)))
      (unless (null? log)
	(inner (cdr log))
	(display (car log))
	(newline))))

  (define (make-user (parent #f) (in #f) (out #f) (global-kill #f) (name "nameless-user"))
    (user parent
	  '() 
	  (if in in (current-input-port))
	  (if out out (current-output-port))
	  #f
	  global-kill
	  name))

  (struct userlist
    ([users #:mutable]
     [count #:mutable]
     [global-kill])
    #:transparent)

  (define (make-userlist (global-kill (box #f)))
    (userlist '()
	      0
	      global-kill)) ;global-kill should be a box

  ;;applies a function to each user in a userlist
  (define (apply-userlist fn ulist . rest)
    (let inner ((users (userlist-users ulist)))
      (unless (null? users)
	(fn (car users) rest)
	(inner (cdr users)))))

  ;;applies a function to each user in a userlist, and removes the users
  ;;that it returns false for
  (define (apply-remove-userlist fn ulist . rest)
    (let inner ((users (userlist-users ulist)))
      (cond [(null? users) ;end of list
	     '()]
	    [(not (fn (car users) rest)) ;function applies to it and is false
	     (set-userlist-count! ulist (sub1 (userlist-count ulist))) ;decrement user count
	     (inner (cdr users))]
	    [else ;function has applied and returned true
	     (cons (car users) (inner (cdr users)))])))

  (define (connection-killed-close! user . rest)
    (if (or (user-kill user)
	    (unbox (user-global-kill user))) ;if the user is killed in any way
	(begin 
	  (close-connections (user-in user) (user-out user)) ;close their connections
	  #f) ;and signal that apply-remove-userlist can remove it
	#t)) ;else say that it can't remove it
  
  (define (add-user! ulist user)
    ;add a user to the ulist
    (set-userlist-users! ulist (cons user (userlist-users ulist)))
    ;increment user count
    (set-userlist-count! ulist (add1 (userlist-count ulist))))

  (define (read-all-chars port)
    (let ((st (open-output-string)))
      (let loop ()
	(when (char-ready? port)
	  (display (read-char port) st)
	  (loop)))
      (get-output-string st)))

  (define (read-all-chars-strip port strip-fn)
    (let ((st (open-output-string)))
      (let loop ()
	(when (char-ready? port)
	  (let ((r (read-char port)))
	    (when (strip-fn r)
	      (display r st)))
	  (loop)))
      (get-output-string st)))

  (define (read-all-chars-blocking port (strip-fn #f))
    (let loop ()
      (if (char-ready? port)
	  (if strip-fn
	      (read-all-chars-strip port strip-fn)
	      (read-all-chars port))
	  (loop))))

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
			(not (equal? "" x)))))

  ;returns true if a list is longer than a certain length, O(len)
  (define (list-longer l len)
    (let inner ((i 0)
		(lis l))
      (cond [(null? lis)
	     #f]
	    [(> i len)
	     #t]
	    [else
	     (inner (add1 i) (cdr lis))])))

  ;string user -> returns false if input wasn't recieved
  (define (handle-string user str)
    (if (equal? "" str) ;if they didn't say anything
	#f
	(begin ;if they did
	  (let ((message (split-str str #\space)))
	    (add-log! user str) ;add it to our message log
	    (cond [(equal? "quit" str) ;if it was "quit" (or some other quit message in the future
		   (display "Bye\r\n" (user-out user)) 
		   (flush-output (user-out user))
		   (set-user-kill! user #t) ;set the user's killswitch to true
		   #t]
		  [(equal? "global-quit" str) ;if we're killing the server
		   (display "Server killed\n")
		   (flush-output)
		   (set-box! (user-global-kill user) #t)
		   #t]
		  [(equal? "name" (car message))
		   (if (list-longer message 0)
		       (set-user-name! user (cadr message))
		       (begin
			 (fprintf (user-out user) 
				  "Enter a name after \"name\" next time\r\n")
			 (flush-output (user-out user))))]
		  [else ;else handle it normally
		   (fprintf (user-out user) "~a Users\r\n" 
			    (userlist-count (user-parent user)))
		   (flush-output (user-out user))
		   (printf "~a said: ~a\r\n" (user-name user) str)
		   (flush-output)
		   (apply-userlist 
		    (lambda (u x)
		      (unless (equal? (user-name u) (car x))
			(fprintf (user-out u) "~a says: ~a\r\n" (user-name user) str)
			(flush-output (user-out u))))
		    (user-parent user)
		    (user-name user))
		   #t])))))

  ;;gets input from the users in socket, then handles it
  (define (input-handle user . rest)
    (unless (or (user-kill user)
		(unbox (user-global-kill user)))
      (let ((str (read-all-chars-strip (user-in user)
				       (lambda (c)
					 (cond [(eq? c #\return) #f]
					       [(eq? c #\newline) #f]
					       [else #t])))))
	(handle-string user str))))

  (define (accept-make-user server ulist)
    (define-values (s-in s-out) (tcp-accept server))
    (make-user ulist s-in s-out (userlist-global-kill ulist)))

  (define (accept-add-user! ulist server)
    (add-user! ulist (accept-make-user server ulist)))

  (define (begin-server (port #f))
    ;;start listening on our specified port
    (define server (tcp-listen (if port
				   port
				   (if (string? (outport))
				       (string->number (outport))
				       (outport)))))

    (define global-kill (box #f))
    (define users (make-userlist global-kill))
    
    (let loop ()
      (unless (unbox global-kill)
	(when (tcp-accept-ready? server) ;if a connection is pending
	  (accept-add-user! users server)) ;add a user from that connection
	;;here we handle the users
	(apply-userlist input-handle users)
	(set-userlist-users! users (apply-remove-userlist connection-killed-close! users))
	(loop)))
    (tcp-close server))

  (provide begin-server outport))

(module rMUD-server racket/base
  (require racket/tcp)
  (require racket/cmdline)

  (define outport (make-parameter 12346)) ;make this set by tcp-listen
  (define hostname (make-parameter "localhost"))
  (define args (current-command-line-arguments))
  (when (= 2 (vector-length args))
    (outport (vector-ref args 0))
    (hostname (vector-ref args 1)))

  (struct user
    ([log #:mutable]
     [in]
     [out]
     [kill #:mutable]
     [global-kill])
    #:transparent)

  (define (add-log! user str)
    (set-user-log! user (cons str (user-log user))))

  (define (print-log user)
    (let inner ((log (user-log user)))
      (unless (null? log)
	(inner (cdr log))
	(display (car log))
	(newline))))

  (define (make-user (in #f) (out #f) (global-kill #f))
    (user '() 
	  (if in in (current-input-port))
	  (if out out (current-output-port))
	  #f
	  global-kill))

  (struct userlist
    ([users
      #:mutable]
     [in
      #:mutable]
     [out
      #:mutable]
     [global-kill]))

  (define (make-userlist (global-kill (box #f)))
    (userlist '()
	      '()
	      '()
	      global-kill)) ;global-kill should be a box
  
  (define (add-user! ulist user)
    (set-userlist-users! (cons user (userlist-users ulist)))
    (set-userlist-in! (cons (user-in user)
			    (userlist-in ulist)))
    (set-userlist-out! (cons (user-out user)
			     (userlist-out ulist))))

  (define (close-connections in out)
    (close-input-port in)
    (close-output-port out))

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

  ;string user -> returns false if input wasn't recieved
  (define (handle-string user str)
    (if (equal? "" str) ;if they didn't say anything
	#f
	(begin ;if they did
	  (add-log! user str) ;add it to our message log
	  (cond [(equal? "quit" str) ;if it was "quit" (or some other quit message in the future
		 (display "Bye\r\n" (user-out user)) 
		 (flush-output (user-out user))
		 (set-user-kill! user #t) ;set the user's killswitch to true
		 #t]
		[else ;else handle it normally
		 (display "You said: " (user-out user))
		 (display str (user-out user))
		 (display "\r\n" (user-out user))
		 (flush-output (user-out user))
		 (display str)
		 (newline)
		 #t]))))

  ;;gets input from the users in socket, then handles it
  (define (input-handle user)
    (unless (or (user-kill user)
		(unbox (user-global-kill user)))
      (let ((str (read-all-chars-strip (user-in user)
				       (lambda (c)
					 (cond [(eq? c #\return) #f]
					       [(eq? c #\newline) #f]
					       [else #t])))))
	(handle-string user str))))

  (define (accept-make-user server global-kill)
    (define-values (s-in s-out) (tcp-accept server))
    (make-user s-in s-out global-kill))

  (define (accept-add-user! ulist server)
    (add-user! ulist (accept-make-user server (userlist-global-kill ulist))))

  (define (begin-server)
    ;;start listening on our specified port
    (define server (tcp-listen (if (string? (outport))
				   (string->number (outport))
				   (outport))))

    (define global-kill (box #f))
    (define users (make-userlist global-kill))
    
    (let loop ((usern 0))
      (let block () ;;block until someone wants to connect
	(unless (or (tcp-accept-ready? server)
		    (unbox global-kill)) ;wait until we have an incoming connection
	  (block)))
      (unless (unbox global-kill) ;;when we know we do, check if we -can- accept it, if not end
	(accept-add-user! users server) ;builds up a list of users
	
	(loop (add1 usern))))
    (tcp-close server))

  (provide begin-server outport handle-string (struct-out user)))

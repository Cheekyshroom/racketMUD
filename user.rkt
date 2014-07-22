(module user racket/base
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
    (let inner ([users (userlist-users ulist)])
      (unless (null? users)
	(fn (car users) rest)
	(inner (cdr users)))))

  (define (add-user! ulist user)
    ;add a user to the ulist
    (set-userlist-users! ulist (cons user (userlist-users ulist)))
    ;increment user count
    (set-userlist-count! ulist (add1 (userlist-count ulist))))

  ;;applies a function to each user in a userlist, and removes the users
  ;;that it returns false for
  (define (apply-remove-userlist fn ulist . rest)
    (let inner ([users (userlist-users ulist)])
      (cond [(null? users) ;end of list
	     '()]
	    [(not (fn (car users) rest)) ;function applies to it and is false
	     (set-userlist-count! ulist (sub1 (userlist-count ulist))) ;decrement user count
	     (inner (cdr users))]
	    [else ;function has applied and returned true
	     (cons (car users) (inner (cdr users)))])))
  
  (provide (struct-out user)
	   (struct-out userlist)
	   add-user!
	   add-log!
	   print-log
	   apply-remove-userlist
	   apply-userlist
	   make-userlist
	   make-user))
	   

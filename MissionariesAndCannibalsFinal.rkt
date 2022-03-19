#lang scheme/load

; Cormac helped me with my ss and gg

; DFS call    (generalized-search (goal? finish) (extend-paths) append (list (list start)) '() 0)))
; BFS call    (generalized-search (goal? finish) (extend-paths) (appendToEnd) (list (list start)) '() 0)))
(define generalized-search
  (lambda (finish path-Extender extend-method path-container closed nodeCnt)
    (newline) (newline) (newline)
    (display "Current set of visited nodes: ")
    (display closed)
    (newline)
    (display "Current Path-container: ")
    (display path-container)
    (newline)
    (cond
      ((null? path-container) (begin (display "search complete") (newline)))
      ((finish (car path-container)) 	
				(display nodeCnt)
       				(display " nodes examined")
       				(newline)
       				(display (reverse (car path-container)))
       				(newline)
       				(display ""))

      ((member? (caar path-container) closed)
       				(display "reject path " )
       				(display (reverse (car path-container)))
       				(newline)
       				(generalized-search finish path-Extender extend-method (cdr path-container) closed (+ 1 nodeCnt)))

      (else 
       (display "Extending path ")
            (display (reverse (car path-container)))
            (newline)
            ;(display "---------------")
            ;(newline)
            (generalized-search finish path-Extender extend-method
                                (extend-method (path-Extender (car path-container)) (cdr path-container))
                                (cons (caar path-container) closed)
								(+ 1 nodeCnt))))))



;;
;; Missionaries&Cannibals.scm
;;

;;
;; Describe a representation for a graph
;;
; A node in the semantic net is a 3-tuple (a b c).  Where
; a represents the number of missionaries on the left side.
; b represents the number of cannibals on the left side.
; c represents which side the boat is on.
; A valid state is one where a and b are between 0 and 3 and c is R or L.

; ss is our start state
(define ss '(3 3 L))
; gg is out goal state
(define gg '(0 0 R))

;;
;; Provide a function, given a partial path and a specified representation, that will
;; create n new paths; each an extension of the provided partial path by one step.
;; Return the n new paths as a list of paths.
;;

(define extend-paths
  (lambda ()
    (lambda (x)
      (get-new-paths x (get-next-step-list (car x))))))

(define get-new-paths
  (lambda (path next-step-list)
    (cond
      ((null? next-step-list) '())
      ((member? (car next-step-list) path) (get-new-paths path (cdr next-step-list)))
      (else (cons (cons (car next-step-list) path) 
                  (get-new-paths path (cdr next-step-list)))))))
    
;;
;; Generates a list of lists that either consists of all the possible nodes for if the boat is on the left side,
;; or all the nodes possible for if the boat is on the right side.
;;

(define get-next-step-list
  (lambda (lst)
    (cond
      ((eqv? (third lst) 'L)
       (valid-states (left_side lst)))
      ((eqv? (third lst) 'R)
       (valid-states (right_side lst)))
      )))

;;
;; Filters out invalid states
;;

(define valid-states
  (lambda (lsts)
    (cond
     ((null? lsts)'() )
     ((< (first (first lsts)) (second (first lsts)))
      (valid-states (cdr lsts)))
     ((or (or (> (first (first lsts)) 3) (< (first (first lsts)) 0)) (> (second (first lsts)) 3) (< (second (first lsts)) 0))
      (valid-states (cdr lsts)))
     (else (append (list (first lsts)) (valid-states (cdr lsts))
     )))))

;;
;; Generate a list of lists consisting of all possible states if the boat is on the left
;;

(define left_side
  (lambda (lst)
    (append (move_x_missionaries_L lst 1)
            (append (move_x_missionaries_L lst 2)
                    (append (move_x_cannibals_L lst 1)
                            (append (move_x_cannibals_L lst 2)
                                    (move_both_L lst)
                        ))))))

;;
;; Generate a list of lists consisting of all possible states if the boat is on the right
;;

(define right_side
  (lambda (lst)
    (append (move_x_missionaries_R lst 1)
            (append (move_x_missionaries_R lst 2)
                    (append (move_x_cannibals_R lst 1)
                            (append (move_x_cannibals_R lst 2)
                                    (move_both_R lst)
                        ))))))

;;
;; A 3 tuple s.t missionaries are subtracted by 1 or 2 and the boat moves to the right.
;;

(define move_x_missionaries_L
  (lambda (lst x)
    (cond
      ((eq? x 1)
       (list (cons (- (first lst) 1) (list (second lst) 'R))))
      ((eq? x 2)
       (list (cons (- (first lst) 2) (list (second lst) 'R))))
      )))

;;
;; A 3 tuple s.t Cannibals are subtracted by 1 or 2 and the boat moves to the right.
;;

(define move_x_cannibals_L
  (lambda (lst x)
    (cond
      ((eq? x 1)
       (list (cons (first lst) (list (- (second lst) 1) 'R))))
      ((eq? x 2)
       (list (cons (first lst) (list (- (second lst) 2) 'R))))
      )))

;;
;; A 3 tuple s.t Missionaries and Cannibals are subtracted by 1 and the boat moves to the right.
;;

(define move_both_L
  (lambda (lst)
    (list (cons (- (first lst) 1) (list (- (second lst) 1) 'R)))
  ))

;;
;; A 3 tuple s.t missionaries are added on by 1 or 2 and the boat moves to the left.
;;

(define move_x_missionaries_R
  (lambda (lst x)
    (cond
      ((eq? x 1)
       (list (cons (+ (first lst) 1) (list (second lst) 'L))))
      ((eq? x 2)
       (list (cons (+ (first lst) 2) (list (second lst) 'L))))
      )))

;;
;; A 3 tuple s.t cannibals are added on by 1 or 2 and the boat moves to the left.
;;

(define move_x_cannibals_R
  (lambda (lst x)
    (cond
      ((eq? x 1)
       (list (cons (first lst) (list (+ (second lst) 1) 'L))))
      ((eq? x 2)
       (list (cons (first lst) (list (+ (second lst) 2) 'L))))
      )))

;;
;; A 3 tuple s.t Missionaries and Cannibals are added on by 1 and the boat moves to the left.
;;

(define move_both_R
  (lambda (lst)
    (list (cons (+ (first lst) 1) (list (+ (second lst) 1) 'L)))
  ))
      

;;-----------------------------------------------
;;
;; Provide a function, given a partial path, that will determine if the goal state 
;; has been reached.
;;

(define goal?
  (lambda (finish)
    (lambda (x)
      (equal? finish (car x)))))


(define 8p-dfs
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths) append (list (list start)) '() 0)))


(define 8p-bfs
  (lambda (start finish)
    (generalized-search (goal? finish) (extend-paths) (appendToEnd) (list (list start)) '() 0)))





;;------------------ Utility Functions --------------------
(define appendToEnd
  (lambda ()
	(lambda (newStuff oldStuff)
      (reverse (append newStuff (reverse oldStuff))))))



(define member? (lambda (a lat)
                  (cond
                    ((null? lat) #f)
                    (else (or (equal? (car lat) a)
                              (member? a (cdr lat)))))))

(define list-index
  (lambda (s los)
    (list-index-helper s los 0)))

(define list-index-helper
  (lambda (s los cnt)
    (cond
      ((null? los) -1)
      ((eq? s (car los)) cnt)
      (else (list-index-helper s (cdr los) (+ cnt 1))))))

(define swapper
  (lambda (pos1 pos2 slst)
    (let ((s1 (list-ref slst pos1))
          (s2 (list-ref slst pos2)))
      (map
        (lambda (selement)
          (cond
            ((eq? selement s1) s2)
            ((eq? selement s2) s1)
            (else selement)))
        slst))))
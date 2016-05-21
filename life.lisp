;;;; Conway's Game of Life.

;; Make sure that SBCL does tail-call optimisation.
(declaim (optimize #+sbcl (sb-c::merge-tail-calls 3) #+sbcl (sb-c::insert-debug-catch 0))) 

(defun make-board ()
  "Creates the board that the game takes place on using list comprehensions."
  (loop
     for i upto 9 collect
       (loop
	  for j upto 9 collect
	    ;; ■ is a populated square, □ is a dead one.
	    (if (= 2 (random 3 (make-random-state T))) "■" "□"))))

(defun display-board (board)
  "Displays the board in a nice, pretty way."
  (dolist (lst board)             ; For each list in the board...
    (format T "~{~a~^ ~}~%" lst)) ; print it to the console, joined by spaces ( ~^ ).
  (format T "Mash buttons to continue. Type Q to quit. ~%"))

(defun nth-with-default (x lst default)
  "Makes sure that `x` is within the bounds of the list.
   If it isn't, it returns a default value."
  (if (> (length lst) x -1)
      (nth x lst)
      default))
  
(defun neighbours (x y board)
  "Finds the neighbours of a square based on its coordinates."
  ;; Flatten the list that the loop returns.
  (apply
   'concatenate
   'list
   (loop
      for mx from -1 to 1 collect
        (loop
 	   for my from -1 to 1
	   ;; Make sure that mx and my aren't both equal to 0.
	   ;; If they were, then the square itself would be counted as one of its neighbours.
	   when (not (and (zerop mx) (zerop my))) collect
	     (nth-with-default (+ y my) (nth-with-default (+ x mx) board NIL) "□")))))

(defun sum-neighbours (x y board)
  "Returns the number of living neighbours that a square has."
  (reduce
   (lambda (num square)
     (+ num (if (equal "■" square) 1 0))) ; Living squares = 1, dead squares = 0.
   (neighbours x y board)
   :initial-value 0))

(defun update-square (x y board)
  "Determines whether a square should live or die."
  ;; Bind it using a `let` to make sure it's not computed twice.
  (let ((neighbour-sum (sum-neighbours x y board)))
    (if (or (and (equal (nth y (nth x board)) "■") ; If the cell is alive...
		 (= neighbour-sum 2))              ; and it has exactly 2 neighbours...
	    (= neighbour-sum 3))                   ; or it has exactly 3 neighbours...
	"■"                                        ; it's alive!
	"□")))                                     ; Otherwise it's dead.

(defun update-board (board)
  "Creates a new board with each tile updated based on the previous state of the board."
  (loop
     for i upto 9 collect
       (loop
	  for j upto 9 collect
	    (update-square i j board))))

(defun state-loop (state)
  "The main game loop.
   Displays the state, then recurses with the updated state."
  (display-board state)
  (if (equal "q" (string-downcase (read-line)))
      (print "Bye!")
      (state-loop (update-board state))))

;;;; It might be a mess, but at least there's no mutable state! :)

(defun begin ()
  (state-loop (make-board)))

(begin)

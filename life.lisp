;;;; Conway's Game of Life.

;; Make sure that SBCL does tail-call optimisation.
(declaim (optimize (debug 0) (space 3) (speed 0)))

(format T "Please input a board size.~%")

;; The string representing a living square.
(defvar *live* "~c[34m■ ")

;; The string representing a dead square.
(defvar *dead* "~c[37;1m□ ")

;; The string representing a newly born square.
(defvar *new* "~c[36m▧ ")

(defun make-board (limit)
  "Creates the board that the game takes place on using list comprehensions."
  (loop
     for i from 1 to limit collect
       (loop
	  for i from 1 to limit collect
	    ;; Randomly populate the board with living squares.
	    (if (= 2 (random 3 (make-random-state T))) *live* *dead*))))

(defun deadp (square)
  (equal *dead* square))

;; Give up all hope, all ye who enter here D:
(defun display-board (board limit)
  "Displays the board in a nice, pretty way."
  (dolist (lst board)
    (format
     T
     (apply
      'format
      'NIL
      ;; Concatenate all the strings in the list together, then add a new line.
      (concatenate 'string (apply 'concatenate 'string lst) "~%")
      ;; Every `~c` in every string needs a corresponding #\ESC.
      (make-list limit :initial-element #\ESC))))
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
	     (nth-with-default (+ y my) (nth-with-default (+ x mx) board NIL) "~c[37;1m□ ")))))

(defun sum-neighbours (x y board)
  "Returns the number of living neighbours that a square has."
  (reduce
   (lambda (num square)
     (+ num (if (deadp square) 0 1))) ; Living squares = 1, dead squares = 0.
   (neighbours x y board)
   :initial-value 0))

(defun update-square (x y board)
  "Determines whether a square should live or die."
  ;; Bind it using a `let` to make sure it's not computed twice.
  (let ((neighbour-sum (sum-neighbours x y board))
	(this-square (nth y (nth x board))))
    (cond
      ;; If the square is dead and has 3 neighbours, it's a new square.
      ((and (deadp this-square) (= neighbour-sum 3))
       *new*)
      ;; If the square is alive and has 2 or 3 neighbours, it's still alive.
      ((and (not (deadp this-square)) (>= 3 neighbour-sum 2))
       *live*)
      ;; Otherwise, it's dead.
      (T *dead*))))

(defun update-board (board limit)
  "Creates a new board with each tile updated based on the previous state of the board."
  (loop
     for i upto limit collect
       (loop
	  for j upto limit collect
	    ;; Update each tile.
	    (update-square i j board))))

(defun state-loop (state limit)
  "The main game loop.
   Displays the state, then recurses with the updated state."
  (display-board state limit)
  (if (equal "q" (string-downcase (read-line)))
      (format T "Bye!~%")
      (state-loop (update-board state (- limit 1)) limit)))

;;;; It might be a mess, but at least there's no mutable state! :)

(defun begin ()
  (let ((limit (parse-integer (read-line))))
    (if (> limit 35)
	(progn
	  (format T "~c[31mThat's too big." #\ESC)
	  (begin))
	(state-loop (make-board limit) limit))))

(begin)


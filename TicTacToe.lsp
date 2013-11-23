;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TicTacToe by Abhishek Kharche;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *winning-positions* '((0 1 2)(3 4 5)(6 7 8)(0 3 6)(1 4 7)(2 5 8)(0 4 8)(2 4 6)))
;; Call init to play the game
(defun init ()
	(let ((board '(nil nil nil nil nil nil nil nil nil));; Initializing BOARD to 9 NIL spots
		  (maxdepth nil)								;; Initializing maxdepth to nil
		  (user nil))									;; Initializing user to nil
		  
		  ;;Now assign values to these variables by asking to user
		  (format t "~%Your choice X or O? ")
		  (setf user (read))							;; set user's preference X or O
		  (format t "~%Select a value from 1 to 9 for maximum depth- ")
		  (setf maxdepth (read))					;; Set maxdepth to whatever user inputs

		  (printBoard board)							;; print the nil board
		  (setf board (userMove board user))			;; get user's move (see function userMove)
		  (printBoard board)							;; print the board with human's move
		  (do ()  										;; loop till someone wins or draw
				((or (whoWins *winning-positions* board 'X) (whoWins *winning-positions* board 'O) (gameDraw board)) ; until game over
				 (cond ((gameDraw board) 'draw)			;; if draw then print draw
					  ((whoWins *winning-positions* board user) (princ "User Wins")) ;; check if human wins and print that
					  (t (princ "Computer Wins"))))		;; check if computer wins and print that
				(setf board (computerMove board (opponent user) maxdepth))		;; opponent is to choose other token X for 0 and 0 for X
				(printBoard board)						;; print board after computer's move
				(when (not (or (whoWins *winning-positions* board 'X) (whoWins *winning-positions* board 'O) (gameDraw board)))
					  (setf board (userMove board user))(printBoard board)))))		;; get human's move and print board

(defun opponent (user)									;; converts 'O to 'X and vice versa
	(cond((equal user 'X)'O)
		 ((equal user 'O)'X)))
 
					  
;; A function to print the board for user to see
(defun printBoard (board)
  (format t " _ _ _~%")
  (format t "|~A|~A|~A|~%" (place 0 board) (place 1 board) (place 2 board))
  (format t " - - -~%")
  (format t "|~A|~A|~A|~%" (place 3 board) (place 4 board) (place 5 board))
  (format t " - - -~%")
  (format t "|~A|~A|~A|~%" (place 6 board) (place 7 board) (place 8 board))
  (format t " - - -~%"))

(defun place (pos board)
  (let ((x (nth pos board)))							;; this function will find posth element in board, for eg 0th in board, 3rd in board etc...
       (if x x " "))) 									;; This will change all nils to blank which we initialize at start		


(defun whoWins (win board user)							;; check if someone wins
  (cond((null win) nil) 
       ((and (eq (nth (nth 0 (car win)) board) user)(eq (nth (nth 1 (car win)) board) user)(eq (nth (nth 2 (car win)) board) user))t) 
	         ;; gets (3,4,5) then get 0th term i.e. 3 and get 3rd position in board and compare to user
	   (t (whoWins (cdr win) board user))))				;; iterate till all items in wins ends

(defun gameDraw (board)    								;; check if drawn
  (cond((whoWins *winning-positions* board 'X) nil) 	;; we have to check if someone wins before declaring as draw
       ((whoWins *winning-positions* board 'O) nil) 
       (t (not (member nil board)))))					;; when nil is not a member of board (board full)

(defun moveForward (board user)							;; to get the next moves
  (cond((whoWins *winning-positions* board (opponent user))nil)	;; check if opponent won 
	   (t(moveForwardxx nil board user))))				;; forever true condition

(defun moveForwardxx (x board user)						;; to get the possible moves at empty spots
  (cond ((null board) nil)
		((null (car board))
			   (cons (append x (list user) (cdr board))(moveForwardxx (append x '(()))(cdr board)user))) ;; call recursively with null list
		(t (moveForwardxx (append x (list (car board)))(cdr board)user))))	;; call recursively with car board

;; Ask user to play
(defun userMove (board user)
  (let ((rownum nil)									;; Initialize rownum to null	
        (columnnum nil)									;; Initialize columnnum to null
		(slot nil))										;; Initialize slot to null		
		;; Ask user for rownum and column number of his move
		(format t "~%Enter Row Number (Min 0 and Max 2): ")
		(setf rownum (read))
		(format t "~%Enter Column Number (Min 0 and Max 2): ")
		(setf columnnum (read))
		(setf slot (+ columnnum(* 3 rownum)))			;; if choice is (1,1) then 3*1+1 i.e 4th position, if choice is (2,1) then 2*3+1 i.e 7th position, Note that first spot is 0 and last is 8
		(cond ((or (< slot 0) (> slot 8)) (format t "~%Invalid move")(userMove board user))
			  ((null (nth slot board)) (replacePos slot user 0 board ))
			  (t (format t "~%Please select vacant position") (userMove board user)))))

;; A function to replace the positions
(defun replacePos (pos x a b)
  (cond((equal a pos )(cons x (cdr b)))
       (t(cons (car b)(replacePos pos x (1+ a) (cdr b))))))

(defparameter *max-val* 9) 								;; maximum value of a board which can be between 0 to 8

;; Ask computer to play
(defun computerMove (board user max-search)
  (let ((board (minimax board user *max-val* (- *max-val*) max-search 0))) board))

;; A function to implement minimax algorithm with alpha beta pruning
;; A minimax algorithm is implemented using book resource
(defun minimax (board user positivemax negativemax maxdepth depth)
  (if (equal depth maxdepth)(heuristic board)		;; when depth is equal to maximum depth set then call heuristic functions
	  (let ((newboard (moveForward board user)))  	;; else rearrange the board by next move
  (if (null newboard)(heuristic board)				;; call heuristic in case of null of newboard
	  (do ((y nil)									;; else have a do loop and assign y to null and x as first element of newboard for later use
		  (x (car newboard)))
		  ((null newboard) (if (= depth 0) x negativemax))
			(setf y (- (minimax (car newboard) (opponent user) (- negativemax)(- positivemax) maxdepth (+ 1 depth))))	;; reassign the value for y
			(when (> y negativemax)					;; do this till y is more than our negative threshold
			  (setf negativemax y)					;; reassign values to negativemax and x and execute loop again
			  (setf x (car newboard)))
			(if (>= negativemax positivemax)(setf newboard nil)  ;; end the loop for this condition
				(setf newboard (cdr newboard))))))))
				
				
;; Heuristic function
;; This is calculated using  the formula (100*A + 10*B + C) - (100*D + 10*E + F)
;; where:
;; A = number of lines of 3 X's (e.g., a win)
;; B = number of unblocked lines with 2 X's
;; C = number of unblocked lines with a single X
;; E, F, and G give corresponding numbers of lines of O's in various cofigurations.		

(defun heuristic (board)
	(-(compute-heuristic board 'X)(compute-heuristic board 'O))) ;; this will calculate two values in two brackets of formula
	
(defun compute-heuristic (board option)  
	;; Make list of winning positions
	(setf l1 (append(list(first board))(list(second board))(list(third board))))    ;; 0 1 2
	(setf l2 (append(list(fourth board))(list(fifth board))(list(sixth board))))	;; 3 4 5 
	(setf l3 (append(list(seventh board))(list(eighth board))(list(ninth board))))	;; 6 7 8 
	(setf l4 (append(list(first board))(list(fourth board))(list(seventh board))))	;; 0 3 6
	(setf l5 (append(list(second board))(list(fifth board))(list(eighth board))))	;; 1 4 7
	(setf l6 (append(list(third board))(list(sixth board))(list(ninth board))))		;; 2 5 8
	(setf l7 (append(list(first board))(list(fifth board))(list(ninth board))))		;; 0 4 8
	(setf l8 (append(list(third board))(list(fifth board))(list(seventh board))))	;; 2 4 6
	(setf lst (append (list l1)(list l2)(list l3)(list l4)(list l5)(list l6)(list l7)(list l8))) ;; append all lists
	(setf c3 (my-count3 option lst))	;; count number of winning positions where option (X or O) present 3 times
	(setf c2 (my-count2 option lst))	;; count number of winning positions where option (X or O) present 2 times
	(setf c1 (my-count1 option lst))	;; count number of winning positions where option (X or O) present 1 time
	(+(+(* c3 100)(-(* c2 10)(* c3 10))(- c1 (+ c3 c2)))))	;; Note we have to subtract 10*c3 as it will be calculated again, same with c2 and c3 in last case 

(defun my-count3 (a L)			;; this will count if X or O is present at all spots of winning positions
 (cond
   ((null L) 0)
	((equal (my-count a (car L)) 3)(+ 1 (my-count3 a (cdr L))))
	 (t (my-count3 a (cdr L)))))
	 
(defun my-count2 (a L)			;; this will count if X or O is present at 2 spots of winning positions
 (cond
   ((null L) 0)
	((equal (my-count a (car L)) 2)(+ 1 (my-count3 a (cdr L))))
	 (t (my-count3 a (cdr L)))))

(defun my-count1 (a L)			;; this will count if X or O is present at 1 spot of winning positions
 (cond
   ((null L) 0)
	((equal (my-count a (car L)) 1)(+ 1 (my-count3 a (cdr L))))
	 (t (my-count3 a (cdr L)))))
	
	
(defun my-count (a L)			;; this will count number of times a is present in list L
  (cond
   ((null L) 0)
   ((equal a (car L)) (+ 1 (my-count a (cdr L))))
    (t (my-count a (cdr L)))))
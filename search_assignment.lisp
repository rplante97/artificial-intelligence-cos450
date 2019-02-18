;;;Game world setup:
;;;Walls will be denoted with ascii X
;;;Non-walls will be denoted by spaces " "
;;;Agent is denoted by ascii A while goal is denoted by ascii G
;;;Example output of a 5x5 game board is as follows:
;;;-----------------------------------------------------
;;; X X X X X X X
;;; X G         X
;;; X           X
;;; X X         X
;;; X X       A X
;;; X   X       X
;;; X X X X X X X

;;;Globals
(defvar board) ;Global for now

;;;Print 2D array function, some of this is taken from stack overflow
;;;REFACTOR
(defun show-board (board)
  (format t "~%")
  (loop for i below (- (first (array-dimensions board)) 2) do
    (format t "~7T~D~0,3@T"  i))
    (format t "~%")
  (loop for i below (first (array-dimensions board)) do
    (if (and (> i 0) (< i (- (first (array-dimensions board)) 1)))
      (format t "~D~3T" (- i 1)) ;if true
      (format t "~3T")) ;else
    (loop for j below  (second (array-dimensions board)) do
      (let ((cell (aref board i j)))
        (format t "~[   ~; X ~; G ~; A ~]" cell)))
    (format t "~%")))

;;;World generation function:
;;;init_world(size, num_obstacles, goal_coordinate, agent_coordinate)
;;;arg size: integer, makes size X size board
;;;arg goal_coordinate, agent_coordinate: list, valid coordinate pair i.e (2 4)
;;;sets initial locations for goal(G) and agent(A)
;;;arg obstacles: integer, amount of randomly placed obstacles
;;;(optional) arg named_board: string, populates obstacles with hardcoded values
;;;(optional) arg trail: int, if true leaves a trail of agents movement
(defun init_world (size num_obstacles goal_coordinate agent_coordinate)
  ;Initialize board
  (setf board (make-array (list (+ size 2) (+ size 2))
                          :initial-element 0))
  (dotimes (i (+ size 2))
           (setf (aref board 0 i) 1)
           (setf (aref board (+ size 1) i) 1)
           (setf (aref board i 0) 1)
           (setf (aref board i (+ size 1)) 1))
  ;Add in obstacles
  (setf )
  (dotimes (i num_obstacles)
          (setf (aref board (+ (random size) 1) (+ (random size) 1)) 1))
  ;Add in goal and initial agent position
  (setf (aref board (+ (first goal_coordinate) 1) (+ (first (rest goal_coordinate)) 1) ) 2) ;Goal
  (setf (aref board(+ (first agent_coordinate) 1) (+ (first (rest agent_coordinate)) 1) ) 3) ;Agent
  ;Draw the board :)
  (show-board board))


;;;TODO
;;;agent
;;;agent programming

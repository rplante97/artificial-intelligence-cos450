;;;show_board is in here for now, will be moved to simulator laterrrrr
;--------------------------------------------------------------------
;;;Print 2D array function, some of this is taken from stack overflow
;;;REFACTOR
(defun show_board (board)
  (format t "~%")
  (loop for i below (- (first (array-dimensions board)) 2) do
    (format t "~7T~D~0,3@T"  i))
  (format t "~%")
  (loop for i below (first (array-dimensions board)) do
    (if (and (> i 0) (< i (- (first (array-dimensions board)) 1)))
      (format t "~D~3T" (- i 1)) ;if true
      (format t "~3T")) ;else
    (loop for j below  (second (array-dimensions board)) do
      (let ((cell (aref board j i)))
        (format t "~[   ~; X ~; G ~; A ~]" cell)))
    (format t "~%"))
  (format t "~4TAgent Heading: N S E W ~%"))
    ;(format t "~V:@<Agent Heading: -> ~%~>" 40) ;Dynamic centering later
;--------------------------------------------------------------------------
;;;;;About our world:
;;;;;A 0 denotes an open space (format displays it as such)
;;;;;A 1 denotes a wall/obstacle (format dispays it as X)
;;;;;A 2 denotes an agent (format dispays it as A)
;;;;;A 3 denotes the goal (format dispays it as G)
;;;World class
(defclass world ()
  ((size
    :initarg :size
    :initform (error "Must supply a world size.")
    :reader get_size)
   (num_obstacles
    :initarg :num_obstacles
    :initform 0)
   (goal_coordinate
    :initarg :goal_coordinate
    :initform '(0 0)
    :reader get_goal_coordinates) ;call to random eventually
   (agent_bearing
    :initarg :agent_bearing
    :initform '(1 1 N)
    :accessor agent_bearing_acc)
   (board
    :accessor get_board)))


;Initializes a board for the world, populates agent and goal
(defmethod initialize-instance :after ((world world) &key)
  (let ((size (slot-value world 'size))
        (num_obstacles (slot-value world 'num_obstacles))
        (goal_coordinate (slot-value world 'goal_coordinate))
        (agent_bearing (slot-value world 'agent_bearing))
        (board 0))
    ;Initialize board
    (setf board (make-array (list (+ size 2) (+ size 2)) :initial-element 0))
    (dotimes (i (+ size 2))
             (setf (aref board 0 i) 1)
             (setf (aref board (+ size 1) i) 1)
             (setf (aref board i 0) 1)
             (setf (aref board i (+ size 1)) 1))
    ;Add in obstacles
    (dotimes (i num_obstacles)
             (setf (aref board (+ (random size) 1) (+ (random size) 1)) 1))
    ;Add in goal and initial agent position
    (setf (aref board (+ (nth 0 goal_coordinate) 1) (+ (nth 1 goal_coordinate) 1)) 2) ;Goal
    (setf (aref board (+ (nth 0 agent_bearing) 1) (+ (nth 1 agent_bearing) 1) ) 3) ;Agent
    (setf (slot-value world 'board) board)
    (show_board board)))



;;(defmethod bump ((self world))
  ;;())































;Bottom comment so my neck stops hurting ;)

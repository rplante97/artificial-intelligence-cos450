;;;About our world:
;;;A 0 denotes an open space (format displays it as such)
;;;A 1 denotes a wall/obstacle (format dispays it as X)
;;;A 2 denotes an agent (format dispays it as A)
;;;A 3 denotes the goal (format dispays it as G)

;;;World class
;;;Stores world size, number of obstacles, the goal and agent coordinates, the agent bearing
;;;and a map of the entire world
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
;This is called as part of the world object initialization
(defmethod initialize-instance :after ((world world) &key)
  (let ((size (slot-value world 'size)) ;Need world object init values to generate board
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
    (show_board board (last agent_bearing)))) ;Print out the game board

;Updates the world object with its new values
(defun update_world (&optional move_type move_heading)
  (sleep 1) ;Sleep so game doesnt spam
  (let ((x (nth 0 (agent_bearing_acc world_map)))
        (y (nth 1 (agent_bearing_acc world_map)))
        (heading (nth 2 (agent_bearing_acc world_map))))
    (cond
      ((equal move_type 'turn) ;If we are turning notify world of new agent heading
       (setf (agent_bearing_acc world_map) (list x y move_heading)))
      ((equal move_type 'move) ;If we are moving get new bearings and update world_map
       (setf (agent_bearing_acc world_map) (get_new_bearings x y move_heading heading))))
   (print (agent_bearing_acc world_map))
   (update_board x y (agent_bearing_acc world_map)))) ;Update the board with the new values

;Edits the board and saves it to the world object
(defun update_board (old_x old_y new_bearing)
  (let ((new_board (get_board world_map)))
    ;Overwrite the old agent position
    (setf (aref new_board (+ old_x 1) (+ old_y 1) ) 0)
    ;Populate the new agent position
    (setf (aref new_board (+ (nth 0 new_bearing) 1) (+ (nth 1 new_bearing) 1) ) 3)
    ;Update the world object with the new board
    (setf (get_board world_map) new_board)
    ;Draw the board
    (show_board new_board (last new_bearing))))

;Uses format (which is pretty awesome) to print out the game board
;Obstacles/Walls are X's, the goal is a G, and the agent is an A
;This function prints the board, axis labels and directional heading.
(defun show_board (board heading)
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
  (format t "~4TAgent Heading: ~a~%" heading))

    ;(format t "~V:@<Agent Heading: -> ~%~>" 40) ;Dynamic centering later

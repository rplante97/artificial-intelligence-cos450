;simulator
;;;Some code restructuring needs to be done for this
;;;simulator should query world for the board and agents location
;;;then it should return the apppriate sense precepts to the agent
;;;it should then receive the agents intended action and simulate the result
;;;(aka update world, update precepts, etc.)
(defclass simulator ()
   ((world_name
     :initform (defvar world_map)
     :reader world_name)
    (agent_name
     :initform (defvar agent1)
     :reader agent_name)
    (agent_progam_name
     :initform (defvar agent_prog)
     :reader agent_program_name)
    (same_bump_count
     :initform 0
     :accessor same_bump_count_acc)
    (last_bump_type
     :initform 0
     :accessor last_bump_type)))

;Dynamic defvar stuff later
;(defmethod initialize-instance :after ((simulator simulator) &key)
;  (defvar `(make-symbol (slot-value simulator 'world_name))))

;;;Starts simulation, calls to agent agent/methods so they start moving etc
(defmethod start_simulation (simulator)
  (print "Simulation starting!")
  (move agent_prog))

;;;Checks move to see if it is allowed
;;;Returns sense values to agent
;;;Updates world map
;;;Move is passed as 2 element list, first element corresponds to the action
;;;(move, turn, nop) the second element corresponds to the direction
(defun simulate_move (move)
  (let ((action (first move)) ;This will be move or turn
        (direction (last move)) ;This wil be forward, backward, left or right
        (board (get_board world_map))
        (heading (last (agent_bearing_acc world_map))) ; N, S , E or W
        (move_heading 'N) ;Init the move heading
        (move_path 0)) ;Init move path
    (setq move_heading (get_move_heading heading direction)) ;Get cardinal direction of move
    (print move_heading)
    (setq move_path (see board move_heading)) ;Determine if move path is blocked by obstacle/wall
    (print move_path)
    (cond
      ((equal action 'move)
       (print "In action move")
       (cond
         ((equal move_path 0) ;Move possible!
          (update_world action move_heading) ;Move agent in world (update world)
          (update_agent)) ;Update agents sensor values
         ((equal move_path 1) ;Move blocked :(
          (print "move blocked")
          (update_world) ;This basically just draws the board again
          (update_agent direction)))) ;Update agent sensor with bump direction
      ((equal action 'turn) ;We can always turn, this serves to update world/front_sensor
       (cond
         ((equal move_path 0)
          (update_world action move_heading) ;update worlds agent heading value
          (update_agent)) ;update front sensor
         ((equal move_path 1)
          (update_world action move_heading)
          (update_agent)))))))

;Obtains the cardinal direction with which our agent will be moving/turning from
;the relative move direction given by the agent_program
(defun get_move_heading (current_heading move_direction)
  (cond ;Is our move/turn in the forward/backward/left/right direction
    ((equal move_direction '(forward))
     (return-from get_move_heading (first current_heading)))
    ((equal move_direction '(backward))
     (cond ;Mapping for backwards
       ((equal current_heading '(N))
        (return-from get_move_heading 'S))
       ((equal current_heading '(S))
        (return-from get_move_heading 'N))
       ((equal current_heading '(W))
        (return-from get_move_heading 'E))
       ((equal current_heading '(E))
        (return-from get_move_heading 'W))))
    ((equal move_direction '(left))
     (cond ;Mapping for left
       ((equal current_heading '(N))
        (return-from get_move_heading 'W))
       ((equal current_heading '(S))
        (return-from get_move_heading 'E))
       ((equal current_heading '(W))
        (return-from get_move_heading 'S))
       ((equal current_heading '(E))
        (return-from get_move_heading 'N))))
    ((equal move_direction '(right))
     (cond ;Mapping for right
       ((equal current_heading '(N))
        (return-from get_move_heading 'E))
       ((equal current_heading '(S))
        (return-from get_move_heading 'W))
       ((equal current_heading '(W))
        (return-from get_move_heading 'N))
       ((equal current_heading '(E))
        (return-from get_move_heading 'S))))))

;Returns the board value directly in front of the agent, based on its heading.
;By manipulting calls to this we can determine where we are moving and what it
;will look like after
(defun see (board &optional direction)
 (let ((x (nth 0 (agent_bearing_acc world_map)))
       (y (nth 1 (agent_bearing_acc world_map)))
       (heading (nth 2 (agent_bearing_acc world_map))))
  (if direction
    (setq heading direction))
  ;;Here we have to remember the game BOARD is a matrix of size+2 X size+2
  ;;and the bearings we receive from the world are 0 indexed at size X size
  ;;this means when looking at the board we need to add 1 to our returned
  ;;bearings for the true agent location. Looking is then acheived by +/-1
  ;;on each coordinate
  (cond ((equal heading 'N) (aref (get_board world_map) (+ x 1) y))
    ((equal heading 'S) (aref board (+ x 1) (+ y 2)))
    ((equal heading 'E) (aref board (+ x 2) (+ y 1)))
    ((equal heading 'W) (aref board x (+ y 1))))))

;Returns the new agent bearings for the simulate move function
(defun get_new_bearings (current_x current_y move_heading curr_heading)
  (cond
    ((equal move_heading 'N)
     (return-from get_new_bearings (list current_x (- current_y 1) curr_heading)))
    ((equal move_heading 'S)
     (return-from get_new_bearings (list current_x (+ current_y 1) curr_heading)))
    ((equal move_heading 'W)
     (return-from get_new_bearings (list (- current_x 1) current_y curr_heading)))
    ((equal move_heading 'E)
     (return-from get_new_bearings (list (+ current_x 1) current_y curr_heading)))))

;Logs the result of a game
;(defun log_game (board)
;  (todo))

;Called on bump. Counts consecutive number of bumps on the same sensor.
(defun bump_counter (bump)
  (let ((count (same_bump_count_acc simulation))
        (bump_type (last_bump_type simulation)))
    (cond
      ((> count 0)
       (if (equal bump bump_type)
         (incf count)
         (setq count 0)))
      ((equal count 0)
       (incf count)
       (setq bump_type bump)
       (print count)
       (print bump_type)))
    (print "got here")
    (setf (same_bump_count_acc simulation) count)
    (setf (last_bump_type simulation) bump_type)
    (print (same_bump_count_acc simulation))))




;Terminates a game that would otherwise not exit
(defun terminate_game ()
   (print "terminating!")
   (signal 'quit nil))

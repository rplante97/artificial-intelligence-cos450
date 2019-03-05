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
     :reader agent_program_name)))

;Dynamic defvar stuff later
;(defmethod initialize-instance :after ((simulator simulator) &key)
;  (defvar `(make-symbol (slot-value simulator 'world_name))))

;;;Starts simulation, calls to agent agent/methods so they start moving etc
(defmethod start_simulation (simulator)
  (print "Simulation starting!")
  (reflex_agent agent_prog))



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
    (setq move_path (see board move_heading)) ;Determine if move path is blocked by obstacle/wall
    (cond
      ((equal action 'move)
       (cond
         ((equal move_path 0) ;Move possible!
          (update_world action move_heading)) ;Move agent in world (update world), update agent afterwards
         ((equal move_path 1) ;Move blocked :(
          (print "not implemented")))) ;update world for draw function (no change), update agent afterwards
      ((equal action 'turn) ;We can always turn, this serves to update world/front_sensor
       (cond
         ((equal move_path 0)
          (print "not implemented")) ;update world with new heading, update agent front_sensor
         ((equal move_path 1)
          (print "not implemented"))))))) ;update world with new heading, update agent front_sensor


      ;;;Map direction we are moving/turning to a cardinal direction

              ;Should refactor this
;       (let ((tmp (agent_bearing_acc world_map)))
;         (setq tmp (list (first tmp) (nth 1 tmp) move_heading))
;         (print tmp)
;         (setf (agent_bearing_acc world_map) tmp)
;         (update_agent)
;         (update_board (first tmp) (nth 1 tmp))))))
;  move(reflex_agent))
;(cond
;  ((equal (see board move_heading) 0) ;The proposed move is allowed
;   (print "Made it here")
;   (update_world move_heading)
;   (update_agent))
;  ((equal (see board move_heading) 1) ;The proposed move is not allowed
;   (update_agent direction)))




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


(defun update_agent (&optional bump)
  (print "start update agent")
  (print bump)
  (setf (agent_front_sensor_acc agent1) (see (get_board world_map)))
  (if bump
    (cond
      ((equal bump '(backward)) (setf (agent_rear_bump_acc agent1) 1))
      ((equal bump '(forward)) (setf (agent_front_bump_acc agent1) 1))
      ((equal bump '(left)) (setf (agent_left_bump_acc agent1) 1))
      ((equal bump '(right)) (setf (agent_right_bump_acc agent1) 1)))
    (reset_bump_sensors)))



(defun update_world (move_type move_heading)
  (let ((x (nth 0 (agent_bearing_acc world_map)))
        (y (nth 1 (agent_bearing_acc world_map)))
        (heading (nth 2 (agent_bearing_acc world_map))))
    (cond
      ((equal move_type 'turn) ;If we are turning notify world of new agent heading
       (setf (agent_bearing_acc world_map) (list x y move_heading)))
      ((equal move_type 'move) ;If we are moving get new bearings and update world_map
       (print "Test")
       (setf (agent_bearing_acc world_map) (get_new_bearings x y move_heading heading))))
   (print (agent_bearing_acc world_map))
   (update_board x y (agent_bearing_acc world_map)))) ;Update the board with the new values


    ;(cond ((equal move_heading 'N)
    ;       ;(setq new_vals (list (+ x 1) y 'N))
    ;       (setf (agent_bearing_acc world_map) (list x (- y 1) heading)))
    ;      ((equal move_heading 'S)
    ;       (setf (agent_bearing_acc world_map) (list x (+ y 1) heading)))
    ;      ((equal move_heading 'E)
    ;       (setf (agent_bearing_acc world_map) (list (+ x 1) y heading)))
    ;      ((equal move_heading 'W)
    ;       (setf (agent_bearing_acc world_map) (list (- x 1) y heading))))
    ;(update_board x y)))

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



























;;resrt

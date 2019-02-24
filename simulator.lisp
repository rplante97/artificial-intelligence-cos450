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
  (move agent_prog))



;;;Checks move to see if it is allowed
;;;Returns sense values to agent
;;;Updates world map
;;;Move is passed as 2 element list, first element corresponds to the action
;;;(move, turn, nop) the second element corresponds to the direction
(defun simulate_move (move)
  (print "Hello")
  (let ((action (first move))
        (direction (last move))
        (board (get_board world_map))
        (heading (last (agent_bearing_acc world_map)))
        (move_heading 'N))
    (print heading)
    ;;;Map direction we are moving/turning to a cardinal direction
    (cond ((or (equal action 'move) (equal action 'turn))
           (cond
             ((equal direction 'forward)
              (print "Do nothing")) ;no heading modifer needed for see
             ((equal direction 'backward) ;Moving backwards inverts our see heading
              (cond
                ((equal heading 'N) (setq move_heading 'S))
                ((equal heading 'S) (setq move_heading 'N))
                ((equal heading 'W) (setq move_heading 'E))
                ((equal heading 'E) (setq move_heading 'W))))
             ((equal direction 'left) ;Moving left heading mappings
              (cond
                ((equal heading 'N) (setq move_heading 'W))
                ((equal heading 'S) (setq move_heading 'E))
                ((equal heading 'W) (setq move_heading 'S))
                ((equal heading 'E) (setq move_heading 'N))))
             ((equal direction 'right)
              (cond
                ((equal heading 'N) (setq move_heading 'E))
                ((equal heading 'S) (setq move_heading 'W))
                ((equal heading 'W) (setq move_heading 'N))
                ((equal heading 'E) (setq move_heading 'S)))))
           ;Look at the board at determined heading
           ;(print (see board move_heading))
           (print "Hello?"))
     ((equal action 'nop) (print "Do nothing")))))

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

























;;resrt

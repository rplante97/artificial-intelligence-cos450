(defclass agent_program ()
  ())

(defclass reflex_agent (agent_program)
  ())

;Agent program can decide to move in any direction or turn 90 degrees
(defmethod move ((agent_program reflex_agent))
  (let ((front_sensor (agent_front_sensor_acc agent1))
        (front_bump (agent_front_bump_acc agent1))
        (left_bump (agent_left_bump_acc agent1))
        (right_bump (agent_right_bump_acc agent1))
        (rear_bump (agent_rear_bump_acc agent1)))
    ;Move logic
    (print "Agent program return")
    (simulate_move '(turn left))))



 ;Try to just move forward for now


;;;Reflex Agent: It's goal is to find (and stay) in the corner of the map
;;;This can be accomplished by moving randomly until front sensor is blocked
;;;once front sensor is blocked it attempts to move left/right until bump
;;;Sensor is triggered, once bump sensor is triggered it stops.

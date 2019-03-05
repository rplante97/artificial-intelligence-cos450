;Agent program class this really just serves to annoy me because I didn't set the classes up
;correctly
(defclass agent_program ()
  ())

;Send move to simulator
(defun move (move)
  (simulate_move move))

;Reflex agent. Repeatedly move forward, if blocked move left
;Should zig zag to a corner provided no false corners exist
(defmethod reflex_agent (agent_program)
  (let ((front_sensor (agent_front_sensor_acc agent1))
        (front_bump (agent_front_bump_acc agent1))
        (left_bump (agent_left_bump_acc agent1))
        (right_bump (agent_right_bump_acc agent1))
        (rear_bump (agent_rear_bump_acc agent1)))
    (if (= front_sensor 0)
     (move '(move forward))
     (move '(move left)))))

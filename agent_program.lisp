;Agent program class this really just serves to annoy me because I didn't set the classes up
;correctly
(defclass reflex_agent ()
  ())

;Reflex agent. Repeatedly move forward, if blocked move left
;Should zig zag to a corner provided no false corners exist
(defmethod move (reflex_agent)
  (if (> (same_bump_count_acc simulation) 3)
    (terminate_game)
    (print "not stalled"))
  (let ((front_sensor (agent_front_sensor_acc agent1))
        (front_bump (agent_front_bump_acc agent1))
        (left_bump (agent_left_bump_acc agent1))
        (right_bump (agent_right_bump_acc agent1))
        (rear_bump (agent_rear_bump_acc agent1)))
    (if (= front_sensor 0)
     (simulate_move '(move forward))
     (simulate_move '(move left)))))

(defclass model_agent ()
  ())

;(defmethod move (model_agent))

(defclass hill_climbing_agent()
  ())

(defclass search_bfs_agent()
  ())

(defclass search_aStar_agent()
  ())

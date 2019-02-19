;;;;;Not much goes on here! Provides implementation of the agent class
;;;;;Essentially just some bump detectors and a forward sensor. The intersting
;;;;;stuff happens in the various agent_program files!
(defvar agent1)
;Agent class with sensor + bump attributes
(defclass agent ()
 ((front_sensor
   :initform (see world_map)
   :accessor agent_front_sensor_acc)
;;Impossible to bump something on init (as we have never moved)
  (front_bump
   :initform 0
   :accessor agent_front_bump_acc)
  (rear_bump
   :initform 0
   :accessor agent_rear_bump_acc)
  (left_bump
   :initform 0
   :accessor agent_left_bump_acc)
  (right_bump
   :initform 0
   :accessor agent_right_bump_acc)))

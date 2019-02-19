;;;;;Not much goes on here! Provides implementation of the agent class
;;;;;Essentially just some bump detectors and a forward sensor. The intersting
;;;;;stuff happens in the various agent_program files!


;Globals
(defvar agent1) ;An agent

;Agent class with sensor + bump attributes
(defclass agent ()
 ((front_sensor
   :initform (see world_map)
   :accessor agent_front_sensor_acc)))

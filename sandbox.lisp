;;;;;Sandbox for testing/properly executing project code. Saves us from having
;;;;;to manually reinit all our objects when the repl crashes

(defun run ()
  (load "world.lisp")
  (load "agent.lisp")
  (setq world_map (make-instance 'world :size 7 :agent_bearing '(1 5 W) :goal_coordinate '(3 3)))
  (setq agent1 (make-instance 'agent))
  (describe agent1))

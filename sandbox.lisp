;;;;;Sandbox for testing/properly executing project code. Saves us from having
;;;;;to manually reinit all our objects when the repl crashes


(defun run ()
  ;General setup
  (defparameter simulation 0)
  (load "simulator.lisp")
  (load "world.lisp")
  (load "agent.lisp")
  (load "agent_program.lisp")
  (setq simulation (make-instance 'simulator))

  ;;;Generates map, agent, goal, and obstacles
  (setq world_map (make-instance 'world :size 25 :num_obstacles 0 :agent_bearing '(3 1 S) :goal_coordinate '(6 0)))

  ;;;Initalizes an agent and agent program
  (setq agent1 (make-instance 'agent))
  (setq agent_prog (make-instance 'reflex_agent))

  ;;;At this point, all the initialization we need to do is done. Start the sim
  (start_simulation simulation)

  ;(describe world_map)
  (print "Done"))

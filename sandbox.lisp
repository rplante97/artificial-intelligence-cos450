;;;;;Sandbox for testing/properly executing project code. Saves us from having
;;;;;to manually reinit all our objects when the repl crashes


(defun run ()
  ;General setup
  (defparameter simulation 0)
  (load "simulator.lisp")
  (setq simulation (make-instance 'simulator))
  (load "world.lisp")
  (load "agent.lisp")
  (load "agent_program.lisp")

  ;;;Generates map, agent, goal, and obstacles
  (setq world_map (make-instance 'world :size 7 :agent_bearing '(6 3 S) :goal_coordinate '(3 3)))

  ;;;Initalizes an agent and agent program
  (setq agent1 (make-instance 'agent))
  (setq agent_prog (make-instance 'agent_program))

  ;;;At this point, all the initialization we need to do is done. Start the sim
  ;(start_simulation simulation)
  (reflex_agent agent_prog)

  ;(describe world_map)
  ;(describe agent1)
  (print "Done"))

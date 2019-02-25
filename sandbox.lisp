;;;;;Sandbox for testing/properly executing project code. Saves us from having
;;;;;to manually reinit all our objects when the repl crashes


(defun run ()
  ;General setup
  (defparameter simulation 0)
  (load "simulator.lisp")
  (setq simulation (make-instance 'simulator))
  (print "Simulator started")
  (load "world.lisp")
  (load "agent.lisp")
  (load "agent_program.lisp")

  ;;;Generates map, agent, goal, and obstacles
  (setq world_map (make-instance 'world :size 7 :agent_bearing '(0 6 N) :goal_coordinate '(3 3)))

  ;;;Initalizes an agent and agent program
  (setq agent1 (make-instance 'agent))
  (setq agent_prog (make-instance 'reflex_agent))

  ;;;At this point, all the initialization we need to do is done. Start the sim
  (start_simulation simulation)
  ;(update_agent)

  (describe world_map)
  (describe agent1)
  (print "Done"))

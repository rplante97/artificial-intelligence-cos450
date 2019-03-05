***This file contains general notes and overall code structure. Basically just helps me get
reacquainted with the code after not looking at it for few days (or hours lets be real here)***

Files:
sandbox.lisp
[Test platform, contains function that does all necessary initializations
allows new code to easily be run and debugged]

simulator.lisp
[The overarching environment within which the agent and world interact. Simulator class sets up
and provides basic information and structure. Contains functions to simulate a move, update the world
and update the agent. Also contains many helper functions.]

world.lisp
[Contains world class + auto initialization of the game world. Also contains the function to draw
the board and a method to update the world]

agent.lisp
[Contains the basic agent class with its 5 sensor. Also contains a method to reset the bumps sensors.]

agent_program.lisp
[Contains the 5 different agent programs and the method move to move preform the move]


Basic program flow:
[Initialization]
instance of simulator created - initializes global variables (ran out of time to fix these),
common vals, etc.
instance of world map is created - this generates a map and a sprite for the agent, goal and obstacles
instance of agent created - this just initializes an object of the agent class
instance of agent program created - initializes what program the agents decisions will be made from

At this point everything is all ready to go and all intelligent objects have the information they need
to make their first move.

[Start]
start_simulation is called - This literally just calls the agent_programs method function so it makes its
can compute its first move

call to move - agent_program analyzes the sensor data and attempts to make a move accordingly by
calling simulate_move

call to simulate_move - Lots goes on here. The simulator takes the agents move and determines whether
or not it is possible. If possible the agent is moved, new sensor values are calculated and set and
the world is updated. If move is not possible the agents sensor values are calculated and set.

call to update_world/update_agent: simulate move calls both update_world and update_agent each time.

Update world --> update_board --> show_board | This chain of calls updates and redraws the game board

Update_agent --> agent_program method --> move | This chain updates the agent sensor values which in
then calls the relevant agent program method to create a new move from the new data. This agent program
method then calls move with its move. At this point we loop on until exit.
































***This footer lets me scroll as I please***

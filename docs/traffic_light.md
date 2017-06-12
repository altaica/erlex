## Traffic Light Controller

Copyright 2017 Phil Dempster.

This code controls a fixed traffic light sequence in pure Erlang. It demonstrates the use of pattern matching to elegantly encapsulate the state transitions.

Some questions to consider:

* How might one modify this to be a dynamic system? What additional state is required?
* How about adding a filter arrow? A pedestrian signal (Pelican crossing)?
* Consider multiple linked traffic lights at a junction. How do they operate together?
* What happens if there is a failure?

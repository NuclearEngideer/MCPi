# MCPi
Fortran Monte Carlo to calculate pi

Does what it says, probably pretty poorly.
Handles `SIGINT` gracefully, but required global variables, sorry.

Compile with gfortran: `gfortran -o mcpi global.f90 MCPi.f90`

Enter an integer for number of trials when prompted. Will break if you enter a float - maybe I'll fix that later (lol).

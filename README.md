# MCPi
Fortran Monte Carlo to calculate pi via rejection sampling using OMP threading.

This parallel version does not currently report the variance/relative error. 

Compile with gfortran: `gfortran -o mcpi global.f90 MCPi.f90 -fopenmp`

Enter an integer for number of trials when prompted. Will break if you enter a float - maybe I'll fix that later (lol).

Example output:
> Calculating Pi with            1000000000  random X,Y pairs...
> 
> With           10 threads.
> 
> estimate for pi   3.14162874    
> 
> calculation took   32.8685074     seconds

Some plots of convergence to the value of Pi and the parallel efficiency follow:

![Estimate of Pi Vs Number of Random Points](https://user-images.githubusercontent.com/56891155/139469603-c23221e2-2b60-4e61-84e8-69db9d1ebafe.png)
![Speedup Vs Number of Threads](https://user-images.githubusercontent.com/56891155/139469625-19f30f11-8325-4762-a149-66f2a995fe08.png)
![Parallel Loop Time Vs Number of Threads](https://user-images.githubusercontent.com/56891155/139469639-6a366d94-d969-441a-9904-1ced445b3299.png)

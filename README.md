# GP
Genetic Programming with a twist: The code generator tries to mutate itself. Haskell-based

Notes:
The source file "GRPSeedhr.hs" is a properly formatted version of GRPSeed.hs - it is there to read and edit the code, not for reflective processing.
Also, due to the way refill works, having a small pool might lead to a possibly undesirable feedback effect, where a genome is handed it's own code for replication.

Example usage: Compile the executable GP, copy into src folder;
./GP --init
./GP --testrun
./GP --start m n test-name --iterations p -- m is the number of new individuals per iteration, n is the size of the population after reduction and p is the number of iterations to perform.

# RubikNxNxNSolver
A general solver for NxNxN Rubik's Cubes

Update: I stopped the project since I realized that some specialized solver for 4x4x4 and 5x5x5 make more sense. So solving of all centers of the NxNxN is what the current code can do but solving of the remaining edges and corners will not be added in the future.

This is work in progress using the free Lazarus IDE  https://www.lazarus-ide.org/ for a program to solve a NxNxN Rubik's Cube. The program yet only is in an early development state and is able to solve the centers now.

For a random 101x101x101 test cube I got the following results:

Phase 1: all U and D centers to the U **or** D face: 18388 moves.

Phase 2: all F and B centers to the F **or** B face: 16636 moves.
R and L centers are automatically in R or L face then.

Phase 3: R centers to R face, L centers to L face, F centers to F face, B centers to B face: 20432 moves.

Phase 4: U centers to U face, D centers to D face: 14298 moves.

**TOTAL: 69754 moves** to fix the 58800 centers.
RAM usage is about 14 GB.

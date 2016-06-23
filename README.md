# SCFDMA
Assignment on an algorithm for allocating 4G channels to users (Uppsala University).

The assignment is on an article *L. Lei, K. C. Ho, S.  Sun, and D. Yuan. A
unified graph labeling algorithm for consecutive-block channel allocation in SC-
FDMA.*

The assignment implied an implementation of the algorithm, and a study of the
algorithm in terms of complexity and correctness. Variations of this algorithm
are also implemented.

# STRUCTURE
    * app: contains Main.hs
    * src: contains library modules
    * test: contains the test suite
    * report: contains the textual report (in TeX)
    * configurations: contains example configuration files (in JSON). 

# BUILD
    `stack build` builds the project.
    `stack build --executable-profiling --library-profiling` builds the project with profiling activated.
    `stack exec -- SCFDMA-exe ./configurations/configuration1.json` runs the program with a configuration file.
    `stack exec -- SCFDMA-exe +RTS -p -RTS ./configurations/configuration1.json` runs the program with a configuration file, and profiling activated.



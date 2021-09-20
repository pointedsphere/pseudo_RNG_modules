# pseudo_RNG_modules
Pseudo random number generator modules in fortran, with option to compile as python modules and randomness tests in python

## Note on Fortran modules
In order to utilise the Fortran modules one can copy the chosen module for the RNG to any project, as lond as the modules:
   - types : Double and single precision real types.
   - util  : Utility scripts used in all RNG modules.
Note, pointers could have been utilised, however they were omitted here for portability of RNG modules.
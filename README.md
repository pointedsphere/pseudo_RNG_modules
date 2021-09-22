# pseudo_RNG_modules

Pseudo random number generator modules in fortran, with option to compile as python modules and some basic randomness tests in python.

## Simple Python Random Testing Script

Note once compiled with the makefile, test_rand.py runs a few tests. It bins 100000 random values in [0,1) generated with each method into 100 bins and plots the results on the top.

Then on the bottom (left) it calcualtes the mean (which should be 0.5), subtracts 0.5 and plots the magnitude of the result. The script also estimates pi using the random numbers and plots the magnitude of the difference between the extimated and actual value of pi in the bottom middle. Bottom right is the average calcualtion time for a single random number.

## Architecture of all `rand` modules

All RNG modules contain 6 subroutines, which can be called in Python as functions. All, except for the `set_seed` routine work in the same way, with the same inputs/outputs. Therefore in python one can call any of the random routines with `MODULE.ROUTINE`, e.g. to set `a` to a random number in [0,1) using the Blum Blum Shub method one can run
```
from RandLib import *
a = bbsrand64.rand()
```

#### `set_seed`

Set the seed value or values of the RNG method. The exact method varied for each RNG.

#### `rand`
Calculate random number in [0,1).
###### Inputs
None.
###### Outputs
- `R` :: Random number in [0,1).

#### `rand_arr`
Calculate 1D array of random numbers in [0,1).
###### Inputs
- `N` :: Number of random numbers in [0,1) to return in a 1D array.
###### Outputs
- `Rarr` :: 1D array of length `N` containing random numbers in [0,1).

#### `rand_range`
Calculate a random number in the range [a,b).
###### Inputs
- `a` :: Start of range to calculate random number in.
- `b` :: End of range to calculate random number in.
###### Outputs
- `R` :: Random number in [a,b).

#### `rand_range_arr`
Calculate 1D array of random numbers in [a,b).
###### Inputs
- `N` :: Number of random numbers to calculate.
- `a` :: Start of range to calculate random numbers in.
- `b` :: End of range to calculate random numbers in.
###### Outputs
- `R` :: `N` random numbers in [a,b) in a 1D array of length `N`.

#### `rand_int`
Calculate a random integer in the range [a,b).
###### Inputs
- `a` :: Start of range to calculate random integer in.
- `b` :: End of range to calculate random integer in.
###### Outputs
- `R_out` :: Random integer in [a,b).

#### `rand_int_arr`
Calculate 1D array of random integers in [a,b).
###### Inputs
- `N` :: Number of random integers to calculate.
- `a` :: Start of range to calculate random integers in.
- `b` :: End of range to calculate random integers in.
###### Outputs
- `R_out` :: `N` random integers in [a,b) in a 1D array of length `N`.







## Module `frand`: Fortran Random Number Generator

The Fortran intrinsic `random_number` RNG is included for the sake of comparison with other implemented RNGs.






## Module `lgmrand`: Multiplicative Congruential Algorithm with Lewis, Goodman, and Miller's Parameters

The simple multiple congruential algorithm is implemented where the j+1 random number is found from the jth random number with [^1]
```
R_{j+1} = a * R_{j}   (mod m)
```
where the parameters given by Lewis, Goodman, and Miller are [^1]
```
a = 7^5 = 16807
m = 2^31 - 1 = 2147483647
```
As we want to keep all our integers 32 bit (if possible) we utilise Schrage’s algorithm [^2] in `lgmrand` to allow for the calculation without overflow of the 32 bit integers.





## Module `bbsrand`: Blum Blum Shub Algorithm (64 bit)

Use the Blum Blum Shub algorithm [^3] where we calculate the j+1 random value R from the jth with
```
R_{j+1} = R_{j}^2  (mod m)
```
where `m=pq` and `p`,`q` are large primes. Furthermore, we require `GCD( (p-3)/2 . (Q-3)/2 )` be sufficiently small and the seed `R_{0}` be a coprime of m.

64 bit integers have been used for the Blum Blum Shub algorithm [^3], this is as an implementation of Schrage’s algorithm would require at least one 64 bit integer [^2]. This can be seen as for Schrage’s algorithm where we consider [^1]
```
R_{j+1} = a * R_{j} (mod m)
```
then for Blum Blum Shub we must calculate m/a for each j (as a=R_{j}), which requires one 64 bit integer. As this already effects portability, for this test module 64 bit integers were left enabled (or at least emulated) by `selected_int_kind=18`.


## Notes

### Note on Compiling with Makefile

Compiling with the makefile will compile RandLib.f90 into a module that can be imported directly into python. This is done for ease of testing and graphing etc. However, for a final implementation one would be advised to copy the RNG of choice into the Fortran program or library.

### Note on Fortran modules

In order to utilise the Fortran modules one can copy the chosen module for the RNG to any project, as long as the following modules are accessible by the RNG module.
   - `types` : Double and single precision real types.
   - `util`  : Utility scripts used in all RNG modules.
Note, pointers could have been utilised, however they were omitted here for portability of RNG modules so some functions are repeated across modules.

## References

[^1]:``Numerical Recipes in Fortran 77, The Art of Scientific Computing, Vol 1'', Press W.H. and Teukolsky S.A. and Vetterling W.T. and  Flannery B.P., 2nd ed, Cambridge University Press.

[^2]:L. Schrage, ACM Trans. Math. Soft., 5, p132-138 (1979).

[^3]:Blum L, Blum M, Shub M. A Simple Unpredictable Pseudo-Random Number Generator. SIAM J Comput. 1986;15(2):364–83. 
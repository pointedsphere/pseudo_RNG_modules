import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import figure
import sys
import math
import scipy.stats as stats
import time

from RandLib import *

# Numpy print output all elements of an array, just for testing
np.set_printoptions(threshold=sys.maxsize)



class rand_test:
    
    def __init__(self,N,numbins,rand_fn,rand_arr_fn):
        self.N = N
        self.numbins = numbins
        self.rand = rand_fn
        self.rand_arr = rand_arr_fn

        # We need an array of the random numbers always, also time this
        tic = time.perf_counter()
        self.R = self.rand_arr(N)
        toc = time.perf_counter()

        # Calc the average time per random number generation
        self.ave_time = (toc-tic)/self.N

        # Analysis of the random number array
        self.mean      = self.rand_mean()
        self.mean_diff = abs(self.mean-0.5)
        self.chi_sq    = self.chi_squared()
        self.chi_sq_p  = self.chi_sq[1]
        self.pi_diff   = abs( self.pi_est() - np.pi )
        
    def plot_hist(self):
        # Plot a histogram of the current random number gen method
        # using nbins and N generated random numbers
        plt.hist(self.R, bins=self.numbins, density=True)
        plt.show()

    def rand_mean(self):
        # Find the mean of the random number array
        return np.sum(self.R)/self.N
        
    def chi_squared(self):
        # Calculate the chi squared error

        # Bin the data from the array of random numbers
        binned = np.bincount(np.digitize(self.R, bins=np.linspace(0+(1/self.numbins),1,self.numbins)))
        
        # The expected height of each bin
        e = self.N/self.numbins

        # Return the chi square with the scipy state module function
        return stats.chisquare(binned,f_exp=np.full(shape=self.numbins,fill_value=e))
        
    def pi_est(self):
        # Calculate pi using the random numbers in the self.R array
        c_cnt = 0 # count of number of points in circle
        for i in range(0,math.floor(N/2)):
            # Convert a pair of random numbers to get a cartesian position in [-1,1]
            x = rngutil.rand_range_(self.R[i],-1,1)
            y = rngutil.rand_range_(self.R[math.floor(N/2)+i],-1,1)
            # See if this point lies within the circle
            if math.hypot(x,y)<=1:
                # If it does then count it
                c_cnt += 1
        return (4.0*c_cnt)/math.floor(self.N/2)


    
if __name__ == "__main__":

    N = 10000     # Number of random numbers to generate
    numbins = 100 # Number of bins for graphing/chi-squared testing
    
    # Calcualte the random numbers
    F   = rand_test(N,numbins,frand.rand,frand.rand_arr)
    lgm = rand_test(N,numbins,lgmrand.rand,lgmrand.rand_arr)
    bbs = rand_test(N,numbins,bbsrand64.rand,bbsrand64.rand_arr)

    fig, ax = plt.subplots(2, 3, figsize=(12,9))
    fig.tight_layout(pad=5,h_pad=4,w_pad=4)

    # Histograms
    histax = plt.subplot2grid((2, 3), (0, 0), colspan=3)
    histax.hist(
        [F.R, lgm.R, bbs.R],
        numbins,
        label=['Fortran','LGM','BBS'],
        histtype='step',
        stacked=False,
        fill=False)
    histax.set_xlim(0,1)
    histax.grid(True)
    histax.legend(loc='lower right')
    histax.set_xlabel('Bins')
    histax.set_ylabel('Number of rands in bin')
    
    # Mean barchart
    meanax = plt.subplot2grid((2, 3), (1, 0), colspan=1)
    meanax.bar(
        ['Fortran','LGM','BBS'],
        [F.mean_diff, lgm.mean_diff, bbs.mean_diff],
        )
    meanax.grid(True)
    meanax.set_ylabel('|Mean of Rand Arr - 0.5|')

    # Pi difference barchart
    piax = plt.subplot2grid((2, 3), (1, 1), colspan=1)
    piax.bar(
        ['Fortran','LGM','BBS'],
        [F.pi_diff, lgm.pi_diff, bbs.pi_diff],
        )
    piax.grid(True)
    piax.set_ylabel('|Estimate of pi - pi|')

    # Calculation time barchart
    piax = plt.subplot2grid((2, 3), (1, 2), colspan=1)
    piax.bar(
        ['Fortran','LGM','BBS'],
        [F.ave_time, lgm.ave_time, bbs.ave_time],
        )
    piax.grid(True)
    piax.set_ylabel('Average calcualtion time (s)')


    plt.show()
    






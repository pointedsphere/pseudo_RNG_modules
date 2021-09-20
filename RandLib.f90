module types
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! A simple module just for the single (sp) and double (dp) precision types     !
  ! explicitly defined.                                                          !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  implicit none

  private
  
  integer, parameter, public :: sp = selected_real_kind(6,37)
  integer, parameter, public :: dp = selected_real_kind(15,300)
  
end module types



module util

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! A utility module for miscellanies useful functions etc.                      !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use types

  implicit none

  private

  public :: stop_E
  public :: convrand
  
contains

  subroutine stop_E(err_msg)
    ! Call stop with an error message
    implicit none
    character(len=*) :: err_msg
    print *, ""
    print *, "ERROR: ", err_msg
    print *, ""
    STOP
  end subroutine stop_E

  subroutine convrand(R,a,b,c)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Convert a random number in the range [0,1) to the range [a,b) informally.    !
    ! e.g. if a=-1 and b=1 then R=0 => -1 and R=1 => 1.                            !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! R ::: Real, random number in [0,1), to be converted to the range [a,b).      !
    ! a ::: Real, start of range to convert random number to.                      !
    ! b ::: Real, end of range to convert random number to.                        !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! c ::: Real, random number R converted to the range [a,b).                    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    real(kind=dp), intent(in)  :: R, a, b
    real(kind=dp), intent(out) :: c
    c = ( R * (b-a) ) + a
  end subroutine convrand
  
end module util



!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!



module frand

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! A module containing routines to calculate random numbers using various       !
  ! algorithms.                                                                  !
  ! Written in such a way as can be compiled by f2py3 into a Python module.      !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use types

  implicit none

  private

  public :: rand
  public :: randarr
  
contains
  
  subroutine rand(R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! The intrinsic Fortran random number generator, used to generate a random     !
    ! number R in                                                                  !
    !    0 <= R < 1                                                                !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    real(kind=dp), intent(out) :: R
    call random_number(R)
  end subroutine rand

  subroutine randarr(N,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! The intrinsic Fortran random number generator, used to generate an array of  !
    ! length N, where all elements are numbers R in                                !
    !    0 <= R < 1                                                                !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp), dimension(N), intent(out) :: R
    call random_number(R)
  end subroutine randarr

end module frand



!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!



module lgmrand
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Module to use Lewis, Goodman, and Miller's (lgm) [Ch 7.1, 1] values of       !
  !                                                                              !
  !    a = 7**5 = 16807                                                          !
  !    m = 2**31 - 1 = 2147483647                                                !
  !                                                                              !
  ! for the simple RNGL                                                          !
  !                                                                              !
  !    R_{j+1} = a * R_{j}   (mod m)                                             !
  !                                                                              !
  ! to generate a random float in [0,1).                                         !
  !                                                                              !
  ! CONTAINS                                                                     !
  ! ========                                                                     !
  !                                                                              !
  ! setseed ::: Set the seed of lgm RNG.                                         !
  ! rand    ::: Generate random number in [0,1) using lgm.                       !
  ! randarr ::: Generate 1D array of N random numbers in [0,1) using lgm.        !
  !                                                                              !
  ! REFERENCES                                                                   !
  ! ==========                                                                   !
  ! [1] : ``Numerical Recipes in Fortran 77, The Art of Scientific Computing,    !
  !       Vol 1'', Press W.H. and Teukolsky S.A. and Vetterling W.T. and         !
  !       Flannery B.P., 2nd ed, Cambridge University Press.                     !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use types
  implicit none

  private

  ! The last integer value calculated for lgmrand random number generation,
  ! i.e. the R_{j} value to use for calculation of R_{j+1}
  integer, save :: last=63887

  public :: setseed
  public :: rand
  public :: randarr
  
contains

  subroutine setseed(seed_in)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Set the seed for lgm random number generation,                               !
    ! i.e. set the value R_{j}                                                     !
    !     R_{j+1} = a * R_{j}   (mod m)                                            !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! seed_in ::: integer, must be > 0, the value to set R_{j} to.                 !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use util, only : stop_E
    implicit none

    integer, intent(in) :: seed_in

    if (seed_in>0) then
      last = seed_in
    else
      call stop_E("Seed must be an integer > 0")
    end if
    
  end subroutine setseed
  
  subroutine rand(R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calculate a random value in [0,1) with Lewis, Goodman, and Miller's          !
    ! [Ch 7.1, 1] values of a and m for                                            !
    !                                                                              !
    !  R_{j+1} = a * R_{j}   (mod m)                                               !
    !                                                                              !
    ! However, the multiplication required for this needs 64 bit integers (which   !
    ! is not portable). Therefore we use Schrage’s algorithm [Ch 7.1, 1] to        !
    ! perform the multiplication, i.e.                                             !
    !                                                                              !
    !                | a * ( z mod q ) - r * int( z/q )      if >= 0               !
    ! a*z (mod m) = <                                                              !
    !                | a * ( z mod q ) - r * int( z/q ) + m  otherwise             !
    !                                                                              !
    ! where z is the previously generated random integer, or the seed for the      !
    ! first call. For the Lewis, Goodman, and Miller we let [Ch 7.1, 1]            !
    !                                                                              !
    !    q = 127773                                                                !
    !    r = 2836                                                                  !
    !                                                                              !
    ! and use the values of a and m from earlier.                                  !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R_ ::: real : A pseudo random value in [0,1).                                !
    !                                                                              !
    ! REFERENCES                                                                   !
    ! ==========                                                                   !
    ! [1] : ``Numerical Recipes in Fortran 77, The Art of Scientific Computing,    !
    !       Vol 1'', Press W.H. and Teukolsky S.A. and Vetterling W.T. and         !
    !       Flannery B.P., 2nd ed, Cambridge University Press.                     !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    real(kind=dp),     intent(out) :: R

    integer, parameter :: a = 16807
    integer, parameter :: q = 127773
    integer, parameter :: r_ = 2836
    integer, parameter :: m = 2147483647
    
    ! Use Schrage’s algorithm to perform the multiplication
    ! NOTE: We save the integer value to use as `seed' for next generated random number
    last = ( a * mod(last,q) ) - ( r_ * int(last/q) )
    if (last .lt. 0) last = last + m

    ! Then translate this large integer to a float
    R = real(last,dp)/real(m,dp)
    
  end subroutine rand

  subroutine randarr(N,Rarr)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Use lgm RNG to generate a 1D array of length N of random numbers in [0,1)    !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, size of the returned 1D array of random values in [0,1).      !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! Rarr ::: 1D array of length N containing random numbers in [0,1) calculated  !
    !          with the lgm method.                                                !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    integer,                     intent(in)  :: N
    real(kind=dp), dimension(N), intent(out) :: Rarr

    integer :: i_

    ! Populate the array
    do i_=1,N
      call rand(Rarr(i_))
    end do
    
  end subroutine randarr

end module lgmrand
  


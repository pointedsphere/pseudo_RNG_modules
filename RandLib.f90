module types
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! A simple module just for the single (sp) and double (dp) precision types     !
  ! explicitly defined.                                                          !
  ! Also define some selected integer kinds to avoid having to use kind=4 etc.   !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  implicit none

  private
  
  integer, parameter, public :: sp = selected_real_kind(6,37)
  integer, parameter, public :: dp = selected_real_kind(15,300)
  integer, parameter, public :: k18 = selected_int_kind(18)
  integer, parameter, public :: k20 = selected_int_kind(20)
  integer, parameter, public :: k32 = selected_int_kind(32)
  
end module types



module RNGutil

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! A utility module for miscellaneous useful functions etc.                     !
  !                                                                              !
  ! CONTAINS                                                                     !
  ! ========                                                                     !
  ! - stop_E          ::: Call STOP to exit program, with an error message given !
  !                       as input.                                              !
  ! - rand_range_     ::: Convert a number from [0,1) to [a,b), with a and b     !
  !                       given as inputs.                                       !
  ! - rand_range_arr_ ::: Convert an array of random values in [0,1) to an array !
  !                       containing random elements scaled to be in [a,b).      !
  ! - rand_int_       ::: Convert a random value in [0,1) to a scaled random     !
  !                       integer in [a,b).                                      !
  ! - rand_int_arr_   ::: Convert an array of floats in [0,1) to an array        !
  !                       containing elements scaled to integers in [1,b).       !
  ! - GCD64_          ::: Find the greatest common divisor of two input integers,!
  !                       where all integers are 64 bit.                         !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use types

  implicit none

  private

  public :: stop_E
  public :: rand_range_
  public :: rand_range_arr_
  public :: rand_int_
  public :: rand_int_arr_
  public :: GCD64_
  public :: XOR_char_
  public :: rule90_char_
  public :: rule150_char_
  public :: rule90_
  public :: rule150_
  public :: rule_90_150_
  public :: bitarr2int
  
contains

  subroutine stop_E(err_msg)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Call STOP with an error message.                                             !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    character(len=*) :: err_msg
    print *, ""
    print *, "ERROR: ", err_msg
    print *, ""
    STOP
  end subroutine stop_E

  subroutine rand_range_(R,a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Convert a random number in the range [0,1) to the range [a,b).               !
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
    real(kind=dp), intent(out) :: R_out
    R_out = ( R * (b-a) ) + a
  end subroutine rand_range_

  subroutine rand_range_arr_(R_in,N,a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Convert an array of random numbers in the range [0,1) to the range [a,b).    !
    ! e.g. if a=-1 and b=1 then R=0 => -1 and R=1 => 1.                            !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! R_in ::: Real array, random numbers in [0,1), to be converted to the range   !
    !          [a,b).                                                              !
    ! N    ::: Integer, size of the array R.                                       !
    ! a    ::: Real, start of range to convert random numbers to.                  !
    ! b    ::: Real, end of range to convert random numbers to.                    !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R_out ::: Real array, length N, random numbers R converted to the range [a,b)!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp), dimension(N), intent(in)  :: R_in
    real(kind=dp),               intent(in)  :: a, b
    real(kind=dp), dimension(N), intent(out) :: R_out
    integer                                  :: iR
    do iR = 1,N
      call rand_range_(R_in(iR),a,b,R_out(iR))
    end do
  end subroutine rand_range_arr_
  
  subroutine rand_int_(R_in,a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Convert a random float in [0,1) to a random integer in [a,b).                !
    ! e.g. if a=-1 and b=1 then R=0.0 => -1 and R=0.9999=> 0.                      !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! R_in  ::: Float, a random value in [0,1).                                    !
    ! a     ::: Integer, start of range of integer to convert R_in to.             !
    ! b     ::: Integer, end of range of integer to convert R_in to.               !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R_out ::: Integer, random integer in [1,b).                                  !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    real(kind=dp), intent(in)  :: R_in
    integer,       intent(in)  :: a, b
    integer,       intent(out) :: R_out
    real(kind=dp)              :: R_tmp
    ! First convert the random number to the desired range
    call rand_range_(R_in,real(a,dp),real(b,dp),R_tmp)
    ! Then convert to an integer, just by flooring
    R_out = floor(R_tmp)
  end subroutine rand_int_

  subroutine rand_int_arr_(R_in,N,a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Convert an array of random floats in [0,1) to an array of random integers    !
    ! in [a,b).                                                                    !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! R_in  ::: 1D float array, a random value in [0,1).                           !
    ! N     ::: Integer, size of R_in and R_out.                                   !
    ! a     ::: Integer, start of range of integer to convert R_in to.             !
    ! b     ::: Integer, end of range of integer to convert R_in to.               !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R_out ::: 1D integer array, size N, contains random integers in [1,b).       !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp), dimension(N), intent(in)  :: R_in
    integer,                     intent(in)  :: a, b
    integer,       dimension(N), intent(out) :: R_out
    real(kind=dp), dimension(N)              :: R_tmp
    integer                                  :: Ri
    ! First convert the random number to the desired range
    call rand_range_arr_(R_in,N,real(a,dp),real(b,dp),R_tmp)
    ! Then convert to an integer, just by flooring
    do Ri=1,N
      R_out(Ri) = floor(R_tmp(Ri))
    end do
  end subroutine rand_int_arr_

  subroutine GCD64_(a,b,gcd)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Find the greatest common divisor of a and b, i.e. GCD(a,b) and return as     !
    ! gcd_out.                                                                     !
    !                                                                              !
    ! NOTE: This is a 64 bit integer routine, or an integer kind 8 routine.        !
    !       This is not universally 64 bit, but we cant use int64 from             !
    !       iso_fortran_env and compile for Python.                                !
    ! NOTE: We require a<b.                                                        !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Integer, kind 8, one value to find GCD of.                             !
    ! b ::: Integer, kind 8, second value to find GCD of, a<b.                     !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! gcd ::: The greatest common divisor of a and b.                              !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer(kind=k18), intent(in)  :: a, b
    integer(kind=k18), intent(out) :: gcd
    integer(kind=k18)              :: a_, b_, tmp
    ! We want a<b so first check this
    print *, a, b
    select case(a<b)
    case(.false.)
      call stop_E("GCD64_ requires a<b")
    case(.true.)
      ! We have a valid a and b, so set the temporary a and b variables
      a_ = a
      b_ = b
      ! Then loop until we find a divisor
      do while (b_ /= 0)
        tmp = a_
        a_  = b_
        b_  = mod(tmp,b_)
      end do
      gcd = a_
      return
    end select
      
  end subroutine GCD64_

  function XOR_char_(XOR1,XOR2) result(XORED)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Carry out the XOR operation on the input characters "1" and "0".             !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! XOR1 ::: Character, must be either "0" or "1", first bit represented as a    !
    !          character to be XORed.                                              !
    ! XOR2 ::: Character, must be either "0" or "1", second bit represented as a   !
    !          character to be XORed.                                              !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! XORED ::: Character, either "0" or "1", XOR1 XORed with XOR2.                !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    character(len=*), intent(in) :: XOR1, XOR2
    character(len=1)             :: XORED

    select case(XOR1)
    case("1")
      select case(XOR2)
      case("1")
        XORED = "0"
        return
      case("0")
        XORED = "1"
        return
      case default
        call stop_E("CAn only XOR_char_ single characters that are 1 or 0")
      end select
    case("0")
      select case(XOR2)
      case("1")
        XORED = "1"
        return
      case("0")
        XORED = "0"
        return
      case default
        call stop_E("CAn only XOR_char_ single characters that are 1 or 0")
      end select
    case default
      call stop_E("CAn only XOR_char_ single characters that are 1 or 0")
    end select

  end function XOR_char_

  function rule90_(s_im1,s_i,s_ip1) result(ruled)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Cellular automata rule 90, i.e.                                              !
    !     s_i(t+1) = s_i-1(t) XOR s_i+1(t)                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer, intent(in)  :: s_im1, s_i, s_ip1
    integer               :: ruled
    ruled = ieor(s_im1,s_ip1)
  end function rule90_

  function rule150_(s_im1,s_i,s_ip1) result(ruled)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Cellular automata rule 150, i.e.                                             !
    !     s_i(t+1) = s_i-1(t) XOR s_i(t) XOR s_i+1(t)                              !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer, intent(in)  :: s_im1, s_i, s_ip1
    integer              :: ruled
    ruled = ieor(s_im1,ieor(s_i,s_ip1))
  end function rule150_
  
  function rule90_char_(s_im1,s_i,s_ip1) result(ruled)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Cellular automata rule 90, i.e.                                              !
    !     s_i(t+1) = s_i-1(t) XOR s_i+1(t)                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    character(len=1), intent(in)  :: s_im1, s_i, s_ip1
    character(len=1)               :: ruled
    ruled = XOR_char_(s_im1,s_ip1)
  end function rule90_char_

  function rule150_char_(s_im1,s_i,s_ip1) result(ruled)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Cellular automata rule 150, i.e.                                             !
    !     s_i(t+1) = s_i-1(t) XOR s_i(t) XOR s_i+1(t)                              !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    character(len=1), intent(in)  :: s_im1, s_i, s_ip1
    character(len=1)              :: ruled
    ruled = XOR_char_(s_im1,XOR_char_(s_i,s_ip1))
  end function rule150_char_

  subroutine rule_90_150_(rule_in,N_bit,rule_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Take integer represented in binary as an array of 1s and 0s then perform rule!
    ! 90, then rule 150 and return the resultant array of 1s and 0s.               !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! rule_in ::: 1D array of integers, each element should be a 1 or 0. The array !
    !             represents an integer in binary.                                 !
    ! N_bit   ::: Integer, the number of bits in the integer represented by        !
    !             rule_in, i.e. length of rule_in.                                 !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! rule_out ::: 1D array of length N_bits, the input array rule_in with rule 90,!
    !              then rule 150 applied. Will only contain 1s or 0s.              !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer,                   intent(in)  :: N_bit
    integer, dimension(N_bit), intent(in)  :: rule_in
    integer, dimension(N_bit), intent(out) :: rule_out

    integer, dimension(N_bit) :: tmp
    integer                   :: k_
    
    ! First apply rule 90 to the input
    ! Note we copy this to tmp as we want to find next time step in rule 150 after 90
    do k_=1,N_bit
      if (k_==1) then
        ! Need to wraparound the periodic boundary for the first
        tmp(1)   = rule90_(rule_in(N_bit),rule_in(1),rule_in(2))
      else if (k_==N_bit) then
        ! Also need to wraparound the periodic boundary for the last
        tmp(N_bit) = rule90_(rule_in(N_bit-1),rule_in(N_bit),rule_in(1))
      else
        ! No wraparound needed in the middle of the string
        tmp(k_) = rule90_(rule_in(k_-1),rule_in(k_),rule_in(k_+1))
      end if
    end do
    
    ! Now apply rule 150 and return
    do k_=1,N_bit
      if (k_==1) then
        ! Need to wraparound the periodic boundary for the first
        rule_out(1)   = rule150_(rule_in(N_bit),rule_in(1),rule_in(2))
      else if (k_==N_bit) then
        ! Also need to wraparound the periodic boundary for the last
        rule_out(N_bit) = rule150_(rule_in(N_bit-1),rule_in(N_bit),rule_in(1))
      else
        ! No wraparound needed in the middle of the string
        rule_out(k_) = rule150_(rule_in(k_-1),rule_in(k_),rule_in(k_+1))
      end if
    end do
    
  end subroutine rule_90_150_

  subroutine bitarr2int(bitArr,N_bits,bitInt)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Convert an input array which contains only 1s and 0s to the integer          !
    ! represented by the binary number in the array.                               !
    ! E.g. an input array [0,0,0,1,0,1,1,0] would return 22.                       !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! bitArr ::: Integer array, must contain only 1s or 0s, this array represents  !
    !            an integer in binary and will be converted to a base 10 integer.  !
    ! N_bits ::: Integer, the number of bits in the binary representation of the   !
    !            integer, i.e. the length of bitArr.                               !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! bitInt ::: Integer, the integer represented in bitArr in base 10.            !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    integer,                    intent(in)  :: N_bits
    integer, dimension(N_bits), intent(in)  :: bitArr
    integer,                    intent(out) :: bitInt

    integer :: pow, i_bit

    ! Initialise everything to 0
    bitInt = 0
    pow = 0

    ! Now loop through all elements of the bit array adding their respective values
    ! to the counter
    i_bit = N_bits
    do while (i_bit>0)
      bitInt = bitInt + ( bitArr(i_bit) * 2**pow )
      pow   = pow   + 1
      i_bit = i_bit - 1
    end do
    
  end subroutine bitarr2int
  
end module RNGutil



!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
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
  public :: rand_arr
  public :: rand_range
  public :: rand_range_arr
  public :: rand_int
  public :: rand_int_arr
  
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

  subroutine rand_arr(N,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! The intrinsic Fortran random number generator, used to generate an array of  !
    ! length N, where all elements are numbers R in                                !
    !    0 <= R < 1                                                                !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp), dimension(N), intent(out) :: R
    call random_number(R)
  end subroutine rand_arr

  subroutine rand_range(a,b,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a random number in [0,1) using this modules rand routine, then      !
    ! scale this random number to be in the range [a,b).                           !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_range_
    implicit none
    real(kind=dp), intent(in)  :: a, b
    real(kind=dp), intent(out) :: R
    real(kind=dp)              :: R_tmp
    ! Gen a random number
    call rand(R_tmp)
    ! And then scale it to the desired range
    call rand_range_(R_tmp,a,b,R)
  end subroutine rand_range
  
  subroutine rand_range_arr(N,a,b,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a 1D array of length N containing random numbers in [0,1) using     !
    ! this modules rand routine, then scale this random number to be in the        !
    ! range [a,b).                                                                 !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, The size of the desired output array R.                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_range_arr_
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp),               intent(in)  :: a, b
    real(kind=dp), dimension(N), intent(out) :: R
    real(kind=dp), dimension(N)              :: R_tmp
    ! Gen an array of random numbers, temp for now as we scale them into the output array
    call rand_arr(N,R_tmp)
    ! And then scale the array
    call rand_range_arr_(R_tmp,N,a,b,R)
  end subroutine rand_range_arr

  subroutine rand_int(a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a random integer in [a,b) using this modules rand routine, then     !
    ! scale this random number to be an integer in the range [a,b).                !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Integer, start of range of integers to scale random number to.         !
    ! b ::: Integer, end of range of integer to scale random number to.            !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random integer in range [a,b).                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_int_
    implicit none
    integer,       intent(in)  :: a, b
    integer,       intent(out) :: R_out
    real(kind=dp)              :: R_tmp
    ! Gen a random number
    call rand(R_tmp)
    ! And then scale it to the desired range and convert to floored integers
    call rand_int_(R_tmp,a,b,R_out)
  end subroutine rand_int
  
  subroutine rand_int_arr(N,a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a 1D array of length N containing random numbers in [0,1) using     !
    ! this modules rand routine, then scale this random number to be in the        !
    ! range [a,b).                                                                 !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, The size of the desired output array R.                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_int_arr_
    implicit none
    integer,                     intent(in)  :: N
    integer,                     intent(in)  :: a, b
    integer,       dimension(N), intent(out) :: R_out
    real(kind=dp), dimension(N)              :: R_tmp
    ! Gen an array of random numbers, temp for now as we scale them into the output array
    call rand_arr(N,R_tmp)
    ! Then scale the array and convert to floored integers
    call rand_int_arr_(R_tmp,N,a,b,R_out)
  end subroutine rand_int_arr
  
end module frand



!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
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
  ! setseed        ::: Set the seed of lgm RNG.                                  !
  ! rand           ::: Generate random number in [0,1) using lgm.                !
  ! rand_arr       ::: Generate 1D array of N random numbers in [0,1).           !
  ! rand_range     ::: Generate a random float in a given range.                 !
  ! rand_range_arr ::: Generate 1D array of N random numbers in given range.     !
  ! rand_int       ::: Generate random integer in given range.                   !
  ! rand_int_arr   ::: Generate 1D array of N random integers in given range.    !
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
  public :: rand_arr
  public :: rand_range
  public :: rand_range_arr
  public :: rand_int
  public :: rand_int_arr

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
    use RNGutil, only : stop_E
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
    ! R_::: real : A pseudo random value in [0,1).                                 !
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

  subroutine rand_arr(N,Rarr)
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
    
  end subroutine rand_arr

  subroutine rand_range(a,b,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a random number in [0,1) using this modules rand routine, then      !
    ! scale this random number to be in the range [a,b).                           !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_range_
    implicit none
    real(kind=dp), intent(in)  :: a, b
    real(kind=dp), intent(out) :: R
    real(kind=dp)              :: R_tmp
    ! Gen a random number
    call rand(R_tmp)
    ! And then scale it to the desired range
    call rand_range_(R_tmp,a,b,R)
  end subroutine rand_range
  
  subroutine rand_range_arr(N,a,b,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a 1D array of length N containing random numbers in [0,1) using     !
    ! this modules rand routine, then scale this random number to be in the        !
    ! range [a,b).                                                                 !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, The size of the desired output array R.                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_range_arr_
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp),               intent(in)  :: a, b
    real(kind=dp), dimension(N), intent(out) :: R
    real(kind=dp), dimension(N)              :: R_tmp
    ! Gen an array of random numbers, temp for now as we scale them into the output array
    call rand_arr(N,R_tmp)
    ! And then scale the array
    call rand_range_arr_(R_tmp,N,a,b,R)
  end subroutine rand_range_arr
  
  subroutine rand_int(a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a random integer in [a,b) using this modules rand routine, then     !
    ! scale this random number to be an integer in the range [a,b).                !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Integer, start of range of integers to scale random number to.         !
    ! b ::: Integer, end of range of integer to scale random number to.            !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random integer in range [a,b).                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_int_
    implicit none
    integer,       intent(in)  :: a, b
    integer,       intent(out) :: R_out
    real(kind=dp)              :: R_tmp
    ! Gen a random number
    call rand(R_tmp)
    ! And then scale it to the desired range and convert to floored integers
    call rand_int_(R_tmp,a,b,R_out)
  end subroutine rand_int
  
  subroutine rand_int_arr(N,a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a 1D array of length N containing random numbers in [0,1) using     !
    ! this modules rand routine, then scale this random number to be in the        !
    ! range [a,b).                                                                 !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, The size of the desired output array R.                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_int_arr_
    implicit none
    integer,                     intent(in)  :: N
    integer,                     intent(in)  :: a, b
    integer,       dimension(N), intent(out) :: R_out
    real(kind=dp), dimension(N)              :: R_tmp
    ! Gen an array of random numbers, temp for now as we scale them into the output array
    call rand_arr(N,R_tmp)
    ! Then scale the array and convert to floored integers
    call rand_int_arr_(R_tmp,N,a,b,R_out)
  end subroutine rand_int_arr
  
end module lgmrand



!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!



module BBSrand64
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Use the Blum Blum Shub algorithm [1] to generate a pseudo random number with !
  !                                                                              !
  !     R_{j+1} = R_{j}^{2} (mod M)                                              !
  !                                                                              !
  ! where M=p*q, p and q are prime and R_{0} is coprime of M.                    !
  !                                                                              !
  ! NOTE: This version used 64 bit integers, or at least an approximation of     !
  !       them with integer kind 8. This is used over the int64 from             !
  !       iso_fortran_env as int64 does not pass 64 bit integers properly from   !
  !       python, and we want these modules to be portable.                      !
  !                                                                              !
  ! CONTAINS                                                                     !
  ! ========                                                                     !
  ! setseed        ::: Set the seed of lgm RNG.                                  !
  ! rand           ::: Generate random number in [0,1) using lgm.                !
  ! rand_arr       ::: Generate 1D array of N random numbers in [0,1).           !
  ! rand_range     ::: Generate a random float in a given range.                 !
  ! rand_range_arr ::: Generate 1D array of N random numbers in given range.     !
  ! rand_int       ::: Generate random integer in given range.                   !
  ! rand_int_arr   ::: Generate 1D array of N random integers in given range.    !
  !                                                                              !
  ! REFERENCES                                                                   !
  ! ==========                                                                   !
  ! [1] Blum L, Blum M, Shub M. A Simple Unpredictable Pseudo-Random Number      !
  !     Generator. SIAM J Comput. 1986;15(2):364–83.                             !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use types
  implicit none

  private

  ! Primes p and q where M = p*q
  integer(kind=k18),   save :: p=4254007              ! = 3 (mod 4)
  integer(kind=k18),   save :: q=1010101039           ! = 3 (mod 4)
  integer(kind=k32), save :: M=4296976890613273_k32 ! p * q
  ! Note: gcd( (p-3)/2, (q-3)/2 ) = 2
  
  ! The last integer value calculated for lgmrand random number generation,
  ! i.e. the R_{j} value to use for calculation of R_{j+1}
  ! NOTE: Integer must be of sufficient size to hold M**2 as an integer
  integer(kind=k32), save :: last=2148488445306635_k32 ! Coprime of p*q
  
  public :: set_seed
  public :: rand
  public :: rand_arr
  public :: rand_range
  public :: rand_range_arr
  public :: rand_int
  public :: rand_int_arr

contains

  subroutine set_seed(seed_in,p_in,q_in)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Set the seed values for the BBS method, this includes the p and q values.    !
    !                                                                              !
    ! NOTE: We require gcd((p-3)/2,(q-3)/2) to be sufficiently small.              !
    !                                                                              !
    ! NOTE: We require seed_in and p*q to be coprime.                              !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! seed_in ::: Integer, kind 8, seed (or last value).                           !
    ! p_in    ::: Integer, kind 8, p value.                                        !
    ! q_in    ::: Integer, kind 8, q value.                                        !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : stop_E, GCD64_
    implicit none

    integer(kind=k18), intent(in) :: seed_in, p_in, q_in
    integer(kind=k18)             :: s_, p_, q_
    integer                       :: gcd_cutoff=10
    
    ! Perform initial checks on the inputs

    ! First, let q be the larger of p_in, q_in
    if (p_in<q_in) then
      p_ = p_in
      q_ = q_in
    else if (p_in>q_in) then
      p_ = q_in
      q_ = p_in
    else
      call stop_E("set_seed for BBS requires p /- q")
    end if
    
    ! Now check the GCD of p and q, where we require this GCD be `sufficiently small'
    call GCD64_((p_-3)/2,(q_-3)/2,s_)
    if (s_>=gcd_cutoff) call stop_E("gcd((p-3)/2,(q-3)/2) >= 10")    
    
    ! Check the seed, i.e. the last computed value which is initialised to the seed
    select case(seed_in)
    case(:1)
      ! Find GCD of new seed and p*q and check that seed and p*q are coprime
      call GCD64_(seed_in,p*q,s_)
      select case(s_)
      case(1)
        ! All tests passed, so write the seed and parameters
        p = p_
        q = q_
        M = p_ * q_
        last = seed_in
      case default
        call stop_E("Seed must be coprime to p*q")
      end select
    case default
      call stop_E("Seed must be an integer > 0")
    end select
    
  end subroutine set_seed
  
  subroutine rand(R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calculate a random number in [0,1) with the Blum Blum Shub RNG [1].          !
    ! This is done using                                                           !
    !                                                                              !
    !      R_{j+1} = R_{j}^2 (mod M)                                               !
    !                                                                              !
    ! Where M=p*q for prime p and q. Also we require gcd((p-3)/2,(q-3)/2)          !
    ! sufficiently small and R_{0} coprime to M.                                   !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Float, random number in [0,1).                                         !
    !                                                                              !
    ! REFERENCES                                                                   !
    ! ==========                                                                   !
    ! [1] Blum L, Blum M, Shub M. A Simple Unpredictable Pseudo-Random Number      !
    !     Generator. SIAM J Comput. 1986;15(2):364–83.                             !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    real(kind=dp), intent(out) :: R
    ! Calculate the current value, which is the `last' value for next call
    last = mod(last**2,M)
    R = real(last,dp)/real(M,dp)
  end subroutine rand

  subroutine rand_arr(N,Rarr)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate an array of length N containing random numbers in [0,1) generated   !
    ! with BBS.                                                                    !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, number of random numbers in Rarr (also length of Rarr).       !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! Rarr ::: 1D float array, length N, contains random floats in [0,1).          !
    !                                                                              !
    ! REFERENCES                                                                   !
    ! ==========                                                                   !
    ! [1] Blum L, Blum M, Shub M. A Simple Unpredictable Pseudo-Random Number      !
    !     Generator. SIAM J Comput. 1986;15(2):364–83.                             !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp), dimension(N), intent(out) :: Rarr
    integer                                  :: i_
    do i_=1,N
      call rand(Rarr(i_))
    end do
  end subroutine rand_arr

  subroutine rand_range(a,b,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a random number in [0,1) using this modules rand routine, then      !
    ! scale this random number to be in the range [a,b).                           !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_range_
    implicit none
    real(kind=dp), intent(in)  :: a, b
    real(kind=dp), intent(out) :: R
    real(kind=dp)              :: R_tmp
    ! Gen a random number
    call rand(R_tmp)
    ! And then scale it to the desired range
    call rand_range_(R_tmp,a,b,R)
  end subroutine rand_range
  
  subroutine rand_range_arr(N,a,b,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a 1D array of length N containing random numbers in [0,1) using     !
    ! this modules rand routine, then scale this random number to be in the        !
    ! range [a,b).                                                                 !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, The size of the desired output array R.                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_range_arr_
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp),               intent(in)  :: a, b
    real(kind=dp), dimension(N), intent(out) :: R
    real(kind=dp), dimension(N)              :: R_tmp
    ! Gen an array of random numbers, temp for now as we scale them into the output array
    call rand_arr(N,R_tmp)
    ! And then scale the array
    call rand_range_arr_(R_tmp,N,a,b,R)
  end subroutine rand_range_arr
  
  subroutine rand_int(a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a random integer in [a,b) using this modules rand routine, then     !
    ! scale this random number to be an integer in the range [a,b).                !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Integer, start of range of integers to scale random number to.         !
    ! b ::: Integer, end of range of integer to scale random number to.            !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random integer in range [a,b).                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_int_
    implicit none
    integer,       intent(in)  :: a, b
    integer,       intent(out) :: R_out
    real(kind=dp)              :: R_tmp
    ! Gen a random number
    call rand(R_tmp)
    ! And then scale it to the desired range and convert to floored integers
    call rand_int_(R_tmp,a,b,R_out)
  end subroutine rand_int
  
  subroutine rand_int_arr(N,a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a 1D array of length N containing random numbers in [0,1) using     !
    ! this modules rand routine, then scale this random number to be in the        !
    ! range [a,b).                                                                 !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, The size of the desired output array R.                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_int_arr_
    implicit none
    integer,                     intent(in)  :: N
    integer,                     intent(in)  :: a, b
    integer,       dimension(N), intent(out) :: R_out
    real(kind=dp), dimension(N)              :: R_tmp
    ! Gen an array of random numbers, temp for now as we scale them into the output array
    call rand_arr(N,R_tmp)
    ! Then scale the array and convert to floored integers
    call rand_int_arr_(R_tmp,N,a,b,R_out)
  end subroutine rand_int_arr
  
end module BBSrand64



!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!
!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!---!



module lfsr258
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !                                                                              !
  ! CONTAINS                                                                     !
  ! ========                                                                     !
  ! setseed        ::: Set the seed of lgm RNG.                                  !
  ! rand           ::: Generate random number in [0,1) using lgm.                !
  ! rand_arr       ::: Generate 1D array of N random numbers in [0,1).           !
  ! rand_range     ::: Generate a random float in a given range.                 !
  ! rand_range_arr ::: Generate 1D array of N random numbers in given range.     !
  ! rand_int       ::: Generate random integer in given range.                   !
  ! rand_int_arr   ::: Generate 1D array of N random integers in given range.    !
  !                                                                              !
  ! REFERENCES                                                                   !
  ! ==========                                                                   !
  ! [1] P. L’Ecuyer, Tables of maximally equidistributed combined lfsr           !
  !     generators, Mathematics of Computation of the American Mathematical      !
  !     Society 68 (225) (1999) 261–269.                                         !
  ! [2] L'Ecuyer's 1999 random number generator by Alan Miller, available at     !
  !     https://wp.csiro.au/alanmiller/random/lfsr258.f90, accessed 22/09/2021.  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use types
  implicit none

  private

  ! Seed values
  integer(kind=k18), save  :: s1 = 153587801, s2 = -759022222, s3 = 1288503317, &
    s4 = -1718083407, s5 = -123456789
  
  public :: set_seed
  public :: lfsr258_
  public :: rand
  public :: rand_arr
  public :: rand_range
  public :: rand_range_arr
  public :: rand_int
  public :: rand_int_arr

contains
  
  subroutine set_seed(i1, i2, i3, i4, i5)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Set the seed values for the lfsr258 method, this requires 5 integer (64 bit) !
    ! values.                                                                      !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! i1, i2, i3, i4, i5 :: 64 bit integer seeds.                                  !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    integer(kind=k18), intent(in) :: i1, i2, i3, i4, i5

    s1 = i1
    s2 = i2
    s3 = i3
    s4 = i4
    s5 = i5
    if (iand(s1,      -2_k18) == 0) s1 = i1 - 8388607_k18
    if (iand(s2,    -512_k18) == 0) s2 = i2 - 8388607_k18
    if (iand(s3,   -4096_k18) == 0) s3 = i3 - 8388607_k18
    if (iand(s4, -131072_k18) == 0) s4 = i4 - 8388607_k18
    if (iand(s5,-8388608_k18) == 0) s5 = i5 - 8388607_k18

    return
  end subroutine set_seed

  function lfsr258_() result(R_)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calculate a random number in [0,1) with lfsr258 based on the implementation  !
    ! in [1], taken from [2] with minor adaptations.                               !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Float, random number in [0,1).                                         !
    !                                                                              !
    ! NOTES                                                                        !
    ! =====                                                                        !
    ! Original notes from Alan Millers function:                                   !
    !                                                                              !
    ! Generates a random number between 0 and 1.  Translated from C function in:   !
    ! L'Ecuyer, P. (1999) `Tables of maximally equidistributed combined LFSR       !
    ! generators', Math. of Comput., 68, 261-269.                                  !
    !                                                                              !
    ! The cycle length is claimed to be about 2^(258) or about 4.6 x 10^77.        !
    ! Actually - (2^63 - 1).(2^55 - 1).(2^52 - 1).(2^47 - 1).(2^41 - 1)            !
    !                                                                              !
    ! REFERENCES                                                                   !
    ! ==========                                                                   !
    ! [1] P. L’Ecuyer, Tables of maximally equidistributed combined lfsr           !
    !     generators, Mathematics of Computation of the American Mathematical      !
    !     Society 68 (225) (1999) 261–269.                                         !
    ! [2] L'Ecuyer's 1999 random number generator by Alan Miller, available at     !
    !     https://wp.csiro.au/alanmiller/random/lfsr258.f90, accessed 22/09/2021.  !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    real(kind=dp) :: R_

    integer(kind=k18)   :: b

    ! N.B. ishft(i,j) is a bitwise (non-circular) shift operation;
    !      to the left if j > 0, otherwise to the right.

    b  = ishft( ieor( ishft(s1,1_k18), s1), -53_k18)
    s1 = ieor( ishft( iand(s1,-2_k18), 10_k18), b)
    b  = ishft( ieor( ishft(s2,24_k18), s2), -50_k18)
    s2 = ieor( ishft( iand(s2,-512_k18), 5_k18), b)
    b  = ishft( ieor( ishft(s3,3_k18), s3), -23_k18)
    s3 = ieor( ishft( iand(s3,-4096_k18), 29_k18), b)
    b  = ishft( ieor( ishft(s4,5_k18), s4), -24_k18)
    s4 = ieor( ishft( iand(s4,-131072_k18), 23_k18), b)
    b  = ishft( ieor( ishft(s5,3_k18), s5), -33_k18)
    s5 = ieor( ishft( iand(s5,-8388608_k18), 8_k18), b)

    ! The constant below is the reciprocal of (2^64 - 1)
    R_ = ieor( ieor( ieor( ieor(s1,s2), s3), s4), s5)  &
      * 5.4210108624275221E-20_dp + 0.5_dp

    return
  end function lfsr258_
  
  subroutine rand(R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calculate a random number in [0,1) with the Blum Blum Shub RNG [1].          !
    ! This is done using                                                           !
    !                                                                              !
    !      R_{j+1} = R_{j}^2 (mod M)                                               !
    !                                                                              !
    ! Where M=p*q for prime p and q. Also we require gcd((p-3)/2,(q-3)/2)          !
    ! sufficiently small and R_{0} coprime to M.                                   !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Float, random number in [0,1).                                         !
    !                                                                              !
    ! REFERENCES                                                                   !
    ! ==========                                                                   !
    ! [1] Blum L, Blum M, Shub M. A Simple Unpredictable Pseudo-Random Number      !
    !     Generator. SIAM J Comput. 1986;15(2):364–83.                             !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    real(kind=dp), intent(out) :: R
    ! Calculate the current value, which is the `last' value for next call
    R = lfsr258_()
  end subroutine rand

  subroutine rand_arr(N,Rarr)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate an array of length N containing random numbers in [0,1) generated   !
    ! with BBS.                                                                    !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, number of random numbers in Rarr (also length of Rarr).       !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! Rarr ::: 1D float array, length N, contains random floats in [0,1).          !
    !                                                                              !
    ! REFERENCES                                                                   !
    ! ==========                                                                   !
    ! [1] Blum L, Blum M, Shub M. A Simple Unpredictable Pseudo-Random Number      !
    !     Generator. SIAM J Comput. 1986;15(2):364–83.                             !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp), dimension(N), intent(out) :: Rarr
    integer                                  :: i_
    do i_=1,N
      call rand(Rarr(i_))
    end do
  end subroutine rand_arr

  subroutine rand_range(a,b,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a random number in [0,1) using this modules rand routine, then      !
    ! scale this random number to be in the range [a,b).                           !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_range_
    implicit none
    real(kind=dp), intent(in)  :: a, b
    real(kind=dp), intent(out) :: R
    real(kind=dp)              :: R_tmp
    ! Gen a random number
    call rand(R_tmp)
    ! And then scale it to the desired range
    call rand_range_(R_tmp,a,b,R)
  end subroutine rand_range
  
  subroutine rand_range_arr(N,a,b,R)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a 1D array of length N containing random numbers in [0,1) using     !
    ! this modules rand routine, then scale this random number to be in the        !
    ! range [a,b).                                                                 !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, The size of the desired output array R.                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_range_arr_
    implicit none
    integer,                     intent(in)  :: N
    real(kind=dp),               intent(in)  :: a, b
    real(kind=dp), dimension(N), intent(out) :: R
    real(kind=dp), dimension(N)              :: R_tmp
    ! Gen an array of random numbers, temp for now as we scale them into the output array
    call rand_arr(N,R_tmp)
    ! And then scale the array
    call rand_range_arr_(R_tmp,N,a,b,R)
  end subroutine rand_range_arr
  
  subroutine rand_int(a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a random integer in [a,b) using this modules rand routine, then     !
    ! scale this random number to be an integer in the range [a,b).                !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! a ::: Integer, start of range of integers to scale random number to.         !
    ! b ::: Integer, end of range of integer to scale random number to.            !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random integer in range [a,b).                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_int_
    implicit none
    integer,       intent(in)  :: a, b
    integer,       intent(out) :: R_out
    real(kind=dp)              :: R_tmp
    ! Gen a random number
    call rand(R_tmp)
    ! And then scale it to the desired range and convert to floored integers
    call rand_int_(R_tmp,a,b,R_out)
  end subroutine rand_int
  
  subroutine rand_int_arr(N,a,b,R_out)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Generate a 1D array of length N containing random numbers in [0,1) using     !
    ! this modules rand routine, then scale this random number to be in the        !
    ! range [a,b).                                                                 !
    !                                                                              !
    ! INPUTS                                                                       !
    ! ======                                                                       !
    ! N ::: Integer, The size of the desired output array R.                       !
    ! a ::: Real, start of range to scale random number to.                        !
    ! b ::: Real, end of range to scale random number to.                          !
    !                                                                              !
    ! RETURNS                                                                      !
    ! =======                                                                      !
    ! R ::: Random number in range [a,b).                                          !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use RNGutil, only : rand_int_arr_
    implicit none
    integer,                     intent(in)  :: N
    integer,                     intent(in)  :: a, b
    integer,       dimension(N), intent(out) :: R_out
    real(kind=dp), dimension(N)              :: R_tmp
    ! Gen an array of random numbers, temp for now as we scale them into the output array
    call rand_arr(N,R_tmp)
    ! Then scale the array and convert to floored integers
    call rand_int_arr_(R_tmp,N,a,b,R_out)
  end subroutine rand_int_arr
  
end module lfsr258


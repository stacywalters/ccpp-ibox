
!> \brief Main SCM program that calls the main SCM subroutine
!!
!! The Doxygen documentation system cannot handle in-body comments in Fortran main programs, so the "main" program was put in the
!! subroutine \ref ibox_main_sub above.
program ibox
  use ibox_main
  call ibox_main_sub()  
end program ibox

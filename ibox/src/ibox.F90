module ibox_main

implicit none

contains

subroutine ibox_main_sub()

  use :: ccpp_api,                           &
         only: ccpp_t,                       &
               ccpp_init,                    &
               ccpp_finalize,                &
               ccpp_physics_init,            &
               ccpp_physics_run,             &
               ccpp_physics_finalize,        &
               ccpp_field_add

  use :: iso_c_binding, only: c_loc

#include "ccpp_modules.inc"

  implicit none

!  type(scm_state_type), target :: scm_state
!  type(scm_input_type), target :: scm_input
!  type(scm_reference_type), target :: scm_reference

! USE THIS SOON
!  type( XXXX_type), target :: XXX_data

  integer                           :: i, j
  real (kind=8), pointer :: my_co(:)
  character(len=512)     :: errmsg
  integer :: errflg

  type(ccpp_t), allocatable, target                      :: cdata(:)

  integer                                                :: cdata_time_index
  integer                                                :: ierr
  integer ,parameter :: ncols=1
  integer ,parameter :: nlevs=8
  integer ,parameter :: ntimes=3

  allocate(my_co(nlevs))
  allocate(cdata(ncols))

  do i = 1, ncols
      call ccpp_init( '../suites/suite_ibox_test_simple1.xml', cdata(i), ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_init for column ', i, '. Exiting...'
          stop
      end if


    !use ccpp_fields.inc to call ccpp_field_add for all variables to be exposed to CCPP (this is auto-generated from /src/ccpp/scripts/ccpp_prebuild.py - the script parses tables in ibox_type_defs.f90)

#  include "ccpp_fields.inc"

      !initialize each column's physics
      call ccpp_physics_init(cdata(i), ierr=ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_init for column ', i, '. Exiting...'
          stop
      end if

  end do


  do j = 1, ntimes
    do i = 1, ncols
       call ccpp_physics_run(cdata(i), ierr=ierr)
       if (ierr/=0) then
           write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_run for column ', i, '. Exiting...'
           stop
       end if

       write(6,*) ' At time j=',j,' my_co(1)=',my_co(1)
     end do
  end do


  do i=1, ncols
      call ccpp_finalize(cdata(i), ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_finalize for column ', i, '. Exiting...'
          stop
      end if
  end do

end subroutine ibox_main_sub

end module ibox_main

!> \brief Main SCM program that calls the main SCM subroutine
!!
!! The Doxygen documentation system cannot handle in-body comments in Fortran main programs, so the "main" program was put in the
!! subroutine \ref ibox_main_sub above.
program ibox
  use ibox_main
  call ibox_main_sub()
end program ibox

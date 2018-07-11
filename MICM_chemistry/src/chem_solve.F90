
module chem_solve

  use micm_type_defs,  only: chem_type, micm_data_type, Solver_type
  use kinetics_module, only: kinetics_type
  
  implicit none

  private
  public :: chem_solve_init 
  public :: chem_solve_finalize
  public :: chem_solve_run

  integer, parameter :: rk = 8
  
contains

!> \section arg_table_chem_solve_init Argument Table
!! | local_name | standard_name                                    | long_name                               | units   | rank | type      | kind      | intent | optional |
!! |------------|--------------------------------------------------|-----------------------------------------|---------|------|-----------|-----------|--------|----------|
!! | co         | my_volume_mixing_ratio_co                        | CO volume mixing ratio                  | kg kg-1 |    1 | real      | kind_phys | inout  | F        |
!! | chem_obj   | chemistry_data                                   | encapsulated stuff for chemistry        | DDT     |    0 | chem_type |           | none   | F        |
!! | micm_obj   | micm_ddt                                         | micm derived data type                  | DDT     |    0 | micm_data_type |           | none   | F        |
!! | errmsg     | error_message                                    | CCPP error message                      | none    |    0 | character | len=512   | out    | F        |
!! | errflg     | error_flag                                       | CCPP error flag                         | flag    |    0 | integer   |           | out    | F        |
!!
  subroutine chem_solve_init (co, chem_obj, micm_obj, errmsg, errflg)

    implicit none

    !--- arguments
    real(rk),           pointer :: co(:)
    type(chem_type),    pointer :: chem_obj
    type(micm_data_type), pointer :: micm_obj
    character(len=512), intent(out) :: errmsg
    integer,            intent(out) :: errflg

    co(:) = 100._rk
    micm_obj%my_co(:) = 200._rk

    call chem_obj%set_some_stuff( 1.0 )
    
    errmsg = ''
    errflg = 0

  end subroutine chem_solve_init

!> \section arg_table_chem_solve_run Argument Table
!! | local_name | standard_name                                    | long_name                               | units   | rank | type      | kind      | intent | optional |
!! |------------|--------------------------------------------------|-----------------------------------------|---------|------|-----------|-----------|--------|----------|
!! | co         | my_volume_mixing_ratio_co                        | CO  volume mixing ratio                 | kg kg-1 |    1 | real      | kind_phys | inout  | F        |
!! | chem_obj   | chemistry_data                                   | encapsulated stuff for chemistry        | DDT     |    0 | chem_type |           | none   | F        |
!! | theKinetics | kinetics_data                                   | encapsulated stuff for chemistry        | DDT     |    0 | kinetics_type |           | none   | F        |
!! | ODE_obj    | ODE_ddt                                          | ODE derived data type                   | DDT     |    0 | Solver_type |           | none   | F        |
!! | errmsg     | error_message                                    | CCPP error message                      | none    |    0 | character | len=512   | out    | F        |
!! | errflg     | error_flag                                       | CCPP error flag                         | flag    |    0 | integer   |           | out    | F        |
!!
!*! | dt         | time_step_for_physics                            | physics time step                       | s       |    0 | real      | kind_phys | in     | F        |
!*! | ncol       | horizontal_loop_extent                           | horizontal dimension                    | count   |    0 | integer   |           | in     | F        |
!*! | nlev       | adjusted_vertical_layer_dimension_for_radiation  | number of vertical layers for radiation | count   |    0 | integer   |           | in     | F        |
  subroutine chem_solve_run ( co, chem_obj, theKinetics, ODE_obj, errmsg, errflg)

    implicit none

    !--- arguments
    real(rk),           pointer :: co(:)
    type(chem_type),    pointer :: chem_obj
    type(kinetics_type), pointer :: theKinetics
    type(Solver_type), pointer :: ODE_obj
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    !--- local variables
    integer :: Ierr
    real    :: x
 
    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    !--- initialize intent(out) variables
    ! initialize all intent(out) variables here

    !--- actual code
    ! add your code here

    call ODE_obj%theSolver%Run( y=co, Ierr=Ierr )
!   co(:) = co(:)*0.5_rk

    x = chem_obj%get_some_stuff()
    x = x*2.
    call chem_obj%set_some_stuff(x)

    ! in case of errors, set errflg to a value != 0,
    ! create a meaningfull error message and return

    return

  end subroutine chem_solve_run

  subroutine chem_solve_finalize()
  end subroutine chem_solve_finalize

end module chem_solve

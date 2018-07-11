!> \file micm_type_defs.f90
!!  Contains type definitions for MICM variables and physics-related variables

module micm_type_defs

 use ODE_solver, only : baseOdeSolver

 implicit none
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! The following definition sets up the variables for use within MICM
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! Filter with CPP for PGI compiler
#ifndef __PGI
!> \section arg_table_micm_data_type
!! | local_name | standard_name                                    | long_name                               | units   | rank | type      |    kind   | intent | optional |
!! |------------|--------------------------------------------------|-----------------------------------------|---------|------|-----------|-----------|--------|----------|
!! | my_co      | my_volume_mixing_ratio_co                        | volume mixing ratio co                  | kg kg-1 |    1 | real      | kind_phys | none   | F        |
!! | chem_obj   | chemistry_data                                   | encapsulated stuff for chemistry        | DDT     |    0 | chem_type |           | none   | F        |
!! | micm_obj   | micm_ddt                                         | micm derived data type                  | DDT     |    0 | micm_data_type |           | none   | F        |
!! | ODE_obj    | ODE_ddt                                          | ODE derived data type                   | DDT     |    0 | Solver_type |           | none   | F        |
!! | dt         | time_step_for_physics                            | physics time step                       | s       |    0 | real      | kind_phys | in     | F        |
!! | ncol       | horizontal_loop_extent                           | horizontal dimension                    | count   |    0 | integer   |           | in     | F        |
!! | nlev       | adjusted_vertical_layer_dimension_for_radiation  | number of vertical layers for radiation | count   |    0 | integer   |           | in     | F        |
!! | errmsg     | error_message                                    | CCPP error message                      | none    |    0 | character | len=*     | out    | F        | 
!! | errflg     | error_flag                                       | CCPP error flag                         | flag    |    0 | integer   |           | out    | F        |
!!

#endif

  integer, parameter :: r8 = 8
  
  type micm_data_type

    real, allocatable      :: my_co(:)
    real                   :: dt
    integer                :: ncol
    integer                :: nlev
    character(len=512)     :: errmsg
    integer                :: errflg

    contains
!      procedure :: create => physics_create
!      procedure :: associate => physics_associate
  end type micm_data_type


 type chem_type
    private
    real, allocatable :: some_stuff(:,:,:)
  contains
    procedure :: create => chem_create
    procedure :: set_some_stuff => chem_set_some_stuff
    procedure :: get_some_stuff => chem_get_some_stuff
    procedure :: print => chem_print
 end type chem_type

 type Solver_type
   class(baseOdeSolver), pointer :: theSolver
 end type Solver_type

contains

  subroutine chem_create(this)
    class(chem_type) :: this
    allocate( this%some_stuff(1,1,2) )
  end subroutine chem_create
  
  subroutine chem_set_some_stuff(this, x)
    class(chem_type) :: this
    real, intent(in) :: x
    this%some_stuff = x
  end subroutine chem_set_some_stuff

  function chem_get_some_stuff(this) result(y)
    class(chem_type) :: this
    real :: y
    y = this%some_stuff(1,1,1)
  end function chem_get_some_stuff
  
  subroutine chem_print( this )
    class(chem_type) :: this
    write(*,*) '  Chemistry some stuff: ', this%some_stuff
  end subroutine chem_print

end module micm_type_defs

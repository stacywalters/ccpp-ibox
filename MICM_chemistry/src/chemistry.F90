!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CCPP-compliant physics scheme template
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! General rules:
!
! - scheme must be in its own module (module name = scheme name) and must
!   have three entry points (subroutines) starting with the name of the module:
!   module scheme_template -> subroutines scheme_template_{init,finalize,run}
!
! - empty schemes (e.g., scheme_template_init below) do not need an argument table
!
! - schemes in use require an argument table as below; order of arguments in the
!   table must be the same as in the argument list of the subroutine
!
! - all external information required by the scheme must be passed in via the
!   argument list, i.e. NO 'use EXTERNAL_MODULE' statements
!
! - if width of argument tables exceeds 250 characters, wrap the table (but only
!   the table) in CPP preprocessor directives #if 0 YOUR_TABLE #endif
!
! - for readibility, it is suggested to align the columns in the metadata table
!
! Input/output variable (argument) rules:
!
! - for a list of variables available for the specific host model, see table
!   "TABLE_NAME_NUMBER_MISSING" [howto keep up to date?] in the CCPP developer's guide
!
! - a standard_name cannot be assigned to more than one local variable (local_name)
!
! - all information (units, rank, index ordering) must match the specifications
!   on the host model side, but subslices can be used/added in the host model:
!   HOST MODEL: real, dimension(:,:,:,:) :: hydrometeors
!
!
! Coding rules:
!
! - code must comply to modern Fortran standards (Fortran 90/95/2003)
!
! - use labeled 'end' statements for modules, subroutines and functions
!   module scheme_template -> end module scheme_template
!
! - use implicit none
!
! - all intent(out) variables must be initialized properly inside the subroutine
!
! - NO permanent state inside the module, i.e. no variables carrying the 'save' attribute
!
! - NO 'goto' statements
!
! - errors are handled by the host model using the two mandatory arguments
!   errmsg and errflg; in the event of an error, assign a meaningful error
!   message to errmsg and set errflg to a value other than 0
!
! - schemes are NOT allowed to abort/stop the program
!
! - schemes are NOT allowed to perform I/O operations (except for reading
!   lookup tables / other information needed to initialize the scheme)
!
! - line lengths of 120 characters are suggested for better readibility
!   (exception: CCPP metadata argument tables)
!
! Parallel programming rules:
!
! - if OpenMP is used, the number of allowed threads must be provided by the
!   host model as an intent(in) argument in the argument list
!
! - if MPI is used, it is restricted to global communications: barrier, broadcast,
!   gather, scatter, reduction; the MPI communicator must be provided by the
!   host model as an intent(in) argument in the argument list
!   - do NOT use MPI_COMM_WORLD
!   - do NOT use any point-to-point communication
!
! - if Fortran coarrays are used, consult with the CCPP development team
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module chemistry

  implicit none

  private
  public :: chemistry_init 
  public :: chemistry_finalize
  public :: chemistry_run

  integer, parameter :: rk = 8
  
contains

!> \section arg_table_chemistry_init Argument Table
!! | local_name | standard_name                                    | long_name                               | units   | rank | type      | kind      | intent | optional |
!! |------------|--------------------------------------------------|-----------------------------------------|---------|------|-----------|-----------|--------|----------|
!! | co         | my_volume_mixing_ratio_co                        | CO volume mixing ratio                  | kg kg-1 |    1 | real      | kind_phys | inout  | F        |
!! | errmsg     | error_message                                    | CCPP error message                      | none    |    0 | character | len=512   | out    | F        |
!! | errflg     | error_flag                                       | CCPP error flag                         | flag    |    0 | integer   |           | out    | F        |
!!
  subroutine chemistry_init (co, errmsg, errflg)

    implicit none

    !--- arguments
    real(rk),pointer, intent(inout) :: co(:)
    character(len=512), intent(out)   :: errmsg
    integer,          intent(out)   :: errflg

    co(:) = 100_rk

  end subroutine chemistry_init

  subroutine chemistry_finalize()
  end subroutine chemistry_finalize

!> \section arg_table_chemistry_run Argument Table
!! | local_name | standard_name                                    | long_name                               | units   | rank | type      | kind      | intent | optional |
!! |------------|--------------------------------------------------|-----------------------------------------|---------|------|-----------|-----------|--------|----------|
!! | co         | my_volume_mixing_ratio_co                        | CO  volume mixing ratio                 | kg kg-1 |    1 | real      | kind_phys | inout  | F        |
!! | errmsg     | error_message                                    | CCPP error message                      | none    |    0 | character | len=512   | out    | F        |
!! | errflg     | error_flag                                       | CCPP error flag                         | flag    |    0 | integer   |           | out    | F        |
!!
!*! | dt         | time_step_for_physics                            | physics time step                       | s       |    0 | real      | kind_phys | in     | F        |
!*! | ncol       | horizontal_loop_extent                           | horizontal dimension                    | count   |    0 | integer   |           | in     | F        |
!*! | nlev       | adjusted_vertical_layer_dimension_for_radiation  | number of vertical layers for radiation | count   |    0 | integer   |           | in     | F        |
!  subroutine chemistry_run (dt, ncol,nlev, co, errmsg, errflg)
  subroutine chemistry_run ( co, errmsg, errflg)

    implicit none

    !--- arguments
!    real(rk),         intent(in)    :: dt
!    integer,          intent(in)    :: ncol,nlev
    real(rk), pointer,intent(inout) :: co(:)      
    character(len=512), intent(out)   :: errmsg
    integer,          intent(out)   :: errflg

    !--- local variables
 
    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    !--- initialize intent(out) variables
    ! initialize all intent(out) variables here

    !--- actual code
    ! add your code here

    co(:) = co(:)*0.5_rk
    
    ! in case of errors, set errflg to a value != 0,
    ! create a meaningfull error message and return

    return

  end subroutine chemistry_run

end module chemistry

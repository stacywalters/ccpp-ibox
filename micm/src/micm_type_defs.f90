!> \file gmtb_scm_type_defs.f90
!!  Contains type definitions for SCM-related variables and physics-related variables

module gmtb_scm_type_defs


! Filter with CPP for PGI compiler
#ifndef __PGI
!> \section arg_table_physics_type
!! | local_name                                            | standard_name                                                                                     | long_name                                                                           | units         | rank | type                  |    kind   | intent | optional |
!! |-------------------------------------------------------|---------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|---------------|------|-----------------------|-----------|--------|----------|
!! | my_co(:)   | my_volume_mixing_ratio_co                        | volume mixing ratio co                  | kg kg-1 |    1 | real      | kind_phys | none   | F        |
!! | dt         | time_step_for_physics                            | physics time step                       | s       |    0 | real      | kind_phys | in     | F        |
!! | ncol       | horizontal_loop_extent                           | horizontal dimension                    | count   |    0 | integer   |           | in     | F        |
!! | nlev       | adjusted_vertical_layer_dimension_for_radiation  | number of vertical layers for radiation | count   |    0 | integer   |           | in     | F        |
!! | errmsg     | error_message                                    | CCPP error message                      | none    |    0 | character | len=*     | out    | F        | 
!! | errflg     | error_flag                                       | CCPP error flag                         | flag    |    0 | integer   |           | out    | F        |
!!

#endif
  type physics_type

    real, allocatable      :: my_co(:)
    real                   :: dt
    real                   :: ncol
    real                   :: nlev
    character(len=512)     :: errmsg
    integer                :: errflg

    contains
!      procedure :: create => physics_create
!      procedure :: associate => physics_associate
  end type physics_type


end module gmtb_scm_type_defs

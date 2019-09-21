module module_read_netcdf
! module for reading netcdf global lat/lon grid files output by FV3GFS

! ifort -I${NETCDF}include -c -traceback module_read_netcdf.f90
  use netcdf

  implicit none
  private

  type Variable
     integer varid
     integer ndims
     integer dtype
     integer deflate_level
     logical shuffle
     character(len=nf90_max_name) :: name
     integer, allocatable, dimension(:) :: dimids 
     integer, allocatable, dimension(:) :: dimlens
  end type Variable   
  type Dimension 
     integer dimid
     integer len
     logical isunlimited
     character(len=nf90_max_name) :: name
  end type Dimension
  type Dataset
     integer :: ncid
     integer :: nvars
     integer :: ndims
     integer :: ngatts
     integer :: nunlimdim
     character(len=500) filename
     type(Variable), allocatable, dimension(:) :: variables
     type(Dimension), allocatable, dimension(:) :: dimensions
  end type Dataset

  interface read_vardata
      module procedure read_vardata_1d_r4, read_vardata_2d_r4, read_vardata_3d_r4,&
      read_vardata_4d_r4, read_vardata_1d_r8, read_vardata_2d_r8, read_vardata_3d_r8,&
      read_vardata_4d_r8, read_vardata_1d_int, read_vardata_2d_int, &
      read_vardata_3d_int, read_vardata_4d_int
  end interface

  interface read_attribute
      module procedure read_attribute_r4_scalar, read_attribute_int_scalar,&
      read_attribute_r8_scalar, read_attribute_r4_1d,&
      read_attribute_int_1d, read_attribute_r8_1d
  end interface

  public :: create_dataset, destroy_dataset, Dataset, Variable, Dimension
  public :: read_vardata, read_attribute, get_vardim, get_dimlen

  contains

  subroutine nccheck(status)
    ! check return code, print error message 
    implicit none
    integer, intent (in) :: status
    if (status /= nf90_noerr) then
      write(0,*) status, trim(nf90_strerror(status))
      stop "stopped"
    end if
  end subroutine nccheck

  integer function get_vardim(dset, varname)
    ! return number of dimensions associated with variable
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    integer nvar
    do nvar=1,dset%nvars
       if (trim(dset%variables(nvar)%name) == trim(varname)) then 
          exit
       endif
    enddo
    get_vardim = dset%variables(nvar)%ndims
  end function get_vardim

  integer function get_dimlen(dset, dimname)
    ! return length of dimension
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: dimname
    integer ndim
    do ndim=1,dset%ndims
       if (trim(dset%dimensions(ndim)%name) == trim(dimname)) then 
          exit
       endif
    enddo
    get_dimlen = dset%dimensions(ndim)%len
  end function get_dimlen
    
  subroutine create_dataset(filename, dset)
    ! create dataset object for netcdf file 
    implicit none
    character(len=*), intent(in) :: filename
    character(len=nf90_max_name) :: dimname
    type(Dataset), intent(out) :: dset
    integer ncerr,ncid,nunlimdim
    integer ndim,nvar,n
    ! open netcdf file, get info, populate Dataset object.
    ncerr = nf90_open(trim(filename), NF90_NOWRITE, ncid=dset%ncid)
    call nccheck(ncerr)
    ncerr = nf90_inquire(dset%ncid, dset%ndims, dset%nvars, dset%ngatts, nunlimdim)
    call nccheck(ncerr)
    dset%filename = trim(filename)
    allocate(dset%variables(dset%nvars))
    allocate(dset%dimensions(dset%ndims))
    do ndim=1,dset%ndims
       dset%dimensions(ndim)%dimid = ndim
       ncerr = nf90_inquire_dimension(dset%ncid, ndim, name=dset%dimensions(ndim)%name, &
                                      len=dset%dimensions(ndim)%len)
       if (ndim == nunlimdim) then
          dset%dimensions(ndim)%isunlimited = .true.
       else
          dset%dimensions(ndim)%isunlimited = .false.
       endif
       call nccheck(ncerr)
    enddo
    do nvar=1,dset%nvars
       dset%variables(nvar)%varid = nvar
       ncerr = nf90_inquire_variable(dset%ncid, nvar,&
                                     name=dset%variables(nvar)%name,&
                                     xtype=dset%variables(nvar)%dtype,&
                                     ndims=dset%variables(nvar)%ndims)
       call nccheck(ncerr)
       allocate(dset%variables(nvar)%dimids(dset%variables(nvar)%ndims))
       allocate(dset%variables(nvar)%dimlens(dset%variables(nvar)%ndims))

       ncerr = nf90_inquire_variable(dset%ncid, nvar,&
                                     dimids=dset%variables(nvar)%dimids,&
                                     deflate_level=dset%variables(nvar)%deflate_level,&
                                     shuffle=dset%variables(nvar)%shuffle)
       call nccheck(ncerr)
       do ndim=1,dset%variables(nvar)%ndims
          do n=1,dset%ndims
            if (dset%variables(nvar)%dimids(ndim) == dset%dimensions(n)%dimid) then
               exit
            endif
          enddo
          dset%variables(nvar)%dimlens(ndim) = dset%dimensions(n)%len
       enddo
    enddo
  end subroutine create_dataset
 
  subroutine destroy_dataset(dset)
    ! deallocate members of dataset object
    type(Dataset), intent(inout) :: dset
    integer ncerr, nvar
    ncerr = nf90_close(ncid=dset%ncid)
    call nccheck(ncerr)
    do nvar=1,dset%nvars
       deallocate(dset%variables(nvar)%dimids)
       deallocate(dset%variables(nvar)%dimlens)
    enddo
    deallocate(dset%variables,dset%dimensions)
  end subroutine destroy_dataset

  integer function nvar_(dset,varname)
    ! private function to get variable index given name
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    logical foundit
    integer nvar
    foundit = .false.
    do nvar=1,dset%nvars
       if (trim(dset%variables(nvar)%name) == trim(varname)) then 
          foundit = .true.
          exit
       endif
    enddo
    if (.not. foundit) then
       print *,'no variable named ',varname
       stop "stopped"
    endif
    nvar_ = nvar
  end function nvar_

  subroutine read_vardata_1d_r4(dset, varname, values)
    real(4), allocatable, dimension(:), intent(inout) :: values
    include "read_vardata_code_1d.f90"
  end subroutine read_vardata_1d_r4

  subroutine read_vardata_2d_r4(dset, varname, values)
    real(4), allocatable, dimension(:,:), intent(inout) :: values
    include "read_vardata_code_2d.f90"
  end subroutine read_vardata_2d_r4

  subroutine read_vardata_3d_r4(dset, varname, values)
    real(4), allocatable, dimension(:,:,:), intent(inout) :: values
    include "read_vardata_code_3d.f90"
  end subroutine read_vardata_3d_r4

  subroutine read_vardata_4d_r4(dset, varname, values)
    real(4), allocatable, dimension(:,:,:,:), intent(inout) :: values
    include "read_vardata_code_4d.f90"
  end subroutine read_vardata_4d_r4

  subroutine read_vardata_1d_r8(dset, varname, values)
    real(8), allocatable, dimension(:), intent(inout) :: values
    include "read_vardata_code_1d.f90"
  end subroutine read_vardata_1d_r8

  subroutine read_vardata_2d_r8(dset, varname, values)
    real(8), allocatable, dimension(:,:), intent(inout) :: values
    include "read_vardata_code_2d.f90"
  end subroutine read_vardata_2d_r8

  subroutine read_vardata_3d_r8(dset, varname, values)
    real(8), allocatable, dimension(:,:,:), intent(inout) :: values
    include "read_vardata_code_3d.f90"
  end subroutine read_vardata_3d_r8

  subroutine read_vardata_4d_r8(dset, varname, values)
    real(8), allocatable, dimension(:,:,:,:), intent(inout) :: values
    include "read_vardata_code_4d.f90"
  end subroutine read_vardata_4d_r8

  subroutine read_vardata_1d_int(dset, varname, values)
    integer, allocatable, dimension(:), intent(inout) :: values
    include "read_vardata_code_1d.f90"
  end subroutine read_vardata_1d_int

  subroutine read_vardata_2d_int(dset, varname, values)
    integer, allocatable, dimension(:,:), intent(inout) :: values
    include "read_vardata_code_2d.f90"
  end subroutine read_vardata_2d_int

  subroutine read_vardata_3d_int(dset, varname, values)
    integer, allocatable, dimension(:,:,:), intent(inout) :: values
    include "read_vardata_code_3d.f90"
  end subroutine read_vardata_3d_int

  subroutine read_vardata_4d_int(dset, varname, values)
    integer, allocatable, dimension(:,:,:,:), intent(inout) :: values
    include "read_vardata_code_4d.f90"
  end subroutine read_vardata_4d_int

  subroutine read_attribute_int_scalar(dset, attname, values, varname)
    integer, intent(inout) :: values
    include "read_scalar_attribute_code.f90"
  end subroutine read_attribute_int_scalar

  subroutine read_attribute_r4_scalar(dset, attname, values, varname)
    real(4), intent(inout) :: values
    include "read_scalar_attribute_code.f90"
  end subroutine read_attribute_r4_scalar

  subroutine read_attribute_r8_scalar(dset, attname, values, varname)
    real(8), intent(inout) :: values
    include "read_scalar_attribute_code.f90"
  end subroutine read_attribute_r8_scalar

  subroutine read_attribute_r4_1d(dset, attname, values, varname)
    real(4), intent(inout), allocatable, dimension(:) :: values
    include "read_attribute_code.f90"
  end subroutine read_attribute_r4_1d

  subroutine read_attribute_r8_1d(dset, attname, values, varname)
    real(8), intent(inout), allocatable, dimension(:) :: values
    include "read_attribute_code.f90"
  end subroutine read_attribute_r8_1d

  subroutine read_attribute_int_1d(dset, attname, values, varname)
    integer, intent(inout), allocatable, dimension(:) :: values
    include "read_attribute_code.f90"
  end subroutine read_attribute_int_1d

end module module_read_netcdf

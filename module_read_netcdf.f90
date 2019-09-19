module module_read_netcdf
  use netcdf

  implicit none
  private

  type Variable
     integer varid
     integer ndims
     integer dtype
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
      read_vardata_4d_r4, &
      read_vardata_1d_r8, read_vardata_2d_r8, read_vardata_3d_r8,&
      read_vardata_4d_r8
  end interface

  public :: create_dataset, destroy_dataset, Dataset, Variable, Dimension
  public :: read_vardata, get_vardim

  contains

  subroutine nccheck(status)
    implicit none
    integer, intent (in) :: status

    if (status /= nf90_noerr) then
      write(0,*) status, trim(nf90_strerror(status))
      stop "stopped"
    end if
  end subroutine nccheck

  integer function get_vardim(dset, varname)
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
    
  subroutine create_dataset(filename, dset)
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

! function nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids, nAtts, &
!    contiguous, chunksizes, deflate_level, shuffle, fletcher32, endianness)
! integer, intent(in) :: ncid, varid
! character (len = *), optional, intent(out) :: name
! integer, optional, intent(out) :: xtype, ndims
! integer, dimension(:), optional, intent(out) :: dimids
! integer, optional, intent(out) :: natts
! logical, optional, intent(out) :: contiguous
! integer, optional, dimension(:), intent(out) :: chunksizes
! integer, optional, intent(out) :: deflate_level
! logical, optional, intent(out) :: shuffle, fletcher32
! integer, optional, intent(out) :: endianness
! integer :: nf90_inquire_variable

       ncerr = nf90_inquire_variable(dset%ncid, nvar,&
                                     dimids=dset%variables(nvar)%dimids)
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
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    real(4), allocatable, dimension(:), intent(inout) :: values
    integer ncerr, nvar, n1, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 1) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    allocate(values(n1))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
  end subroutine read_vardata_1d_r4

  subroutine read_vardata_2d_r4(dset, varname, values)
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    real(4), allocatable, dimension(:,:), intent(inout) :: values
    integer ncerr, nvar, n1,n2, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 2) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    allocate(values(n1,n2))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
  end subroutine read_vardata_2d_r4

  subroutine read_vardata_3d_r4(dset, varname, values)
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    real(4), allocatable, dimension(:,:,:), intent(inout) :: values
    integer ncerr, nvar, n1,n2,n3, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 3) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    n3 = dset%variables(nvar)%dimlens(3)
    allocate(values(n1,n2,n3))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
  end subroutine read_vardata_3d_r4

  subroutine read_vardata_4d_r4(dset, varname, values)
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    real(4), allocatable, dimension(:,:,:,:), intent(inout) :: values
    integer ncerr, nvar, n1,n2,n3,n4, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 4) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    n3 = dset%variables(nvar)%dimlens(3)
    n4 = dset%variables(nvar)%dimlens(4)
    allocate(values(n1,n2,n3,n4))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
  end subroutine read_vardata_4d_r4

  subroutine read_vardata_1d_r8(dset, varname, values)
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    real(8), allocatable, dimension(:), intent(inout) :: values
    integer ncerr, nvar, n1, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 1) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    allocate(values(n1))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
  end subroutine read_vardata_1d_r8

  subroutine read_vardata_2d_r8(dset, varname, values)
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    real(8), allocatable, dimension(:,:), intent(inout) :: values
    integer ncerr, nvar, n1,n2, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 2) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    allocate(values(n1,n2))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
  end subroutine read_vardata_2d_r8

  subroutine read_vardata_3d_r8(dset, varname, values)
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    real(8), allocatable, dimension(:,:,:), intent(inout) :: values
    integer ncerr, nvar, n1,n2,n3, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 3) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    n3 = dset%variables(nvar)%dimlens(3)
    allocate(values(n1,n2,n3))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
  end subroutine read_vardata_3d_r8

  subroutine read_vardata_4d_r8(dset, varname, values)
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    real(8), allocatable, dimension(:,:,:,:), intent(inout) :: values
    integer ncerr, nvar, n1,n2,n3,n4, ndim
    nvar = nvar_(dset,varname)
    if (dset%variables(nvar)%ndims /= 4) then
       print *,'rank of data array does not match number of variable dims'
       stop "stopped"
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    n3 = dset%variables(nvar)%dimlens(3)
    n4 = dset%variables(nvar)%dimlens(4)
    allocate(values(n1,n2,n3,n4))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
  end subroutine read_vardata_4d_r8

end module module_read_netcdf

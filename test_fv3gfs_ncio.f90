program test_fv3gfs_ncio
! ifort -traceback test_read_netcdf.f90 module_fv3gfs_ncio.o -L${NETCDF}/lib
! -lnetcdf -lnetcdff
  use module_fv3gfs_ncio
  character(len=500) filename
  type(Dataset) :: dset, dsetout
  real(4), allocatable, dimension(:) :: values_1d
  real(8), allocatable, dimension(:) :: values8_1d
  real(4), allocatable, dimension(:,:) :: values_2d
  real(4), allocatable, dimension(:,:,:) :: values_3d
  real(4), allocatable, dimension(:,:,:,:) :: values_4d
  integer ndim,nvar,ndims,ival
  filename='test_data/dynf000.nc'
  call open_dataset(filename, dset)
  print *,'ncid=',dset%ncid
  print *,'nvars=',dset%nvars
  print *,'ndims=',dset%ndims
  print *,'natts=',dset%natts
  do ndim=1,dset%ndims
  print *,'dim',ndim,trim(dset%dimensions(ndim)%name),dset%dimensions(ndim)%len,dset%dimensions(ndim)%isunlimited
  enddo
  do nvar=1,dset%nvars
  print *,'var',nvar,trim(dset%variables(nvar)%name),dset%variables(nvar)%ndims,dset%variables(nvar)%dimlens
  enddo
  ndims = get_vardim(dset,'vgrd') 
  print *,'vgrd has ',ndims,' dims'
  call read_vardata(dset, 'vgrd', values_4d)
  print *,'min/max vgrd (4d)'
  print *,minval(values_4d),maxval(values_4d)
  print *,'min/max hgtsfc (3d)'
  call read_vardata(dset, 'hgtsfc', values_3d)
  print *,minval(values_3d),maxval(values_3d)
  print *,'min/max pfull (1d_r8)'
  call read_vardata(dset, 'pfull', values8_1d)
  print *,minval(values8_1d),maxval(values8_1d)
  call read_attribute(dset,'max_abs_compression_error',r4val,'pressfc')
  print *,'max_abs_compression_error for pressfc = ',r4val
  call read_attribute(dset,'ncnsto',ival)
  print *,'ncnsto =',ival
  call read_attribute(dset,'ak',values_1d)
  print *,'ak =',values_1d
! create a copy of the dataset
  filename='test_data/dynf000_copy.nc'
  call create_dataset(dset, filename, dsetout, copy_vardata=.true.)
  call close_dataset(dset)
  call close_dataset(dsetout)
end program test_fv3gfs_ncio

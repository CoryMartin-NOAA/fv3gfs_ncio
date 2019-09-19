program test_read_netcdf
  use module_read_netcdf
  character(len=500) filename
  type(Dataset) :: dset
  real(4), allocatable, dimension(:) :: values_1d
  real(8), allocatable, dimension(:) :: values8_1d
  real(4), allocatable, dimension(:,:) :: values_2d
  real(4), allocatable, dimension(:,:,:) :: values_3d
  real(4), allocatable, dimension(:,:,:,:) :: values_4d
  integer ndim,nvar,ndims
  filename='test_data/dynf000.nc'
  call create_dataset(filename, dset)
  print *,'ncid=',dset%ncid
  print *,'nvars=',dset%nvars
  print *,'ndims=',dset%ndims
  print *,'ngatts=',dset%ngatts
  do ndim=1,dset%ndims
  print *,'dim',ndim,trim(dset%dimensions(ndim)%name),dset%dimensions(ndim)%len,dset%dimensions(ndim)%isunlimited
  enddo
  do nvar=1,dset%nvars
  print *,'var',nvar,trim(dset%variables(nvar)%name),dset%variables(nvar)%ndims,dset%variables(nvar)%dimlens
  enddo
  ndims = get_vardim(dset,'vgrd') 
  !if (ndims == 1) then
  !   call read_vardata_1d(dset, 'vgrd', values_1d)
  !else if (ndims == 2) then
  !   call read_vardata_2d(dset, 'vgrd', values_2d)
  !else if (ndims == 3) then
  !   call read_vardata_3d(dset, 'vgrd', values_3d)
  !else if (ndims == 4) then
  !   call read_vardata_4d(dset, 'vgrd', values_4d)
  !   print *,minval(values_4d),maxval(values_4d)
  !endif
  call read_vardata(dset, 'vgrd', values_4d)
  print *,minval(values_4d),maxval(values_4d)
  call read_vardata(dset, 'pfull', values8_1d)
  print *,minval(values8_1d),maxval(values8_1d)
  call destroy_dataset(dset)
end program test_read_netcdf

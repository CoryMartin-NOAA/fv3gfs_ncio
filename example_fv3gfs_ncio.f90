program example_fv3gfs_ncio
! ifort -traceback example_fv3gfs_ncio.f90 module_fv3gfs_ncio.o -L${NETCDF}/lib
! -lnetcdf -lnetcdff
  use module_fv3gfs_ncio
  character(len=500) filename
  type(Dataset) :: dsetin, dset
  real(8), allocatable, dimension(:) :: values8_1d
  integer ndim,nvar
  ! template file created by running ncdump -cs on full file, then running ncgen
  ! only coordinate variables have data
  dsetin = open_dataset('test_data/dynf000_template.nc')
  ! create a copy of the template file
  dset = create_dataset('dynf000x.nc',dsetin)
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
  ! read and print a coordinate variable.
  call read_vardata(dset, 'grid_yt', values8_1d)
  print *,'grid_yt='
  print *,values8_1d
  call close_dataset(dset)
  call close_dataset(dsetin)
  stop
end program example_fv3gfs_ncio

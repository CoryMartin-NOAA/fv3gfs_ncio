# fv3gfs_ncio
module for reading/writing FV3 netcdf lat/lon data


* open a Dataset
```fortran
use module_fv3gfs_ncio
type(Dataset) :: ds
ds = open_dataset('gfs.t00z.atmf240.nc')
```
* read an attribute
```fortran
real(4), allocatable, dimension(:) :: ak,bk
character(len=32) charatt
! ak,bk are allocated and filled.
call read_attribute(ds, 'ak', ak)
call read_attribute(ds, 'bk', bk)
! read character variable attribute
call read_attribute(ds, 'long_name', charatt, 'vgrd')
```
* read a variable
```fortran
real(4), allocatable, dimension(:) :: lats,lons
real(4), allocatable, dimension(:,:,:) :: psfc
! arrays must be of correct rank (but not necessarily
! the same type). They are allocated and filled.
! The entire variable is read at once.
call read_vardata(dset,'latitudes',lats)
call read_vardata(dset,'latitudes',lons)
call read_vardata(dset,'pressfc',psfc)
```

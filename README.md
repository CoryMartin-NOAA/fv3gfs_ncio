# fv3gfs_ncio
module for reading/writing FV3 netcdf lat/lon data.


* open a Dataset.
```fortran
use module_fv3gfs_ncio
type(Dataset) :: ds
ds = open_dataset('gfs.t00z.atmf240.nc')
```
* read an attribute.
```fortran
real(4), allocatable, dimension(:) :: ak,bk
character(len=32) charatt
! ak,bk are allocated and filled.
call read_attribute(ds, 'ak', ak)
call read_attribute(ds, 'bk', bk)
! read character variable attribute
call read_attribute(ds, 'long_name', charatt, 'vgrd')
```
* read a variable.
```fortran
real(4), allocatable, dimension(:) :: lats,lons
real(4), allocatable, dimension(:,:,:) :: psfc
! arrays must be of correct rank (but not necessarily
! the same type). They are allocated and filled.
! The entire variable is read at once.
call read_vardata(ds,'latitudes',lats)
call read_vardata(ds,'latitudes',lons)
call read_vardata(ds,'pressfc',psfc)
```
* create a new dataset from a template dataset.
```fortran
type(Dataset) :: dso
! copy_vardata is optional, default .false. means just copy
! variables, dimensions and attributes and don't copy variable data.
dso = create_dataset('gfs.t00z.atmf240_copy.nc', ds, copy_vardata=.true.)
```
* write a variable.
```fortran
real(8), allocatable, dimension(:) :: times
call read_vardata(dso, 'time', times)
! times is now allocated and filled with values from template dataset.
! now overwrite with new values and write back.
times = times + 6 ! add six hours.
call write_vardata(dso, 'time', times)
```
* write an attribute.
```fortran
charatt = 'hours since 2016-01-04 06:00:00'
call write_attribute(dso, 'units', charatt, 'time')
```
* access Variable and Dimension derived data types.
```fortran
type(Variable) :: var
type(Dimension) :: dim
! see module_fv3gfs_ncio.f90 for type definitions.
! type members can be used to the call netcdf-fortran90 interface
! directly.
var = get_var(ds, 'ugrd')
dim = get_dim(ds, 'time')
```
* close a dataset.
```fortran
call close_dataset(ds)
call close_dataset(dso)

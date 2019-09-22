# fv3gfs_ncio
module for reading/writing FV3 netcdf lat/lon data


* open a Dataset
```fortran
use module_fv3gfsio
type(Dataset) :: ds
ds = open_dataset('gfs.t00z.atmf240.nc')
```

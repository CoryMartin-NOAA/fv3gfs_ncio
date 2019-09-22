    type(Dataset), intent(inout) :: dset
    character(len=*), intent(in) :: varname
    integer ncerr, nvar
    nvar = get_nvar(dset,varname)
    ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
    ! reset unlim dim size for all variables
    if (dset%variables(nvar)%hasunlim) call set_varunlimdimlens_(dset)

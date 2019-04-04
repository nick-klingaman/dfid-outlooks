PROGRAM fix_netcdf_variable_monotonic
  USE netcdf
  USE nicks_subroutines, ONLY : error_handler,rrapid_fill
  IMPLICIT NONE

  INTEGER :: file_id, varid, dimid, n_vals, status
  REAL,dimension(:),allocatable :: newvar,var
  REAL :: start_val,interval
  CHARACTER(LEN=20) :: var_name,temp_name
  CHARACTER(LEN=200) :: infile
  NAMELIST /ctl/ infile, var_name, start_val, interval, n_vals
  LOGICAL :: defdim=.FALSE.

  READ(*,ctl)
  WRITE(6,*) infile
  status = NF90_OPEN(infile,NF90_WRITE,file_id)
  IF (status.NE.0) THEN
     CALL ERROR_HANDLER(status,'opening file '//infile)
     STOP
  ENDIF
  status = NF90_INQ_DIMID(file_id,TRIM(var_name),dimid)
  IF (status.NE.0) THEN
     WRITE(6,*) 'No dimension to match requested variable ',TRIM(var_name)
     WRITE(6,*) 'Dimension will be created.'
     defdim=.TRUE.
  ELSE
     status = NF90_INQUIRE_DIMENSION(file_id,dimid,temp_name,n_vals)
     IF (status.NE.0) CALL ERROR_HANDLER(status,'retrieving length of '//TRIM(var_name))        
  ENDIF
  allocate(newvar(n_vals))
  allocate(var(n_vals))

  WRITE(6,*) 'Inquiring about variable '//TRIM(var_name)//' in file '//infile

  status = NF90_INQ_VARID(file_id,TRIM(var_name),varid)  
  IF (status .NE. 0) THEN
     status=NF90_REDEF(file_id)
     IF(status.NE.0) THEN
        CALL ERROR_HANDLER(status,'opening file '//infile//' for re-definition')
        STOP
     ENDIF
!     IF (defdim) status=NF90_DEF_DIM(file_id,TRIM(var_name),n_vals,dimid)
     WRITE(6,*) 'Defining new variable '//TRIM(var_name)//' in the input file.'
     status=NF90_DEF_VAR(file_id,TRIM(var_name),NF90_DOUBLE,dimid,varid)
     IF(status.NE.0) THEN 
        CALL ERROR_HANDLER(status,'defining new variable '//TRIM(var_name))
        WRITE(6,*) 'Failed to find or create the variable ',TRIM(var_name),&
             ' in the file ',infile
        STOP
     ENDIF
     status=NF90_ENDDEF(file_id)
  ENDIF

  CALL rrapid_fill(newvar,interval,start_val,zero=.true.)
  
  status = NF90_PUT_VAR(file_id,varid,newvar)
  status = NF90_CLOSE(file_id)

END PROGRAM fix_netcdf_variable_monotonic

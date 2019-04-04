
!*****************************************************
! MODULE:     nicks_subroutines (Nick's Subroutines)
! VERSION:    0.07
! AUTHOR:     Nicholas Klingaman
! LAST MOD:   28/6/07
! 
! HISTORY:
!             0.07 (28/6/07)  Added subroutines find_netcdf_dimension and find_netcdf_variable
!             0.06 (11/1/06)  Added subroutine julian_distance
!             0.05 (10/1/06)  Added subroutines total_julian and increment_julian
!             0.04 (30/11/05) Added subroutines rrapid_fill and irapid_fill
!             0.03 (30/11/05) Added subroutine increment_date
!             0.02 (30/11/05) Added subroutine error_handler
!             0.01 (30/11/05) Added subroutine total_days
!
! CONTAINS:
!             Subroutine rrapid_fill: Fill a real array with regularly spaced values.
!
!             Subroutine irapid_fill: Same as rrapid_fill but for integers.
!
!             Subroutine increment_date: Move from the given day to the next day.
!                                        Able to wrap from one month or year to the next.
!          
!             Subroutine increment_julian: Same as increment_date but for Julian dates.
!
!             Subroutine error_handler: Spit out error message from netCDF routines.
! 
!             Subroutine total_days: Calculate total number of days between
!                                    beginning and ending month and date.
!
!             Subroutine total_julian: Same as total_days, but for Julian dates
!
!             Subroutine julian_distance: Calculate the total number of days
!                                         between two Julian dates.  Able to move through
!                                         years.  Note that this is different from total_julian,
!                                         which calculates the total number of days including
!                                         both endpoints.
!
!******************************************************

MODULE nicks_subroutines
  USE netcdf
  IMPLICIT NONE
  CONTAINS

    SUBROUTINE rrapid_fill(x, interval, start_value, zero)
      REAL, dimension(:), intent(inout) :: x
      REAL, intent(in) :: interval
      REAL, intent(in), optional :: start_value
      LOGICAL, intent(in), optional :: zero
      INTEGER :: i
      
      DO i = 1,size(x)
         IF (i.eq.1 .and. PRESENT(zero) .and. zero) THEN
            x(i) = 0.
         ELSE IF(PRESENT(zero) .and. zero) THEN 
            x(i) = interval*(i-1)
         ELSE 
            x(i) = interval*i
         ENDIF
      END DO

      IF(PRESENT(start_value)) x = x+start_value
         
      RETURN
    END SUBROUTINE rrapid_fill

    SUBROUTINE irapid_fill(x,interval,start_value,zero)
      INTEGER, dimension(:), intent(inout) :: x
      INTEGER, intent(in) :: interval
      INTEGER, intent(in), optional :: start_value
      LOGICAL, intent(in), optional :: zero
      INTEGER :: i
      
      DO i = 1,size(x)
         IF (i.eq.1 .and. PRESENT(zero) .and. zero) THEN
            x(i) = 0.
         ELSE IF(PRESENT(zero) .and. zero) THEN 
            x(i) = interval*(i-1)
         ELSE 
            x(i) = interval*i
         ENDIF
      END DO

      IF(PRESENT(start_value)) x = x+start_value
         
      RETURN
    END SUBROUTINE irapid_fill
      
    SUBROUTINE total_days(start_date,end_date,ndays)
      INTEGER, dimension(:), intent(in) :: start_date,end_date
      INTEGER, intent(out) :: ndays
      INTEGER, dimension(12), parameter :: month_end=(/31,28,31,30,31,30,31,31,30,31,30,31/)
      INTEGER :: i
      
      ! Determine the total number of days required

      ! First, determine whether we need one month only or more than one
      IF (start_date(1) .eq. end_date(1)) THEN
         ! If we need only month, simply subtract end date from start date
         ndays = end_date(2) - start_date(2) + 1
      ELSE
         ! Otherwise, we need to find the number of days to the end of the
         ! current month
         ndays = month_end(start_date(1))-start_date(2)+1
         ! Now loop through the middle months
         DO i=start_date(1)+1,end_date(1)-1
            ndays = ndays+month_end(i)
         END DO
         ! Now find the number of days from the last day to the first of the month
         ndays = ndays + end_date(2)
      ENDIF
      
      RETURN
    END SUBROUTINE total_days

    SUBROUTINE total_julian(start_date,end_date,ndays)
      
      INTEGER, dimension(:), intent(in) :: start_date, end_date
      INTEGER, intent(out) :: ndays
      INTEGER :: i

      ! For start_date and end_date, element (1) is the year;
      ! element (2) is the julian date

      IF (start_date(1) .eq. end_date(1)) THEN
         ndays = end_date(2) - start_date(2) + 1
      ELSE IF (start_date(1) .lt. end_date(1)) THEN
         IF(MOD(start_date(1),4) .eq. 0) THEN
            ndays = end_date(2)+366-start_date(2)+1
         ELSE
            ndays = end_date(2)+365-start_date(2)+1
         END IF
         DO i=start_date(1)+1, end_date(1)
            IF(MOD(i,4) .eq. 0) THEN 
               ndays = ndays+366
            ELSE
               ndays = ndays+365
            END IF
         END DO
      ELSE
         WRITE(6,*) 'Total Julian : Start date is after end date - cannot compute total days.'
      ENDIF

      RETURN
    END SUBROUTINE total_julian

    SUBROUTINE julian_distance(first_date,last_date,distance)
      
      INTEGER, dimension(:), intent(in) :: first_date, last_date
      INTEGER, intent(out) :: distance
      INTEGER :: i

      ! Element 1 of first_date and last_date is the year
      ! Element 2 of first_date and last_date is the Julian date

      distance = 0
      IF (first_date(1) .eq. last_date(1)) THEN
         distance = last_date(2) - first_date(2)-1
      ELSE IF (first_date(1) .lt. last_date(1)) THEN
         IF (MOD(first_date(1),4) .eq. 0) THEN
            distance = 366-first_date(2)
         ELSE
            distance = 365-first_date(2)
         END IF
         DO i=first_date(1)+1,last_date(1)-1
            IF(MOD(i,4) .eq. 0) THEN
               distance = distance+366
            ELSE
               distance = distance+365
            END IF
         END DO
         distance = distance+last_date(2)-1
      ELSE
         WRITE(6,*) 'Julian Distance : Start date must be before end date'
      END IF

      RETURN
    END SUBROUTINE julian_distance

    SUBROUTINE increment_date(current_date)
      
      INTEGER, dimension(:), intent(inout) :: current_date
      INTEGER, dimension(12), parameter :: month_end=(/31,28,31,30,31,30,31,31,30,31,30,31/)

      current_date(2) = current_date(2) + 1
      IF (current_date(2) .gt. month_end(current_date(1))) THEN
         current_date(2) = 1
         current_date(1) = current_date(1) + 1
         IF (current_date(1) .gt. 12) THEN
            current_date(1) = 1
            RETURN
         ELSE ; RETURN
         END IF
      ELSE ; RETURN
      END IF

      RETURN
    END SUBROUTINE increment_date

    SUBROUTINE increment_julian(current_date)
      
      INTEGER, dimension(:), intent(inout) :: current_date
      INTEGER :: year_end = 365

      current_date(2) = current_date(2) + 1
      IF (MOD(current_date(1),4) .eq. 0) year_end = 366
      IF (current_date(2) .gt. year_end) THEN
         current_date(2) = 1
         current_date(1) = current_date(1) + 1
      END IF
      
      RETURN
    END SUBROUTINE increment_julian

    SUBROUTINE error_handler(error, routine, lc1, lc2, lc3)
         
      INTEGER, INTENT(in) :: error
      CHARACTER(*), INTENT(in) :: routine
      INTEGER, INTENT(in), OPTIONAL :: lc1, lc2, lc3
      
      IF (error /= nf90_noerr) THEN
         IF (PRESENT(lc1)) THEN
            IF (PRESENT(lc2)) THEN
               IF (PRESENT(lc3)) THEN
                  WRITE (6,*) 'Error code from ', routine, ' is ', error, &
                       'at location codes: ', lc1, lc2, lc3
               ELSE
                  WRITE (6,*) 'Error code from ', routine, ' is ', error, &
                       'at location codes: ', lc1, lc2
               END IF
            ELSE
               WRITE (6,*) 'Error code from ', routine, ' is ', error, &
                    'at location code: ', lc1
            END IF
         ELSE
            WRITE (6,*) 'Error code from ', routine, ' is ', error
         END IF
         WRITE (6,'("NFS_STRERROR for this code is:",/,A)') nf90_strerror(error)
      END IF
      RETURN
    END SUBROUTINE error_handler
    
    SUBROUTINE find_netcdf_variable(ncid,name,your_name,varid,right_name,output)
      ! Locates a variable in a netCDF file, given an array of possible names
      ! If no variable is found, a varid code of -99 is returned; 
      ! otherwise the variable id and name are returned.
      
      INTEGER, intent(in) :: ncid
      CHARACTER(LEN=*),intent(in) :: name,your_name
      INTEGER, intent(out) :: varid
      CHARACTER(LEN=*),intent(out) :: right_name
      INTEGER :: error
      LOGICAL, intent(in) :: output

      error = NF90_INQ_VARID(ncid=ncid,name=TRIM(name),varid=varid)
      IF (error .eq. NF90_NOERR) THEN
         right_name = TRIM(name)
      ELSE
         varid = -99
         right_name = 'not_found'
      ENDIF
      IF (output) THEN
         IF (varid .ne. -99) THEN
            WRITE(6,*) 'Found the ',TRIM(your_name),' variable, which is called ',TRIM(right_name),'.'
            WRITE(6,*) 'It has variable ID',varid,'.'
         ELSE
            WRITE(6,*) 'Could not find the ',TRIM(your_name),' variable.  We tried these names: ',name,'.'
            RETURN
         ENDIF
      ENDIF
      RETURN
    END SUBROUTINE find_netcdf_variable

    SUBROUTINE find_netcdf_dimension(ncid,name,your_name,dimid,right_name,length,output)
      ! Locates a dimension in a netCDF file, given an array of possible names
      ! If no dimension is found, a dimid code of -99 is returned;
      ! otherwise the dimension id and the name are returned
    
      INTEGER, intent(in) :: ncid
      CHARACTER(LEN=*),intent(in) :: name, your_name
      INTEGER, intent(out) :: dimid,length
      CHARACTER(LEN=*),intent(out) :: right_name
      INTEGER :: error
      LOGICAL,intent(in) :: output
      
      error = NF90_INQ_DIMID(ncid=ncid,name=TRIM(name),dimid=dimid)
      IF (error .eq. 0) THEN
         right_name = TRIM(name)
      ELSE
         dimid = -99
         right_name = 'not_found'
      ENDIF
      IF (output) THEN
         IF (dimid .ne. -99) THEN
            WRITE(6,*) 'Found the ',TRIM(your_name),' dimension, which is called ',TRIM(right_name),'.'
            error=NF90_INQUIRE_DIMENSION(ncid=ncid,dimid=dimid,name=right_name,len=length)
            WRITE(6,*) 'The dimension has ID',dimid,'and',length,'elements.'
         ELSE
            WRITE(6,*) 'Could not find the ',TRIM(your_name),' dimension.  We tried these names: ',name
            RETURN
         ENDIF
      ENDIF
      RETURN
    END SUBROUTINE find_netcdf_dimension
    
    SUBROUTINE create_netcdf_dimvar_float(ncid,name,dimid,varid,values,error,record)
      ! Creates a dimension in an open netCDF file and an accompanying one-dimensional
      ! variable with the data specified in the values array.  Assumes that the netCDF file
      ! is in DEFINE mode and will return the file to DEFINE mode when finished.
      
      INTEGER,intent(in) :: ncid
      INTEGER,intent(out) :: dimid, varid, error
      CHARACTER(LEN=*),intent(in) :: name
      REAL, dimension(:), intent(in) :: values
      LOGICAL,optional :: record
            
      IF (PRESENT(record) .AND. record) THEN
         error=NF90_DEF_DIM(ncid=ncid,name=name,len=NF90_UNLIMITED,dimid=dimid)
      ELSE
         error=NF90_DEF_DIM(ncid=ncid,name=name,len=SIZE(values),dimid=dimid)
      ENDIF
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_DEF_DIM for dimension with name '//name)
         RETURN
      ENDIF
      
      CALL create_netcdf_1dvar_float(ncid=ncid,name=name,dimid=dimid,varid=varid,&
           values=values,error=error)

      RETURN
    END SUBROUTINE create_netcdf_dimvar_float

    SUBROUTINE create_netcdf_dimvar_int(ncid,name,dimid,varid,values,error,record)
      ! Creates a dimension in an open netCDF file and an accompanying one-dimensional
      ! variable with the data specified in the values array.  Assumes that the netCDF file
      ! is in DEFINE mode and will return the file to DEFINE mode when finished.
      
      INTEGER,intent(in) :: ncid
      INTEGER,intent(out) :: dimid, varid, error
      CHARACTER(LEN=*),intent(in) :: name
      INTEGER, dimension(:), intent(in) :: values
      LOGICAL,optional :: record

      IF (PRESENT(record) .AND. record) THEN
         error=NF90_DEF_DIM(ncid=ncid,name=name,len=NF90_UNLIMITED,dimid=dimid)
      ELSE
         error=NF90_DEF_DIM(ncid=ncid,name=name,len=SIZE(values),dimid=dimid)
      ENDIF
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_DEF_DIM for dimension with name '//name)
         RETURN
      ENDIF
      
      CALL create_netcdf_1dvar_int(ncid=ncid,name=name,dimid=dimid,varid=varid,&
           values=values,error=error)

      RETURN
    END SUBROUTINE create_netcdf_dimvar_int

    SUBROUTINE create_netcdf_1dvar_float(ncid,name,dimid,varid,values,error)
      ! Create a one-dimensional netCDF variable with specified name, type, and values
      ! Assumes that the netCDF file is in DEFINE mode and will return the file to
      ! DEFINE mode when finished.
      
      INTEGER,intent(in) :: ncid,dimid
      INTEGER,intent(out) :: varid,error
      CHARACTER(LEN=*) :: name
      REAL,intent(in) :: values(:)
      
      error=NF90_DEF_VAR(ncid=ncid,name=name,xtype=NF90_FLOAT,dimids=(/dimid/),varid=varid)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_DEF_VAR for variable with name '//name)
         RETURN
      ENDIF
      error=NF90_ENDDEF(ncid=ncid)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_ENDDEF')
         RETURN
      ENDIF
      error=NF90_PUT_VAR(ncid=ncid,varid=varid,values=values)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_PUT_VAR for variable with name '//name)
         RETURN
      ENDIF
      error=NF90_REDEF(ncid=ncid)
      IF(error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_REDEF for variable with name '//name)
         RETURN
      ENDIF

      WRITE(6,*) 'Created variable ',TRIM(name),' with ID',varid,'and put values.'

      RETURN
    END SUBROUTINE create_netcdf_1dvar_float

    SUBROUTINE create_netcdf_1dvar_int(ncid,name,dimid,varid,values,error)
      ! Create a one-dimensional netCDF variable with specified name, type, and values
      ! Assumes that the netCDF file is in DEFINE mode and will return the file to
      ! DEFINE mode when finished.
      
      INTEGER,intent(in) :: ncid,dimid
      INTEGER,intent(out) :: varid,error
      CHARACTER(LEN=*),intent(in) :: name
      INTEGER,intent(in) :: values(:)
      
      error=NF90_DEF_VAR(ncid=ncid,name=name,xtype=NF90_SHORT,dimids=(/dimid/),varid=varid)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_DEF_VAR for variable with name '//name)
         RETURN
      ENDIF
      error=NF90_ENDDEF(ncid=ncid)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_ENDDEF')
         RETURN
      ENDIF
      error=NF90_PUT_VAR(ncid=ncid,varid=varid,values=values)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_PUT_VAR for variable with name '//name)
         RETURN
      ENDIF
      error=NF90_REDEF(ncid=ncid)
      IF(error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_REDEF for variable with name '//name)
         RETURN
      ENDIF

      WRITE(6,*) 'Created variable ',TRIM(name),' with ID',varid,'and put values.'

      RETURN
    END SUBROUTINE create_netcdf_1dvar_int

    SUBROUTINE create_netcdf_2dvar_float(ncid,name,dimids,varid,values,error)
      ! Create a two-dimensional netCDF variable with specified name, type, and values
      ! Assumes that the netCDF file is in DEFINE mode and will return the file to
      ! DEFINE mode when finished.
      
      INTEGER,intent(in) :: ncid,dimids(2)
      INTEGER,intent(out) :: varid,error
      CHARACTER(LEN=*),intent(in) :: name
      REAL,intent(in) :: values(:,:)
      
      error=NF90_DEF_VAR(ncid=ncid,name=name,xtype=NF90_FLOAT,dimids=dimids,varid=varid)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_DEF_VAR for variable with name '//name)
         RETURN
      ENDIF
      error=NF90_ENDDEF(ncid=ncid)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_ENDDEF')
         RETURN
      ENDIF
      error=NF90_PUT_VAR(ncid=ncid,varid=varid,values=values)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_PUT_VAR for variable with name '//name)
         RETURN
      ENDIF
      error=NF90_REDEF(ncid=ncid)
      IF(error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_REDEF for variable with name '//name)
         RETURN
      ENDIF

      WRITE(6,*) 'Created variable ',TRIM(name),' with ID',varid,'and put values.'

      RETURN
    END SUBROUTINE create_netcdf_2dvar_float

    SUBROUTINE create_netcdf_3dvar_float(ncid,name,dimids,varid,values,error)
      ! Create a three-dimensional netCDF variable with specified name, type, and values
      ! Assumes that the netCDF file is in DEFINE mode and will return the file to
      ! DEFINE mode when finished.
      
      INTEGER,intent(in) :: ncid,dimids(3)
      INTEGER,intent(out) :: varid,error
      CHARACTER(LEN=*),intent(in) :: name
      REAL,intent(in) :: values(:,:,:)
      
      error=NF90_DEF_VAR(ncid=ncid,name=name,xtype=NF90_FLOAT,dimids=dimids,varid=varid)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_DEF_VAR for variable with name '//name)
         RETURN
      ENDIF
      error=NF90_ENDDEF(ncid=ncid)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_ENDDEF')
         RETURN
      ENDIF
      error=NF90_PUT_VAR(ncid=ncid,varid=varid,values=values)
      IF (error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_PUT_VAR for variable with name '//name)
         RETURN
      ENDIF
      error=NF90_REDEF(ncid=ncid)
      IF(error .ne. NF90_NOERR) THEN
         CALL error_handler(error,'NF90_REDEF for variable with name '//name)
         RETURN
      ENDIF

      WRITE(6,*) 'Created variable ',TRIM(name),' with ID',varid,'and put values.'

      RETURN
    END SUBROUTINE create_netcdf_3dvar_float

  END MODULE nicks_subroutines

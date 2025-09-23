        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:12 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NC_OPEN_PAR_FORTRAN__genmod
          INTERFACE 
            FUNCTION NC_OPEN_PAR_FORTRAN(STRING,MODE,COMM,INFO,NCID)    &
     & BIND(C, NAME = 'nc_open_par_fortran')
              CHARACTER(LEN=1) :: STRING(*)
              INTEGER(KIND=4) ,VALUE :: MODE
              INTEGER(KIND=4) ,VALUE :: COMM
              INTEGER(KIND=4) ,VALUE :: INFO
              INTEGER(KIND=4) ,VALUE :: NCID
              INTEGER(KIND=4) :: NC_OPEN_PAR_FORTRAN
            END FUNCTION NC_OPEN_PAR_FORTRAN
          END INTERFACE 
        END MODULE NC_OPEN_PAR_FORTRAN__genmod

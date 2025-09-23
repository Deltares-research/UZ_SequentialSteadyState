        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:12 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NC_VAR_PAR_ACCESS__genmod
          INTERFACE 
            FUNCTION NC_VAR_PAR_ACCESS(NCID,VARID,ACCESS)               &
     & BIND(C, NAME = 'nc_var_par_access')
              INTEGER(KIND=4) ,VALUE :: NCID
              INTEGER(KIND=4) ,VALUE :: VARID
              INTEGER(KIND=4) ,VALUE :: ACCESS
              INTEGER(KIND=4) :: NC_VAR_PAR_ACCESS
            END FUNCTION NC_VAR_PAR_ACCESS
          END INTERFACE 
        END MODULE NC_VAR_PAR_ACCESS__genmod

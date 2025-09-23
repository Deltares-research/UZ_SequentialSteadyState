        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:17 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_GET_VAR1_DOUBLE__genmod
          INTERFACE 
            FUNCTION NF_GET_VAR1_DOUBLE(NCID,VARID,NDEX,DVAL) RESULT(   &
     &STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: NDEX(*)
              REAL(KIND=8), INTENT(OUT) :: DVAL
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_GET_VAR1_DOUBLE
          END INTERFACE 
        END MODULE NF_GET_VAR1_DOUBLE__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:17 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_GET_VAR1__genmod
          INTERFACE 
            FUNCTION NF_GET_VAR1(NCID,VARID,NDEX,VALUES) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: NDEX(*)
              CHARACTER(LEN=1) ,TARGET, INTENT(OUT) :: VALUES(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_GET_VAR1
          END INTERFACE 
        END MODULE NF_GET_VAR1__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:14 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_COPY_VAR__genmod
          INTERFACE 
            FUNCTION NF_COPY_VAR(NCID_IN,VARID,NCID_OUT) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID_IN
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: NCID_OUT
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_COPY_VAR
          END INTERFACE 
        END MODULE NF_COPY_VAR__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:14 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_INQ_VARID__genmod
          INTERFACE 
            FUNCTION NF_INQ_VARID(NCID,NAME,VARID) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              CHARACTER(*), INTENT(IN) :: NAME
              INTEGER(KIND=4), INTENT(OUT) :: VARID
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_INQ_VARID
          END INTERFACE 
        END MODULE NF_INQ_VARID__genmod

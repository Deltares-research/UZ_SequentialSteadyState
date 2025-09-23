        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:17 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_INQ_DIMLEN__genmod
          INTERFACE 
            FUNCTION NF_INQ_DIMLEN(NCID,DIMID,DLEN) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: DIMID
              INTEGER(KIND=4), INTENT(OUT) :: DLEN
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_INQ_DIMLEN
          END INTERFACE 
        END MODULE NF_INQ_DIMLEN__genmod

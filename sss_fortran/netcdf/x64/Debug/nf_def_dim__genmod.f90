        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:17 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_DEF_DIM__genmod
          INTERFACE 
            FUNCTION NF_DEF_DIM(NCID,NAME,DLEN,DIMID) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              CHARACTER(*), INTENT(IN) :: NAME
              INTEGER(KIND=4), INTENT(IN) :: DLEN
              INTEGER(KIND=4), INTENT(OUT) :: DIMID
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_DEF_DIM
          END INTERFACE 
        END MODULE NF_DEF_DIM__genmod

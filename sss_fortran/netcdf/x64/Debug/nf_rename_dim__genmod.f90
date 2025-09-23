        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:17 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_RENAME_DIM__genmod
          INTERFACE 
            FUNCTION NF_RENAME_DIM(NCID,DIMID,NAME) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: DIMID
              CHARACTER(*), INTENT(IN) :: NAME
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_RENAME_DIM
          END INTERFACE 
        END MODULE NF_RENAME_DIM__genmod

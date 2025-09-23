        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:14 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_RENAME_VAR__genmod
          INTERFACE 
            FUNCTION NF_RENAME_VAR(NCID,VARID,NAME) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              CHARACTER(*), INTENT(IN) :: NAME
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_RENAME_VAR
          END INTERFACE 
        END MODULE NF_RENAME_VAR__genmod

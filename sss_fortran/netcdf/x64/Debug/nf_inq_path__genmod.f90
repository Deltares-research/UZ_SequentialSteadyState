        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_INQ_PATH__genmod
          INTERFACE 
            FUNCTION NF_INQ_PATH(NCID,PATHLEN,PATH) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(INOUT) :: PATHLEN
              CHARACTER(*), INTENT(INOUT) :: PATH
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_INQ_PATH
          END INTERFACE 
        END MODULE NF_INQ_PATH__genmod

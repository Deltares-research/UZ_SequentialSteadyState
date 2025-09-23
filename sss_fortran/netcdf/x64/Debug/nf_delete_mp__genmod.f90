        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:14 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_DELETE_MP__genmod
          INTERFACE 
            FUNCTION NF_DELETE_MP(PATH,PE) RESULT(STATUS)
              CHARACTER(*), INTENT(IN) :: PATH
              INTEGER(KIND=4), INTENT(IN) :: PE
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_DELETE_MP
          END INTERFACE 
        END MODULE NF_DELETE_MP__genmod

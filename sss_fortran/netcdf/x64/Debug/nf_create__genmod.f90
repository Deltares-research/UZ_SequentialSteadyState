        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_CREATE__genmod
          INTERFACE 
            FUNCTION NF_CREATE(PATH,CMODE,NCID) RESULT(STATUS)
              CHARACTER(*), INTENT(IN) :: PATH
              INTEGER(KIND=4), INTENT(IN) :: CMODE
              INTEGER(KIND=4), INTENT(OUT) :: NCID
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_CREATE
          END INTERFACE 
        END MODULE NF_CREATE__genmod

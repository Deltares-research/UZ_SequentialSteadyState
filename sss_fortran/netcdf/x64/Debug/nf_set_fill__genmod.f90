        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_SET_FILL__genmod
          INTERFACE 
            FUNCTION NF_SET_FILL(NCID,FILLMODE,OLD_MODE) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: FILLMODE
              INTEGER(KIND=4), INTENT(OUT) :: OLD_MODE
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_SET_FILL
          END INTERFACE 
        END MODULE NF_SET_FILL__genmod

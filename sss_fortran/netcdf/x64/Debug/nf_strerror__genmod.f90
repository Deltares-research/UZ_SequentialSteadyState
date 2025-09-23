        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:17 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_STRERROR__genmod
          INTERFACE 
            FUNCTION NF_STRERROR(NCERR) RESULT(ERRMSG)
              INTEGER(KIND=4), INTENT(IN) :: NCERR
              CHARACTER(LEN=80) :: ERRMSG
            END FUNCTION NF_STRERROR
          END INTERFACE 
        END MODULE NF_STRERROR__genmod

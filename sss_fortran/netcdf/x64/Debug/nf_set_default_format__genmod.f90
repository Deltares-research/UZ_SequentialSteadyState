        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:14 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_SET_DEFAULT_FORMAT__genmod
          INTERFACE 
            FUNCTION NF_SET_DEFAULT_FORMAT(NEWFORM,OLD_FORMAT) RESULT(  &
     &STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NEWFORM
              INTEGER(KIND=4), INTENT(OUT) :: OLD_FORMAT
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_SET_DEFAULT_FORMAT
          END INTERFACE 
        END MODULE NF_SET_DEFAULT_FORMAT__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:16 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_PUT_VAR1_INT__genmod
          INTERFACE 
            FUNCTION NF_PUT_VAR1_INT(NCID,VARID,NDEX,IVAL) RESULT(STATUS&
     &)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: NDEX(*)
              INTEGER(KIND=4), INTENT(IN) :: IVAL
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_PUT_VAR1_INT
          END INTERFACE 
        END MODULE NF_PUT_VAR1_INT__genmod

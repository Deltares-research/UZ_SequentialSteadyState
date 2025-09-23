        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:17 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_PUT_VARA_INT2__genmod
          INTERFACE 
            FUNCTION NF_PUT_VARA_INT2(NCID,VARID,START,COUNTS,I2VALS)   &
     & RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: START(*)
              INTEGER(KIND=4), INTENT(IN) :: COUNTS(*)
              INTEGER(KIND=2), INTENT(IN) :: I2VALS(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_PUT_VARA_INT2
          END INTERFACE 
        END MODULE NF_PUT_VARA_INT2__genmod

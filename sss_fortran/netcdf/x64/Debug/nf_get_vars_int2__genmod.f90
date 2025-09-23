        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:12 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_GET_VARS_INT2__genmod
          INTERFACE 
            FUNCTION NF_GET_VARS_INT2(NCID,VARID,START,COUNTS,STRIDES,  &
     &I2VALS) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: START(*)
              INTEGER(KIND=4), INTENT(IN) :: COUNTS(*)
              INTEGER(KIND=4), INTENT(IN) :: STRIDES(*)
              INTEGER(KIND=2), INTENT(OUT) :: I2VALS(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_GET_VARS_INT2
          END INTERFACE 
        END MODULE NF_GET_VARS_INT2__genmod

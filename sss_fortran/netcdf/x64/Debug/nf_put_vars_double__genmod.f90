        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:12 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_PUT_VARS_DOUBLE__genmod
          INTERFACE 
            FUNCTION NF_PUT_VARS_DOUBLE(NCID,VARID,START,COUNTS,STRIDES,&
     &DVALS) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: START(*)
              INTEGER(KIND=4), INTENT(IN) :: COUNTS(*)
              INTEGER(KIND=4), INTENT(IN) :: STRIDES(*)
              REAL(KIND=8), INTENT(IN) :: DVALS(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_PUT_VARS_DOUBLE
          END INTERFACE 
        END MODULE NF_PUT_VARS_DOUBLE__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:12 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_PUT_VARS__genmod
          INTERFACE 
            FUNCTION NF_PUT_VARS(NCID,VARID,START,COUNTS,STRIDES,VALUES)&
     & RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: START(*)
              INTEGER(KIND=4), INTENT(IN) :: COUNTS(*)
              INTEGER(KIND=4), INTENT(IN) :: STRIDES(*)
              CHARACTER(LEN=1) ,TARGET, INTENT(IN) :: VALUES(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_PUT_VARS
          END INTERFACE 
        END MODULE NF_PUT_VARS__genmod

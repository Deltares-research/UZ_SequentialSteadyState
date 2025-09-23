        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:12 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_PUT_VARS_TEXT__genmod
          INTERFACE 
            FUNCTION NF_PUT_VARS_TEXT(NCID,VARID,START,COUNTS,STRIDES,  &
     &TEXT) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: START(*)
              INTEGER(KIND=4), INTENT(IN) :: COUNTS(*)
              INTEGER(KIND=4), INTENT(IN) :: STRIDES(*)
              CHARACTER(*), INTENT(IN) :: TEXT
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_PUT_VARS_TEXT
          END INTERFACE 
        END MODULE NF_PUT_VARS_TEXT__genmod

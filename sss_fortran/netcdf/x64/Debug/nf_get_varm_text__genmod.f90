        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_GET_VARM_TEXT__genmod
          INTERFACE 
            FUNCTION NF_GET_VARM_TEXT(NCID,VARID,START,COUNTS,STRIDES,  &
     &MAPS,TEXT) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: START(*)
              INTEGER(KIND=4), INTENT(IN) :: COUNTS(*)
              INTEGER(KIND=4), INTENT(IN) :: STRIDES(*)
              INTEGER(KIND=4), INTENT(IN) :: MAPS(*)
              CHARACTER(*), INTENT(OUT) :: TEXT
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_GET_VARM_TEXT
          END INTERFACE 
        END MODULE NF_GET_VARM_TEXT__genmod

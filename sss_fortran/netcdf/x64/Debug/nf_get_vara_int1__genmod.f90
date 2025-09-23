        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:17 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_GET_VARA_INT1__genmod
          INTERFACE 
            FUNCTION NF_GET_VARA_INT1(NCID,VARID,START,COUNTS,I1VALS)   &
     & RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: START(*)
              INTEGER(KIND=4), INTENT(IN) :: COUNTS(*)
              INTEGER(KIND=1), INTENT(OUT) :: I1VALS(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_GET_VARA_INT1
          END INTERFACE 
        END MODULE NF_GET_VARA_INT1__genmod

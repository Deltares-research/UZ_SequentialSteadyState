        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:15 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_GET_ATT_REAL__genmod
          INTERFACE 
            FUNCTION NF_GET_ATT_REAL(NCID,VARID,NAME,RVALS) RESULT(     &
     &STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              CHARACTER(*), INTENT(IN) :: NAME
              REAL(KIND=4), INTENT(OUT) :: RVALS(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_GET_ATT_REAL
          END INTERFACE 
        END MODULE NF_GET_ATT_REAL__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_COPY_ATT__genmod
          INTERFACE 
            FUNCTION NF_COPY_ATT(NCID_IN,VARID_IN,NAME,NCID_OUT,        &
     &VARID_OUT) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID_IN
              INTEGER(KIND=4), INTENT(IN) :: VARID_IN
              CHARACTER(*), INTENT(IN) :: NAME
              INTEGER(KIND=4), INTENT(IN) :: NCID_OUT
              INTEGER(KIND=4), INTENT(IN) :: VARID_OUT
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_COPY_ATT
          END INTERFACE 
        END MODULE NF_COPY_ATT__genmod

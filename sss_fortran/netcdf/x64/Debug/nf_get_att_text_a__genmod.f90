        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:15 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_GET_ATT_TEXT_A__genmod
          INTERFACE 
            FUNCTION NF_GET_ATT_TEXT_A(NCID,VARID,NAME,TEXT) RESULT(    &
     &STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              CHARACTER(*), INTENT(IN) :: NAME
              CHARACTER(LEN=1), INTENT(OUT) :: TEXT(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_GET_ATT_TEXT_A
          END INTERFACE 
        END MODULE NF_GET_ATT_TEXT_A__genmod

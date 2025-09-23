        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:15 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_PUT_ATT_TEXT__genmod
          INTERFACE 
            FUNCTION NF_PUT_ATT_TEXT(NCID,VARID,NAME,NLEN,TEXT) RESULT( &
     &STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              CHARACTER(*), INTENT(IN) :: NAME
              INTEGER(KIND=4), INTENT(IN) :: NLEN
              CHARACTER(*), INTENT(IN) :: TEXT
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_PUT_ATT_TEXT
          END INTERFACE 
        END MODULE NF_PUT_ATT_TEXT__genmod

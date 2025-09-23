        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_RENAME_ATT__genmod
          INTERFACE 
            FUNCTION NF_RENAME_ATT(NCID,VARID,NAME,NEWNAME) RESULT(     &
     &STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              CHARACTER(*), INTENT(IN) :: NAME
              CHARACTER(*), INTENT(IN) :: NEWNAME
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_RENAME_ATT
          END INTERFACE 
        END MODULE NF_RENAME_ATT__genmod

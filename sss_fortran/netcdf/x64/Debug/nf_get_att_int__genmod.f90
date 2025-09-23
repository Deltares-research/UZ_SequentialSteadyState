        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:15 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_GET_ATT_INT__genmod
          INTERFACE 
            FUNCTION NF_GET_ATT_INT(NCID,VARID,NAME,IVALS) RESULT(STATUS&
     &)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              CHARACTER(*), INTENT(IN) :: NAME
              INTEGER(KIND=4), INTENT(OUT) :: IVALS(*)
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_GET_ATT_INT
          END INTERFACE 
        END MODULE NF_GET_ATT_INT__genmod

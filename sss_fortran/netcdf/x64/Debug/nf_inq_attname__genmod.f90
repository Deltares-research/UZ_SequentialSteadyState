        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_INQ_ATTNAME__genmod
          INTERFACE 
            FUNCTION NF_INQ_ATTNAME(NCID,VARID,ATTNUM,NAME) RESULT(     &
     &STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(IN) :: ATTNUM
              CHARACTER(*), INTENT(OUT) :: NAME
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_INQ_ATTNAME
          END INTERFACE 
        END MODULE NF_INQ_ATTNAME__genmod

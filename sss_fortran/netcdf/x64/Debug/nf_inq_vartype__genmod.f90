        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:14 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_INQ_VARTYPE__genmod
          INTERFACE 
            FUNCTION NF_INQ_VARTYPE(NCID,VARID,XTYPE) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              INTEGER(KIND=4), INTENT(OUT) :: XTYPE
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_INQ_VARTYPE
          END INTERFACE 
        END MODULE NF_INQ_VARTYPE__genmod

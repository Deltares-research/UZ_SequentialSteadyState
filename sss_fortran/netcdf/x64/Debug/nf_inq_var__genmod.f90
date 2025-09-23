        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:14 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_INQ_VAR__genmod
          INTERFACE 
            FUNCTION NF_INQ_VAR(NCID,VARID,NAME,XTYPE,NDIMS,DIMIDS,NATTS&
     &) RESULT(STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(IN) :: VARID
              CHARACTER(*), INTENT(OUT) :: NAME
              INTEGER(KIND=4), INTENT(OUT) :: XTYPE
              INTEGER(KIND=4), INTENT(OUT) :: NDIMS
              INTEGER(KIND=4), INTENT(OUT) :: DIMIDS(*)
              INTEGER(KIND=4), INTENT(OUT) :: NATTS
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_INQ_VAR
          END INTERFACE 
        END MODULE NF_INQ_VAR__genmod

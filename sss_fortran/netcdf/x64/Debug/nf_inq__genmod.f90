        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF_INQ__genmod
          INTERFACE 
            FUNCTION NF_INQ(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID) RESULT( &
     &STATUS)
              INTEGER(KIND=4), INTENT(IN) :: NCID
              INTEGER(KIND=4), INTENT(OUT) :: NDIMS
              INTEGER(KIND=4), INTENT(OUT) :: NVARS
              INTEGER(KIND=4), INTENT(OUT) :: NGATTS
              INTEGER(KIND=4), INTENT(OUT) :: UNLIMDIMID
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF_INQ
          END INTERFACE 
        END MODULE NF_INQ__genmod

        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF__CREATE_MP__genmod
          INTERFACE 
            FUNCTION NF__CREATE_MP(PATH,CMODE,INITIALSZ,BASEPE,         &
     &CHUNKSIZEHINTP,NCID) RESULT(STATUS)
              CHARACTER(*), INTENT(IN) :: PATH
              INTEGER(KIND=4), INTENT(IN) :: CMODE
              INTEGER(KIND=4), INTENT(IN) :: INITIALSZ
              INTEGER(KIND=4), INTENT(IN) :: BASEPE
              INTEGER(KIND=4), INTENT(IN) :: CHUNKSIZEHINTP
              INTEGER(KIND=4), INTENT(OUT) :: NCID
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF__CREATE_MP
          END INTERFACE 
        END MODULE NF__CREATE_MP__genmod

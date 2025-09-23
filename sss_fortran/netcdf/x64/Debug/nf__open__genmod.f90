        !COMPILER-GENERATED INTERFACE MODULE: Tue Sep 23 11:34:13 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NF__OPEN__genmod
          INTERFACE 
            FUNCTION NF__OPEN(PATH,MODE,CHUNKSIZEHINTP,NCID) RESULT(    &
     &STATUS)
              CHARACTER(*), INTENT(IN) :: PATH
              INTEGER(KIND=4), INTENT(IN) :: MODE
              INTEGER(KIND=4), INTENT(IN) :: CHUNKSIZEHINTP
              INTEGER(KIND=4), INTENT(INOUT) :: NCID
              INTEGER(KIND=4) :: STATUS
            END FUNCTION NF__OPEN
          END INTERFACE 
        END MODULE NF__OPEN__genmod

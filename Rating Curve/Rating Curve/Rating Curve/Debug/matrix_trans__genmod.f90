        !COMPILER-GENERATED INTERFACE MODULE: Mon Jul  3 08:34:10 2023
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MATRIX_TRANS__genmod
          INTERFACE 
            SUBROUTINE MATRIX_TRANS(IN1,IN2,NROW,NCOL)
              REAL(KIND=4) ,ALLOCATABLE :: IN1(:,:)
              REAL(KIND=4) ,ALLOCATABLE :: IN2(:,:)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
            END SUBROUTINE MATRIX_TRANS
          END INTERFACE 
        END MODULE MATRIX_TRANS__genmod

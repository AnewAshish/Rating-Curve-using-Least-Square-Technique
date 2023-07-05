
program Rating_Curve
    implicit none
    integer :: n,m,o,i,j
    real, dimension (:,:), allocatable :: S, Q, log_S, log_Q, TR, PR, INV, ID, log_H, H, Q_SIM
    !ID = Intermediate matrix
    interface
        subroutine Matrix_Trans(In1, In2,nrow,ncol)
        implicit none
        integer :: nrow, ncol, i, j
        real, dimension (:,:), allocatable :: In1, In2
        end subroutine Matrix_Trans      
    
        subroutine Matrix_Multi(In1,In2,Output, Row1, Col, Col2)
        implicit none
        integer :: Row1, Col, Col2
        real, dimension (:,:), allocatable :: In1, In2, Output
        end subroutine Matrix_Multi
    
        subroutine MatrixInverse(Input1, nrow, ncol, Output1)
        implicit none    
        integer :: nrow, ncol
            real, dimension(:,:), allocatable :: Input1, Output1
        end subroutine MatrixInverse    
    end interface

    open(1, file = "E:\Fortran\Exercies\Rating Curve\Stage.txt")
    open(2,file = "E:\Fortran\Exercies\Rating Curve\Q.txt")
    open(3,file = "E:\Fortran\Exercies\Rating Curve\Test.txt")
    print *, "Enter Number of ordinates"
    read *, n
    allocate (S(n,1),Q(n,1),log_S(n,2), log_Q(n,1), H(2,1), Q_SIM(n,1))
    do i = 1,n
        read(1,*) S(i,1)
    end do
    do i = 1,n
        read(2,*) Q(i,1)
    end do
!Writing[log_S 1] matrix
    do i = 1,n
        log_S(i,1) = log(S(i,1))
        log_S(i,2) = 1
    end do
!Writing[log_Q] matrix
    do i = 1,n
        log_Q(i,1) = log(Q(i,1))
    end do

    call Matrix_Trans(log_S,TR,n,2)
    call Matrix_Multi(TR,log_S,PR,2,n,2)
    call Matrix_Multi(TR,log_Q,ID,2,N,1)
    call MatrixInverse(PR,2,2,INV)
    call Matrix_Multi(INV,ID,log_H,2,2,1)

        H(2,1) = exp(log_H(2,1))
        H(1,1) = log_H(1,1)

    do i = 1,n
        Q_SIM(i,1) = H(1,1)*S(i,1)**H(2,1)
    end do
    do i = 1,2
        print *, H(i,1)
    end do
!Test Prints
    do i = 1,n
        write (3,'(f8.3)',advance = 'no'), Q_SIM(i,1) 
 
        write (3,*)
    end do
    end program Rating_Curve
    
subroutine Matrix_Trans(In1, In2,nrow,ncol)
    implicit none
    integer :: nrow, ncol, i, j
    real, dimension (:,:), allocatable :: In1, In2
    allocate (In2(ncol,nrow))

    do j = 1, ncol
        do i = 1,nrow
            In2(j,i) = In1(i,j)
        end do
    end do
    end subroutine Matrix_Trans

subroutine Matrix_Multi(In1,In2,Output, Row1, Col, Col2)
    implicit none
    integer :: Row1, Col, Col2, i, j, k
    real, dimension (:,:), allocatable :: In1, In2, Output
    allocate (Output(Row1,Col2))

    do i = 1,Row1
        do j = 1,Col2
            Output(i,j) = 0.0
            do k = 1,Col
                Output(i,j) = Output(i,j) + In1(i,k)*In2(k,j)
            end do
        end do
    end do
    end subroutine Matrix_Multi
    
    
    
subroutine MatrixInverse(Input1, nrow, ncol, Output1)
    implicit none
    integer :: nrow, ncol
    real, dimension(:,:), allocatable :: Input1, Output1
    integer :: i, j, k
    real :: X11, Xik
    allocate(Output1(nrow, ncol))
    
    !writing an Identity matrix
    do i = 1, nrow
        do j = 1, ncol
            if (i == j) then
                Output1(i,j) = 1
            else
                Output1(i,j) = 0
            end if
        end do
    end do
    
    do k = 1, nrow
        X11 = Input1(k,k)
        do j = 1, ncol
            Input1(k,j) = Input1(k,j)/X11
            Output1(k,j) = Output1(k,j)/X11
        end do
          
        do i = 1, nrow
            if (i /= k) then
                Xik = Input1(i,k)
                do j = 1, ncol
                  Input1(i,j) = Input1(i,j) - Xik * Input1(k,j)
                  Output1(i,j) = Output1(i,j) - Xik * Output1(k,j)
                end do
            end if
        end do
    end do
    end subroutine MatrixInverse 

program main
    use const
    use init
    use meth
    use fonct
    implicit none

    integer :: N, NN
    real(PR) :: borne_a, borne_b, h
    real(PR), dimension(:), allocatable :: b, A_val
    real(PR), dimension(:,:), allocatable :: A
    integer, dimension(:), allocatable :: A_col, A_row

    N = 500

    borne_a = 0
    borne_b = 1
    h = (borne_b-borne_a)/(N-1)

    allocate(b(N))
    allocate(A(N,N))

    call init_A_b(A, b, h, N)

    call recup_NN(A, N, NN)

    allocate(A_val(NN))
    allocate(A_col(NN))
    allocate(A_row(N+1))

    call convert_A_CSR(A, A_val, A_col, A_row, N, NN)
    


    call meth_lapack(A, b, borne_a, h, "res_solv.dat", N)
    !call meth_grad_conj(A_val, A_col, A_row, b, borne_a, h, "res_solv.dat", N, NN)

    deallocate(b)
    deallocate(A)
    deallocate(A_val)
    deallocate(A_col)
    deallocate(A_row)
    
end program main
program main
    use const
    use init
    use meth
    use fonct
    use err
    implicit none

    integer :: N, NN, N_sol
    real(PR) :: borne_a, borne_b, h
    real(PR), dimension(:), allocatable :: b, A_val, x
    real(PR), dimension(:,:), allocatable :: A
    integer, dimension(:), allocatable :: A_col, A_row

    N = 100
    N_sol = 1000

    borne_a = 0
    borne_b = 1.6_PR
    h = (borne_b-borne_a)/(N-1)

    allocate(b(N))
    allocate(A(N,N))
    allocate(x(N))

    call init_A_b(A, b, h, borne_b, N)

    call recup_NN(A, N, NN)

    allocate(A_val(NN))
    allocate(A_col(NN))
    allocate(A_row(N+1))

    call convert_A_CSR(A, A_val, A_col, A_row, N, NN)

    call meth_piv(A, b, N, x)
    !call meth_lapack(A, b, N, x)
    !call meth_grad_conj(A_val, A_col, A_row, b, N, NN, x)
    
    call write_in_file("../doc/res_solv.dat", x, N, h, borne_a)

    ! correspondance des methodes avec des numéros pour utilisé correctement print_err
    ! meth_piv -> 1
    ! meth_lapack -> 2
    ! meth_grad_conj -> 3
    call print_err(2, borne_a, borne_b, "../doc/erreur.dat")

    call print_sol(borne_a, borne_b, N_sol)

    deallocate(b)
    deallocate(A)
    deallocate(x)
    deallocate(A_val)
    deallocate(A_col)
    deallocate(A_row)

end program main
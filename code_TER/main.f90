program main
    use const
    use init
    use meth
    use fonct
    use err
    implicit none

    integer :: N, NN
    real(PR) :: h
    real(PR), dimension(:), allocatable :: b, A_val, x
    real(PR), dimension(:,:), allocatable :: A
    integer, dimension(:), allocatable :: A_col, A_row

    N = 100

    h = (borne_b-borne_a)/(N-1)

    allocate(b(N))
    allocate(A(N,N))
    allocate(x(N))

    call init_A_b(A, b, h, N)

    call recup_NN(A, N, NN)

    allocate(A_val(NN))
    allocate(A_col(NN))
    allocate(A_row(N+1))

    call convert_A_CSR(A, A_val, A_col, A_row, N, NN)

    !call meth_piv(A, b, N, x)
    call meth_lapack(A, b, N, x)
    !call meth_grad_conj(A_val, A_col, A_row, b, N, NN, x)
    
    call write_in_file("../doc/res_solv.dat", x, N, h, borne_a)

    call write_exp_val()

    ! correspondance des methodes avec des numéros pour utilisé correctement print_err
    ! meth_piv -> 1
    ! meth_lapack -> 2
    ! meth_grad_conj -> 3

    ! correspondance des cas de solutions avec des numéros pour utilisé correctement print_err
    ! solution analytique connu (à definir dans la fonction prévu) -> 1
    ! solution exact approchée avec un grand pas de discrétisation -> 2

    call print_err(2, "../doc/erreur.dat", 2)  ! <- relativement long à calculé avec les 2 premières méthodes (dizaine de sec)
    ! et un seul appel est suffisant pour le moment
     
    ! call print_sol(borne_a, borne_b, N_sol)

    deallocate(b)
    deallocate(A)
    deallocate(x)
    deallocate(A_val)
    deallocate(A_col)
    deallocate(A_row)

end program main
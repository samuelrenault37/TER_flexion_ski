program main
    use const
    use init
    use meth
    use fonct
    use err
    implicit none

    !real(PR), dimension(:), allocatable :: x
    !type(syst_lin) :: sl

    !sl%N = 10

    !allocate(x(sl%N))

    !call init_sl(sl)
    !call convert_A_CSR(sl)

    !call meth_piv(sl, x)
    !call meth_lapack(sl, x)
    !call meth_grad_conj(sl, x)
    !call meth_LU_home_made(sl, x)
    
    !call write_in_file("../donnees/1D/res_solv.dat", x, sl%N, sl%h, sl%deric_x, sl%deric_y)

    !call write_exp_val("../donnees/1D/exp.dat")

    ! correspondance des methodes avec des numéros pour utilisé correctement print_err (premier argument)
    ! meth_piv -> 1
    ! meth_lapack -> 2
    ! meth_grad_conj -> 3
    ! meth_LU_home_made -> 4

    ! correspondance des cas de solutions avec des numéros pour utilisé correctement print_err (dernier argument)
    ! solution analytique connu (à definir dans la fonction prévu) -> 1
    ! solution exact approchée avec un grand pas de discrétisation -> 2

    !call print_err(2, "../donnees/1D/erreur.dat", 2)
    ! un seul appel est suffisant pour le moment
     
    !call print_sol(1000)
    call print_sol_2D(5, 9)

    !call free_CSR(sl)
    !call free_syst_lin(sl)
    !deallocate(x)

end program main
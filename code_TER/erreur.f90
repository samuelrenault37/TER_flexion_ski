module err

    use const
    use fonct
    use meth
    use init
    implicit none

    contains

    function f_sol(x) result(y)
        real(PR), intent(in) :: x
        real(PR) :: y, L

        L = (borne_b - borne_a)

        ! sol analytique pour la fléxion 3pts
        if (x <= L/2) then
            y = - (F*x*(3._PR*L**2-4._PR*x**2)/(48._PR*E*I))
        else
            y = - (F*(L-x)*(3._PR*L**2-4._PR*(L-x)**2)/(48._PR*E*I))
        end if
    
    end function f_sol

    function f_sol_2D(d1, d2) result(f_d1_d2)
        real(PR), intent(in) :: d1, d2
        real(PR) :: f_d1_d2, L
        integer :: k

        L = (borne_b - borne_a)

        select case(cas_init)
        case(4)
            f_d1_d2 = 0
            do k = 0,6
                f_d1_d2 = f_d1_d2 + EXP(-D*((2*REAL(k,PR)+1._PR)**2)*(PI**2)*d1)*(8._PR/((2*REAL(k,PR)+1._PR)*PI)**3)*SIN((2*REAL(k,PR)+1._PR)*PI*d2)
            end do
        case default
            print *, "pas de methode correspondant à ce numéro"
            stop
        end select
    end function f_sol_2D

    subroutine print_err(meth, file, cas_sol)
        character(len=*), intent(in) :: file
        integer, intent(in) :: meth, cas_sol
        real(PR) :: err_max, err_cur, abs_sol, abs_num, h_sol
        integer :: i, k, k_sol, N_sol
        real(PR), dimension(:), allocatable :: x, x_sol
        type(syst_lin) :: sl
        
        open(unit = 1, file = file, action = "write")

        select case(cas_sol)
        case(1)
            do i = 2, 10
                sl%N = 2**i
                call init_sl(sl)

                allocate(x(sl%N))

                select case(meth)
                case(1)
                    call meth_piv (sl, x)
                case(2)
                    call meth_lapack (sl, x)
                case(3)
                    call convert_A_CSR(sl)
                    call meth_grad_conj(sl, x)
                    call free_CSR(sl)
                case(4)
                    call meth_LU_home_made(sl, x)
                case default
                    print *, "pas de methode correspondant à ce numéro"
                    stop
                end select


                err_max = 0
                do k = 1, sl%N
                    err_cur = ABS(f_sol(borne_a + sl%h*(k)) - x(k))
                    if (err_cur > err_max) then
                        err_max = err_cur
                    end if
                end do
                    
                write (1, *) sl%h, err_max

                call free_syst_lin(sl)
                deallocate(x)
            end do
        case(2)
            N_sol = 5000
            h_sol = (borne_b-borne_a)/(N_sol+1)

            sl%N = N_sol
            call init_sl(sl)

            allocate(x_sol(N_sol))

            call meth_lapack (sl, x_sol)

            call free_syst_lin(sl)

            do i = 2, 10
                sl%N = 2**i
                call init_sl(sl)

                allocate(x(sl%N))

                select case(meth)
                case(1)
                    call meth_piv (sl, x)
                case(2)
                    call meth_lapack (sl, x)
                case(3)
                    call convert_A_CSR(sl)
                    call meth_grad_conj(sl, x)
                    call free_CSR(sl)
                case(4)
                    call meth_LU_home_made(sl, x)
                case default
                    print *, "pas de methode correspondant à ce numéro"
                    stop
                end select


                err_max = 0
                abs_sol = borne_a + h_sol
                abs_num = borne_a + sl%h
                k_sol = 1
                do k = 1, sl%N
                    do while (abs_sol <= abs_num)
                        abs_sol = abs_sol + h_sol
                        k_sol = k_sol +1
                    end do
                    abs_num = abs_num + sl%h
                    err_cur = ABS(x_sol(k_sol-1) - x(k))
                    if (err_cur > err_max) then
                        err_max = err_cur
                    end if
                end do
                    
                write (1, *) sl%h, err_max

                call free_syst_lin(sl)
                deallocate(x)
            end do

            deallocate(x_sol)

        case default
            print *, "pas de type de solution exact correspondant à ce numéro"
            stop
        end select
        
        
        close(1)

    end subroutine print_err

    subroutine print_sol(N_sol)
        integer, intent(in) :: N_sol
        real(PR), dimension(N_sol) :: x_sol
        real(PR) :: h, L
        integer :: i

        h = (borne_b-borne_a)/(N_sol+1)
        L = borne_b-borne_a

        do i = 1, N_sol
            x_sol(i) = f_sol(borne_a + h*(i))
        end do
    
        call write_in_file("../donnees/1D/sol.dat", x_sol, N_sol, h, (/0._PR, L/), (/0._PR, 0._PR/))
        
    end subroutine print_sol

    subroutine print_sol_2D(N_sol_d1, N_sol_d2)
        integer, intent(in) :: N_sol_d1, N_sol_d2
        real(PR), dimension(N_sol_d2) :: x_sol
        real(PR) :: h1, h2, L1, L2, d1_k
        integer :: i, k

        L1 = borne_b_d1-borne_a_d1
        L2 = borne_b_d2-borne_a_d2
        h1 = (L1)/(N_sol_d1-1)
        h2 = (L2)/(N_sol_d2+1)

        select case(cas_init)
        case(4)
            do k = 1, N_sol_d1
                d1_k = borne_a_d1+ (k-1)*h1
                do i = 1, N_sol_d2
                    x_sol(i) = f_sol_2D(d1_k, borne_a_d2 + i*h2)
                end do
                call write_in_file_2D("../donnees/2D/chaleur/sol_2D_", k, x_sol, N_sol_d2, h2, (/0._PR, L2/), (/0._PR, 0._PR/))
            end do
        case default
            print *, "pas de cas correspondant à ce numéro en 2D"
            stop
        end select
        
    end subroutine print_sol_2D

end module err

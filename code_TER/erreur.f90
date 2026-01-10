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
            y = - (F*x*(3._PR*L*L-4._PR*x**2)/(48._PR*E*I))
        else
            y = - (F*(L-x)*(3._PR*L**2-4._PR*(L-x)*(L-x))/(48._PR*E*I))
        end if
    
    end function f_sol

    subroutine print_err(meth, file, cas_sol)
        character(len=*), intent(in) :: file
        integer, intent(in) :: meth, cas_sol
        real(PR) :: h, err_max, err_cur, abs_sol, abs_num, h_sol
        integer :: N, NN, i, k, k_sol, N_sol
        real(PR), dimension(:), allocatable :: b, A_val, x, x_sol
        real(PR), dimension(:,:), allocatable :: A
        integer, dimension(:), allocatable :: A_col, A_row
        
        open(unit = 1, file = file, action = "write")

        select case(cas_sol)
        case(1)
            do i = 2, 10
                N = 2**i
                h = (borne_b-borne_a)/(N-1)

                allocate(b(N))
                allocate(A(N,N))
                allocate(x(N))

                call init_A_b(A, b, h, N)

                select case(meth)
                case(1)
                    call meth_piv (A, b, N, x)
                case(2)
                    call meth_lapack (A, b, N, x)
                case(3)
                    call recup_NN(A, N, NN)

                    allocate(A_val(NN))
                    allocate(A_col(NN))
                    allocate(A_row(N+1))

                    call convert_A_CSR(A, A_val, A_col, A_row, N, NN)

                    call meth_grad_conj(A_val, A_col, A_row, b, N, NN, x)

                    deallocate(A_val)
                    deallocate(A_col)
                    deallocate(A_row)
                case default
                    print *, "pas de methode correspondant à ce numéro"
                    stop
                end select


                err_max = 0
                do k = 1, N
                    err_cur = ABS(f_sol(borne_a + h*(k-1)) - x(k))
                    if (err_cur > err_max) then
                        err_max = err_cur
                    end if
                end do
                    
                write (1, *) h, err_max

                deallocate(b)
                deallocate(A)
                deallocate(x)
            end do
        case(2)
            N_sol = 5000

            allocate(b(N_sol))
            allocate(A(N_sol, N_sol))
            allocate(x_sol(N_sol))

            h_sol = (borne_b-borne_a)/(N_sol-1)
            call init_A_b(A, b, h_sol, N_sol)
            call meth_lapack (A, b, N_sol, x_sol)

            deallocate(b)
            deallocate(A)


            do i = 2, 10
                N = 2**i
                h = (borne_b-borne_a)/(N-1)

                allocate(b(N))
                allocate(A(N,N))
                allocate(x(N))

                call init_A_b(A, b, h, N)

                select case(meth)
                case(1)
                    call meth_piv (A, b, N, x)
                case(2)
                    call meth_lapack (A, b, N, x)
                case(3)
                    call recup_NN(A, N, NN)

                    allocate(A_val(NN))
                    allocate(A_col(NN))
                    allocate(A_row(N+1))

                    call convert_A_CSR(A, A_val, A_col, A_row, N, NN)

                    call meth_grad_conj(A_val, A_col, A_row, b, N, NN, x)

                    deallocate(A_val)
                    deallocate(A_col)
                    deallocate(A_row)
                case default
                    print *, "pas de methode correspondant à ce numéro"
                    stop
                end select


                err_max = 0
                abs_sol = borne_a
                abs_num = borne_a
                k_sol = 1
                do k = 1, N
                    do while (abs_sol <= abs_num)
                        abs_sol = abs_sol + h_sol
                        k_sol = k_sol +1
                    end do
                    abs_num = abs_num + h
                    err_cur = ABS(x_sol(k_sol-1) - x(k))
                    if (err_cur > err_max) then
                        err_max = err_cur
                    end if
                end do
                    
                write (1, *) h, err_max

                deallocate(b)
                deallocate(A)
                deallocate(x)
            end do

            deallocate(x_sol)

        case default
            print *, "pas de type de solution exact correspondant à ce numéro"
            stop
        end select
        
        
        close(1)

    end subroutine print_err

    subroutine print_sol(borne_a, borne_b, N_sol)
        integer, intent(in) :: N_sol
        real(PR), intent(in) :: borne_a, borne_b
        real(PR), dimension(N_sol) :: x_sol
        real(PR) :: h
        integer :: i

        h = (borne_b-borne_a)/(N_sol-1)

        do i = 1, N_sol
            x_sol(i) = f_sol(borne_a + h*(i-1))
        end do
    
        call write_in_file("../doc/sol.dat", x_sol, N_sol, h, borne_a)
        
    end subroutine print_sol

end module err

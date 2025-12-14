module err

    use const
    use fonct
    use meth
    use init
    implicit none

    contains

    function f_sol(L, x) result(y)
        real(PR), intent(in) :: x, L
        real(PR) :: y, E, F, I

        E = 10d9 ! en Pa
        F = 784.8_PR ! en N
        I = 1.44d-8 ! en m**4

        if (x <= L/2) then
            y = - (F*x*(3._PR*L*L-4._PR*x**2)/(48._PR*E*I))
        else
            y = - (F*(L-x)*(3._PR*L**2-4._PR*(L-x)*(L-x))/(48._PR*E*I))
        end if
    
        
    end function f_sol

    subroutine print_err(meth, borne_a, borne_b, file)
        character(len=*), intent(in) :: file
        integer, intent(in) :: meth
        real(PR), intent(in) :: borne_a, borne_b
        real(PR) :: h, err_max, err_cur
        integer :: N, NN, i, k
        real(PR), dimension(:), allocatable :: b, A_val, x, sol
        real(PR), dimension(:,:), allocatable :: A
        integer, dimension(:), allocatable :: A_col, A_row
        
        open(unit = 1, file = file, action = "write")

        do i = 2, 10
            N = 2**i
            h = (borne_b-borne_a)/(N-1)

            allocate(b(N))
            allocate(A(N,N))
            allocate(x(N))
            allocate(sol(N))

            call init_A_b(A, b, h, borne_b, N)

            select case(meth)
            case(1)
                call meth_piv (A, b, N, x)
            case(2)
                call meth_piv (A, b, N, x)
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
                err_cur = ABS(f_sol(borne_b - borne_a, borne_a + h*(k-1)) - x(k))
                if (err_cur > err_max) then
                    err_max = err_cur
                end if
            end do
                
            write (1, *) h, err_max

            deallocate(b)
            deallocate(A)
            deallocate(x)
            deallocate(sol)
        end do
        
        close(1)

    end subroutine print_err

end module err

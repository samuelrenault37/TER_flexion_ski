module meth
    use init
    use const
    use fonct
    use LU
    implicit none
    
    contains

    subroutine meth_piv(sl, x)
        type(syst_lin), intent(in) :: sl
        real(PR), dimension(sl%N), intent(out) :: x
        real(PR), dimension(sl%N,sl%N) :: Id, M
        real(PR), dimension(sl%N) :: echange
        real(PR) :: facteur
        integer :: i, j, k

        M = sl%A
        Id = 0
        do i = 1,sl%N
            Id(i,i) = 1
        end do
        
        do i = 1,sl%N
            if(ABS(M(i,i)) < epsilon) then
                j = i+1
                do while (j <= sl%N .AND. ABS(M(j,i)) < epsilon)
                    j = j + 1
                end do
                if (j <= sl%N) then
                    echange = M(j, :)
                    M(j, :) = M(i, :)
                    M(i, :) = echange

                    echange = Id(j, :)
                    Id(j, :) = Id(i, :)
                    Id(i, :) = echange
                else
                    print *, "pas inversible avec cette méthode"
                    stop
                end if
            end if

            facteur = M(i,i)
            do j = 1,sl%N
                M(i,j) = M(i,j)/facteur
                Id(i,j) = Id(i,j)/facteur
            end do

            do k = 1,sl%N
                if(k /= i) then
                    facteur = M(k,i)
                    do j = 1,sl%N
                        M(k,j) = M(k,j) - facteur*M(i,j)
                        Id(k,j) = Id(k,j) - facteur*Id(i,j)
                    end do
                end if
            end do
        end do
    
        x = MATMUL(Id, sl%b)
    end subroutine meth_piv

    subroutine meth_lapack(sl, x)
        type(syst_lin), intent(in) :: sl
        real(PR), dimension(sl%N), intent(out) :: x
        integer, dimension(sl%N) :: ipiv
        integer :: info
        real(PR), dimension(sl%N,sl%N) :: M

        M = sl%A ! M contient la decomposition LU de A à la fin de l'appel
        x = sl%b ! x contient la solution du syst linéaie à la fin de l'appel

        call dgesv(sl%N, 1, M, sl%N, ipiv, x, sl%N, info)

        if (info /= 0) then
            print *, "Erreur, info =", info
        end if

    end subroutine meth_lapack


    subroutine meth_LU_home_made(sl, x)
        type(syst_lin), intent(in) :: sl
        real(PR), dimension(sl%N), intent(out) :: x
        real(PR), dimension(sl%N,sl%N) :: M
        logical :: bool

        call lu_decomposition(sl%A, M, bool)
        if(.NOT.(bool)) then
            stop
        end if
        call lu_res(M, sl%b, x, bool)
    end subroutine meth_LU_home_made


    subroutine meth_grad_conj(sl, x)
        type(syst_lin), intent(in) :: sl
        real(PR), dimension(sl%N), intent(out) :: x
        real(PR), dimension(sl%N) :: r, p, Ap
        real(PR) :: alpha, beta, rho_cur, rho_new
        integer :: i, max_iter

        x = 0
        max_iter = 10000

        call matvec_csr(sl%N, sl%NN, sl%A_val, sl%A_col, sl%A_row, x, Ap)
        r = sl%b - Ap
        p = r
        rho_cur = dot_product(r, r)

        do i = 1, max_iter
            call matvec_csr(sl%N, sl%NN, sl%A_val, sl%A_col, sl%A_row, p, Ap)
            alpha = rho_cur / dot_product(p, Ap)
            x = x + alpha * p
            r = r - alpha * Ap
            rho_new = dot_product(r, r)

            if (sqrt(rho_new) < epsilon) exit

            beta = rho_new / rho_cur
            p = r + beta * p
            rho_cur = rho_new
        end do
    
    end subroutine meth_grad_conj

    subroutine init_sol_2D(sol_n)
        real(PR), dimension(:), intent(out) :: sol_n
        integer :: i, N_d2, k
        real(PR) :: h2, L2, x

        N_d2 = SIZE(sol_n, 1)

        L2 = borne_b_d2-borne_a_d2
        h2 = (L2)/(N_d2+1)

        do i = 1, N_d2
            x = borne_a_d2 + i* h2
            sol_n(i) = 0
            do k = 0,6
                sol_n(i) = sol_n(i) + (8._PR/((2*REAL(k,PR)+1._PR)*PI)**3)*SIN((2*REAL(k,PR)+1._PR)*PI*x)
            end do
        end do
        
    end subroutine init_sol_2D

    subroutine resol_explicite(N_d1, N_d2)
        integer, intent(in) :: N_d1, N_d2
        real(PR), dimension(N_d2) :: sol_n, sol_np1
        real(PR), dimension(2) :: deric_x, deric_y
        real(PR) :: h1, h2, L1, L2
        integer :: i, k

        L1 = borne_b_d1-borne_a_d1
        L2 = borne_b_d2-borne_a_d2
        h1 = (L1)/(N_d1-1)
        h2 = (L2)/(N_d2+1)


        select case(cas_init)
        case(4)

            deric_x(1) = 0
            deric_x(2) = L2
            deric_y(1) = 0
            deric_y(2) = 0

            call init_sol_2D(sol_n)

            do k = 1, N_d1
                call write_in_file_2D("../../donnees/2D/chaleur/res_solv_2D_t", k-1, sol_n, N_d2, h2, deric_x, deric_y)
                sol_np1(1) = ((h1*D)/h2**2)*(sol_n(2)-2*sol_n(1)+deric_y(1))+sol_n(1)
                do i = 2, N_d2-1
                    sol_np1(i) = ((h1*D)/h2**2)*(sol_n(i+1)-2*sol_n(i)+sol_n(i-1))+sol_n(i)
                end do
                sol_np1(N_d2) = ((h1*D)/h2**2)*(deric_y(2)-2*sol_n(N_d2)+sol_n(N_d2-1))+sol_n(N_d2)
                sol_n = sol_np1
            end do
            
            call write_in_file_2D("../donnees/2D/chaleur/res_solv_2D_t", N_d1, sol_n, N_d2, h2, deric_x, deric_y)
        case default
            print *, "pas de cas de resolution correspondant à ce numéro"
            stop
        end select
        
    end subroutine

    subroutine resol_implicite(N_d1, N_d2)
        integer, intent(in) :: N_d1, N_d2
        real(PR), dimension(N_d2) :: sol_n, sol_np1
        type(syst_lin) :: sl
        real(PR) :: h1, L1
        integer :: i

        sl%N = N_d2

        L1 = borne_b_d1-borne_a_d1
        h1 = L1/(N_d1-1)

        call init_sl(sl)

        select case(cas_init)
        case(4)

            call init_sol_2D(sol_n)

            sl%A = h1*sl%A

            do i = 1,N_d2
                sl%A(i,i) = sl%A(i,i) + 1
            end do

            do i = 1, N_d1
                call write_in_file_2D("../../donnees/2D/chaleur/res_solv_2D_t", i-1, sol_n, sl%N, sl%h, sl%deric_x, sl%deric_y)

                sl%b = sol_n

                call meth_lapack(sl, sol_np1)
                sol_n = sol_np1
                
            end do
            call write_in_file_2D("../../donnees/2D/chaleur/res_solv_2D_t", N_d1, sol_n, sl%N, sl%h, sl%deric_x, sl%deric_y)
        case default
            print *, "pas de cas de resolution correspondant à ce numéro"
            stop
        end select
    
        
    end subroutine resol_implicite

end module meth
module meth
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

end module meth
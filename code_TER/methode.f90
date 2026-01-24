module meth
    use const
    use fonct
    use LU
    implicit none
    
    contains

    ! attention A est modifié en sortie
    subroutine meth_piv(A, b, N, x)
        integer, intent(in) :: N
        real(PR), dimension(N,N), intent(inout) :: A
        real(PR), dimension(N), intent(in) ::  b
        real(PR), dimension(N), intent(out) :: x
        real(PR), dimension(N,N) :: Id
        real(PR), dimension(N) :: echange
        real(PR) :: facteur
        integer :: i, j, k

        Id = 0
        do i = 1,N
            Id(i,i) = 1
        end do
        
        do i = 1,N
            if(ABS(A(i,i)) < epsilon) then
                j = i+1
                do while (j <= N .AND. ABS(A(j,i)) < epsilon)
                    j = j + 1
                end do
                if (j <= N) then
                    echange = A(j, :)
                    A(j, :) = A(i, :)
                    A(i, :) = echange

                    echange = Id(j, :)
                    Id(j, :) = Id(i, :)
                    Id(i, :) = echange
                else
                    print *, "pas inversible avec cette méthode"
                    stop
                end if
            end if

            facteur = A(i,i)
            do j = 1,N
                A(i,j) = A(i,j)/facteur
                Id(i,j) = Id(i,j)/facteur
            end do

            do k = 1,N
                if(k /= i) then
                    facteur = A(k,i)
                    do j = 1,N
                        A(k,j) = A(k,j) - facteur*A(i,j)
                        Id(k,j) = Id(k,j) - facteur*Id(i,j)
                    end do
                end if
            end do
        end do
    
        x = MATMUL(Id, b)
    end subroutine meth_piv

    subroutine meth_lapack(A, b, N, x)
        integer, intent(in) :: N
        real(PR), dimension(N,N), intent(in) :: A
        real(PR), dimension(N), intent(in) :: b
        real(PR), dimension(N), intent(out) :: x
        real(PR), dimension(3*N) :: work
        integer :: info, lda, lwork, ipiv(N)

        lda = N
        lwork = 3 * N

        ! factorisation LU
        call dgetrf(N, N, A, lda, ipiv, info)
        if (info /= 0) then
            print *, "Erreur dans DGETRF, info =", info
            stop
        end if

        ! calcul de l'inverse à partir de la factorisation
        call dgetri(N, A, lda, ipiv, work, lwork, info)
        if (info /= 0) then
            print *, "Erreur dans DGETRI, info =", info
            stop
        end if

        x = MATMUL(A, b)

    end subroutine meth_lapack


    subroutine meth_lapack_v2(A, b, N, x)
        integer, intent(in) :: N
        real(PR), dimension(N,N), intent(in) :: A
        real(PR), dimension(N), intent(in) :: b
        real(PR), dimension(N), intent(out) :: x
        integer, dimension(N) :: ipiv
        integer :: info
        real(PR), dimension(N,N) :: M

        M = A ! M contient la decomposition LU de A à la fin de l'appel
        x = b ! x contient la solution du syst linéaie à la fin de l'appel

        call dgesv(N, 1, M, N, ipiv, x, N, info)

        if (info /= 0) then
            print *, "Erreur, info =", info
        end if

    end subroutine meth_lapack_v2


    subroutine meth_LU_home_made(A, b, N, x)
        integer, intent(in) :: N
        real(PR), dimension(N,N), intent(in) :: A
        real(PR), dimension(N), intent(in) :: b
        real(PR), dimension(N), intent(out) :: x
        real(PR), dimension(N,N) :: M
        logical :: bool

        call lu_decomposition(A, M, bool)
        if(.NOT.(bool)) then
            stop
        end if
        call lu_res(M, b, x, bool)
    end subroutine meth_LU_home_made


    subroutine meth_grad_conj(A_val, A_col, A_row, b, N, NN, x)
        integer, intent(in) :: N, NN
        real(PR), dimension(NN), intent(in) :: A_val !toutes les valeurs non nuls de la matrice en ligne
        integer, dimension(NN), intent(in) :: A_col !contient la colonnes de chacune de ses valeurs
        integer, dimension(N+1), intent(in) :: A_row !pointeurs de début de chaque ligne + 1
        real(PR), dimension(N), intent(in) :: b
        real(PR), dimension(N), intent(out) :: x
        real(PR), dimension(N) :: r, p, Ap
        real(PR) :: alpha, beta, rho_cur, rho_new
        integer :: i, max_iter

        x = 0
        max_iter = 10000

        call matvec_csr(N, NN, A_val, A_col, A_row, x, Ap)
        r = b - Ap
        p = r
        rho_cur = dot_product(r, r)

        do i = 1, max_iter
            call matvec_csr(N, NN, A_val, A_col, A_row, p, Ap)
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
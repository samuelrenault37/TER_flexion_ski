module meth
    use const
    use fonct
    implicit none
    
    contains

    ! attention A est modifié en sortie
    subroutine meth_piv(A, b, borne_a, h, file, N)
        integer, intent(in) :: N
        real(PR), dimension(N,N), intent(inout) :: A
        real(PR), dimension(N), intent(in) ::  b
        real(PR), intent(in) :: borne_a, h
        character(len=*), intent(in) :: file
        real(PR), dimension(N,N) :: Id
        real(PR) :: facteur
        integer :: i, j, k

        Id = 0
        do i = 1,N
            Id(i,i) = 1
        end do
        
        do i = 1,N
            if(ABS(A(i,i)) < epsilon) then
                print *, "pas inversible avec cette méthode"
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

    call write_in_file("../doc/"//file, MATMUL(Id, b), N, h, borne_a)
        
    end subroutine meth_piv

    subroutine meth_lapack(A, b, borne_a, h, file, N)
        integer, intent(in) :: N
        real(PR), intent(in) :: borne_a, h
        real(PR), dimension(N,N), intent(in) :: A
        real(PR), dimension(N), intent(in) :: b
        character(len=*), intent(in) :: file
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

        call write_in_file("../doc/"//file, MATMUL(A, b), N, h, borne_a)

    end subroutine meth_lapack


    !cette methode ne marche enfait pas en l'état (je sais pas l'expliquer), je la réparerai quand on aura eu un vrai cours dessus
    subroutine meth_grad_conj(A_val, A_col, A_row, b, borne_a, h, file, N, NN)
        integer, intent(in) :: N, NN
        real(PR), intent(in) :: borne_a, h
        real(PR), dimension(NN), intent(in) :: A_val !toutes les valeurs non nuls de la matrice en ligne
        integer, dimension(NN), intent(in) :: A_col !contient la colonnes de chacune de ses valeurs
        integer, dimension(N+1), intent(in) :: A_row !pointeurs de début de chaque ligne + 1
        real(PR), dimension(N), intent(in) :: b
        character(len=*), intent(in) :: file
        real(PR), dimension(N) :: x, r, p, Ap
        real(PR) :: alpha, beta, rho_cur, rho_new
        integer :: i, max_iter

        x = 0
        max_iter = 1000

        call matvec_csr(N, A_val, A_col, A_row, x, Ap)
        r = b - Ap
        p = r
        rho_cur = dot_product(r, r)

        do i = 1, max_iter
            call matvec_csr(N, A_val, A_col, A_row, p, Ap)
            alpha = rho_cur / dot_product(p, Ap)
            x = x + alpha * p
            r = r - alpha * Ap
            rho_new = dot_product(r, r)

            if (sqrt(rho_new) < epsilon) exit

            beta = rho_new / rho_cur
            p = r + beta * p
            rho_cur = rho_new
        end do

        call write_in_file("../doc/"//file, x, N, h, borne_a)
    
        
    end subroutine meth_grad_conj

end module meth
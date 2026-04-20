module mod_algebre
    use donnees
    implicit none
    
contains

subroutine meth_lapack()
        integer, dimension(N) :: ipiv
        integer :: info
        real(PR), dimension(N,N) :: M

        M = A ! M contient la decomposition LU de A à la fin de l'appel
        u = b ! u contient la solution du syst linéaire à la fin de l'appel

        call dgesv(N, 1, M, N, ipiv, u, N, info)

        if (info /= 0) then
            print *, "Erreur, info =", info
        end if

    end subroutine meth_lapack

    subroutine meth_grad_conj()
        integer :: NN
        real(PR), dimension(:), allocatable :: A_val
        integer, dimension(:), allocatable :: A_col, A_row
        real(PR), dimension(N) :: r, p, Ap
        real(PR) :: alpha, beta, rho_cur, rho_new
        integer :: i, max_iter

        u = 0
        max_iter = 10000

        call convert_A_CSR(A_val, A_col, A_row, NN)
        
        call matvec_csr(NN, A_val, A_col, A_row, x, Ap)
        r = b - Ap
        p = r
        rho_cur = dot_product(r, r)

        do i = 1, max_iter
            call matvec_csr(NN, A_val, A_col, A_row, p, Ap)
            alpha = rho_cur / dot_product(p, Ap)
            x = x + alpha * p
            r = r - alpha * Ap
            rho_new = dot_product(r, r)

            if (sqrt(rho_new) < epsilon) exit

            beta = rho_new / rho_cur
            p = r + beta * p
            rho_cur = rho_new
        end do

        deallocate(A_val, A_col, A_row)
    
    end subroutine meth_grad_conj

    subroutine matvec_csr(NN, val, col, row, x, y)
        integer, intent(in) :: NN
        real(PR), dimension(NN), intent(in) :: val
        integer, dimension(NN), intent(in) :: col
        integer, dimension(N+1), intent(in) :: row
        real(PR), dimension(N), intent(in) :: x
        real(PR), dimension(N), intent(out) :: y
        integer :: i, j

        y = 0

        do i = 1, N
            do j = row(i), row(i+1)-1
                y(i) = y(i) + val(j) * x(col(j))
            end do
        end do
    end subroutine matvec_csr

    subroutine recup_NN(NN)
        integer, intent(out) :: NN
        integer :: i, j

        NN = 0
        do i = 1,N
            do j = 1,N
                if (ABS(A(i,j))>epsilon) then
                    NN = NN + 1
                end if
            end do
        end do
        
    end subroutine recup_NN

    subroutine convert_A_CSR(A_val, A_col, A_row, NN)
        integer, intent(out):: NN
        real(PR), dimension(:), allocatable, intent(out):: A_val
        integer, dimension(:), allocatable, intent(out):: A_col, A_row
        integer :: i, j, k, r, compteur
        real(PR) :: prec

        call recup_NN(NN)

        allocate(A_val(NN)) !toutes les valeurs non nuls de la matrice en ligne
        allocate(A_col(NN)) !contient la colonnes de chacune de ses valeurs
        allocate(A_row(N+1)) !pointeurs de début de chaque ligne (dans les autres tableaux)

        k = 1
        r = 2
        compteur = 1
        A_row(1) = compteur
        prec = 0
        
        do i = 1,N
            do j = 1,N
                if(ABS(A(i,j)) > epsilon) then
                    A_val(k) = A(i,j)
                    A_col(k) = j
                    k = k +1
                    compteur = compteur + 1
                else
                    if (ABS(prec) > epsilon) then
                        A_row(r) = compteur
                        r = r + 1
                    end if
                end if
                prec = A(i,j)
            end do
        end do

        if (r < N+2) then
            A_row(N+1) = compteur
        end if
        
    end subroutine convert_A_CSR

    subroutine meth_piv()
        real(PR), dimension(N,N) :: Id, M
        real(PR), dimension(N) :: echange
        real(PR) :: facteur
        integer :: i, j, k

        M = A
        Id = 0
        do i = 1, N
            Id(i,i) = 1
        end do
        
        do i = 1,N
            if(ABS(M(i,i)) < epsilon) then
                j = i+1
                do while (j <= N .AND. ABS(M(j,i)) < epsilon)
                    j = j + 1
                end do
                if (j <= N) then
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
            do j = 1, N
                M(i,j) = M(i,j)/facteur
                Id(i,j) = Id(i,j)/facteur
            end do

            do k = 1, N
                if(k /= i) then
                    facteur = M(k,i)
                    do j = 1, N
                        M(k,j) = M(k,j) - facteur*M(i,j)
                        Id(k,j) = Id(k,j) - facteur*Id(i,j)
                    end do
                end if
            end do
        end do
    
        u = MATMUL(Id, b)
    end subroutine meth_piv
    
end module mod_algebre
program inverse_matrix
    implicit none
    integer, parameter :: n = 2000, PR = 8
    integer :: info, lda, lwork, i, ipiv(n)
    real(PR), dimension(n,n) :: A
    real(PR), dimension(3*n) :: work
    
    ! Matrice à inverser
    A(1,1) = -2
    A(1,2) = 1
    do i = 2,n-1
        A(i,i-1) = 1
        A(i,i) = -2
        A(i,i+1) = 1
    end do
    A(n,n-1) = 1
    A(n,n) = -2

    lda = n
    lwork = 3 * n

    ! Étape 1 : factorisation LU
    call dgetrf(n, n, A, lda, ipiv, info)
    if (info /= 0) then
        print *, "Erreur dans DGETRF, info =", info
        stop
    end if

    ! Étape 2 : calcul de l'inverse à partir de la factorisation
    call dgetri(n, A, lda, ipiv, work, lwork, info)
    if (info /= 0) then
        print *, "Erreur dans DGETRI, info =", info
        stop
    end if

    ! Affichage de la matrice inversée
    print *, "Matrice inversée :"
    call aff_matrix(A, n)

contains

    subroutine aff_matrix(Mat, n)
        real(8),dimension(n,n), intent(in) :: Mat
        integer, intent(in) ::  n
        integer :: i
        do i = 1,n
            print *, Mat(i,:)
        end do
    end subroutine aff_matrix

end program inverse_matrix

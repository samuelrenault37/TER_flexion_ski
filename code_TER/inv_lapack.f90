program inverse_matrix
    implicit none
    integer, parameter :: PR = 8, N = 4
    integer :: info, lda, lwork, ipiv(N), i
    real(PR), dimension(N,N) :: A
    real(PR), dimension(3*N) :: work
    
    ! Matrice à inverser
    A(1,1) = -2
    A(1,2) = 1
    do i = 2,N-1
        A(i,i-1) = 1
        A(i,i) = -2
        A(i,i+1) = 1
    end do
    A(N,N-1) = 1
    A(N,N) = -2

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

    ! Affichage de la matrice inversée
    print *, "la matrice inverse de A est :"
    call aff_matrix(A, N)

contains

    subroutine aff_matrix(Mat, n)
        real(PR),dimension(n,n), intent(in) :: Mat
        integer, intent(in) ::  n
        integer :: i
        do i = 1,n
            print *, Mat(i,:)
        end do
    end subroutine aff_matrix

end program inverse_matrix

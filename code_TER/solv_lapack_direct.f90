program inverse_matrix
    implicit none
    integer, parameter :: PR = 8, N = 1500 ! limite liée au temps de calcul mais meilleur que piv_gauss
    real(PR) :: borne_a, borne_b, h, xa, xb
    integer :: info, lda, lwork, ipiv(N), i
    real(PR), dimension(N,N) :: A
    real(PR), dimension(3*N) :: work
    real(PR), dimension(N) :: x, b

    ! intialisation des bornes, du pas de discrétisation et des CL (nul ici sinon jsp comment ça marche)
    borne_a = 0
    xa = 0
    borne_b = 1
    xb = 0
    h = (borne_b-borne_a)/(N+1)
    
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
    ! print *, "la matrice inverse de A est :"
    ! call aff_matrix(A, N)

    b = 2

    x = MATMUL(A, b)

    call write_in_file("../doc/res_solv_lapack.txt", x, N, h, borne_a, borne_b, xa, xb)

contains

    subroutine aff_matrix(Mat, n)
        real(PR),dimension(N,N), intent(in) :: Mat
        integer, intent(in) ::  n
        integer :: i
        do i = 1,n
            print *, Mat(i,:)
        end do
    end subroutine aff_matrix

    ! écrit les valeurs utiles dans un fichier texte pour pouvoir les exploiter avec gnuplot (tracé de courbe)
    subroutine write_in_file(file, x, N, h, a, b, xa, xb)
        character(len=*), intent(in) :: file
        real(PR), dimension(:), intent(in) ::  x
        integer, intent(in) :: N
        real(PR), intent(in) :: h, a, b, xa, xb

        open(unit = 1, file = file, action = "write")
        write (1, '(A)') "#abscisse               ordonnée"
        write (1, *) a , xa
        do i = 1,N
        write (1, *) a + (i)*(h), x(i)*h**2
        end do
        write (1, *) b , xb
        close(1)
    end subroutine write_in_file

end program inverse_matrix

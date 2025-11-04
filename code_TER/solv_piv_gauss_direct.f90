program inv_matrix
    implicit none
    integer, parameter :: PR = 8, N = 500 !limité par le temps de calcul plutôt que par l'echelle des valeurs manipulées
    real(PR) :: borne_a, borne_b, h, xa, xb
    real(PR), parameter :: epsilon = 1.d-10
    real(PR) :: facteur
    real(PR), dimension(N,N) :: A, Id
    real(PR), dimension(N) :: x, b
    integer :: i, j, k

    ! intialisation des bornes, du pas de discrétisation et des CL (nul ici sinon jsp comment ça marche)
    borne_a = 0
    xa = 0
    borne_b = 1
    xb = 0
    h = (borne_b-borne_a)/(N+1)

    ! initialisation de A et Id
    Id = 0
    A = 0
    A(1,1) = -2
    A(1,2) = 1
    Id(1,1) = 1
    do i = 2,N-1
        A(i,i-1) = 1
        A(i,i) = -2
        A(i,i+1) = 1
        Id(i,i) = 1
    end do
    Id(N,N) = 1
    A(N,N-1) = 1
    A(N,N) = -2

    ! Méthode du pivot de Gauss
    do i = 1,N
        if(ABS(A(i,i)) < epsilon) then
            print *, "pas ineversible avec cette méthode"
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

    ! print *, "la matrice inverse de A est :"
    ! call aff_matrix(Id, N)

    b = 2

    x = MATMUL(Id, b)

    call write_in_file("../doc/res_solv_piv.txt", x, N, h, borne_a, borne_b, xa, xb)

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
    
    end program inv_matrix
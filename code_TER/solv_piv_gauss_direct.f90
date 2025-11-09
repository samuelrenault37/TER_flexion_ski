program inv_matrix
    implicit none
    integer, parameter :: PR = 8, N = 500 !limité par le temps de calcul plutôt que par l'echelle des valeurs manipulées
    real(PR) :: borne_a, borne_b, h
    real(PR), parameter :: epsilon = 1.d-10
    real(PR) :: facteur
    real(PR), dimension(N,N) :: A, Id
    real(PR), dimension(N) :: x, b
    integer :: i, j, k

    ! intialisation des bornes, du pas de discrétisation
    borne_a = 0
    borne_b = 1
    h = (borne_b-borne_a)/(N-1)

    ! initialisation de A et Id
    call init_A_b_2(A, b, h)
    Id = 0
    do i = 1,N
        Id(i,i) = 1
    end do

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

    x = MATMUL(Id, b)

    call write_in_file("../doc/res_solv_piv.txt", x, N, h, borne_a, borne_b)

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
    subroutine write_in_file(file, x, N, h, a, b)
        character(len=*), intent(in) :: file
        real(PR), dimension(:), intent(in) ::  x
        integer, intent(in) :: N
        real(PR), intent(in) :: h, a, b

        open(unit = 1, file = file, action = "write")
        write (1, '(A)') "#abscisse               ordonnée"
        do i = 1,N
        write (1, *) a + (i-1)*(h), x(i)
        end do
        close(1)
    end subroutine write_in_file

    subroutine init_A_b_1(A, b, h)
        real(PR), dimension(N,N), intent(out) :: A
        real(PR), dimension(N),intent(out) :: b
        real(PR), intent(in) :: h
        integer :: i

        A = 0

        A(1,1) = 1
        do i = 2,N-1
            A(i,i-1) = 1
            A(i,i) = -2
            A(i,i+1) = 1
        end do
        A(N,N) = 1

        ! CL
        b = 2*h**2
        b(1) = 0 !u(a)
        b(N) = 0 !u(b)

    end subroutine init_A_b_1

    subroutine init_A_b_2(A, b, h)
        real(PR), dimension(N,N), intent(out) :: A
        real(PR), dimension(N),intent(out) :: b
        real(PR), intent(in) :: h
        integer :: k
        real(PR) :: F, E, I

        E = 1
        F = -1
        I = 1

        A = 0

        ! Conditions pour les 2 premières lignes (encastrement) 
        A(1,1) = 1

        A(2,1) = -1
        A(2,2) =  1

        ! Conditions pour les 2 dernières lignes de matrice
        A(N-1, N-2) =  1
        A(N-1, N-1) = -2
        A(N-1, N)   =  1

        A(N, N-3) = -1
        A(N, N-2) =  3
        A(N, N-1) = -3
        A(N, N)   =  1

        ! Reste de la matrice A

        do k=3, N-2
            A(k,k-2) =  1
            A(k,k-1) = -4
            A(k,k)   =  6
            A(k,k+1) = -4
            A(k,k+2) =  1
        end do

        ! CL
        b = 0*h**4
        b(1) = 0 !u(a)
        b(2) = 0*h !u'(a)
        b(N-1) = 0*h**2 !u''(b)
        b(N) = -F*h**3/(E*I) !u'''(b)
    end subroutine init_A_b_2
    
    end program inv_matrix
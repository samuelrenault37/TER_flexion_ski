program inv_matrix
    implicit none
    integer, parameter :: PR = 8, N = 5
    real(PR) :: epsilon = 1.d-10, facteur
    real(PR), dimension(N,N) :: A, Id
    integer :: i, j, k

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
            print *, "pas inevrsible avec cette méthode"
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

    print *, "la matrice inverse de A est :"
    call aff_matrix(Id, N)

    contains

    subroutine aff_matrix(Mat, n)
        real(PR),dimension(n,n), intent(in) :: Mat
        integer, intent(in) ::  n
        integer :: i
        do i = 1,N
            print *, Mat(i,:)
        end do
    end subroutine aff_matrix
    
    end program inv_matrix
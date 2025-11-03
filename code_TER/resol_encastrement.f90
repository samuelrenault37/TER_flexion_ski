program flexion_poutre

    implicit none

    integer, parameter           :: PR = 8
    integer, parameter           :: N = 10
    real(PR)                     :: L, E, F, I, h
    real(PR), dimension(N+1,N+1) :: A
    real(PR), dimension(N+1)     :: b, y
    integer                      :: i

    L = 1._PR
    E = 1._PR
    F = 1._PR
    I = 1._PR
    
    h = L / N

    A = 0._PR
    b = 0._PR

    ! Conditions pour les 2 premières lignes (encastrement) 
    A(1,1) = 1._PR

    A(2,1) = -1._PR
    A(2,2) =  1._PR

    ! Conditions pour les 2 dernières lignes de matrice
    A(N, N-2) =  1._PR
    A(N, N-1) = -2._PR
    A(N, N)   =  1._PR

    A(N+1, N-3) = -1._PR
    A(N+1, N-2) =  3._PR
    A(N+1, N-1) = -3._PR
    A(N+1, N)   =  1._PR
    b(N+1)      = -F*h**3/(E*I)

    ! Reste de la matrice A

    do i=3, N-1
        A(i,i-2) =  1._PR
        A(i,i-1) = -4._PR
        A(i,i)   =  6._PR
        A(i,i+1) = -4._PR
        A(i,i+2) =  1._PR
        b(i)      = 0._PR
    end do

    ! Résolution
    
    call gauss(A,b,y,N+1)
    
    print*, "i","x(i)","y(i)"
    do i = 1, N+1
        print*, i, (i-1)*h, y(i)
    end do

    contains

    subroutine gauss(A, b, x, N)
    
        implicit none
        
        integer, intent(in)     :: N
        real(PR), intent(inout) :: A(N,N)
        real(PR), intent(inout) :: b(N)
        real(PR), intent(out)   :: x(N)
        real(PR)                :: facteur, epsilon
        integer                 :: i, k

        epsilon = 1.d-12
        x = 0._PR

        
        do i = 1,N
            if(ABS(A(i,i)) < epsilon) then
                print *, "pas ineversible avec cette méthode"
            end if


            facteur = A(i,i)
            A(i,:) = A(i,:) / facteur
            b(i) = b(i) / facteur

            do k = 1, N
                if (k /= i) then
                    facteur = A(k,i)
                    A(k,:) = A(k,:) - facteur * A(i,:)
                    b(k) = b(k) - facteur * b(i)
                end if
            end do
        end do

        x = b
        
    end subroutine gauss


    
end program flexion_poutre

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
    A(N, N)   =  1.PR

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

    

    
end program flexion

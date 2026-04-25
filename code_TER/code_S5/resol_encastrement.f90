program flexion_poutre

    implicit none

    integer, parameter           :: PR = 8
    integer, parameter           :: N = 10
    real(PR)                     :: L, E, F, I, h
    real(PR), dimension(N+1,N+1) :: A
    real(PR), dimension(N+1)     :: b, y
    integer                      :: k
    real(PR)                     :: yi_exact, err_max, ymax_exact, err_rel

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

    do k=3, N-1
        A(k,k-2) =  1._PR
        A(k,k-1) = -4._PR
        A(k,k)   =  6._PR
        A(k,k+1) = -4._PR
        A(k,k+2) =  1._PR
        b(k)      = 0._PR
    end do

    ! Résolution
    
    call gauss(A,b,y,N+1)
    
    print*, "k","x(k)","y(k)"
    do k = 1, N+1
        print*, k, (k-1)*h, y(k)
    end do

        ! Vérification de la convergence numérique
    
    err_max = 0._PR
    ymax_exact = 0._PR

    do k = 1, N+1
        yi_exact = (F/(6._PR*E*I)) * ((real(k-1,PR)*h)**2) * (3._PR*L - real(k-1,PR)*h)
        err_max = max(err_max, abs(y(k) - yi_exact))
        ymax_exact = max(ymax_exact, abs(yi_exact))
    end do

    err_rel = err_max / max(ymax_exact, 1.e-30_PR)

    print*, "Erreur relative maximale :", err_rel


    contains

    subroutine gauss(A, b, x, N)
    
        implicit none
        
        integer, intent(in)     :: N
        real(PR), intent(inout) :: A(N,N)
        real(PR), intent(inout) :: b(N)
        real(PR), intent(out)   :: x(N)
        real(PR)                :: facteur, epsilon
        integer                 :: j, k

        epsilon = 1.d-12
        x = 0._PR

        
        do j = 1,N
            if(ABS(A(j,j)) < epsilon) then
                print *, "pas ineversible avec cette méthode"
            end if


            facteur = A(j,j)
            A(j,:) = A(j,:) / facteur
            b(j) = b(j) / facteur

            do k = 1, N
                if (k /= j) then
                    facteur = A(k,j)
                    A(k,:) = A(k,:) - facteur * A(j,:)
                    b(k) = b(k) - facteur * b(j)
                end if
            end do
        end do

        x = b
        
    end subroutine gauss


    
end program flexion_poutre

program flexion_poutre_3points

    implicit none

    integer, parameter           :: PR = 8
    integer, parameter           :: N  = 10
    real(PR)                     :: L, E, F, I, h
    real(PR), dimension(N+1,N+1) :: A
    real(PR), dimension(N+1)     :: b, y
    integer                      :: k
    real(PR)                     :: yi_exact, err_max, ymax_exact, err_rel, x, Mx

   
    L = 1._PR
    E = 1._PR
    F = 1._PR
    I = 1._PR

    h = L / real(N,PR)

    A = 0._PR
    b = 0._PR

    ! Condition pour la première ligne 
    A(1,1) = 1._PR

    ! Condition pour la dernière ligne
    A(N+1, N+1) = 1._PR

    ! Reste de la matrice A
    
    do k = 2, N
    
        x = real(k-1,PR)*h

        A(k, k-1) = -1._PR
        A(k, k  ) =  2._PR
        A(k, k+1) = -1._PR

        ! Moment M(x) par morceaux
        
        if (x <= L/2._PR) then
            Mx = 0.5_PR*F*x
        else
            Mx = 0.5_PR*F*(L-x)
        end if

        b(k) = - (h**2*Mx/(E*I))
        
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
        x = real(k-1,PR) * h

        if (x <= L/2._PR) then
            yi_exact = - (F*x*(3._PR*L*L-4._PR*x**2)/(48._PR*E*I))
        else
            yi_exact = - (F*(L-x)*(3._PR*L**2-4._PR*(L-x)*(L-x))/(48._PR*E*I))
        end if


        err_max    = max(err_max, abs(y(k) - yi_exact))
        ymax_exact = max(ymax_exact, abs(yi_exact))
    end do

    err_rel = err_max / max(ymax_exact, 1.e-30_PR)

    print*, "Erreur relative maximale :", err_rel

    !-----------------------------
    contains
    !-----------------------------

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
                print *, "pas inversible avec cette méthode"
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


end program flexion_poutre_3points

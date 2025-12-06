module init
    use const
    use fonct
    implicit none
    
contains

  subroutine init_A_b(A, b, h, L, N)
        integer, intent(in) :: N
        real(PR), intent(in) :: h, L
        real(PR), dimension(N,N), intent(out) :: A
        real(PR), dimension(N), intent(out) :: b
        integer :: k
        real(PR) :: F, E, I, Mx, x

        E = 10d9 ! en Pa
        F = 784.8_PR ! en N
        I = 1.44d-8 ! en m**4

        A = 0

        ! Condition pour la première ligne 
        A(1,1) = 1

        ! Condition pour la dernière ligne
        A(N, N) = 1


        ! A
        do k = 2, N-1
    
            x = (k-1)*h

            A(k, k-1) = 1
            A(k, k  ) = -2
            A(k, k+1) = 1

            ! Moment M(x) par morceaux
            
            if (x <= L/2._PR) then
                Mx = 0.5_PR*F*x
            else
                Mx = 0.5_PR*F*(L-x)
            end if

            b(k) = (h**2*Mx/(E*I))
            
        end do

        b(1) = 0
        b(N) = 0
    end subroutine init_A_b

    ! A avec une seule zone non nul par ligne
    subroutine convert_A_CSR(A, A_val, A_col, A_row, N, NN)
        integer, intent(in) :: N, NN
        real(PR), dimension(N,N), intent(in) :: A
        real(PR), dimension(NN), intent(out) :: A_val !toutes les valeurs non nuls de la matrice en ligne
        integer, dimension(NN), intent(out) :: A_col !contient la colonnes de chacune de ses valeurs
        integer, dimension(N+1), intent(out) :: A_row !pointeurs de début de chaque ligne (dans les autres tableaux)
        integer :: i, j, k, r, compteur
        real(PR) :: prec

        k = 1
        r = 2
        compteur = 1
        A_row(1) = compteur
        prec = 0
        
        do i = 1,N
            do j = 1,N
                if(ABS(A(i,j))>epsilon) then
                    A_val(k) = A(i,j)
                    A_col(k) = j
                    k = k +1
                    compteur = compteur + 1
                else
                    if (ABS(prec)>epsilon) then
                        A_row(r) = compteur
                        r = r + 1
                    end if
                end if
                prec = A(i,j)
            end do
        end do

        if (r < N+2) then
            A_row(N+1) = compteur
        end if
        
    end subroutine convert_A_CSR

end module init
module init
    use const
    use fonct
    implicit none
    
contains

  subroutine init_A_b(A, b, h, N)
        integer, intent(in) :: N
        real(PR), intent(in) :: h
        real(PR), dimension(N,N), intent(out) :: A
        real(PR), dimension(N), intent(out) :: b
        integer :: k
        real(PR) :: Mx, x, I_fs, I_sfs, L

        L = (borne_b - borne_a)

        A = 0

        select case(cas_init)
            case(1)
                ! Condition pour la première ligne 
                A(1,1) = 1

                ! Condition pour la dernière ligne
                A(N, N) = 1

                !Les intégrales sur f sont calculée directement à partir de la discrétisation faite pour la méthode des différences finies
                I_fs = 0
                I_sfs = 0
                ! A
                do k = 2, N-1
            
                    x = (k-1)*h

                    A(k, k-1) = 1
                    A(k, k  ) = -2
                    A(k, k+1) = 1

                    ! Moment M(x) par morceaux pour la fléxion 3 pts
                    
                    ! if (x <= L/2._PR) then
                    !     Mx = 0.5_PR*F*x
                    ! else
                    !     Mx = 0.5_PR*F*(L-x)
                    ! end if

                    ! Moment M(x) pour une charge répartie f_rep
                    I_fs = I_fs + f_rep(x-(h/2))*h
                    I_sfs = I_sfs + (x-(h/2))*f_rep(x-(h/2))*h

                    Mx = 0.5_PR*F*x - x*(I_fs) + I_sfs
                    
                    b(k) = (h**2*Mx/(E*I))
                    
                end do

                !print *, I_fs ! pour vérifier que le coefficient A est bien choisi

                b(1) = 0
                b(N) = 0

            case(2)
                ! Condition sur la flèche aux extrémitées
                A(1,1) = 1

                A(N, N) = 1
                
                ! Condition sur les moments aux extrémitées
                A(2,1) = 1
                A(2,2) = -2
                A(2,3) = 1

                A(N-1, N-2) = 1
                A(N-1, N-1) = -2
                A(N-1, N) = 1

                do k = 3, N-2
            
                    x = (k-1)*h

                    A(k, k-2) = 1
                    A(k, k-1) = -4
                    A(k, k  ) = 6
                    A(k, k+1) = -4
                    A(k, k+2) = 1
                    
                    b(k) = -h**4*f_rep(x)/(E*I)
                    
                end do

                !print *, I_fs ! pour vérifier que le coefficient A est bien choisi

                
                ! Condition sur la flèche aux extrémitées
                b(1) = 0
                b(N) = 0

                ! Condition sur les moments aux extrémitées
                b(2) = h**2*0
                b(N-1) = h**2*0
            
            case default
                print *, "pas d'initialisation associée à ce cas"

        end select

    end subroutine init_A_b

    function f_rep(x) result(fx)
        real(PR), intent(in) :: x
        real(PR) :: fx, A, pic1, pic2, largeur_pic

        A = F/0.4253889206_PR ! nb obtenu en calculant 2* l'Integrale de 0 à L de exp((-1/2)*(x - pic1)**2/(largeur_pic**2))
        pic1 = 0.4785_PR ! valeur réfléchie
        pic2 = 0.7385_PR ! valeur réfléchie
        largeur_pic = 0.06_PR ! valeur réfléchie

        fx = A*EXP(-(1._PR/2)*(((x - pic1)**2)/(2*largeur_pic**2))) + A*EXP(-(1._PR/2)*(((x - pic2)**2)/(2*largeur_pic**2)))
    end function f_rep

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
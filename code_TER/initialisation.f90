module init
    use const
    use fonct
    implicit none
    
contains

  subroutine init_sl(sl)
        type(syst_lin), intent(inout) :: sl
        integer :: k
        real(PR) :: Mx, x, I_fs, I_sfs, L, Mx_max, sx_max

        allocate(sl%b(sl%N))
        allocate(sl%A(sl%N,sl%N))

        sl%A = 0

        select case(cas_init)
            case(1)

                L = (borne_b - borne_a)
                sl%h = L/(sl%N+1)

                ! Condition pour la première ligne 
                sl%A(1,1) = -2
                sl%A(1,2) = 1


                ! Condition pour la dernière ligne
                sl%A(sl%N, sl%N) = -2
                sl%A(sl%N,sl%N-1) = 1

                !Les intégrales sur f sont calculée directement à partir de la discrétisation faite pour la méthode des différences finies
                I_fs = 0
                I_sfs = 0

                Mx_max = 0
                sx_max = -1

                ! A
                do k = 1, sl%N
            
                    x = k*sl%h

                    if (2<=k .AND. k<=sl%N-1) then
                        sl%A(k, k-1) = 1
                        sl%A(k, k  ) = -2
                        sl%A(k, k+1) = 1
                    end if

                    ! Moment M(x) pour une charge répartie f_rep
                    I_fs = I_fs + f_rep(x-(sl%h/2))*sl%h
                    I_sfs = I_sfs + (x-(sl%h/2))*f_rep(x-(sl%h/2))*sl%h

                    Mx = 0.5_PR*F*x - x*(I_fs) + I_sfs

                    if (Mx > Mx_max) then
                        Mx_max = Mx
                        sx_max = x
                    end if

                    sl%b(k) = (sl%h**2*Mx/(E*I))
                    
                end do

                allocate(sl%deric_x(2))
                allocate(sl%deric_y(2))

                sl%deric_x(1) = 0
                sl%deric_x(2) = L
                sl%deric_y(1) = 0
                sl%deric_y(2) = 0

                !print *, I_fs ! pour vérifier que le coefficient A est bien choisi
                !print *, "sigma_max = ", (Mx_max/I)*0.005, "atteint en x =", sx_max
                !on obtient : sigma_max =    13039066.941155676      atteint en x =  0.61422845691382766

            case(2)

                L = (borne_b - borne_a)
                sl%h = L/(sl%N+1)

                ! Condition limites tenant compte de Neuman et Derichlet
                
                sl%A(1, 1) = 5
                sl%A(1, 2) = -4
                sl%A(1, 3) = 1

                sl%A(2, 1) = -4
                sl%A(2, 2) = 6
                sl%A(2, 3) = -4
                sl%A(2, 4) = 1
                

                sl%A(sl%N-1, sl%N-3) = 1
                sl%A(sl%N-1, sl%N-2) = -4
                sl%A(sl%N-1, sl%N-1) = 6
                sl%A(sl%N-1, sl%N) = -4

                sl%A(sl%N, sl%N-2) = 1
                sl%A(sl%N, sl%N-1) = -4
                sl%A(sl%N, sl%N) = 5

                do k = 1, sl%N
            
                    x = k*sl%h

                    if (3<=k .AND. k<=sl%N-2) then
                    sl%A(k, k-2) = 1
                    sl%A(k, k-1) = -4
                    sl%A(k, k  ) = 6
                    sl%A(k, k+1) = -4
                    sl%A(k, k+2) = 1
                    end if
                    
                    sl%b(k) = -sl%h**4*f_rep(x)/(E*I)
                    
                end do

                allocate(sl%deric_x(2))
                allocate(sl%deric_y(2))

                sl%deric_x(1) = 0
                sl%deric_x(2) = L
                sl%deric_y(1) = 0
                sl%deric_y(2) = 0

                !print *, I_fs ! pour vérifier que le coefficient A est bien choisi

            case(3)

                L = (borne_b - borne_a)
                sl%h = L/(sl%N+1)

                ! Condition pour la première ligne 
                sl%A(1,1) = -2
                sl%A(1,2) = 1


                ! Condition pour la dernière ligne
                sl%A(sl%N, sl%N) = -2
                sl%A(sl%N, sl%N-1) = 1

                ! A
                do k = 1, sl%N
            
                    x = k*sl%h

                    if (2<=k .AND. k<=sl%N-1) then
                        sl%A(k, k-1) = 1
                        sl%A(k, k  ) = -2
                        sl%A(k, k+1) = 1
                    end if

                    ! Moment M(x) par morceaux pour la fléxion 3 pts
                    
                    if (x <= L/2._PR) then
                        Mx = 0.5_PR*F*x
                    else
                        Mx = 0.5_PR*F*(L-x)
                    end if
                    
                    sl%b(k) = (sl%h**2*Mx/(E*I))
                    
                end do

                allocate(sl%deric_x(2))
                allocate(sl%deric_y(2))

                sl%deric_x(1) = 0
                sl%deric_x(2) = L
                sl%deric_y(1) = 0
                sl%deric_y(2) = 0
            case(4)

                L = (borne_b_d2 - borne_a_d2)
                sl%h = L/(sl%N+1)

                ! Condition pour la première ligne 
                sl%A(1,1) = -2
                sl%A(1,2) = 1


                ! Condition pour la dernière ligne
                sl%A(sl%N, sl%N) = -2
                sl%A(sl%N,sl%N-1) = 1

                do k = 2, sl%N-1

                    sl%A(k, k-1) = 1
                    sl%A(k, k  ) = -2
                    sl%A(k, k+1) = 1
                    
                end do

                sl%A = -(D/sl%h**2) * sl%A

                allocate(sl%deric_x(2))
                allocate(sl%deric_y(2))

                sl%deric_x(1) = 0
                sl%deric_x(2) = L
                sl%deric_y(1) = 0
                sl%deric_y(2) = 0


            case default
                print *, "pas d'initialisation associée à ce cas"

        end select

    end subroutine init_sl

    function f_rep(x) result(fx)
        real(PR), intent(in) :: x
        real(PR) :: fx, A, pic1, pic2, largeur_pic

        A = F/0.4253889216_PR ! nb obtenu en calculant 2* l'Integrale de 0 à L de exp((-1/2)*(x - pic1)**2/(largeur_pic**2))
        pic1 = (borne_b - borne_a)/2 - 0.13 ! valeur réfléchie
        pic2 = (borne_b - borne_a)/2 + 0.13 ! valeur réfléchie
        largeur_pic = 0.06_PR ! valeur réfléchie

        fx = A*EXP(-(1._PR/2)*(((x - pic1)**2)/(2*largeur_pic**2))) + A*EXP(-(1._PR/2)*(((x - pic2)**2)/(2*largeur_pic**2)))
    end function f_rep

    ! A avec une seule zone non nul par ligne
    subroutine convert_A_CSR(sl)
        type(syst_lin), intent(inout) :: sl
        integer :: i, j, k, r, compteur
        real(PR) :: prec

        call recup_NN(sl%A, sl%N, sl%NN)

        allocate(sl%A_val(sl%NN)) !toutes les valeurs non nuls de la matrice en ligne
        allocate(sl%A_col(sl%NN)) !contient la colonnes de chacune de ses valeurs
        allocate(sl%A_row(sl%N+1)) !pointeurs de début de chaque ligne (dans les autres tableaux)

        k = 1
        r = 2
        compteur = 1
        sl%A_row(1) = compteur
        prec = 0
        
        do i = 1,sl%N
            do j = 1,sl%N
                if(ABS(sl%A(i,j))>epsilon) then
                    sl%A_val(k) = sl%A(i,j)
                    sl%A_col(k) = j
                    k = k +1
                    compteur = compteur + 1
                else
                    if (ABS(prec)>epsilon) then
                        sl%A_row(r) = compteur
                        r = r + 1
                    end if
                end if
                prec = sl%A(i,j)
            end do
        end do

        if (r < sl%N+2) then
            sl%A_row(sl%N+1) = compteur
        end if
        
    end subroutine convert_A_CSR

end module init
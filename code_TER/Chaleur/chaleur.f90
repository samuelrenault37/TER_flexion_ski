program chaleur

    use constantes
    use functions
    use algebre

    implicit none 

    integer                                 :: i, j, k, n, imax_x, imax_y
    character(len=40)                       :: ct
    real(PR), parameter                     :: cfl = 0.9_PR
    real(PR)                                :: h_x, h_y, tps_2D, h_t, err_2D, sum
    real(PR), dimension(:,:), allocatable   :: T_2D, Tnp1_2D


    ! -----------------
    ! Solution exacte
    ! ----------------- 

    imax_x = 256
    imax_y = 256

    h_x = 1._PR/(imax_x +1)
    h_y = 1._PR/(imax_y +1)

    do n = 0,10

        tps_2D = n * 0.01_PR

        write(ct,'(I3.3)') n
        open(unit=9,file='sol_exacte_2D_'//trim(adjustl(ct))//'.dat')

        do i = 1, imax_x

            do j = 1, imax_y

                write(9,'(E18.8, 4X, E18.8, 4X, E18.8)') i*h_x, j*h_y, Texacte_2D(tps_2D,i*h_x, j*h_y)

            end do 

            write(9, *) ! Pour séparer à chaque fois pour la compréhension Gnuplot

        end do

        close(9)

    end do 

    ! -----------------------------
    ! Solution approchée avec EE /!\ L'erreur ne fonctionne pas
    ! -----------------------------

    h_t = cfl*(1._PR/(2*D*(1._PR/(h_x**2) + 1._PR/(h_y**2))))

    allocate(T_2D(0:imax_x+1, 0:imax_y+1))
    allocate(Tnp1_2D(0:imax_x+1, 0:imax_y+1))

    ! Ouverture du fichier pour l'erreur
    !open(unit = 3, file = 'erreur_2D.dat', ACTION = 'WRITE')

    do j = 0, 10

        !sum = 0._PR !Pour le calcul d'erreur à la fin

        tps_2D = j * 0.01_PR

        write(ct,'(I3.3)') j
        open(unit=2,file='sol_EE_2D_'//trim(adjustl(ct))//'.dat')

        ! CL de T_2D
        do i = 0, imax_x + 1
            T_2D(i, 0)         = 0._PR
            T_2D(i, imax_y +1) = 0._PR
        end do

        do i = 0, imax_y + 1
            T_2D(0, i)         = 0._PR
            T_2D(imax_x +1, i) = 0._PR
        end do

        ! Coeur de la matrice T_2D
        do i = 1, imax_x 

            do k = 1,  imax_y

            T_2D(i,k) = T_init_2D(i*h_x, k*h_y)

            end do 

        end do

        do n = 1, INT(tps_2D/h_t)

            ! CL de Tnp1_2D
            do i = 0, imax_x + 1
                Tnp1_2D(i,0)          = 0._PR
                Tnp1_2D(i, imax_y +1) = 0._PR
            end do 

            do i = 0, imax_y + 1
                Tnp1_2D(0,i)          = 0._PR
                Tnp1_2D(imax_x +1, i) = 0._PR
            end do 

            ! Coeur de la matrice Tnp1_2D
            do i = 1, imax_x 

                do k = 1, imax_y

                    Tnp1_2D(i, k) = T_2D(i, k) + h_t*D*( &
                    (T_2D(i+1,k) - 2*T_2D(i,k) + T_2D(i-1,k))/(h_x**2) &
                    + (T_2D(i,k+1) - 2*T_2D(i,k) + T_2D(i,k-1))/(h_y**2) )

                end do

            end do

            T_2D = Tnp1_2D

        end do 

        do i = 1, imax_x 

            do k = 1, imax_y

                write(2,'(E18.8, 4X, E18.8, 4X, E18.8)') i*h_x, k*h_y, T_2D(i,k)
                !sum = sum + (T_2D(i,k) - Texacte_2D(tps_2D, i*h_x, k*h_y))**2
                
            end do 

            write(2, *) ! Pour séparer à chaque fois pour la compréhension Gnuplot

        end do 

        close(2)

        !err_2D = SQRT(sum*h_x*h_y)

        ! Ecriture des pas de temps et des erreurs
        !write(3, '(E18.8, 4X, E18.8, 4X, E18.8)') h_x, h_y, err_2D

    end do

    !close(3)

    ! -----------------
    
    deallocate(T_2D, Tnp1_2D)

end program chaleur

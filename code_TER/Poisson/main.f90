program poisson

    use constantes
    use functions
    use algebre

    implicit none 

    integer                               :: deg, nb_pts_ds_schema, schema
    logical                               :: alors

    real(PR)          :: ug, ud, delta_xn
    character(len=20) :: nc
    integer           :: i, n, j, k, m
    real(PR), dimension(:,:), allocatable :: R, L2
    real(PR), dimension(:),   allocatable :: w, v

    real(PR) :: sum2, err_g_2D

    integer  :: Nx, Ny
    real(PR) :: delta_x_2D


    ! -----------------
    ! Ecriture schéma
    ! -----------------  

    deg = 1
    nb_pts_ds_schema = 5
    schema = 1
    Nx = 40  ! Il faut que Nx = Ny absolument dans notre cas 
    Ny = 40  ! Sinon la résolution ne se fera pas (ou alors faut améliorer le code)
    delta_x_2D = 1._PR/REAL(Nx-1, PR) ! Comme Nx = Ny on a delta_x_2D = delta_y_2D ce qui évite de les différencier

    ug = 0._PR
    ud = 0._PR

    ! Ouverture du fichier pour l'erreur
    open(unit = 22, file = 'courbe_erreur_2D.dat', ACTION = 'WRITE')

    ! Calculs et écriture des approximations
    do i = 0, 4

        n = 3*2**i
        delta_xn = 1._PR/REAL(n-1, PR)

        ! Création de la matrice R du système Rv = w
        allocate(R(n*n,n*n))

        R = 0._PR

        call construction_matrice_2D(deg, nb_pts_ds_schema, schema, n, n, delta_xn, R)

        ! Construction du vecteur w
        allocate(w(n*n))
        
        do j = 1, n
            do m = 1, n
        
                k = m + (j-1)*n


                ! Points intérieurs
                if (m >= 2 .AND. m <= n-1 .AND. j >= 2 .AND. j <= n-1) then

                    w(k) = f_2D((m-1)*delta_xn, (j-1)*delta_xn)

                else
                    ! Condition de bord
                    w(k) = 0._PR
        
                end if
        
            end do
        end do

        ! Résolution de v
        allocate(v(n*n))
        allocate(L2(n*n,n*n))


        ! Vérification que tout fonctionne
        call llt_factorisation(R, L2, alors)
        !print *, "Est ce qu'on a bien une décomposition de Cholesky qui fonctionne ?"
        !print *, alors

        if (alors .EQV. .TRUE.) then

            call llt_res(L2, w, v, alors)
            !print *, "Est ce que la résolution de Rv = w a fonctionnée ?"
            !print *, alors

            !if (alors .EQV. .TRUE.) then

                !if (MAXVAL(ABS(MATMUL(R,v) - w)) <= epsilon1) then 
                    !alors = .TRUE.
                !end if

                !print *, "A t-on bien Rv = w ?"
                !print *, alors

            !end if

        end if  

        ! Création des fichiers des approximations
        write(nc, * ) n
        open(unit=9,file='sol_num_2D_'//trim(adjustl(nc))//'.dat')

        sum2 = 0._PR

        do j = 1, n

            do m = 1,n

                k = m + (j-1)*n

            write(9, '(I4, 4X, I4, 4X, E18.8)') m, j, v(k)

            sum2 = sum2 + (sol_exacte_2D((m-1)*delta_xn, (j-1)*delta_xn) - v(k))**2

            end do 

            write(9, *) !Pour que gnuplot puisse afficher la courbe (càd qu'il comprenne quand i change)

        end do

        err_g_2D = SQRT(sum2)*delta_xn

        ! Ecriture des pas de temps et des erreurs de la question 3
        write(22, '(E18.8, 4X, E18.8)') delta_xn , err_g_2D

        close(9)

        deallocate(R, L2, v, w)

    end do

    close (22)

    ! Calcul et écriture de la solution exacte
    open(unit=3, file='sol_exacte_2D.dat', ACTION='WRITE')

    do j = 1, Ny
        do i = 1, Nx

        write(3, '(I4, 4X, I4, 4X, E18.8)') i, j, sol_exacte_2D((i-1)*delta_x_2D, (j-1)*delta_x_2D)

        end do
        write(3,*) !Pour que gnuplot puisse afficher la courbe (càd qu'il comprenne quand i change)
    end do 

    close(3)


    ! La courbe de l'erreur par régression linéaire est 2.03628*x - 0.773165
    ! On obtient donc un ordre du schéma de 2


end program poisson

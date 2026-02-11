program poisson

    use constantes
    use functions
    use algebre

    implicit none 

    real(PR), dimension(:,:), allocatable :: A, L
    real(PR), dimension(:),   allocatable :: b, u
    integer                               :: deg, nb_pts_ds_schema, schema, nb_total_pts
    real(PR)                              :: delta_x
    logical                               :: alors

    real(PR)          :: ug, ud, delta_xn
    character(len=20) :: nc
    integer           :: i, n, j, k, m
    real(PR), dimension(:,:), allocatable :: R, L2
    real(PR), dimension(:),   allocatable :: w, v

    real(PR) :: err_g, sum1

    integer  :: Nx, Ny
    real(PR) :: delta_x_2D

    ! ---------------------------
    ! PARTIE 1
    ! ---------------------------


    ! -----------------
    ! QUESTION 1
    ! -----------------

    deg = 1
    nb_pts_ds_schema = 3
    schema = 1
    nb_total_pts = 40
    delta_x = 1._PR/REAL(nb_total_pts+1, PR)

    allocate(A(nb_total_pts, nb_total_pts))
    allocate(L(size(A,1), size(A,2)))

    allocate(b(size(A,1)))
    allocate(u(size(A,1)))

    call construction_matrice(deg, nb_pts_ds_schema, schema, nb_total_pts, delta_x, A)

    !call print_mat(A,  'Matrice A')

    call llt_factorisation(A, L, alors)

    alors = llt_check(A,L)

    print *, "Est ce qu'on a bien une décomposition de Cholesky qui fonctionne ?"
    print *, alors

    if (alors .EQV. .TRUE.) then

        call random_number(b)

        call llt_res(L, b, u, alors)

        print *, "Est ce que la résolution de Au = b a fonctionnée ?"
        print *, alors

        if (alors .EQV. .TRUE.) then

            if (MAXVAL(ABS(MATMUL(A,u) - B)) <= epsilon1) then 
                alors = .TRUE.
            end if

            print *, "A t-on bien Au = b ?"
            print *, alors

        end if 

    end if

    print *, ""

    ! -----------------
    ! QUESTION 2
    ! -----------------

    ug = 1._PR/((20*PI)**2)
    ud = 1._PR/((20*PI)**2)

    ! Ouverture du fichier pour la question 3
    open(unit = 2, file = 'courbe_erreur.dat', ACTION = 'WRITE')

    ! Calculs et écriture des approximations
    do i = 0, 4

        n = 20*2**i
        delta_xn = 1._PR/REAL(n+1, PR)

        ! Création de la matrice A (qu'on appelle R)
        allocate(R(n,n))

        R = 0._PR

        call construction_matrice(deg, nb_pts_ds_schema, schema, n, delta_xn, R)

        ! Construction du vecteur b (qu'on appelle w)
        allocate(w(n))

        w(1) = f(delta_xn)  + ug/((delta_xn)**2)
        w(n) = f(n*delta_xn) + ud/((delta_xn)**2)

        do k = 2, n-1
            w(k) = f(k*delta_xn)
        end do

        ! Résolution de u (qu'on appelle v)
        allocate(v(n))
        allocate(L2(n,n))

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


        ! Création de fichiers
        write(nc, * ) n
        open(unit=10,file='sol_num_'//trim(adjustl(nc))//'.dat')

        ! -----------------
        ! QUESTION 3 (Ajout)
        ! -----------------  

        sum1 = 0._PR

        do j = 1, n

            write(10, '(I3, 4X, E18.8)') j, v(j)

            sum1 = sum1 + (sol_exacte(j*delta_xn, ug, ud) - v(j))**2

        end do 

        err_g = SQRT(sum1*delta_xn)

        print *, "Pour delta_xn valant :"
        print *, delta_xn

        print *, "L'erreur correspondante est :"
        print *, err_g

        print *, ""

        ! Ecriture des pas de temps et des erreurs de la question 3
        write(2, '(E18.8, 4X, E18.8)') delta_xn , err_g

        close(10)


        deallocate(R, L2, v, w)

    end do 

    close (2)

    ! Calcul et écriture de la solution exacte
    nb_total_pts = 1000
    delta_x = 1._PR/REAL(nb_total_pts-1, PR)

    open(unit=1, file='sol_exacte.dat', ACTION='WRITE')

    do i = 1, nb_total_pts

        write(1, '(I4, 4X, E18.8)') i-1, sol_exacte((i-1)*delta_x, ug, ud)

    end do 

    close(1)

    ! -----------------
    ! QUESTION 3
    ! -----------------   

    ! Voir la partie sur la création d'erreurs entre les lignes 141 et 163

    ! La courbe de l'erreur par régression linéaire est 2.16693*x - 1.96117
    ! On obtient donc bien l'ordre 2 du schéma attendu

    ! ------------------   

    deallocate(A, L, b, u)

    ! ---------------------------
    ! PARTIE 2
    ! ---------------------------  


    ! -----------------
    ! RE QUESTION 1
    ! -----------------  

    deg = 1
    nb_pts_ds_schema = 5
    schema = 1
    Nx = 40  ! Il faut que Nx = Ny absolument dans notre cas 
    Ny = 40  ! Sinon la résolution ne se fera pas (ou alors faut améliorer le code)
    delta_x_2D = 1._PR/REAL(Nx-1, PR) ! Comme Nx = Ny on a delta_x_2D = delta_y_2D ce qui évite de les différencier
    
    !allocate(A(Nx*Ny, Nx*Ny))
    !allocate(L(Nx*Ny, Nx*Ny))

    !allocate(b(size(A,1)))
    !allocate(u(size(A,1)))

    !call construction_matrice_2D(deg, nb_pts_ds_schema, schema, Nx, Ny, delta_x_2D, A)

    !call print_mat(A,  'Matrice A')

    !call llt_factorisation(A, L, alors)

    !alors = llt_check(A,L)

    !print *, "Est ce qu'on a bien une décomposition de Cholesky qui fonctionne ?"
    !print *, alors

    !if (alors .EQV. .TRUE.) then

        !call random_number(b)

        !call llt_res(L, b, u, alors)

        !print *, "Est ce que la résolution de Au = b a fonctionnée ?"
        !print *, alors

        !if (alors .EQV. .TRUE.) then

            !if (MAXVAL(ABS(MATMUL(A,u) - B)) <= epsilon1) then 
                !alors = .TRUE.
            !end if

            !print *, "A t-on bien Au = b ?"
            !print *, alors

        !end if 

    !end if

    !print *, ""

    ! -----------------
    ! RE QUESTION 2
    ! ----------------- 

    ug = 0._PR
    ud = 0._PR

    ! Calculs et écriture des approximations
    do i = 0, 4

        n = 3*2**i
        delta_xn = 1._PR/REAL(n-1, PR)

        ! Création de la matrice A (qu'on appelle R)
        allocate(R(n*n,n*n))

        R = 0._PR

        call construction_matrice_2D(deg, nb_pts_ds_schema, schema, n, n, delta_xn, R)

        ! Construction du vecteur b (qu'on appelle w)
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

        ! Résolution de u (qu'on appelle v)
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

        ! Création de fichiers
        write(nc, * ) n
        open(unit=9,file='sol_num_2D_'//trim(adjustl(nc))//'.dat')

        do j = 1, n

            do m = 1,n

                k = m + (j-1)*n

            write(9, '(I4, 4X, I4, 4X, E18.8)') m, j, v(k)

            end do 

            write(9, *) !Pour que gnuplot puisse afficher la courbe (càd qu'il comprenne quand i change)

        end do

        close(9)

        deallocate(R, L2, v, w)

    end do

    ! Calcul et écriture de la solution exacte
    open(unit=3, file='sol_exacte_2D.dat', ACTION='WRITE')

    do j = 1, Ny
        do i = 1, Nx

        write(3, '(I4, 4X, I4, 4X, E18.8)') i, j, sol_exacte_2D((i-1)*delta_x_2D, (j-1)*delta_x_2D)

        end do
        write(3,*) !Pour que gnuplot puisse afficher la courbe (càd qu'il comprenne quand i change)
    end do 

    close(3)


    ! ------------------

    !deallocate(A, L, b, u)

end program poisson

module schema_DF
    use donnees
    use fonctions
    use mod_algebre
    implicit none
    
contains

    subroutine calc_DF

        call init_sl_cl
        select case(resol)
        case(1)
            call meth_lapack
        case(2)
            call meth_piv
        case(3)
            call meth_grad_conj
        case default
            print*, "L'algorithme de résolution choisi n'existe pas"
        end select
        
    end subroutine calc_DF

    subroutine init_sl_cl()
        integer :: i, n_block
        real(PR), dimension(2*nx, 2*nx) :: M, N, Q, PM2, M1, P1, Q_1, P1_1, P2_1, P3_1, Q_ad, P1_ad, Q_d, M1_d, M2_d, M3_d

        ! initialisation des CL de dirichlet

        n_dirich = nx
        n_block = 2*nx

        allocate(dirich(n_dirich, 2))
        do i = 1, n_dirich
            dirich(i, 1) = x(i)
            dirich(i, 2) = 0
        end do

        ! initialisations des matrices principales qui serviront à construire toute la matrice A par blocs
        
        ! init de M
        M = 0
        do i = 1, n_block-1, 2
            M(i, i) = mu
            M(i+1, i+1) = lambda + 2*mu
        end do
        M = (1/dy**2)*M

        ! print*, "M :"
        ! call aff_mat(M)

        ! init de N
        N = 0
        do i = 3, 2*(nx-1)-1, 2
            N(i, i-1) = -1
            N(i, i+3) = 1
            N(i+1, i-2) = -1
            N(i+1, i+2) = 1
        end do
        N = ((lambda + mu)/(4*dx*dy))*N
        N(1, 2) = lambda/(dx*dy)
        N(2, 1) = mu/(dx*dy)
        N(n_block-1, n_block) = -lambda/(dx*dy)
        N(n_block, n_block-1) = -mu/(dx*dy)

        ! print*, "N :"
        ! call aff_mat(N)

        ! init de Q
        Q = 0
        do i = 3, 2*(nx-1)-1, 2
            Q(i, i-2) = lambda + 2*mu
            Q(i, i+2) = lambda + 2*mu
            Q(i+1, i-1) = mu
            Q(i+1, i+3) = mu
        end do
        Q = (1/dx**2)*Q
        do i = 3, 2*(nx-1)-1, 2
            Q(i, i) = -2*((lambda + 2*mu)/dx**2 + mu/dy**2) 
            Q(i+1, i+1) = -2*((lambda + 2*mu)/dy**2 + mu/dx**2)
        end do
        Q(1,1) = (lambda - 3*mu)/(2*dy**2) - (2*(lambda + 2*mu))/dx**2
        Q(1, 3) = (2*(lambda + 2*mu))/dx**2
        Q(2, 2) = (lambda*(lambda + mu) - 4*(lambda + 2*mu)**2)/(2*(lambda + 2*mu)*dy**2) - (2*mu)/dx**2
        Q(2, 4) = (2*mu)/dx**2
        Q(n_block-1, n_block-1) = (lambda - 3*mu)/(2*dy**2) - (2*(lambda + 2*mu))/dx**2
        Q(n_block-1, n_block-3) = (2*(lambda + 2*mu))/dx**2
        Q(n_block, n_block) = (lambda*(lambda + mu) - 4*(lambda + 2*mu)**2)/(2*(lambda + 2*mu)*dy**2) - (2*mu)/dx**2
        Q(n_block, n_block-2) = (2*mu)/dx**2

        ! print*, "Q :"
        ! call aff_mat(Q)

        ! init de PM2
        PM2 = 0
        PM2(1, 1) = -(lambda + mu)/(4*dy**2)
        PM2(2, 2) = -(lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)
        PM2(n_block-1, n_block-1) = -(lambda + mu)/(4*dy**2)
        PM2(n_block, n_block) = -(lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)

        M1 = M - N
        P1 = M + N

        ! print*, "M1 :"
        ! call aff_mat(M1)
        ! print*, "P1 :"
        ! call aff_mat(P1)






        ! remplissage de A

        A = 0

        !modif des matrices de base pour la premières lignes

        Q_1 = Q

        Q_1(1, 1:3) = (/(-5*lambda - 13*mu)/(4*dy**2) - 2*(lambda + 2*mu)/dx**2, -3*lambda/(dx*dy), 2*(lambda + 2*mu)/dx**2/)
        Q_1(2, 1:4) = (/-3*mu/(dx*dy), (-5*lambda*(lambda + mu) - 8*(lambda + 2*mu)**2)/(4*(lambda + 2*mu)*dy**2) - 2*mu/dx**2, 0._PR, 2*mu/dx**2/)

        do i = 3, 2*(nx-1)-1, 2
            Q_1(i, i-1) = 3*(lambda + mu)/(4*dx*dy)
            Q_1(i, i+3) = -3*(lambda + mu)/(4*dx*dy)
            Q_1(i+1, i-2) = 3*(lambda + mu)/(4*dx*dy)
            Q_1(i+1, i+2) = -3*(lambda + mu)/(4*dx*dy)
        end do

        Q_1(n_block-1, n_block-3:n_block) = (/2*(lambda + 2*mu)/dx**2, 0._PR, (-5*lambda - 13*mu)/(4*dy**2) - 2*(lambda + 2*mu)/dx**2, 3*lambda/(dx*dy)/)
        Q_1(n_block, n_block-2:n_block) = (/2*mu/dx**2, 3*mu/(dx*dy), (-5*lambda*(lambda + mu) - 8*(lambda + 2*mu)**2)/(4*(lambda + 2*mu)*dy**2) - 2*mu/dx**2/)
        ! ^ peut etre une source d'erreur

        ! print*, "Q_1 :"
        ! call aff_mat(Q_1)

        P1_1 = M + 4*N

        P1_1(1, 1:2) = (/(11*lambda + 15*mu)/(4*dy**2), 4*lambda/(dx*dy)/)
        P1_1(2, 1:2) = (/4*mu/(dx*dy), (11*lambda*(lambda + mu) + 4*(lambda + 2*mu)**2)/(4*(lambda + 2*mu)*dy**2)/)

        P1_1(n_block-1, n_block-1:n_block) = (/(11*lambda + 15*mu)/(4*dy**2), -4*lambda/(dx*dy)/)
        P1_1(n_block, n_block-1:n_block) = (/-4*mu/(dx*dy), (11*lambda*(lambda + mu) + 4*(lambda + 2*mu)**2)/(4*(lambda + 2*mu)*dy**2)/)
        ! ^ peut etre une source d'erreur

        ! print*, "P1_1 :"
        ! call aff_mat(P1_1)

        P2_1 = -N

        P2_1(1, 1:2) = (/-7*(lambda + mu)/(4*dy**2), -lambda/(dx*dy)/)
        P2_1(2, 1:2) = (/-mu/(dx*dy), (-7*lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)/)

        P2_1(n_block-1, n_block-1:n_block) = (/-7*(lambda + mu)/(4*dy**2), lambda/(dx*dy)/)
        P2_1(n_block, n_block-1:n_block) = (/mu/(dx*dy), (-7*lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)/)
        ! ^ peut etre une source d'erreur

        ! print*, "P2_1 :"
        ! call aff_mat(P2_1)

        P3_1 = 0

        P3_1(1, 1) = (lambda + mu)/(4*dy**2)
        P3_1(2, 2) = (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)

        P3_1(n_block-1, n_block-1) = (lambda + mu)/(4*dy**2)
        P3_1(n_block, n_block) = (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)
        ! ^ peut etre une source d'erreur

        ! print*, "P3_1 :"
        ! call aff_mat(P3_1)

        ! remplissage de la première ligne

        A(1:n_block, 1:n_block) = Q_1
        A(1:n_block, n_block+1:n_block*2) = P1_1
        A(1:n_block, n_block*2+1:n_block*3) = P2_1
        A(1:n_block, n_block*3+1:n_block*4) = P3_1

        ! remplissage de la deuxième ligne (rien à faire grâce aux CL de dirichlet nulles)

        A(n_block+1:n_block*2, 1:n_block) = M1
        A(n_block+1:n_block*2, n_block+1:n_block*2) = Q
        A(n_block+1:n_block*2, n_block*2+1:n_block*3) = P1
        A(n_block+1:n_block*2, n_block*3+1:n_block*4) = PM2

        ! remplissage du coeur de A

        do i = 3, ny - 2
            A(n_block*(i-1)+1:n_block*i, n_block*(i-3)+1:n_block*(i-2)) = PM2
            A(n_block*(i-1)+1:n_block*i, n_block*(i-2)+1:n_block*(i-1)) = M1
            A(n_block*(i-1)+1:n_block*i, n_block*(i-1)+1:n_block*i) = Q
            A(n_block*(i-1)+1:n_block*i, n_block*i+1:n_block*(i+1)) = P1
            A(n_block*(i-1)+1:n_block*i, n_block*(i+1)+1:n_block*(i+2)) = PM2
        end do

        ! remplissage de la ligne jmax-1 avec les conditions obtenues grâce au schéma décentré (aval)

        ! modification des matrices de base

        Q_ad = Q

        Q_ad(1, 1) = Q_ad(1, 1) - (lambda + mu)/(4*dy**2)
        Q_ad(2, 2) = Q_ad(2, 2) - (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)
        Q_ad(n_block-1, n_block-1) = Q_ad(n_block-1, n_block-1) - (lambda + mu)/(4*dy**2)
        Q_ad(n_block, n_block) = Q_ad(n_block, n_block) - (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)

        ! print*, "Q_ad:"
        ! call aff_mat(Q_ad)

        P1_ad = P1

        P1_ad(1, 2:6) = P1_ad(1, 2:6) + (lambda + mu)/(4*dx*dy)*(/-3, 0, 4, 0, -1/)
        P1_ad(2, 1:5) = P1_ad(2, 1:5) + (lambda*(lambda + mu))/(4*dx*dy)*(/-3, 0, 4, 0, -1/)
        P1_ad(n_block-1, n_block-4:n_block) = P1_ad(n_block-1, n_block-4:n_block) + (lambda + mu)/(4*dx*dy)*(/1, 0, -4, 0, 3/)
        P1_ad(n_block, n_block-5:n_block-1) = P1_ad(n_block, n_block-5:n_block-1) + (lambda*(lambda + mu))/(4*dx*dy)*(/1, 0, -4, 0, 3/)

        ! print*, "P1_ad:"
        ! call aff_mat(P1_ad)

        ! remplissage de l'avant dernière ligne

        A(n_block*(ny-2)+1:n_block*(ny-1), n_block*(ny-4)+1:n_block*(ny-3)) = PM2
        A(n_block*(ny-2)+1:n_block*(ny-1), n_block*(ny-3)+1:n_block*(ny-2)) = M1
        A(n_block*(ny-2)+1:n_block*(ny-1), n_block*(ny-2)+1:n_block*(ny-1)) = Q_ad
        A(n_block*(ny-2)+1:n_block*(ny-1), n_block*(ny-1)+1:n_block*ny) = P1_ad

        !modif des matrices de base pour la dernière lignes

        Q_d = Q

        Q_d(1, 1:3) = (/(-5*lambda - mu)/(4*dy**2) - 2*(lambda + 2*mu)/dx**2, 3*lambda/(dx*dy), 2*(lambda + 2*mu)/dx**2/)
        Q_d(2, 1:4) = (/3*mu/(dx*dy), (-5*lambda*(lambda + mu) - 8*(lambda + 2*mu)**2 + 12*lambda**2)/(4*(lambda + 2*mu)*dy**2) - 2*mu/dx**2, 0._PR, 2*mu/dx**2/)

        do i = 3, 2*(nx-1)-1, 2
            Q_d(i, i-1) = (-3*lambda + mu)/(4*dx*dy)
            Q_d(i, i+3) = -(-3*lambda + mu)/(4*dx*dy)
            Q_d(i+1, i-2) = (lambda - 3*mu)/(4*dx*dy)
            Q_d(i+1, i+2) = -(lambda - 3*mu)/(4*dx*dy)
        end do

        Q_d(n_block-1, n_block-3:n_block) = (/2*(lambda + 2*mu)/dx**2, 0._PR, (-5*lambda - mu)/(4*dy**2) - 2*(lambda + 2*mu)/dx**2, -3*lambda/(dx*dy)/)
        Q_d(n_block, n_block-2:n_block) = (/2*mu/dx**2, -3*mu/(dx*dy), (-5*lambda*(lambda + mu) - 8*(lambda + 2*mu)**2 + 12*lambda**2)/(4*(lambda + 2*mu)*dy**2) - 2*mu/dx**2/)
        
        ! print*, "Q_d :"
        ! call aff_mat(Q_d)

        M1_d = 2*M - 4*N

        M1_d(1, 1:2) = (/(11*lambda + 3*mu)/(4*dy**2), -4*lambda/(dx*dy)/)
        M1_d(2, 1:2) = (/-4*mu/(dx*dy), (11*lambda*(lambda + mu) + 8*(lambda + 2*mu)**2 - 16*lambda**2)/(4*(lambda + 2*mu)*dy**2)/)

        M1_d(n_block-1, n_block-1:n_block) = (/(11*lambda + 3*mu)/(4*dy**2), 4*lambda/(dx*dy)/)
        M1_d(n_block, n_block-1:n_block) = (/4*mu/(dx*dy), (11*lambda*(lambda + mu) + 8*(lambda + 2*mu)**2 - 16*lambda**2)/(4*(lambda + 2*mu)*dy**2)/)
        
        ! print*, "M1_d :"
        ! call aff_mat(M1_d)
        
        M2_d = N

        M2_d(1, 1:2) = (/(-7*lambda - 3*mu)/(4*dy**2), lambda/(dx*dy)/)
        M2_d(2, 1:2) = (/mu/(dx*dy), (-7*lambda*(lambda + mu) + 4*lambda**2)/(4*(lambda + 2*mu)*dy**2)/)

        M2_d(n_block-1, n_block-1:n_block) = (/(-7*lambda - 3*mu)/(4*dy**2), -lambda/(dx*dy)/)
        M2_d(n_block, n_block-1:n_block) = (/-mu/(dx*dy), (-7*lambda*(lambda + mu) + 4*lambda**2)/(4*(lambda + 2*mu)*dy**2)/)

        ! print*, "M2_d :"
        ! call aff_mat(M2_d)

        M3_d = 0

        M3_d(1, 1) = (lambda + mu)/(4*dy**2)
        M3_d(2, 2) = (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)

        M3_d(n_block-1, n_block-1) = (lambda + mu)/(4*dy**2)
        M3_d(n_block, n_block) = (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)

        ! print*, "M3_d :"
        ! call aff_mat(M3_d)

        ! remplissage de la dernière ligne

        A(n_block*(ny-1)+1:n_block*ny, n_block*(ny-4)+1:n_block*(ny-3)) = M3_d
        A(n_block*(ny-1)+1:n_block*ny, n_block*(ny-3)+1:n_block*(ny-2)) = M2_d
        A(n_block*(ny-1)+1:n_block*ny, n_block*(ny-2)+1:n_block*(ny-1)) = M1_d
        A(n_block*(ny-1)+1:n_block*ny, n_block*(ny-1)+1:n_block*ny) = Q_d

        ! print*, "A :"
        ! call aff_mat(A)

        b = 0
        ! do i = 2, n_block*(ny-1), 2
        !     b(i) = 0
        ! end do
        do i = n_block*(ny-1)+2, n_block*ny, 2
            b(i) = - (2*f_app)/dy
        end do

    end subroutine init_sl_cl

    subroutine aff_mat(M)
        real(PR), dimension(:,:), intent(in) :: M
        integer nl, i
        nl = SIZE(M, 1)

        do i = 1, nl
            if (nl == 10) then
                print '(F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1)', M(i, :)
            else
                print*, M(i, :)
            end if
        end do
        
    end subroutine aff_mat

end module schema_DF
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
        real(PR), dimension(2*nx, 2*nx) :: M, N, Q, PM2, M1, P1, Q_1, P1_1, P2_1, PM3, Q_2, M1_2, Q_ad, P1_ad, Q_d, M1_d, M2_d


        n_block = 2*nx

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

        ! print*, "PM2 :"
        ! call aff_mat(PM2)

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

        Q_1(1, 1:3) = (/0._PR, 0._PR, 2*(lambda + 2*mu)/dx**2/)
        Q_1(2, 1:4) = (/0._PR, 0._PR, 0._PR, 2*mu/dx**2/)

        do i = 3, 2*(nx-1)-1, 2
            Q_1(i, i-1) = -(-3*lambda + mu)/(4*dx*dy)
            Q_1(i, i+3) = (-3*lambda + mu)/(4*dx*dy)
            Q_1(i+1, i-2) = -(lambda - 3*mu)/(4*dx*dy)
            Q_1(i+1, i+2) = (lambda - 3*mu)/(4*dx*dy)
        end do

        Q_1(n_block-1, n_block-3:n_block) = (/2*(lambda + 2*mu)/dx**2, 0._PR, 0._PR, 0._PR/)
        Q_1(n_block, n_block-2:n_block) = (/2*mu/dx**2, 0._PR, 0._PR/)

        ! print*, "Q_1 :"
        ! call aff_mat(Q_1)

        P1_1 = 2*M + 4*N

        P1_1(1, 1:2) = (/(11*lambda + 3*mu)/(4*dy**2), 4*lambda/(dx*dy)/)
        P1_1(2, 1:2) = (/4*mu/(dx*dy), (11*lambda*(lambda + mu) + 8*(lambda + 2*mu)**2 - 16*lambda**2)/(4*(lambda + 2*mu)*dy**2)/)

        P1_1(n_block-1, n_block-1:n_block) = (/(11*lambda + 3*mu)/(4*dy**2), -4*lambda/(dx*dy)/)
        P1_1(n_block, n_block-1:n_block) = (/-4*mu/(dx*dy), (11*lambda*(lambda + mu) + 8*(lambda + 2*mu)**2 - 16*lambda**2)/(4*(lambda + 2*mu)*dy**2)/)

        ! print*, "P1_1 :"
        ! call aff_mat(P1_1)

        P2_1 = -N

        P2_1(1, 1:2) = (/(-7*lambda - 3*mu)/(4*dy**2), -lambda/(dx*dy)/)
        P2_1(2, 1:2) = (/-mu/(dx*dy), (-7*lambda*(lambda + mu) + 4*lambda**2)/(4*(lambda + 2*mu)*dy**2)/)

        P2_1(n_block-1, n_block-1:n_block) = (/(-7*lambda - 3*mu)/(4*dy**2), lambda/(dx*dy)/)
        P2_1(n_block, n_block-1:n_block) = (/mu/(dx*dy), (-7*lambda*(lambda + mu) + 4*lambda**2)/(4*(lambda + 2*mu)*dy**2)/)

        ! print*, "P2_1 :"
        ! call aff_mat(P2_1)

        PM3 = 0

        PM3(1, 1) = (lambda + mu)/(4*dy**2)
        PM3(2, 2) = (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)

        PM3(n_block-1, n_block-1) = (lambda + mu)/(4*dy**2)
        PM3(n_block, n_block) = (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)

        ! print*, "PM3 :"
        ! call aff_mat(PM3)

        ! remplissage de la première ligne

        A(1:n_block, 1:n_block) = Q_1
        A(1:n_block, n_block+1:n_block*2) = P1_1
        A(1:n_block, n_block*2+1:n_block*3) = P2_1
        A(1:n_block, n_block*3+1:n_block*4) = PM3

        ! remplissage de la deuxième ligne avec les conditions obtenues grâce au schéma décentré (amont)

        Q_2 = Q

        Q_2(1, 1) = Q_2(1, 1) - (lambda + mu)/(4*dy**2)
        Q_2(2, 2) = Q_2(2, 2) - (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)
        Q_2(n_block-1, n_block-1) = Q_2(n_block-1, n_block-1) - (lambda + mu)/(4*dy**2)
        Q_2(n_block, n_block) = Q_2(n_block, n_block) - (lambda*(lambda + mu))/(4*(lambda + 2*mu)*dy**2)

        ! print*, "Q_2 :"
        ! call aff_mat(Q_2)

        M1_2 = M1
        M1_2(1:2,1:2) = 0
        M1_2(n_block-1:n_block,n_block-1:n_block) = 0

        M1_2(1, 2:6) = M1_2(1, 2:6) + (lambda + mu)/(4*dx*dy)*(/0, 0, -4, 0, 1/)
        M1_2(2, 1:5) = M1_2(2, 1:5) + (lambda + mu)/(4*dx*dy)*(/0, 0, -4, 0, 1/) ! il y a eu une modification ici (un facteur lambda)
        M1_2(n_block-1, n_block-4:n_block) = M1_2(n_block-1, n_block-4:n_block) + (lambda + mu)/(4*dx*dy)*(/-1, 0, 4, 0, 0/)
        M1_2(n_block, n_block-5:n_block-1) = M1_2(n_block, n_block-5:n_block-1) + (lambda + mu)/(4*dx*dy)*(/-1, 0, 4, 0, 0/)

        ! print*, "M1_2 :"
        ! call aff_mat(M1_2)

        A(n_block+1:n_block*2, 1:n_block) = M1_2
        A(n_block+1:n_block*2, n_block+1:n_block*2) = Q_2
        A(n_block+1:n_block*2, n_block*2+1:n_block*3) = P1
        A(n_block+1:n_block*2, n_block*3+1:n_block*4) = PM2

        ! remplissage du coeur de A

        A(n_block*2+1:n_block*3, n_block+1:n_block*2) = M1
        A(n_block*2+1:n_block*3, n_block*2+1:n_block*3) = Q
        A(n_block*2+1:n_block*3, n_block*3+1:n_block*4) = P1
        A(n_block*2+1:n_block*3, n_block*4+1:n_block*5) = PM2

        do i = 4, ny - 2
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
        P1_ad(2, 1:5) = P1_ad(2, 1:5) + (lambda + mu)/(4*dx*dy)*(/-3, 0, 4, 0, -1/) ! meme modif que plus haut
        P1_ad(n_block-1, n_block-4:n_block) = P1_ad(n_block-1, n_block-4:n_block) + (lambda + mu)/(4*dx*dy)*(/1, 0, -4, 0, 3/)
        P1_ad(n_block, n_block-5:n_block-1) = P1_ad(n_block, n_block-5:n_block-1) + (lambda + mu)/(4*dx*dy)*(/1, 0, -4, 0, 3/)

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

        ! remplissage de la dernière ligne

        A(n_block*(ny-1)+1:n_block*ny, n_block*(ny-4)+1:n_block*(ny-3)) = PM3
        A(n_block*(ny-1)+1:n_block*ny, n_block*(ny-3)+1:n_block*(ny-2)) = M2_d
        A(n_block*(ny-1)+1:n_block*ny, n_block*(ny-2)+1:n_block*(ny-1)) = M1_d
        A(n_block*(ny-1)+1:n_block*ny, n_block*(ny-1)+1:n_block*ny) = Q_d

        ! print*, "A :"
        ! call aff_mat(A)

        b = 0
        do i = 1, nx
            b(n_block*(ny-1)+2*i) = (2*f_rep(x(i)))/dy
            ! print*, (2*f_rep(x(i)))/dy
        end do
        

    end subroutine init_sl_cl

    subroutine calc_contrainte(type_contrainte, sigma)
        integer, intent(in) :: type_contrainte
        real(PR), dimension(nx*ny), intent(out) :: sigma
        integer :: n_block, i, j

        n_block = 2*nx

        select case(type_contrainte)
        case(1)

            sigma(1) = (lambda +2*mu)*(u(3)-u(1))/dx + lambda*(u(n_block+2)-u(2))/dy
            do i = 2,nx-1
                sigma(i) = (lambda +2*mu)*(u(2*i+1)-u(2*(i-2)+1))/(2*dx) + lambda*(u(n_block+2*i)-u(2*i))/dy
            end do
            sigma(nx) = (lambda +2*mu)*(u(2*(nx-1)+1)-u(2*(nx-2)+1))/dx + lambda*(u(n_block+2*nx)-u(2*nx))/dy

            do j = 2,ny-1
                sigma(nx*(j-1)+1) = (lambda +2*mu)*(u(n_block*(j-1)+3)-u(n_block*(j-1)+1))/dx + lambda*(u(n_block*(j)+2)-u(n_block*(j-2)+2))/(2*dy)
                do i = 2,nx-1
                    sigma(nx*(j-1)+i) = (lambda +2*mu)*(u(n_block*(j-1)+2*i+1)-u(n_block*(j-1)+2*(i-2)+1))/(2*dx) + lambda*(u(n_block*(j)+2*i)-u(n_block*(j-2)+2*i))/(2*dy)
                end do
                sigma(nx*(j-1)+nx) = (lambda +2*mu)*(u(n_block*(j-1)+2*(nx-1)+1)-u(n_block*(j-1)+2*(nx-2)+1))/dx + lambda*(u(n_block*(j)+2*nx)-u(n_block*(j-2)+2*nx))/(2*dy)
            end do

            sigma(nx*(ny-1)+1) = (lambda +2*mu)*(u(n_block*(ny-1)+3)-u(n_block*(ny-1)+1))/dx + lambda*(u(n_block*(ny-1)+2)-u(n_block*(ny-2)+2))/dy
            do i = 2,nx-1
                sigma(nx*(ny-1)+i) = (lambda +2*mu)*(u(n_block*(ny-1)+2*i+1)-u(n_block*(ny-1)+2*(i-2)+1))/(2*dx) + lambda*(u(n_block*(ny-1)+2*i)-u(n_block*(ny-2)+2*i))/dy
            end do
            sigma(nx*(ny-1)+nx) = (lambda +2*mu)*(u(n_block*(ny-1)+2*(nx-1)+1)-u(n_block*(ny-1)+2*(nx-2)+1))/dx + lambda*(u(n_block*(ny-1)+2*nx)-u(n_block*(ny-2)+2*nx))/dy
            
        case(2)

            sigma(1) = mu*((u(n_block+1)-u(1))/dy + (u(4)-u(2))/dx)
            do i = 2,nx-1
                sigma(i) = mu*((u(n_block+2*(i-1)+1)-u(2*(i-1)+1))/dy + (u(2*(i+1))-u(2*(i-1)))/(2*dx))
            end do
            sigma(nx) = mu*((u(n_block+2*(nx-1)+1)-u(2*(nx-1)+1))/dy + (u(2*nx)-u(2*(nx-1)))/dx)
            
            do j = 2,ny-1
                sigma(nx*(j-1)+1) = mu*((u(n_block*j+1)-u(n_block*(j-2)+1))/(2*dy) + (u(n_block*(j-1)+4)-u(n_block*(j-1)+2))/dx)
                do i = 2,nx-1
                    sigma(nx*(j-1)+i) = mu*((u(n_block*j+2*(i-1)+1)-u(n_block*(j-2)+2*(i-1)+1))/(2*dy) + (u(n_block*(j-1)+2*(i+1))-u(n_block*(j-1)+2*(i-1)))/(2*dx))
                end do
                sigma(nx*(j-1)+nx) = mu*((u(n_block*j+2*(nx-1)+1)-u(n_block*(j-2)+2*(nx-1)+1))/(2*dy) + (u(n_block*(j-1)+2*nx)-u(n_block*(j-1)+2*(nx-1)))/dx)
            end do

            sigma(nx*(ny-1)+1) = mu*((u(n_block*(ny-1)+1)-u(n_block*(ny-2)+1))/dy + (u(n_block*(ny-1)+4)-u(n_block*(ny-1)+2))/dx)
            do i = 2,nx-1
                sigma(nx*(ny-1)+i) = mu*((u(n_block*(ny-1)+2*(i-1)+1)-u(n_block*(ny-2)+2*(i-1)+1))/dy + (u(n_block*(ny-1)+2*(i+1))-u(n_block*(ny-1)+2*(i-1)))/(2*dx))
            end do
            sigma(nx*(ny-1)+nx) = mu*((u(n_block*(ny-1)+2*(nx-1)+1)-u(n_block*(ny-2)+2*(nx-1)+1))/dy + (u(n_block*(ny-1)+2*nx)-u(n_block*(ny-1)+2*(nx-1)))/dx)
            
        case(3)

            sigma(1) = lambda*(u(3)-u(1))/dx + (lambda +2*mu)*(u(n_block+2)-u(2))/dy
            do i = 2,nx-1
                sigma(i) = lambda*(u(2*i+1)-u(2*(i-2)+1))/(2*dx) + (lambda +2*mu)*(u(n_block+2*i)-u(2*i))/dy
            end do
            sigma(nx) = lambda*(u(2*(nx-1)+1)-u(2*(nx-2)+1))/dx + (lambda +2*mu)*(u(n_block+2*nx)-u(2*nx))/dy

            do j = 2,ny-1
                sigma(nx*(j-1)+1) = lambda*(u(n_block*(j-1)+3)-u(n_block*(j-1)+1))/dx + (lambda +2*mu)*(u(n_block*(j)+2)-u(n_block*(j-2)+2))/(2*dy)
                do i = 2,nx-1
                    sigma(nx*(j-1)+i) = lambda*(u(n_block*(j-1)+2*i+1)-u(n_block*(j-1)+2*(i-2)+1))/(2*dx) + (lambda +2*mu)*(u(n_block*(j)+2*i)-u(n_block*(j-2)+2*i))/(2*dy)
                end do
                sigma(nx*(j-1)+nx) = lambda*(u(n_block*(j-1)+2*(nx-1)+1)-u(n_block*(j-1)+2*(nx-2)+1))/dx + (lambda +2*mu)*(u(n_block*(j)+2*nx)-u(n_block*(j-2)+2*nx))/(2*dy)
            end do

            sigma(nx*(ny-1)+1) = lambda*(u(n_block*(ny-1)+3)-u(n_block*(ny-1)+1))/dx + (lambda +2*mu)*(u(n_block*(ny-1)+2)-u(n_block*(ny-2)+2))/dy
            do i = 2,nx-1
                sigma(nx*(ny-1)+i) = lambda*(u(n_block*(ny-1)+2*i+1)-u(n_block*(ny-1)+2*(i-2)+1))/(2*dx) + (lambda +2*mu)*(u(n_block*(ny-1)+2*i)-u(n_block*(ny-2)+2*i))/dy
            end do
            sigma(nx*(ny-1)+nx) = lambda*(u(n_block*(ny-1)+2*(nx-1)+1)-u(n_block*(ny-1)+2*(nx-2)+1))/dx + (lambda +2*mu)*(u(n_block*(ny-1)+2*nx)-u(n_block*(ny-2)+2*nx))/dy 

        end select
    
        
    end subroutine calc_contrainte

    subroutine calc_eps(type_deformation, eps)
        integer, intent(in) :: type_deformation
        real(PR), dimension(nx*ny), intent(out) :: eps
        integer :: n_block, i, j

        n_block = 2*nx

        select case(type_deformation)
        case(1)

            eps(1) = (u(3)-u(1))/dx
            do i = 2,nx-1
                eps(i) = (u(2*i+1)-u(2*(i-2)+1))/(2*dx)
            end do
            eps(nx) = (u(2*(nx-1)+1)-u(2*(nx-2)+1))/dx

            do j = 2,ny-1
                eps(nx*(j-1)+1) = (u(n_block*(j-1)+3)-u(n_block*(j-1)+1))/dx
                do i = 2,nx-1
                    eps(nx*(j-1)+i) = (u(n_block*(j-1)+2*i+1)-u(n_block*(j-1)+2*(i-2)+1))/(2*dx)
                end do
                eps(nx*(j-1)+nx) = (u(n_block*(j-1)+2*(nx-1)+1)-u(n_block*(j-1)+2*(nx-2)+1))/dx
            end do

            eps(nx*(ny-1)+1) = (u(n_block*(ny-1)+3)-u(n_block*(ny-1)+1))/dx
            do i = 2,nx-1
                eps(nx*(ny-1)+i) = (u(n_block*(ny-1)+2*i+1)-u(n_block*(ny-1)+2*(i-2)+1))/(2*dx)
            end do
            eps(nx*(ny-1)+nx) = (u(n_block*(ny-1)+2*(nx-1)+1)-u(n_block*(ny-1)+2*(nx-2)+1))/dx
            
        case(2)

            eps(1) = 0.5_PR*((u(n_block+1)-u(1))/dy + (u(4)-u(2))/dx)
            do i = 2,nx-1
                eps(i) = 0.5_PR*((u(n_block+2*(i-1)+1)-u(2*(i-1)+1))/dy + (u(2*(i+1))-u(2*(i-1)))/(2*dx))
            end do
            eps(nx) = 0.5_PR*((u(n_block+2*(nx-1)+1)-u(2*(nx-1)+1))/dy + (u(2*nx)-u(2*(nx-1)))/dx)
            
            do j = 2,ny-1
                eps(nx*(j-1)+1) = 0.5_PR*((u(n_block*j+1)-u(n_block*(j-2)+1))/(2*dy) + (u(n_block*(j-1)+4)-u(n_block*(j-1)+2))/dx)
                do i = 2,nx-1
                    eps(nx*(j-1)+i) = 0.5_PR*((u(n_block*j+2*(i-1)+1)-u(n_block*(j-2)+2*(i-1)+1))/(2*dy) + (u(n_block*(j-1)+2*(i+1))-u(n_block*(j-1)+2*(i-1)))/(2*dx))
                end do
                eps(nx*(j-1)+nx) = 0.5_PR*((u(n_block*j+2*(nx-1)+1)-u(n_block*(j-2)+2*(nx-1)+1))/(2*dy) + (u(n_block*(j-1)+2*nx)-u(n_block*(j-1)+2*(nx-1)))/dx)
            end do

            eps(nx*(ny-1)+1) = 0.5_PR*((u(n_block*(ny-1)+1)-u(n_block*(ny-2)+1))/dy + (u(n_block*(ny-1)+4)-u(n_block*(ny-1)+2))/dx)
            do i = 2,nx-1
                eps(nx*(ny-1)+i) = 0.5_PR*((u(n_block*(ny-1)+2*(i-1)+1)-u(n_block*(ny-2)+2*(i-1)+1))/dy + (u(n_block*(ny-1)+2*(i+1))-u(n_block*(ny-1)+2*(i-1)))/(2*dx))
            end do
            eps(nx*(ny-1)+nx) = 0.5_PR*((u(n_block*(ny-1)+2*(nx-1)+1)-u(n_block*(ny-2)+2*(nx-1)+1))/dy + (u(n_block*(ny-1)+2*nx)-u(n_block*(ny-1)+2*(nx-1)))/dx)
            
        case(3)

            eps(1) = (u(n_block+2)-u(2))/dy
            do i = 2,nx-1
                eps(i) = (u(n_block+2*i)-u(2*i))/dy
            end do
            eps(nx) = (u(n_block+2*nx)-u(2*nx))/dy

            do j = 2,ny-1
                eps(nx*(j-1)+1) = (u(n_block*(j)+2)-u(n_block*(j-2)+2))/(2*dy)
                do i = 2,nx-1
                    eps(nx*(j-1)+i) = (u(n_block*(j)+2*i)-u(n_block*(j-2)+2*i))/(2*dy)
                end do
                eps(nx*(j-1)+nx) = (u(n_block*(j)+2*nx)-u(n_block*(j-2)+2*nx))/(2*dy)
            end do

            eps(nx*(ny-1)+1) = (u(n_block*(ny-1)+2)-u(n_block*(ny-2)+2))/dy
            do i = 2,nx-1
                eps(nx*(ny-1)+i) = (u(n_block*(ny-1)+2*i)-u(n_block*(ny-2)+2*i))/dy
            end do
            eps(nx*(ny-1)+nx) = (u(n_block*(ny-1)+2*nx)-u(n_block*(ny-2)+2*nx))/dy 

        end select
    
    end subroutine calc_eps

    subroutine aff_mat(M)
        real(PR), dimension(:,:), intent(in) :: M
        integer nl, i
        nl = SIZE(M, 1)

        do i = 1, nl
            if (nl == 12) then
                print '(F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1, 1X, F10.1)', M(i, :)
            else
                print*, M(i, :)
            end if
        end do
        
    end subroutine aff_mat

end module schema_DF
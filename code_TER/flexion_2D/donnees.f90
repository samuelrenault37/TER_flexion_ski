module donnees
    implicit none
    integer, parameter :: PR = 16
    real(PR), parameter :: epsilon = 1d-14, epaisseur = 0.085
    integer :: nx, ny, resol, N, NN, type_contrainte
    real(PR) :: Lx, Ly, Lz, dx, dy, mu, lambda, E, nu, F
    real(PR), dimension(:), allocatable :: u, x, y, b, A_val, sigma
    integer, dimension(:), allocatable :: A_col, A_row
    real(PR), dimension(:,:), allocatable :: A
contains

    subroutine init_para()
        integer :: i, j

        open(unit = 1, file = "parametres.dat", action = "read")
        read(1, *) Lx
        read(1, *) Ly
        read(1, *) Lz
        read(1, *) nx
        read(1, *) ny
        read(1, *) resol
        read(1, *) type_contrainte
        read(1, *) E
        read(1, *) nu
        read(1, *) F
        close(1)
        dx = Lx/(nx-1)
        dy = Ly/(ny-1)
        N = nx*ny*2
        mu = E/(2*(1+nu))
        lambda = (E*nu)/((1+nu)*(1-2*nu))
        allocate(u(N), x(nx), y(ny), b(N),A(N,N), sigma(nx*ny))
        do i = 1, nx
            x(i) = (i-1)*dx
        end do
        do j = 1, ny
            y(j) = (j-1)*dy
        end do
    end subroutine init_para
    
end module donnees
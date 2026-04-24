module donnees
    implicit none
    integer, parameter :: PR = 16
    real(PR), parameter :: epsilon = 1d-14
    integer :: nx, ny, resol, N, n_dirich, NN
    real(PR) :: Lx, Ly, dx, dy, mu, lambda, E, nu, F, f_app
    real(PR), dimension(:), allocatable :: u, x, y, b, A_val
    integer, dimension(:), allocatable :: A_col, A_row
    real(PR), dimension(:,:), allocatable :: A, dirich
contains

    subroutine init_para()
        integer :: i, j

        open(unit = 1, file = "parametres.dat", action = "read")
        read(1, *) Lx
        read(1, *) Ly
        read(1, *) nx
        read(1, *) ny
        read(1, *) resol
        read(1, *) E
        read(1, *) nu
        read(1, *) F
        close(1)
        dx = Lx/(nx-1)
        dy = Ly/ny
        N = nx*ny*2
        mu = E/(2*(1+nu))
        lambda = (E*nu)/((1+nu)*(1-2*nu))
        f_app = F/Lx
        allocate(u(N), x(nx), y(ny), b(N),A(N,N))
        do i = 1, nx
            x(i) = (i-1)*dx
        end do
        do j = 1, ny
            y(j) = j*dy
        end do
    end subroutine init_para

    ! subroutine raffinement(j)
    !     integer, intent(in) :: j
    !     integer :: i

    !     if (allocated(u) .AND. allocated(x).AND. allocated(A) .AND. allocated(b)) then
    !         deallocate(u)
    !         deallocate(x)
    !         deallocate(A)
    !         deallocate(b)
    !     end if
        
    !     nx = nx0*2**j
    !     dx = Lx/(nx+1)

    !     allocate(u(nx))
    !     allocate(x(nx))
    !     allocate(b(nx))
    !     allocate(A(nx,nx))

    !     do i = 1, nx
    !         x(i) = i*dx
    !     end do
        
    ! end subroutine raffinement
    
end module donnees
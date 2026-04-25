module LU
    use const
    implicit none
    
    contains

    subroutine m_to_lu(M, L, U)
        real(PR), dimension(:,:), intent(in) :: M
        real(PR), dimension(:,:), intent(out) :: L, U
        integer :: i, j, N
        L = 0
        U = 0
        N = SIZE(M, 1)

        do i = 1, N
            L(i,i) = 1
            do j = 1, i-1
                L(i,j) = M(i,j)
            end do
            do j = i, N
                U(i,j) = M(i,j)
            end do
        end do
        
    end subroutine m_to_lu

    subroutine lu_decomposition(A, M, bool)
        real(PR), dimension(:,:), intent(in) :: A
        real(PR), dimension(:,:), intent(out) :: M
        logical, intent(out) :: bool
        integer :: N, k, j

        bool = SIZE(M, 1) == SIZE(A, 1) .AND. &
               SIZE(M, 2) == SIZE(A, 2) .AND. &
               SIZE(M, 1) == SIZE(M, 2) ! vérifcation que les matrices sont carrées

        if (bool) then
            N = SIZE(A, 1)
            M = A
            do k = 1, N-1
                do j = k+1, N
                    if (ABS(M(k,k)) > epsilon) then
                        M(j,k) = M(j,k)/M(k,k)
                        M(j,k+1 : n) = M(j,k+1 : n) - M(j,k)*M(k,k+1 : n)
                    else
                        print *, "cette matrice n'admet pas de décompostion LU"
                        bool = .FALSE.
                        return
                    end if
                end do
            end do
        else
            print*, "problème dans les tailles des matrices données en argument"
            return
        end if
    end subroutine lu_decomposition

    subroutine lu_res(M, b, x, bool)
        real(PR), dimension(:,:), intent(in) :: M
        real(PR), dimension(:), intent(in) :: b
        real(PR), dimension(:), intent(out) :: x
        logical, intent(out) :: bool
        real(PR), dimension(:), allocatable :: y
        real(PR), dimension(:,:), allocatable :: L, U
        integer :: N, i, j

        bool = SIZE(M, 1) == SIZE(b, 1) .AND. &
               SIZE(M, 1) == SIZE(M, 2) ! vérifcation que M est carrée

        if (bool) then
            N = SIZE(b, 1)
            allocate(L(N,N))
            allocate(U(N,N))
            allocate(y(N))
            call m_to_lu(M, L, U)

            do i = 1, N
                y(i) = b(i)
                do j = 1, i-1
                    y(i) = y(i) - L(i,j)*y(j)
                end do
            end do

            do i = N, 1, (-1)
                x(i) = y(i)
                do j = i+1, N
                    x(i) = x(i) - U(i,j)*x(j)
                end do
                if (ABS(U(i,i)) > epsilon) then
                    x(i) = x(i)/U(i,i)
                else
                    print *, "systéme non résolvalble"
                    bool = .FALSE.
                    return
                end if
            end do
        end if

    end subroutine lu_res
    
end module LU
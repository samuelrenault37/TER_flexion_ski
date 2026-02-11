module algebre

    use constantes

    implicit none

    !--------------------------
    contains
    !--------------------------

    subroutine print_mat(tab, nom)

        real(PR), dimension(:,:), intent(in)    :: tab 
        character(len=*), intent(in)            :: nom

        integer :: i

        print*, nom
        do i = 1, size(tab,1)
            print*, tab(i,:)
        end do 

    end subroutine print_mat
    
    !--------------------------

    function llt_check(A, L) result(alors)

        real(PR), dimension(:,:), intent(in) ::  A, L
        logical                              :: alors

        integer :: n
        real(PR), dimension(size(L,1), size(L,2)) :: Lt

        alors = .FALSE.
        n = size(A,1)
        Lt = TRANSPOSE(L)

        if (size(A,2)/= n .OR. size(L,1)/= n .OR. size(L,2)/= n) then
            alors = .FALSE.
        end if

        if (MAXVAL(ABS(MATMUL(L,Lt) - A)) <= epsilon1) then 
            alors = .TRUE.
        end if 

    end function llt_check

    !--------------------------

    subroutine llt_factorisation(A, L, alors)

        real(PR), dimension(:,:), intent(in)  :: A
        real(PR), dimension(:,:), intent(out) :: L
        logical, intent(out)                  :: alors

        integer :: n, i, j, k
        real(PR) :: sum1, sum2

        alors = .FALSE.
        n = size(A,1)

        if (size(A,2)== n .AND. size(L,1)== n .AND. size(L,2)== n) then
            alors = .TRUE.

            ! Ajout du test de matrice Symétrique           
            if (MAXVAL(ABS(A - TRANSPOSE(A))) >= epsilon1) then 
                alors = .FALSE.
                return
            end if 

            L = 0._PR 

            do i = 1,n 

                sum1 = 0._PR

                do k = 1, i-1
                    sum1 = sum1 + L(i,k)**2
                end do

                ! Ajout du test de matrice DP
                if ((A(i,i)- sum1) < 0._PR) then
                    alors = .FALSE.
                    return
                end if

                L(i,i) = SQRT(A(i,i)-sum1)

                do j = i+1, n

                    sum2 = 0._PR 

                    do k=1, i-1
                        sum2 = sum2 + L(i,k)*L(j,k)
                    end do 

                    L(j,i) = (A(j,i) - sum2)/L(i,i)
            
                end do 

            end do

        end if

    end subroutine llt_factorisation

    !--------------------------

    subroutine llt_res(L, b, x, alors)

        real(PR), dimension(:,:), intent(in)        :: L 
        real(PR), dimension(:), intent(in)          :: b 
        real(PR), dimension(:), intent(out)         :: x
        logical, intent(out)                        :: alors

        real(PR), dimension(size(L,2)) :: y
        real(PR), dimension(size(L,1), size(L,2)) :: Lt
        integer :: n, i, j

        alors = .FALSE.

        Lt = TRANSPOSE(L)
        n = size(L,2)

        if (size(L,1)== n .AND. size(b)== n .AND. size(x)== n) then
            alors = .TRUE.

            ! Résolution Ly = b
            do i = 1, n 

                y(i) = b(i)

                do j = 1, i-1
                    y(i) = y(i) - L(i,j)*y(j)
                end do

                y(i) =y(i)/L(i,i)

            end do 

            ! Résolution Ltx = y
            do i = n, 1, -1

                x(i) = y(i)

                do j = i+1, n
                    x(i) = x(i) - Lt(i,j)*x(j)
                end do 

                x(i) = x(i)/Lt(i,i)

            end do
        
        end if

    end subroutine llt_res

    !------------------------------------
    ! PARTIE 2
    !------------------------------------


    function lu_check(A,M) result(alors)

        real(PR), dimension(:,:), intent(in) ::  A, M
        logical                              :: alors

        real(PR), dimension(size(A,1), size(A,2)) :: L, U
        integer :: n, i, j

        alors = .TRUE.
        n = size(A,1)

        L = 0._PR
        U = 0._PR

        ! Matrice L
        do i = 1, n
            do j =1, i-1
                L(i,j) = M(i,j)
            end do
            L(i,i) = 1._PR
        end do 

        ! Matrice U
        do i = 1, n
            do j =i, n
                U(i,j) = M(i,j)
            end do
        end do

        if (MAXVAL(ABS(MATMUL(L,U) - A)) <= epsilon1) then 
            alors = .TRUE.
        end if


    end function lu_check

    !--------------------------

    subroutine lu_factorisation(A, M, alors)

        real(PR), dimension(:,:), intent(in)  ::  A
        real(PR), dimension(:,:), intent(out) ::  M
        logical, intent(out)                  :: alors

        integer :: n, j, k

        alors = .TRUE.

        n = size(A,1)

        if (size(A,2) /= n .OR. size(M,1) /= n .OR. size(M,2) /= n) then
            alors = .FALSE.
            return
        end if 

        M = A

        do k = 1, n-1

            if (ABS(M(k,k)) <= epsilon1) then 
                alors = .FALSE.
                return
            end if

            do j = k+1, n

                M(j,k) = M(j,k)/M(k,k)
                M(j,k+1:n) = M(j,k+1:n) - M(j,k)*M(k,k+1:n)

            end do
        end do 


    end subroutine

    !--------------------------

    subroutine lu_res(M, b, x, alors)

        real(PR), dimension(:,:), intent(in)        :: M 
        real(PR), dimension(:), intent(in)          :: b 
        real(PR), dimension(:), intent(out)         :: x
        logical, intent(out)                        :: alors

        real(PR), dimension(size(M,2)) :: y
        real(PR), dimension(size(M,1), size(M,2)) :: L, U
        
        integer :: n, i, j

        alors = .FALSE.
        n = size(M,2)

        if (size(M,1)== n .AND. size(b)== n .AND. size(x)== n) then
            alors = .TRUE.

            L = 0._PR
            U = 0._PR
    
            ! Matrice L
            do i = 1, n
                do j =1, i-1
                    L(i,j) = M(i,j)
                end do
                L(i,i) = 1._PR
            end do 
    
            ! Matrice U
            do i = 1, n
                do j =i, n
                    U(i,j) = M(i,j)
                end do
            end do

            ! Résolution Ly = b 
            do i = 1, n

                y(i) = b(i)

                do j = 1, i-1
                    y(i) = y(i)-L(i,j)*y(j)
                end do 

            end do 

            ! Résolution Ux = y
            do i = n, 1, -1

                if (ABS(U(i,i)) <= epsilon1) then 
                    alors = .FALSE.
                    return
                end if

                x(i) = y(i)

                do j = i+1, n
                    x(i) = x(i)- U(i,j)*x(j)
                end do 

                x(i) = x(i)/U(i,i)

            end do

        end if 

    end subroutine lu_res 

    subroutine construction_matrice(degre_approx, nb_pts, schema, nb_total_pts, delta_x, A)

        integer, intent(in)                                 :: degre_approx
        integer, intent(in)                                 :: nb_pts
        integer, intent(in)                                 :: nb_total_pts
        integer, intent(in)                                 :: schema
        real(PR), intent(in)                                :: delta_x
        real(PR), dimension(:,:), intent(out)  :: A

        integer :: i

        if (size(A,1) /= size(A,2)) then
            return
        end if

        select case(degre_approx)

        case(1)

            select case(nb_pts)

            case(3)

                select case(schema)

                case(1)

                    A = 0._PR

                    ! CL

                    A(1,1) = -2._PR
                    A(1,2) =  1._PR

                    A(nb_total_pts, nb_total_pts) =     -2._PR
                    A(nb_total_pts, nb_total_pts - 1) =  1._PR

                    ! Coeur de la matrice

                    do i = 2, nb_total_pts-1
                            A(i, i-1) =  1._PR 
                            A(i, i)   = -2._PR
                            A(i, i+1) =  1._PR
                    end do

                    A = (-1._PR/(delta_x**2))*A

                end select

            end select

        end select

    end subroutine construction_matrice

end module algebre

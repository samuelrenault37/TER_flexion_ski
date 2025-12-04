module init
    use const
    implicit none
    
contains

  subroutine init_A_b(A, b, h, N)
        integer, intent(in) :: N
        real(PR), intent(in) :: h
        real(PR), dimension(N,N), intent(out) :: A
        real(PR), dimension(N), intent(out) :: b
        integer :: k
        real(PR) :: F, E, I

        E = 1
        F = 1
        I = 1

        A = 0

        ! A
        A(1,1) = 1

        A(2,1) = -1
        A(2,2) =  1

        A(N-1, N-2) =  1
        A(N-1, N-1) = -2
        A(N-1, N)   =  1

        A(N, N-3) = -1
        A(N, N-2) =  3
        A(N, N-1) = -3
        A(N, N)   =  1

        do k=3, N-2
            A(k,k-2) =  1
            A(k,k-1) = -4
            A(k,k)   =  6
            A(k,k+1) = -4
            A(k,k+2) =  1
        end do


        !CL et b
        b = 0*h**4
        b(1) = 0 !u(a)
        b(2) = 0*h !u'(a)
        b(N-1) = 0*h**2 !u''(b)
        b(N) = -F*h**3/(E*I) !u'''(b)

    end subroutine init_A_b

    ! A avec une seule zone non nul par ligne
    subroutine convert_A_CSR(A, A_val, A_col, A_row, N, NN)
        integer, intent(in) :: N, NN
        real(PR), dimension(N,N), intent(in) :: A
        real(PR), dimension(NN), intent(out) :: A_val !toutes les valeurs non nuls de la matrice en ligne
        integer, dimension(NN), intent(out) :: A_col !contient la colonnes de chacune de ses valeurs
        integer, dimension(N+1), intent(out) :: A_row !pointeurs de début de chaque ligne (dans les autres tableaux)
        integer :: i, j, k, r, compteur
        real(PR) :: prec

        k = 1
        r = 2
        compteur = 1
        A_row(1) = compteur
        prec = 0
        
        do i = 1,N
            do j = 1,N
                if(ABS(A(i,j))>epsilon) then
                    A_val(k) = A(i,j)
                    A_col(k) = j
                    k = k +1
                    compteur = compteur + 1
                else
                    if (ABS(prec)>epsilon) then
                        A_row(r) = compteur
                        r = r + 1
                    end if
                end if
                prec = A(i,j)
            end do
        end do

        if (r < N+2) then
            A_row(N+1) = compteur
        end if
        
    end subroutine convert_A_CSR

end module init
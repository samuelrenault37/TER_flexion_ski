module fonct
    use const
    implicit none
    
    contains
    
    ! calcul le produit matriciel entre une matrice sous format CSR et un vecteur (ne tenant donc pas compte des 0 de la matrice)
    subroutine matvec_csr(N, NN, val, col, row, x, y)
        integer, intent(in) :: N, NN
        real(PR), dimension(NN), intent(in) :: val
        integer, dimension(NN), intent(in) :: col
        integer, dimension(N+1), intent(in) :: row
        real(PR), dimension(N), intent(in) :: x
        real(PR), dimension(N), intent(out) :: y
        integer :: i, j

        y = 0

        do i = 1, N
            do j = row(i), row(i+1)-1
                y(i) = y(i) + val(j) * x(col(j))
            end do
        end do
    end subroutine matvec_csr

    ! écrit les valeurs utiles dans un fichier texte pour pouvoir les exploiter avec gnuplot (tracé de courbe)
    subroutine write_in_file(file, x, N, h, a)
        character(len=*), intent(in) :: file
        real(PR), dimension(:), intent(in) ::  x
        integer, intent(in) :: N
        real(PR), intent(in) :: h, a
        integer :: i

        open(unit = 1, file = file, action = "write")
        write (1, '(A)') "#abscisse               ordonnée"
        do i = 1,N
            write (1, *) a + (i-1)*(h), x(i)
        end do
        close(1)
    end subroutine write_in_file

    subroutine recup_NN(A, N, NN)
        integer, intent(in) :: N
        real(PR), dimension(N,N), intent(in) :: A
        integer, intent(out) :: NN
        integer :: i, j

        NN = 0
        do i = 1,N
            do j = 1,N
                if (ABS(A(i,j))>epsilon) then
                    NN = NN + 1
                end if
            end do
        end do
        
    end subroutine recup_NN

    subroutine aff_matrice(A, N)
        integer, intent(in) :: N
        real(PR), dimension(N,N), intent(in) :: A
        integer :: i
        do i = 1,N
            print *, A(i,:)
        end do
    end subroutine aff_matrice

end module fonct
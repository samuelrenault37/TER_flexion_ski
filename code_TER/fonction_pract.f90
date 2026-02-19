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
    subroutine write_in_file(file, x, N, h, deric_x, deric_y)
        character(len=*), intent(in) :: file
        real(PR), dimension(:), intent(in) ::  x, deric_x, deric_y
        integer, intent(in) :: N
        real(PR), intent(in) :: h
        integer :: i

        open(unit = 1, file = file, action = "write")
        write (1, '(A)') "#abscisse               ordonnée"
        write (1, *) deric_x(1), deric_y(1)
        do i = 1,N
            write (1, *) borne_a + (i)*(h), x(i)
        end do
        write (1, *) deric_x(2), deric_y(2)
        close(1)
    end subroutine write_in_file

    subroutine write_in_file_2D(file, n_d1, x_d2, N, h, deric_x, deric_y)
        character(len=*), intent(in) :: file
        real(PR), dimension(:), intent(in) ::  x_d2, deric_x, deric_y
        integer, intent(in) :: N, n_d1
        real(PR), intent(in) :: h
        integer :: i
        character(len=3) :: cd1

        write(cd1,'(i1)') n_d1

        open(unit = 1, file = file//cd1//".dat")

        write (1, '(A)') "#abscisse               ordonnée"
        write (1, *) deric_x(1), deric_y(1)
        do i = 1,N
            write (1, *) borne_a_d2 + (i)*(h), x_d2(i)
        end do
        write (1, *) deric_x(2), deric_y(2)
        close(1)
    end subroutine write_in_file_2D

    subroutine write_exp_val(file)
        character(len=*), intent(in) :: file
        integer :: N_exp
        real(PR), dimension(:), allocatable  ::  x_exp, y_exp
        integer :: i

        N_exp = 26

        allocate(x_exp(N_exp))
        allocate(y_exp(N_exp))

        x_exp = (/0.0002565, 0.04622, 0.1030, 0.1570, 0.1980, 0.2500, 0.3010, 0.3530, 0.4030, 0.4560, 0.5050, 0.5570, 0.6050, 0.6550, 0.7050, 0.7540, 0.8050, 0.8540, 0.9040, 0.9540, 1.004, 1.055, 1.104, 1.157, 1.210, 1.226/)

        y_exp = (/0.000387, -0.002559, -0.006685, -0.01007, -0.01081, -0.01214, -0.01493, -0.01722, -0.0193, -0.02222, -0.02159, -0.0218, -0.02138, -0.02138, -0.02034, -0.01992, -0.01909, -0.01759, -0.01536, -0.01332, -0.0109, -0.007748, -0.002737, 0.001347, 0.005059, 0.005039/)

        open(unit = 1, file = file, action = "write")
        write (1, '(A)') "#abscisse               ordonnée"
        do i = 1,N_exp
            write (1, *) x_exp(i), y_exp(i)
        end do
        close(1)

        deallocate(x_exp)
        deallocate(y_exp)

    end subroutine write_exp_val


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

    subroutine free_syst_lin(sl)
        type(syst_lin), intent(inout) :: sl
        deallocate(sl%b)
        deallocate(sl%A)
        deallocate(sl%deric_x)
        deallocate(sl%deric_y)
    end subroutine free_syst_lin

    subroutine free_CSR(sl)
        type(syst_lin), intent(inout) :: sl
        deallocate(sl%A_val)
        deallocate(sl%A_col)
        deallocate(sl%A_row)
    end subroutine free_CSR

end module fonct
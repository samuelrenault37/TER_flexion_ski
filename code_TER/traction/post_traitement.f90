module post_traitement
    use donnees
    use fonctions
    implicit none
    
contains

    subroutine ecrit_sol_num()
        integer :: i, j

        open(unit = 1, file = "../../donnees/2D/traction/res_sol_num.dat", action = "write")
        do i = 1, n_dirich
            write (1,*) dirich(i,1), dirich(i, 2)
        end do
        do j = 1, ny
            do i = 1, nx
                write (1,*) x(i) + u((j-1)*2*nx+2*i-1), y(j) + u((j-1)*2*nx+2*i)
            end do
        end do
        close(1)

    end subroutine ecrit_sol_num
 
end module post_traitement
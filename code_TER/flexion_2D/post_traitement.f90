module post_traitement
    use donnees
    use fonctions
    use, intrinsic :: ieee_arithmetic
    implicit none
    
contains

    subroutine ecrit_sol_num()
        integer :: i, j
        ! real(PR) :: maxi, NaN

        open(unit = 1, file = "../../donnees/2D/flexion/res_sol_num.dat", action = "write")
        do j = 1, ny
            do i = 1, nx
                write (1,*) x(i) + u((j-1)*2*nx+2*i-1), y(j) + u((j-1)*2*nx+2*i), sigma(nx*(j-1)+i)
            end do
        end do

        ! maxi = MAXVAL(ABS(u))
        ! maxi = maxi + 1

        ! NaN = ieee_value(NaN, ieee_quiet_nan)

        ! do j = 0, 100
        !     do i = 1, nx
        !         write (1,*) x(i), Ly-(maxi*j)/100, NaN
        !     end do
        ! end do


        close(1)

    end subroutine ecrit_sol_num
 
end module post_traitement
module post_traitement
    use donnees
    use fonctions
    use schema_DF
    implicit none
    
contains

    subroutine ecrit_sol_num_posi
        integer :: i, j

        open(unit = 1, file = "../../donnees/2D/flexion/res_sol_num.dat", action = "write")
        do j = 1, ny
            do i = 1, nx
                write (1,*) x(i) + u((j-1)*2*nx+2*i-1), y(j) + u((j-1)*2*nx+2*i)
            end do
        end do

        close(1)

    end subroutine ecrit_sol_num_posi


    subroutine ecrit_sol_num_u
        integer :: i, j

        open(unit = 1, file = "../../donnees/2D/flexion/res_sol_num_u.dat", action = "write")
        do j = 1, ny
            do i = 74, 78
                write (1,*) x(i) + u((j-1)*2*nx+2*i-1), y(j) + u((j-1)*2*nx+2*i), u((j-1)*2*nx+2*i-1)
            end do
        end do

        close(1)

    end subroutine ecrit_sol_num_u


    subroutine ecrit_sol_num_v
        integer :: i, j

        open(unit = 1, file = "../../donnees/2D/flexion/res_sol_num_v.dat", action = "write")
        do j = 1, ny
            do i = 74, 78
                write (1,*) x(i) + u((j-1)*2*nx+2*i-1), y(j) + u((j-1)*2*nx+2*i), u((j-1)*2*nx+2*i)
            end do
        end do

        close(1)

    end subroutine ecrit_sol_num_v


    subroutine ecrit_sol_num_cont_pt
        integer :: i, j

        open(unit = 1, file = "../../donnees/2D/flexion/res_sol_num_cont_pt.dat", action = "write")
        do j = 1, ny
            do i = 2, nx-1
                write (1,*) x(i) + u((j-1)*2*nx+2*i-1), y(j) + u((j-1)*2*nx+2*i), sigma(nx*(j-1)+i)
            end do
        end do

        close(1)

    end subroutine ecrit_sol_num_cont_pt


    subroutine ecrit_sol_num_zoom
        integer :: i, j

        open(unit = 1, file = "../../donnees/2D/flexion/res_sol_num_zoom.dat", action = "write")
        do j = 1, ny
            do i = 74, 78
                write (1,*) x(i) + u((j-1)*2*nx+2*i-1), y(j) + u((j-1)*2*nx+2*i), sigma(nx*(j-1)+i)
            end do
        end do

        close(1)

    end subroutine ecrit_sol_num_zoom


    subroutine ecrit_sol_num_eps
        integer :: i, j

        open(unit = 1, file = "../../donnees/2D/flexion/res_sol_num_eps.dat", action = "write")
        do j = 1, ny
            do i = 74, 78
                write (1,*) x(i) + u((j-1)*2*nx+2*i-1), y(j) + u((j-1)*2*nx+2*i), eps(nx*(j-1)+i)
            end do
        end do

        close(1)

    end subroutine ecrit_sol_num_eps


    subroutine ecrit_resume_zoom
        real(PR), dimension(nx*ny) :: eps_xx, eps_xy, eps_yy, sigma_xx, sigma_xy, sigma_yy
        integer :: i, j

        call calc_contrainte(1, sigma_xx)
        call calc_contrainte(2, sigma_xy)
        call calc_contrainte(3, sigma_yy)

        call calc_eps(1, eps_xx)
        call calc_eps(2, eps_xy)
        call calc_eps(3, eps_yy)

        open(unit = 1, file = "../../donnees/2D/flexion/resume_sol_num_.dat", action = "write")
        write (1,*) "longeur caractéristique utilisé pour adimentionaler : 0.0315 m (longueur de la zone étudiée)"
        write (1,*) " "
        write (1,*) "---------------------------------------------------------"
        do j = 2, ny-1
            do i = 75, 77
                write (1,*) " "
                write (1,*) "posi x : ", x(i) + u((j-1)*2*nx+2*i-1), "en m"
                write (1,*) "posi y : ", y(j) + u((j-1)*2*nx+2*i), "en m"
                write (1,*) "depl u : ", u((j-1)*2*nx+2*i-1), "en m"
                write (1,*) "depl u adimentionalisé : ", u((j-1)*2*nx+2*i-1)/0.0315_PR
                write (1,*) "delp v : ", u((j-1)*2*nx+2*i), "en m"
                write (1,*) "delp v adimentionalisé : ", u((j-1)*2*nx+2*i)/0.0315_PR
                write (1,*) "eps_xx : ", eps_xx(nx*(j-1)+i)
                write (1,*) "eps_xy : ", eps_xy(nx*(j-1)+i)
                write (1,*) "eps_yy : ", eps_yy(nx*(j-1)+i)
                write (1,*) "sigma_xx : ", sigma_xx(nx*(j-1)+i), "en Pa"
                write (1,*) "sigma_xy : ", sigma_xy(nx*(j-1)+i), "en Pa"
                write (1,*) "sigma_yy : ", sigma_yy(nx*(j-1)+i), "en Pa"
                write (1,*) " "
                write (1,*) "---------------------------------------------------------"
            end do
        end do

        close(1)
    
        
    end subroutine ecrit_resume_zoom
 
end module post_traitement
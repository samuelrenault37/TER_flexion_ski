module const
    implicit none
    integer, parameter :: PR = 8

    type :: syst_lin
        integer :: N, NN
        real(PR) :: h
        real(PR), dimension(:), allocatable :: b, A_val, deric_x, deric_y
        real(PR), dimension(:,:), allocatable :: A
        integer, dimension(:), allocatable :: A_col, A_row
    end type

    ! parametres
    real(PR), parameter :: epsilon = 1.d-10, PI = 4*ATAN(1._PR)
    real(PR), parameter :: F = 157._PR ! en N
    real(PR), parameter :: E = 18d9 ! en Pa
    real(PR), parameter :: I = 1.44d-8 ! en m**4
    real(PR), parameter :: D = 2._PR ! coeff de diffusitivité


    ! bornes 1D
    real(PR), parameter :: borne_a = 0, borne_b = 1.226_PR ! en m

    ! bornes 2D
    real(PR), parameter :: borne_a_d1 = 0, borne_b_d1 = 0.1_PR 
    real(PR), parameter :: borne_a_d2 = 0, borne_b_d2 = 1 


    integer, parameter :: N_sol = 1000 !pas de discretisation pour tracer la solution analytique (pas de raison de la modifier)


    ! indication pour cas_init :
    ! 1 -> resolution avec dérivée seconde
    ! 2 -> avec dérivée quatrième
    ! 3 -> fléxion 3pts
    ! 4 -> equation de la chaleur (2D)
    integer, parameter :: cas_init = 4


end module const

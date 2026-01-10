module const
    implicit none
    integer, parameter :: PR = 8
    real(PR), parameter :: epsilon = 1.d-10
    real(PR), parameter :: F = 157_PR ! en N
    real(PR), parameter :: E = 10d9 ! en Pa
    real(PR), parameter :: I = 1.44d-8 ! en m**4
    real(PR), parameter :: borne_a = 0, borne_b = 1.226_PR ! en m
    integer, parameter :: N_sol = 1000 !pas de discretisation pour tracer la solution analytique (pas de raison de la modifier)
    ! 1 -> resolution avec dérivée seconde
    ! 2 -> avec dérivée quatrième
    ! 3 -> fléxion 3pts
    integer, parameter :: cas_init = 2
end module const

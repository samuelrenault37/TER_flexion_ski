module functions

    use constantes
    
    implicit none 

    ! ----------------------
    contains
    ! ----------------------

    function T_init_2D(x, y) result(result)

        real(PR), intent(in) :: x
        real(PR), intent(in) :: y
        real(PR)             :: result

        result = SIN(PI*x)*SIN(PI*y)

    end function T_init_2D

    ! ----------------------

    function Texacte_2D(t,x,y) result(result)

        real(PR), intent(in) :: x
        real(PR), intent(in) :: t
        real(PR), intent(in) :: y
        real(PR)             :: result

        result = EXP(-2*D*t*PI**2)*SIN(PI*x)*SIN(PI*y)

    end function Texacte_2D


end module functions

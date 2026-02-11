module functions

    use constantes

    implicit none

    ! ----------------
    contains
    ! ----------------

    function f(x) result(y)

        real(PR), intent(in) :: x
        real(PR)             :: y

        y = SIN(20*PI*x)

    end function f 

    ! ----------------

    function sol_exacte(x, ug, ud) result(y)

        real(PR), intent(in) :: x
        real(PR), intent(in) :: ug
        real(PR), intent(in) :: ud
        real(PR)             :: y

        y = (SIN(20*PI*x))/((20*PI)**2) + (ud -ug)*x + ug

    end function sol_exacte

    ! ----------------

    function f_2D(x, y) result(result)

        real(PR), intent(in) :: x
        real(PR), intent(in) :: y
        real(PR)             :: result

        result = 2*PI**2*SIN(PI*x)*SIN(PI*y)

    end function f_2D 

    ! ----------------

    function sol_exacte_2D(x, y) result(result)

        real(PR), intent(in) :: x
        real(PR), intent(in) :: y
        real(PR)             :: result

        result = SIN(PI*x)*SIN(PI*y)

    end function sol_exacte_2D

end module functions

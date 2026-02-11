module mod_functions

    use constantes

    implicit none

    !---------------------
    contains
    !---------------------

    function f(x) result(y)

        real(PR), intent(in) :: x
        real(PR)             :: y

        y = COS(x)**2

    end function f

    !---------------------

    function d1f(x) result(y)

        real(PR), intent(in) :: x
        real(PR)             :: y

        y = -2._PR*SIN(x)*COS(x)

    end function d1f

    !---------------------

    function d2f(x) result(y)

        real(PR), intent(in) :: x
        real(PR)             :: y

        y = -2._PR*COS(x)**2 + 2._PR*SIN(x)**2

    end function d2f

    !---------------------

end module mod_functions

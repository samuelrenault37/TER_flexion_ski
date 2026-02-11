module mod_df

    use constantes
    use mod_functions

    implicit none 

    !---------------------
    contains
    !---------------------

    function dfm(x, delta_x) result(y)

        real(PR), intent(in) :: x 
        real(PR), intent(in) :: delta_x
        real(PR)             :: y

        y = (f(x+delta_x) - f(x)) / delta_x

    end function dfm

    !---------------------

    function dfc(x, delta_x) result(y)

        real(PR), intent(in) :: x 
        real(PR), intent(in) :: delta_x
        real(PR)             :: y

        y = (f(x+delta_x) - f(x-delta_x)) / (2._PR*delta_x)

    end function dfc

    !---------------------

    function dfp(x, delta_x) result(y)

        real(PR), intent(in) :: x 
        real(PR), intent(in) :: delta_x
        real(PR)             :: y

        y = (f(x) - f(x-delta_x)) / delta_x

    end function dfp

    !---------------------

    function df_2(x, delta_x) result(y)

        real(PR), intent(in) :: x 
        real(PR), intent(in) :: delta_x
        real(PR)             :: y

        y = (-f(x + 2*delta_x) + 16*f(x+delta_x) - 30*f(x) + 16*f(x-delta_x) - f(x - 2*delta_x))/(12*delta_x**2)

    end function df_2

    !---------------------

    function df2c_3pts(x, delta_x) result(y)

        real(PR), intent(in) :: x 
        real(PR), intent(in) :: delta_x
        real(PR)             :: y

        y = (f(x + delta_x) - 2*f(x) + f(x-delta_x))/(delta_x**2)

    end function df2c_3pts

end module mod_df

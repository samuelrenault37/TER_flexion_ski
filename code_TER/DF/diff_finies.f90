program diff_finies

    use constantes
    use mod_functions
    use mod_df

    implicit none

    real(PR), parameter :: x = 0.5_PR
    real(PR), parameter :: delta_x = 0.1_PR

    real(PR) :: dfp_approx1, dfm_approx1, dfc_approx1, sol_exacte1, dfp_approx2
    real(PR) :: err1_d, err1_c, err1_g, err2_d

    real(PR), dimension(:), allocatable :: tab_err_dfp, tab_err_dfm, tab_err_dfc
    integer, parameter                  :: k = 10
    integer                             :: i
    real(PR)                            :: delta_xk

    real(PR)                            :: sol_exacte2, df2_approx, err2
    real(PR), dimension(:), allocatable :: tab_err_df2
    real(PR)                            :: delta_x_critique

    real(PR) :: df2c_approx, err2_c, delta_x_critique2
    real(PR), dimension(:), allocatable :: tab_err2_df2

    ! -----------------
    ! QUESTION 1
    ! -----------------

    dfm_approx1 = dfm(x, delta_x)
    dfc_approx1 = dfc(x, delta_x)
    dfp_approx1 = dfp(x, delta_x)
    sol_exacte1 = d1f(x)

    err1_c = ABS(sol_exacte1 - dfc_approx1)
    err1_d = ABS(sol_exacte1 - dfp_approx1)
    err1_g = ABS(sol_exacte1 - dfm_approx1)

    print*, "La valeur de f' exacte en x est :"
    print *, sol_exacte1

    print *, ""

    print*, "La valeur de f' par DD avec le schéma décentré à gauche est :"
    print *, dfm_approx1
    print*, "Ainsi l'erreur pour ce schéma est :"
    print *, err1_g

    print *, ""

    print*, "La valeur de f' par DD avec le schéma centré est :"
    print *, dfc_approx1
    print*, "Ainsi l'erreur pour ce schéma est :"
    print *, err1_c

    print *, ""

    print*, "La valeur de f' par DD avec le schéma décentré à droite est :"
    print *, dfp_approx1
    print*, "Ainsi l'erreur pour ce schéma est :"
    print *, err1_d

    print *, ""

    ! L'erreur obtenue pour les 3 schémas est très petite, cela confirme
    ! et valide les schémas utilisées ainsi que les résultats obtenues

    ! -----------------
    ! QUESTION 2
    ! -----------------

    dfp_approx2 = dfp(x, delta_x*0.5_PR)


    err2_d = ABS(sol_exacte1 - dfp_approx2)

    print*, "Pour delta_x on obtient comme erreur du schémas décentré à droite :"
    print *, err1_d

    print*, "Pour delta_x/2 on obtient comme erreur du schémas décentré à droite :"
    print *, err2_d

    print *, ""

    allocate(tab_err_dfp(k+1))

    open(unit=1, file='cv_dfp.dat', action='WRITE')

    do i = 1, k+1
        delta_xk = 0
        delta_xk = 1._PR/(10._PR* (2._PR**(i-1)))
        tab_err_dfp(i) = ABS(sol_exacte1 - dfp(x, delta_xk))

        write(1, '(E13.8, 4X, E13.8)') delta_xk, tab_err_dfp(i)
    end do 

    close(1)

    ! Avec gnuplot, on obtient une courbe 1.01023*delta_x - 0.538781
    ! Ce qui montre bien une convergence d'ordre 1

    ! -----------------
    ! QUESTION 3
    ! -----------------

    allocate(tab_err_dfm(k+1))

    open(unit=2, file='cv_dfm.dat', action='WRITE')

    do i = 1, k+1
        delta_xk = 0
        delta_xk = 1._PR/(10._PR* (2._PR**(i-1)))
        tab_err_dfm(i) = ABS(sol_exacte1 - dfm(x, delta_xk))

        write(2, '(E13.8, 4X, E13.8)') delta_xk, tab_err_dfm(i)
    end do 

    close(2)

    ! Avec gnuplot, on obtient une courbe 0.988344*delta_x - 0.702844
    ! Ce qui montre bien une convergence d'ordre 1

    ! -----------------

    allocate(tab_err_dfc(k+1))

    open(unit=3, file='cv_dfc.dat', action='WRITE')

    do i = 1, k+1
        delta_xk = 0
        delta_xk = 1._PR/(10._PR* (2._PR**(i-1)))
        tab_err_dfc(i) = ABS(sol_exacte1 - dfc(x, delta_xk))

        write(3, '(E13.8, 4X, E13.8)') delta_xk, tab_err_dfc(i)
    end do 

    close(3)

    ! Avec gnuplot, on obtient une courbe 1.99985*delta_x - 0.5792
    ! Ce qui montre bien une convergence d'ordre 2

    ! -----------------
    ! QUESTION 4
    ! -----------------

    df2_approx = df_2(x, delta_x)
    sol_exacte2 = d2f(x)

    err2 = ABS(sol_exacte2 - df2_approx)

    print*, "La valeur de f'' exacte en x est :"
    print *, sol_exacte2

    print *, ""

    print*, "La valeur de f'' par DD avec le schéma donné est :"
    print *, df2_approx
    print*, "Ainsi l'erreur pour ce schéma est :"
    print *, err2

    print *, ""

    allocate(tab_err_df2(k+1))

    open(unit=4, file='cv_df2.dat', action='WRITE')

    do i = 1, k+1
        delta_xk = 0
        delta_xk = 1._PR/(10._PR* (2._PR**(i-1)))
        tab_err_df2(i) = ABS(sol_exacte2 - df_2(x, delta_xk))

        write(4, '(E13.8, 4X, E13.8)') delta_xk, tab_err_df2(i)
    end do 

    close(4)

    ! L'erreur ne diminue plus à partir de k = 6 soit .15625000E-02 
    ! En supprimant tous les points après k = 6 on peut tracer la courbe
    ! et obtenir une courbe 3.97457*delta_x - 1.7338
    ! Ce qui montre bien une convergence d'ordre 4


    ! On a log(C) = b => C = exp(b)

    delta_x_critique = (Epsilon(x)/EXP(- 1.7338))**(1._PR/6._PR)

    print *, "La valeur pour laquelle le pas de temps se fait dominer par l'erreur d'arrondi est :"
    print *, delta_x_critique

    print *, ""

    ! On obtient delta_x_critique = 3.2852619288835281E-003
    ! Cela correspond parfaitement avec ce qu'on observe sur la courbe d'erreur

    ! -----------------
    ! QUESTION 5
    ! -----------------

    df2c_approx = df2c_3pts(x, delta_x)

    err2_c = ABS(sol_exacte2 - df2c_approx)

    print*, "La valeur de f'' par DD avec le schéma centré est :"
    print *, df2_approx
    print*, "Ainsi l'erreur pour ce schéma est :"
    print *, err2_c

    delta_x_critique2 = Epsilon(x)**(1._PR/4._PR)

    print *, "La valeur pour laquelle le pas de temps se fait dominer par l'erreur d'arrondi est :"
    print *, delta_x_critique2

    print *, ""

    ! On obtient delta_x_critique = 1.2207031250000000E-004
    
    allocate(tab_err2_df2(k+1))

    open(unit=7, file='cv_df2c.dat', action='WRITE')

    do i = 1, k+1
        delta_xk = 0
        delta_xk = 1._PR/(10._PR* (2._PR**(i-1)))
        tab_err2_df2(i) = ABS(sol_exacte2 - df2c_3pts(x, delta_xk))

        write(7, '(E13.8, 4X, E13.8)') delta_xk, tab_err2_df2(i)
    end do 

    close(7)

    ! On obtient ainsi un delta_x_critique qui correspond à celui calculé

    ! Cette fois ci, en supprimant tous les points à partir de k = 9 on peut tracer la courbe
    ! et obtenir une courbe 2.05526*delta_x - 0.792656
    ! Ce qui montre que le schéma est d'ordre 2

    ! ----------------

    deallocate(tab_err_dfp, tab_err_dfm, tab_err_dfc, tab_err_df2, tab_err2_df2)

end program diff_finies

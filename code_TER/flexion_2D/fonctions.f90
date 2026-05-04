module fonctions
    use donnees
    implicit none

contains

    function f_rep(x) result(fx)
        real(PR), intent(in) :: x
        real(PR) :: fx, c, pic1, pic2, largeur_pic

        c = F/(Lz*0.4253889216_PR) ! nb obtenu en calculant 2* l'Integrale de 0 à L de exp((-1/2)*(x - pic1)**2/(largeur_pic**2))
        pic1 = Lx/2 - 0.13 ! valeur réfléchie
        pic2 = Lx/2 + 0.13 ! valeur réfléchie
        largeur_pic = 0.06_PR ! valeur réfléchie

        fx = c*EXP(-(1._PR/2)*(((x - pic1)**2)/(2*largeur_pic**2))) + c*EXP(-(1._PR/2)*(((x - pic2)**2)/(2*largeur_pic**2)))
    end function f_rep
    
end module fonctions
program main
    use donnees
    use schema_DF
    use post_traitement
    implicit none

    call init_para

    call calc_DF
    print *, MAXVAL(ABS(MATMUL(A, u)- b))  !resolution du système linéaire fonctionne bien
    
    call ecrit_sol_num

    deallocate(u, x, y, b, A, dirich)
    
end program main
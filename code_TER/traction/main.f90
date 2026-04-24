program main
    use donnees
    use schema_DF
    use post_traitement
    implicit none

    call init_para

    call calc_DF
    print *, MAXVAL(ABS(MATMUL(A, u)- b))  !resolution du système linéaire fonctionne bien
    
    call ecrit_sol_num


    ! sert pour le debug
    ! allocate(A(4,4), u(4), b(4))
    ! N = 4
    ! A = RESHAPE( (/1,0,0,0,0,0,5,0,8,4,0,3,9,0,0,7/), (/4,4/))
    ! u = (/2, 8, 1, 3/)
    ! call convert_A_CSR(A_val, A_col, A_row, NN)
    ! call matvec_csr(NN, A_val, A_col, A_row, u, b)

    ! print*, b
    
    ! deallocate(A, u, b)

    deallocate(u, x, y, b, A, dirich)
    
end program main
program solveur_creux
  implicit none
  integer, parameter :: PR = 8 , N = 5

  ! Matrice creuse en format CSR
  integer, parameter :: NN = (N-2)*3 + 4  !nombre de valeurs non nulles
  real(PR), dimension(NN) :: A_val !toutes les valeurs non nuls de la matrice en ligne
  integer, dimension(NN) :: A_col !contient la colonnes de chacune de ses valeurs
  integer, dimension(N+1) :: A_row !pointeurs de début de chaque ligne + 1

  real(PR), dimension(N) :: x, b, r, p, Ap
  real(PR) :: alpha, beta, rho_cur, rho_new
  integer :: i, k, max_iter
  real(PR) :: epsilon

  ! Initialisation de la matrice au format CSR pour toute taille N de matrice

  A_row(1) = 1  ! A_row = [1, 3, 6, 9, 12, 14] ici
  A_col(1) = 1  ! A_col = [1,2, 1,2,3, 2,3,4, 3,4,5, 4,5] ici
  A_col(2) = 2  ! A_val = [-2,1, 1,-2,1, 1,-2,1, 1,-2,1, 1,-2] ici
  A_val(1) = -2
  A_val(2) = 1
  do i = 1,N-2
    A_row(i+1) = 3*i
    A_col(3*i) = i
    A_col(3*i+1) = i+1
    A_col(3*i+2) = i+2
    A_val(3*i) = 1
    A_val(3*i+1) = -2
    A_val(3*i+2) = 1
  end do
  A_row(N) = 3*(N-1)
  A_row(N+1) = A_row(N) + 2
  A_col(NN-1) = N-1
  A_col(NN) = N
  A_val(NN-1) = 1
  A_val(NN) = -2

  ! Second membre b
  b = 2

  ! Initialisation
  x = 0
  epsilon = 1.0d-8
  max_iter = 1000

  call matvec_csr(N, A_val, A_col, A_row, x, Ap)
  r = b - Ap
  p = r
  rho_cur = dot_product(r, r)

  do k = 1, max_iter
     call matvec_csr(N, A_val, A_col, A_row, p, Ap)
     alpha = rho_cur / dot_product(p, Ap)
     x = x + alpha * p
     r = r - alpha * Ap
     rho_new = dot_product(r, r)

     if (sqrt(rho_new) < epsilon) exit

     beta = rho_new / rho_cur
     p = r + beta * p
     rho_cur = rho_new
  end do

  print *, "Solution après ", k, " itérations :"
  do i = 1, N
     print *, x(i)
  end do

contains
  ! calcul le produit matriciel entre une matrice sous format CSR et un vecteur (ne tenant donc pas compte des 0 de la matrice)
  subroutine matvec_csr(N, val, col, row, x, y)
    integer, intent(in) :: N
    real(PR), dimension(N), intent(in) :: val
    integer, dimension(N), intent(in) :: col, row
    real(PR), dimension(N), intent(in) :: x
    real(PR), dimension(N), intent(out) :: y
    integer :: i, j

    y = 0
    do i = 1, N
       do j = row(i), row(i+1)-1
          y(i) = y(i) + val(j) * x(col(j))
       end do
    end do
  end subroutine matvec_csr

end program solveur_creux

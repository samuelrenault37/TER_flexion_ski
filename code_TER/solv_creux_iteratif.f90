program solveur_creux
  implicit none
  integer, parameter :: PR = 8 , N = 2000 ! 2000 max sinon on atteint des valeurs trop grandes donc fausses (on plafonne)
  real(PR) :: borne_a, borne_b, h, xa, xb

  ! Matrice creuse en format CSR
  integer, parameter :: NN = (N-2)*3 + 4  !nombre de valeurs non nulles
  real(PR), dimension(NN) :: A_val !toutes les valeurs non nuls de la matrice en ligne
  integer, dimension(NN) :: A_col !contient la colonnes de chacune de ses valeurs
  integer, dimension(N+1) :: A_row !pointeurs de début de chaque ligne + 1

  real(PR), dimension(N) :: x, b, r, p, Ap
  real(PR) :: alpha, beta, rho_cur, rho_new
  integer :: i, k, max_iter
  real(PR) :: epsilon

  ! intialisation des bornes, du pas de discrétisation et des CL (nul ici sinon jsp comment ça marche)
  borne_a = 0
  xa = 0
  borne_b = 1
  xb = 0
  h = (borne_b-borne_a)/(N+1)

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

  A_val = (1/h**2) * A_val

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

  call write_in_file("../doc/res_solv_creux.txt", x, N, h, borne_a, borne_b, xa, xb)

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

  ! écrit les valeurs utiles dans un fichier texte pour pouvoir les exploiter avec gnuplot (tracé de courbe)
  subroutine write_in_file(file, x, N, h, a, b, xa, xb)
    character(len=*), intent(in) :: file
    real(PR), dimension(:), intent(in) ::  x
    integer, intent(in) :: N
    real(PR), intent(in) :: h, a, b, xa, xb

    open(unit = 1, file = file, action = "write")
    write (1, '(A)') "#abscisse               ordonnée"
    write (1, *) a , xa
    do i = 1,N
      write (1, *) a + (i)*(h), x(i)
    end do
    write (1, *) b , xb
  end subroutine write_in_file

end program solveur_creux

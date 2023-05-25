program persamaan_diferensial_tugas_08
  implicit none
  integer, parameter :: dpr = kind(1.0D0)
  integer :: i, k, n
  real(dpr), allocatable :: X_analitik(:), Y_analitik(:), X_numerik(:), Y_numerik(:)
  real(dpr), allocatable :: sol_numerik(:,:)
  real(dpr) :: h, x00, xf, x0, y0, u0, m, delta_x

  !membaca input besar step h dan jumlah output
  write(*, *) 'masukkan jumlah titik data output yang diinginkan'
  write(*, *) 'jumlah titik data output ='
  read(*, *) k
  write(*, *) 'masukkan jarak h antar titik data output (x) yang diinginkan'
  write(*, *) 'jarak antar titik data (x) ='
  read(*, *) h

  !men-generate data x_analitik untuk plotting
  x00 = 0.0
  xf = h*k
  n = 10000
  m = n
  delta_x = (xf-x00)/(m-1)

  allocate(X_analitik(n), Y_analitik(n))

  do i = 1, n
     X_analitik(i) = x00 + (i - 1) * delta_x
  end do

  !men-generate solusi analitik
  Y_analitik = solusi_analitik(X_analitik, n)

  !men-generate solusi numerik

  x0 = 0.000001
  y0 = 0.000001
  u0 = 1.0

  allocate(sol_numerik(k,2),X_numerik(k),Y_numerik(k))

  !memanggil fungsi runge-kutta untuk mencari solusi numerik
  sol_numerik = runge_kutta_4_solve_2nd_ode(x0, k, h, y0, u0)
  
  do i = 1,k 
     X_numerik(i) = sol_numerik(i,1)
     Y_numerik(i) = sol_numerik(i,2)
  end do

  !menyimpan file output
  open(unit=2, file='output_xa_pd2_tugas_08.txt', status='replace', action='write')
  do i = 1, n
    write(2, *) X_analitik(i)
  end do
  close(2)

  open(unit=3, file='output_ya_pd2_tugas_08.txt', status='replace', action='write')
  do i = 1, n
    write(3, *) Y_analitik(i)
  end do
  close(3)

  open(unit=4, file='output_xn_pd2_tugas_08.txt', status='replace', action='write')
  do i = 1, k
    write(4, *) X_numerik(i)
  end do
  close(4)

  open(unit=5, file='output_yn_pd2_tugas_08.txt', status='replace', action='write')
  do i = 1, k
    write(5, *) Y_numerik(i)
  end do
  close(5)
  
write(*,*) 'Perhitungan berhasil, cek hasilnya di file output tugas 08'
write(*,*) 'Untuk melihat plot grafiknya, silahkan run program "plot_tugas08.m" di matlab'

  deallocate(X_analitik, Y_analitik, X_numerik, Y_numerik)
  stop

contains

  function solusi_analitik(X, k) result(Y)
    implicit none
    integer, intent(in) :: k
    real(dpr), intent(in) :: X(k)
    real(dpr) :: Y(k)
    integer :: i

    !fungsi solusi analitik

    do i = 1, k
       Y(i) = fungsi(X(i))
    end do

  end function solusi_analitik

  function fungsi(x) result(y)
    implicit none
    real(dpr), intent(in) :: x
    real(dpr) :: y
    y = x * exp(-x)
  end function fungsi

  function runge_kutta_4_solve_2nd_ode(x0, k, h, y0, u0) result(x_y)
    implicit none
    real(dpr), intent(in) :: x0, h, y0, u0
    integer, intent(in) :: k
    integer :: i
    real(dpr) :: y(k), u(k), x(k), x_y(k,2)
    real(dpr) :: u0y, u1y, u2y, u3y
    real(dpr) :: f0u, f1u, f2u, f3u

    !men-set nilai awal

    do i = 1,k 
       y(i) = 0.0
       u(i) = 0.0
       x(i) = 0.0
    end do

    y(1) = y0
    u(1) = u0
    x(1) = x0

    !melukakan perhitungan berulang untuk setiap nilai x y u
    do i = 1, k-1
      !menghitung nilai tengah
      u0y = u(i)
      f0u = func(x(i), y(i), u(i))

      u1y = (u(i) + h * f0u / 2.0)
      f1u = func(x(i) + h / 2.0, y(i) + h * u0y / 2.0, u(i) + h * f0u / 2.0)

      u2y = (u(i) + h * f1u / 2.0)
      f2u = func(x(i) + h / 2.0, y(i) + h * u1y / 2.0, u(i) + h * f1u / 2.0)

      u3y = (u(i) + h * f2u)
      f3u = func(x(i) + h, y(i) + h * u2y, u(i) + h * f2u)

      !meng-update nilai x y u
      y(i+1) = y(i) + h * (u0y + 2.0 * u1y + 2.0 * u2y + u3y) / 6.0
      u(i+1) = u(i) + h * (f0u + 2.0 * f1u + 2.0 * f2u + f3u) / 6.0
      x(i+1) = x(i) + h
    end do

    ! simpan output ke x_y
    do i = 1,k 
       x_y(i,1) = x(i)
       x_y(i,2) = y(i)
    end do
    
  end function runge_kutta_4_solve_2nd_ode

  function func(x, y, u) result(d2y_dt2)
    implicit none
    real(dpr), intent(in) :: x, y, u
    real(dpr) :: d2y_dt2

    ! mendefinisikan fungsi y''
    d2y_dt2 = - y/x - u

  end function func

end program persamaan_diferensial_tugas_08

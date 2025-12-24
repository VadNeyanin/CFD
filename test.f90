program do
use multi_vect_line
real*4, allocatable :: x(:,:,:), y(:,:,:), z(:,:,:)
real*4 kx, ky, kz, T1, T2
integer :: n, i, j, k
n = 900
kx = 2.
ky = 2.
kz = 2.
allocate(x(n, n ,n), y(n, n ,n), z(n, n ,n))
do i = 1, n
    do j = 1, n
        do k = 1, n
            x(i, j, k) = real(i-1)
            y(i, j, k) = real(j-1)
            z(i, j, k) = real(k-1)
        end do
    end do
end do
call cpu_time(T1)
call replace(x, y, z, kx, ky, kz, n)
call cpu_time(T2)
write(*,*) 'computational time', T2-T1
deallocate(X,Y,Z)
end PROGRAM
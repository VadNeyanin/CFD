module single_vect
    implicit none
contains
    subroutine replace(x, y, z, kx, ky, kz, n)
        integer, intent(in) :: n
        integer :: i, j, k
        real*8, intent(inout) :: x(n, n ,n), y(n, n ,n), z(n, n ,n)
        real*8, intent(in) :: kx, ky, kz
        !$omp simd
        DO k = 1, n
            DO j = 1, n
                DO i = 1, n
                    x(i, j, k) = x(i, j, k) - 0.5*(0.5**real(i-1) - 1.)/(-0.5)
                    y(i, j, k) = y(i, j, k) - 0.5*(0.5**real(j-1) - 1.)/(-0.5)
                    z(i, j, k) = z(i, j, k) - 0.5*(0.5**real(k-1) - 1.)/(-0.5)
                END DO
            END DO
        END DO
        !$omp end simd
    end subroutine replace
end module single_vect
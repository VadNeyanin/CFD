module multi_vect_line
    use omp_lib
    implicit none
    integer, parameter :: ALIGN_SIZE = 32
contains
    subroutine replace(x, y, z, kx, ky, kz, n)
        integer, intent(in) :: n
        integer :: i, j, k
        real*4, intent(inout) :: x(n, n ,n), y(n, n ,n), z(n, n ,n)
        real*4, intent(in) :: kx, ky, kz
        !DIR$ ASSUME_ALIGNED x: ALIGN_SIZE
        !DIR$ ASSUME_ALIGNED y: ALIGN_SIZE  
        !DIR$ ASSUME_ALIGNED z: ALIGN_SIZE
        
        !DIR$ ASSUME (MOD(n, 4) == 0)
        !$omp parallel private(i, j, k) 
        !$omp do simd
        DO k = 1, n
            DO j = 1, n
                DO i = 1, n
                    x(i, j, k) = x(i, j, k) - 0.5*(0.5**real(i-1) - 1.)/(-0.5)
                    y(i, j, k) = y(i, j, k) - 0.5*(0.5**real(j-1) - 1.)/(-0.5)
                    z(i, j, k) = z(i, j, k) - 0.5*(0.5**real(k-1) - 1.)/(-0.5)
                END DO
            END DO
        END DO
        !$omp end do simd
        !$omp end parallel
    end subroutine replace
end module multi_vect_line
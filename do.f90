
PRORAM DO

	INCLUDE 'omp_lib.h'
	DOUBLE PRECISION, ALLOCATABLE :: x(:,:,:), y(:,:,:), z(:,:,:)
	DOUBLE PRECISION kx, ky, kz, T1, T2
	INTEGER :: n, i, j, k, L
	n = 200
	kx = 2.
	ky = 2.
	kz = 2.
	ALLOCATE(x(n, n ,n), y(n, n ,n), z(n, n ,n))
	DO i = 1, n
		DO j = 1, n
			DO k = 1, n
				x(i, j, k) = real(i-1)
				y(i, j, k) = real(j-1)
				z(i, j, k) = real(k-1)
			END DO
		END DO
	END DO
!    OPEN(1, file = 'cube_old.csv', status = 'new')
!    WRITE(1, *) 'x y z'
!      DO i=1, n
!       DO j=1, n 
!         DO k=1, n 
!          WRITE(1, "(f7.4,1x,f7.4,1x,f7.4)") x(I,J,K), y(I,J,K), z(I,J,K) 
!         END DO 
!        END DO
!      END DO
!      CLOSE(1)
	T1 = OMP_GET_WTIME()
!$OMP PARALLEL PRIVATE(i,j,k)
!$OMP DO
	DO k = 1, n
	!#$OMP DO	
		DO j = 1, n
		!#$OMP DO
			DO i = 1, n
				x(i, j, k) = x(i, j, k) - 0.5*(0.5**real(i-1) - 1.)/(-0.5)
				y(i, j, k) = y(i, j, k) - 0.5*(0.5**real(j-1) - 1.)/(-0.5)
				z(i, j, k) = z(i, j, k) - 0.5*(0.5**real(k-1) - 1.)/(-0.5)
			END DO
		!#$END DO		
		END DO	
	!#$END DO
	END DO
!$END DO
!$OMP END PARALLEL
	T2 = OMP_GET_WTIME()
    WRITE(*,*) 'computational time', T2-T1
!    OPEN(1, file = 'cube_new.csv', status = 'new')
!    WRITE(1, *) 'x y z'
!      DO i=1, n
!        DO j=1, n 
!         DO k=1, n 
!          WRITE(1, "(f7.4,1x,f7.4,1x,f7.4)") x(I,J,K), y(I,J,K), z(I,J,K) 
!         END DO 
!        END DO
!      END DO
!      CLOSE(1)
      DEALLOCATE(X,Y,Z)
END PROGRAM

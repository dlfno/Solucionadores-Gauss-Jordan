PROGRAM Inversa_Gauss_Jordan
  IMPLICIT NONE
  REAL(8), DIMENSION(:,:), ALLOCATABLE :: A_aum ! Matriz aumentada
  REAL(8), DIMENSION(:,:), ALLOCATABLE :: A_inv ! Para guardar resultado
  INTEGER :: i, j, k, n
  REAL(8) :: pivote, factor

  PRINT *, "Introduzca el orden de la matriz (n):"
  READ *, n

  ALLOCATE(A_aum(n, 2*n), A_inv(n,n))

  PRINT *, "Introduzca los elementos de la matriz A fila por fila:"
  DO i = 1, n
     READ *, A_aum(i, 1:n)
     ! Inicializar la parte derecha como Identidad
     DO j = n+1, 2*n
        IF (j-n == i) THEN
           A_aum(i, j) = 1.0d0
        ELSE
           A_aum(i, j) = 0.0d0
        ENDIF
     ENDDO
  ENDDO

  ! Eliminación Gauss-Jordan
  DO i = 1, n
     ! 1. Normalizar el pivote (hacer que A(i,i) sea 1)
     pivote = A_aum(i, i)
     
     ! Verificación simple de singularidad
     IF (ABS(pivote) < 1.0d-12) THEN
        PRINT *, "El pivote es cero o muy cercano a cero. Matriz singular."
        STOP
     END IF
     
     A_aum(i, :) = A_aum(i, :) / pivote

     ! 2. Hacer ceros en la columna i (arriba y abajo)
     DO k = 1, n
        IF (k /= i) THEN
           factor = A_aum(k, i)
           A_aum(k, :) = A_aum(k, :) - factor * A_aum(i, :)
        END IF
     END DO
  END DO

  ! Extraer la inversa (la mitad derecha)
  A_inv = A_aum(:, n+1:2*n)

  PRINT *, "La Matriz Inversa es:"
  DO i = 1, n
     PRINT "(10F12.6)", A_inv(i, :)
  END DO

  DEALLOCATE(A_aum, A_inv)

END PROGRAM Inversa_Gauss_Jordan

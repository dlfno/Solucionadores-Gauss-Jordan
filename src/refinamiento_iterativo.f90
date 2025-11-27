PROGRAM Refinamiento_Iterativo
  IMPLICIT NONE
  ! Declaración de variables
  INTEGER :: n, i, j, iter, max_iter
  REAL(8), DIMENSION(:,:), ALLOCATABLE :: A, A_orig, A_temp
  REAL(8), DIMENSION(:), ALLOCATABLE :: b, b_orig, x, r, e, b_temp
  REAL(8) :: norma_error, tolerancia

  ! Configuración
  tolerancia = 1.0d-12
  max_iter = 10

  PRINT *, "Introduzca el numero de ecuaciones (n):"
  READ *, n

  ! Asignación de memoria
  ALLOCATE(A(n,n), A_orig(n,n), A_temp(n,n))
  ALLOCATE(b(n), b_orig(n), x(n), r(n), e(n), b_temp(n))

  ! Lectura de datos
  PRINT *, "Introduzca la matriz A (fila por fila):"
  DO i = 1, n
     READ *, A(i, :)
  END DO
  
  PRINT *, "Introduzca el vector b:"
  READ *, b

  ! 1. GUARDAR COPIAS EXACTAS (Crucial para el refinamiento)
  A_orig = A
  b_orig = b

  ! 2. SOLUCIÓN INICIAL
  ! Preparamos copias temporales porque la subrutina modifica los argumentos
  A_temp = A_orig
  b_temp = b_orig
  
  PRINT *, "Calculando solucion inicial..."
  CALL Resolver_GaussJordan(n, A_temp, b_temp, x)
  
  PRINT *, "Solucion Inicial:"
  PRINT *, x
  PRINT *, "----------------------------------------"

  ! 3. BUCLE DE REFINAMIENTO ITERATIVO
  DO iter = 1, max_iter
     
     ! A) Calcular el Residuo: r = b - Ax
     ! Usamos MATMUL para multiplicar matriz por vector
     r = b_orig - MATMUL(A_orig, x)
     
     ! B) Resolver el sistema para el error: A * e = r
     ! Necesitamos recargar A_temp porque la anterior se destruyó
     A_temp = A_orig 
     b_temp = r      ! El lado derecho ahora es el residuo 'r'
     
     CALL Resolver_GaussJordan(n, A_temp, b_temp, e) ! Obtenemos 'e'
     
     ! C) Actualizar la solución: x_nueva = x_vieja + e
     x = x + e

     ! D) Chequeo de convergencia (Norma del error)
     norma_error = MAXVAL(ABS(e)) ! Norma infinito
     
     PRINT "(A, I2, A, ES12.5)", "Iteracion ", iter, " - Norma del error correccion: ", norma_error
     
     IF (norma_error < tolerancia) THEN
        PRINT *, "--> Convergencia alcanzada."
        EXIT
     END IF
  END DO

  PRINT *, "----------------------------------------"
  PRINT *, "Solucion Final Refinada:"
  PRINT *, x

  DEALLOCATE(A, A_orig, A_temp, b, b_orig, x, r, e, b_temp)

CONTAINS

  ! **********************************************************
  ! Subrutina Gauss-Jordan (Modifica A_in y b_in)
  ! **********************************************************
  SUBROUTINE Resolver_GaussJordan(n, A_in, b_in, x_out)
    INTEGER, INTENT(IN) :: n
    REAL(8), DIMENSION(n,n), INTENT(INOUT) :: A_in
    REAL(8), DIMENSION(n), INTENT(INOUT) :: b_in
    REAL(8), DIMENSION(n), INTENT(OUT) :: x_out
    
    INTEGER :: i, j, k
    REAL(8) :: factor, pivote

    ! Eliminación Gauss-Jordan
    DO i = 1, n
       pivote = A_in(i,i)
       
       ! Normalizar fila pivote
       A_in(i,:) = A_in(i,:) / pivote
       b_in(i) = b_in(i) / pivote
       
       ! Eliminar columna i en las demás filas
       DO k = 1, n
          IF (k /= i) THEN
             factor = A_in(k,i)
             A_in(k,:) = A_in(k,:) - factor * A_in(i,:)
             b_in(k) = b_in(k) - factor * b_in(i)
          END IF
       END DO
    END DO
    
    ! En Gauss-Jordan completo, la solución es directa
    x_out = b_in
    
  END SUBROUTINE Resolver_GaussJordan

END PROGRAM Refinamiento_Iterativo

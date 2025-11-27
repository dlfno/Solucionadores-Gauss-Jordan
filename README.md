# Solucionadores de Álgebra Lineal en Fortran 90

Este repositorio contiene una colección de algoritmos numéricos implementados en **Fortran 90** para resolver problemas de álgebra lineal. El proyecto se enfoca en el método de **Eliminación de Gauss-Jordan**, el cálculo de matrices inversas y técnicas avanzadas como el **Refinamiento Iterativo** para manejar sistemas mal condicionados.

Además, incluye herramientas de visualización en **Python** para analizar gráficamente la estabilidad de los sistemas.

## Estructura del Proyecto

```text
├── src/
│   ├── inversa.f90       # Cálculo de inversa con Gauss-Jordan
│   └── refinamiento_iterativo.f90 # Solución robusta con refinamiento iterativo
├── scripts/
│   └── visualize_system.py      # Generador de gráficas en Python
├── docs/
│   └── ill_conditioned_system.png # Gráfica generada (output)
├── Makefile                     # Automatización de compilación y ejecución
├── requirements.txt             # Dependencias de Python
└── README.md                    # Documentación
````

## Uso Rápido (Makefile)

Este proyecto utiliza `Make` para facilitar la compilación y ejecución.

| Comando | Acción |
| :--- | :--- |
| `make` | Compila todos los ejecutables de Fortran (`.out`). |
| `make plot` | Ejecuta el script de Python y genera la gráfica en `docs/`. |
| `make deps` | Instala las librerías de Python necesarias. |
| `make clean` | Elimina binarios y archivos temporales. |

**Flujo de trabajo recomendado:**

```bash
make deps    # Solo la primera vez
make         # Compilar Fortran
make plot    # Generar gráficas
./refinamiento.out # Ejecutar el análisis numérico
```
##  Ejecutar Caso de Estudio

Para reproducir los resultados del sistema mal condicionado sin introducir datos manualmente, se ha incluido un archivo de entrada predefinido en `inputs/test_case.txt`.

Puede ejecutar la demostración completa con un solo comando:

```bash
make run
-----

## 1\. Cálculo de la Matriz Inversa

Se implementó una variante del algoritmo de Gauss-Jordan para transformar una matriz aumentada $[A | I]$ en $[I | A^{-1}]$. El algoritmo realiza una diagonalización completa normalizando el pivote en cada paso.

  - **Código:** `src/inversa.f90`
  - **Compilación manual:** `gfortran src/inversa.f90 -o inversa.out`

-----

## 2\. Análisis de Sistemas Mal Condicionados

El proyecto analiza el comportamiento numérico de sistemas donde la matriz $A$ es casi singular (determinante cercano a cero). Se utiliza el siguiente sistema como caso de estudio:

$$
\begin{cases}
y = 1.000000001 x + 5 \\
y = 1.0 x + 5
\end{cases} \implies
\begin{pmatrix} -1.000000001 & 1 \\ -1 & 1 \end{pmatrix} 
\begin{pmatrix} x \\ y \end{pmatrix} = 
\begin{pmatrix} 5 \\ 5 \end{pmatrix}
$$

### Número de Condición $\kappa(A)$

El número de condición cuantifica qué tan sensible es la solución ante errores de redondeo. Se calcula como:

$$
 \kappa(A) = \|A\| \cdot \|A^{-1}\|
$$

**Resultados del análisis:**

  * Norma Infinito $\|A\|_\infty \approx 2.0$
  * Norma de la Inversa $\|A^{-1}\|_\infty \approx 2.0 \times 10^9$
  * **Número de Condición $\kappa(A) \approx 4.0 \times 10^9$**

Esto indica que el sistema es **altamente inestable**; se pueden perder hasta 9 dígitos significativos de precisión si no se utilizan técnicas de corrección.

### Visualización

Las rectas son casi paralelas, lo que hace que la intersección sea extremadamente difícil de determinar con precisión numérica estándar.

-----

## 3\. Refinamiento Iterativo

Para mitigar los errores de cancelación catastrófica en sistemas mal condicionados, se implementó el método de **Refinamiento Iterativo**.

**Algoritmo:**

1.  **Resolver:** Obtener una solución inicial aproximada $\tilde{x}$ usando Gauss-Jordan.
2.  **Residuo:** Calcular el residuo $r = b - A\tilde{x}$ (Es crítico realizar este paso con **doble precisión**).
3.  **Corrección:** Resolver el sistema $Ae = r$ para encontrar el error $e$.
4.  **Actualizar:** Mejorar la solución $x_{nuevo} = \tilde{x} + e$.
5.  **Repetir:** Iterar pasos 2-4 hasta que $\|e\| < \text{tolerancia}$.

<!-- end list -->

  - **Código:** `src/refinamiento_iterativo.f90`
  - **Compilación manual:** `gfortran src/refinamiento_iterativo.f90 -o refinamiento.out`

-----

## Requisitos del Sistema

  * **Compilador Fortran:** `gfortran` (GNU Fortran) o `ifort`.
  * **Python 3.x:** Con librerías `numpy` y `matplotlib`.
  * **Make:** (Opcional) Para automatización.

-----

**Autor:** Alonso Delfino Cervantes Flores
**Licencia:** MIT

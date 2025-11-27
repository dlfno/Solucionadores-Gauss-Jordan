# -----------------------------------------------------------------------------
# Makefile para Proyecto
# -----------------------------------------------------------------------------

# Variables del Compilador Fortran
FC = gfortran
FFLAGS = -O2 -Wall -fcheck=all

# Variables para Python
PYTHON = python3
PIP = pip

# Directorios
SRC_DIR = src
SCRIPT_DIR = scripts

# Nombres de los ejecutables de salida
EXE_INVERSA = inversa.out
EXE_REFINAMIENTO = refinamiento.out

# Rutas de los archivos fuente
SRC_INVERSA = $(SRC_DIR)/inversa.f90
SRC_REFINAMIENTO = $(SRC_DIR)/refinamiento_iterativo.f90
SCRIPT_VISUALIZACION = $(SCRIPT_DIR)/visualización.py

# -----------------------------------------------------------------------------
# Reglas (Targets)
# -----------------------------------------------------------------------------

# Regla por defecto (se ejecuta al escribir solo 'make')
all: info $(EXE_INVERSA) $(EXE_REFINAMIENTO)
	@echo "--> ¡Compilacion completada con exito!"

# Compilar el programa de Matriz Inversa
$(EXE_INVERSA): $(SRC_INVERSA)
	@echo "Compilando Matriz Inversa..."
	$(FC) $(FFLAGS) -o $@ $<

# Compilar el programa de Refinamiento Iterativo
$(EXE_REFINAMIENTO): $(SRC_REFINAMIENTO)
	@echo "Compilando Refinamiento Iterativo..."
	$(FC) $(FFLAGS) -o $@ $<

# Ejecutar el script de visualización (Python)
plot:
	@echo "Generando graficas con Python..."
	$(PYTHON) $(SCRIPT_VISUALIZACION)

# Instalar dependencias de Python
deps:
	@echo "Instalando dependencias de Python..."
	$(PIP) install -r requirements.txt

# Limpiar archivos generados (binarios, módulos, etc.)
clean:
	@echo "Limpiando proyecto..."
	rm -f *.out *.o *.mod $(SRC_DIR)/*.mod
	@echo "--> Limpieza completada."

# Ayuda
help:
	@echo "Opciones disponibles:"
	@echo "  make          -> Compila todos los ejecutables Fortran"
	@echo "  make plot     -> Ejecuta el script de Python para graficar"
	@echo "  make clean    -> Borra ejecutables y archivos temporales"
	@echo "  make deps     -> Instala las librerías de Python requeridas"

# Regla interna para imprimir cabecera
info:
	@echo "--------------------------------------------------"
	@echo "Iniciando proceso de construccion..."
	@echo "--------------------------------------------------"

.PHONY: all clean plot deps help info

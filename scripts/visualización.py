import numpy as np
import matplotlib.pyplot as plt
import os

def plot_ill_conditioned_system():
    # 1. Definir el rango de x
    # Usamos un rango pequeño para ver el cruce, aunque las líneas parezcan paralelas
    x = np.linspace(-10, 10, 400)

    # 2. Definir las ecuaciones del sistema
    # Ecuación 1: y = 1.000000001x + 5
    y1 = 1.000000001 * x + 5
    
    # Ecuación 2: y = 1.0x + 5
    y2 = 1.0 * x + 5

    # 3. Configurar la gráfica
    plt.figure(figsize=(10, 6), dpi=100)
    
    # Graficar las líneas
    plt.plot(x, y1, label=r'$y = 1.000000001x + 5$', color='blue', linestyle='--')
    plt.plot(x, y2, label=r'$y = 1.0x + 5$', color='red', alpha=0.5, linewidth=2)

    # Añadir detalles
    plt.title('Visualización de Sistema Mal Condicionado', fontsize=14)
    plt.xlabel('x')
    plt.ylabel('y')
    plt.legend(fontsize=12)
    plt.grid(True, linestyle=':', alpha=0.6)
    
    # Añadir nota explicativa en el gráfico
    plt.annotate('Las rectas son casi paralelas\n(La intersección es muy sensible)', 
                 xy=(0, 5), xytext=(2, 2),
                 arrowprops=dict(facecolor='black', shrink=0.05))

    # 4. Guardar la imagen en la carpeta 'docs'
    # Subimos un nivel (..) y entramos a 'docs'
    output_dir = os.path.join(os.path.dirname(__file__), '..', 'docs')
    
    # Crear carpeta docs si no existe
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Directorio creado: {output_dir}")

    output_path = os.path.join(output_dir, 'ill_conditioned_system.png')
    
    plt.savefig(output_path)
    print(f"Gráfica guardada exitosamente en: {output_path}")
    
    # Opcional: Mostrar la gráfica si tienes interfaz gráfica
    # plt.show()

if __name__ == "__main__":
    plot_ill_conditioned_system()

#  Demanda y Generación eléctrica en Estados Unidos
<img width="3497" height="1962" alt="imagen" src="https://github.com/user-attachments/assets/28d944a9-72c2-4f58-b5e3-5933f13e043c" />



Este es el repositorio del proyecto final del modulo 8 del Diplomado Introducción Analítica a la Ciencia de Datos. 
Es un dashboard basado en flexdashboard programado en .R en su mayoría. Con el reporte escrito en .qmd. 
Para acceder al dashboard [entre a la liga](https://melodiouszero.github.io/proyecto_modulo_8)

## Funcionalidades 

### Carga Automática de datos

Cada día a las 3:00AM se ejecuta una acción de GitHub que carga la data usando `load.R`, después se ejecuta otra acción de GitHub para renderizar el dashboard y subirlo al repositorio, actualizando así la vista del mismo. 
De igual manera se puede cargar de forma manual con la acción  "Update Data & Deploy Dashboard" 

### Reproducción

En una terminal de R, instala renv
```r
install.packages("renv")
```
Después, desde el lugar de donde clonaste este repositorio, ejecuta:
```r
renv::restore()
```
Esto debería de instalar las paqueterías necesarias para correr todo el proyecto. Incluido cosas relacionadas con el entrenamiento. 

> [!WARNING] 
> No he probado el uso de `renv`, por lo que no sé que tan efectivo sea. Pero he creado el `renv.lock` y empujado la carpeta `renv` que tiene el script necesario para descargar las paqueterías.

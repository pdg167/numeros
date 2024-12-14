# 1. Leer el fichero de entrada
leer_numeros <- function(nombre_archivo) {
  # Verificar si el archivo existe
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe.")
  }
  # Leer los números y convertirlos en un vector de enteros
  numeros <- as.integer(readLines(nombre_archivo))
  return(numeros)
}

# 2. Calcular estadísticas: media, mediana y desviación estándar
calcular_estadisticos <- function(numeros) {
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion_estandar <- sd(numeros)
  
  # Verificar si la desviación estándar es mayor que 10
  if (desviacion_estandar > 10) {
    cat("Alta variabilidad detectada: desviación estándar mayor a 10\n")
  }
  
  return(list(media = media, mediana = mediana, desviacion_estandar = desviacion_estandar))
}

# 3. Calcular los cuadrados de los números usando sapply()
calcular_cuadrados <- function(numeros) {
  cuadrados <- sapply(numeros, function(x) x^2)
  return(cuadrados)
}

# 4. Guardar los resultados en un archivo resultados.txt
guardar_resultados <- function(estadisticos, cuadrados, nombre_archivo_salida) {
  # Crear un mensaje con los resultados
  resultado <- paste("Media: ", estadisticos$media, "\n",
                     "Mediana: ", estadisticos$mediana, "\n",
                     "Desviación estándar: ", estadisticos$desviacion_estandar, "\n",
                     "\nCuadrados de los números:\n", paste(cuadrados, collapse = "\n"), "\n")
  
  # Escribir los resultados en el archivo de salida
  writeLines(resultado, nombre_archivo_salida)
}

# 5. Script principal
nombre_archivo_entrada <- "numeros.txt"
nombre_archivo_salida <- "resultados.txt"

# Leer los números desde el archivo
numeros <- leer_numeros(nombre_archivo_entrada)

# Calcular las estadísticas
estadisticos <- calcular_estadisticos(numeros)

# Calcular los cuadrados de los números
cuadrados <- calcular_cuadrados(numeros)

# Guardar los resultados en el archivo de salida
guardar_resultados(estadisticos, cuadrados, nombre_archivo_salida)


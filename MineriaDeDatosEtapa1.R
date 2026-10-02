# 1. Cargar librerías necesarias
library(dplyr)

# 2. Importación correcta del archivo
datos <- read.csv2("atenciones_salud_sucio.csv", sep = ",", stringsAsFactors = FALSE)

# 3. Registro de dimensiones y tipo de objeto (Exigido en pauta: class, nrow, ncol)
cat("--- ESTRUCTURA GENERAL Y DIMENSIONES ---\n")
print(class(datos))
cat("Total de filas (nrow):", nrow(datos), "\n")
cat("Total de columnas (ncol):", ncol(datos), "\n")
print(dim(datos))

# 4. Estructura y primeros registros (Exigido en pauta: str, head)
cat("\n--- ESTRUCTURA DE LAS VARIABLES (str) ---\n")
str(datos)

cat("\n--- PRIMERAS 6 FILAS (head) ---\n")
head(datos)

# 5. Estadísticas descriptivas generales (Exigido en pauta: summary)
cat("\n--- RESUMEN ESTADÍSTICO GENERAL (summary) ---\n")
summary(datos)

# 6. Cuantificación de valores faltantes (NA) y celdas en blanco
cat("\n--- VALORES FALTANTES FORMALES (NA) POR VARIABLE ---\n")
print(colSums(is.na(datos)))

cat("\n--- CELDAS EN BLANCO O VACÍAS (Texto \"\" o espacios) ---\n")
espacios_vacios <- colSums(datos == "" | trimws(datos) == "", na.rm = TRUE)
print(espacios_vacios)

# 7. Cuantificación de registros duplicados
cat("\n--- DUPLICADOS EXACTOS (Fila completa) ---\n")
duplicados_totales <- sum(duplicated(datos))
print(duplicados_totales)

cat("\n--- DUPLICADOS SEGÚN CLAVE RELEVANTE (id_paciente) ---\n")
duplicados_id <- sum(duplicated(datos$id_paciente))
print(duplicados_id)

# 8. Identificación de valores imposibles, rangos anómalos y códigos centinela
cat("\n--- VALORES ANÓMALOS: EDAD (summary) ---\n")
print(summary(suppressWarnings(as.numeric(datos$edad))))

cat("\n--- VALORES ANÓMALOS: DÍAS DE HOSPITALIZACIÓN (summary) ---\n")
print(summary(suppressWarnings(as.numeric(datos$dias_hospitalizacion))))

# 9. Inconsistencias de texto y categorías
cat("\n--- FRECUENCIA DE COMUNAS (Inconsistencias mayúsculas/minúsculas) ---\n")
print(table(datos$comuna, useNA = "always"))

cat("\n--- FRECUENCIA DE DIAGNÓSTICOS (Siglas vs nombres completos) ---\n")
print(table(datos$diagnostico_principal, useNA = "always"))

# 10. Detección de problemas en tipos de datos específicos
cat("\n--- MUESTRA DE FORMATOS EN FECHAS ---\n")
head(datos$fecha_ultima_atencion, 15)

cat("\n--- MUESTRA DE FORMATOS EN COSTOS (Símbolos no numéricos) ---\n")
head(datos$costo_atencion, 15)
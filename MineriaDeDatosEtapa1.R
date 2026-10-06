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




#Proceso de evaluación para limpieza

#Para empezar se hará una diferenciación sobre los duplicados que existen en el dataset
#Aquellos registros que sean duplicados idénticos serán eliminados, por otro lado, aquellos que tienen el mismo ID
#pero distinta atención se conservarán


duplicados_exactos <- duplicated(datos)
datos <- datos[!duplicados_exactos, ] #se eliminan los duplicados exactos ( [!duplicados_exactos], )
                                                       #al indexar con corchetes hacemos referencia a todas las filas que no sean
                                                       #duplicados y después de la coma al estar vacío significa que se conservan todas


#Ahora procedemos a estandarizar todas las variables que sean string, para esto utilizaremos funciones como trimws(), tolower() y gsub()

datos$comuna <- tolower(datos$comuna) #pasar a minúsculas
datos$comuna <- trimws(datos$comuna) #quitar espacios al inicio y al final
datos$comuna <- gsub("\\s+"," ", datos$comuna) #reemplazar espacios dobles por un solo espacio "\\s+" -> doble espacio

datos$diagnostico_principal <- tolower(datos$diagnostico_principal)
datos$diagnostico_principal <- trimws(datos$diagnostico_principal)
datos$diagnostico_principal <- gsub("\\s+", " ", datos$diagnostico_principal)

#Ahora procedemos a homologar datos que refieren a un mismo valor, como podría ser valparaíso | valparaiso

datos$comuna <- gsub("^valparaiso$", "valparaíso", datos$comuna)
datos$comuna <- gsub("^maipu$", "maipú", datos$comuna)
datos$comuna <- gsub("^pte\\.? alto$", "puente alto", datos$comuna)

datos$diagnostico_principal <- gsub("^(dm2|diabetes mellitus 2)$", "diabetes mellitus tipo 2", datos$diagnostico_principal)
datos$diagnostico_principal <- gsub("^hta$", "hipertensión arterial", datos$diagnostico_principal)
datos$diagnostico_principal <- gsub("^ira$", "infección respiratoria aguda", datos$diagnostico_principal)


#A continuación se procede a tratar las columnas numéricas, en específico los valores anómalos | errores

datos$edad[datos$edad < 0 | datos$edad > 110] <- NA #se pasan a NA todas aquellas edades exageradas (sobre 110) y negativas

datos$dias_hospitalizacion[datos$dias_hospitalizacion >= 300] <- NA #se pasan a NA todos aquellos días de hospitalización mayores o iguales a 300

datos$cantidad_consultas[datos$cantidad_consultas < 0 | datos$cantidad_consultas > 50] <- NA #se pasan a NA cantidades de consultas negaivas o exageradas

datos$costo_atencion[grepl("-", datos$costo_atencion)] <- NA #se pasan a NA todos los costos negativos (se ocupa grepl porque la variable es de tipo chr)

datos$fecha_ultima_atencion[datos$fecha_ultima_atencion %in% c("Sin registro", "2027-15-40", "2030-01-01")] <- NA #Se pasan a NA todas las fechas que se 
                                                                                                                  #encontraron como inválidas durante el análisis
                                                                                                                  #del dataset

#Corregir los tipos de datos

datos$costo_atencion <- gsub("[\\$ ]", "", datos$costo_atencion) #eliminamos el signo $ de los registros
datos$costo_atencion <- ifelse(grepl("\\.", datos$costo_atencion) & nchar(sub(".*\\.", "", datos$costo_atencion)) == 3,
                               gsub("\\.", "", datos$costo_atencion),
                               datos$costo_atencion)      #se evalúa si el punto que se encuentra es separador de miles (se elimina) o de decimal (se mantiene)

costo_anterior_na <- sum(is.na(datos$costo_atencion)) #valores NA antes de la conversión
datos$costo_atencion <- as.numeric(datos$costo_atencion) #se pasa a tipo de dato numerico

cat("NA generados por coerción: ", sum(is.na(datos$costo_atencion)) - costo_anterior_na)


datos$cantidad_consultas <- as.numeric(datos$cantidad_consultas)

fechas_formato_guion <- as.Date(datos$fecha_ultima_atencion, "%Y-%m-%d")
fechas_formato_barra <- as.Date(datos$fecha_ultima_atencion, "%d/%m/%Y")

fechas_formato_guion[is.na(fechas_formato_guion)] <- fechas_formato_barra[is.na(fechas_formato_guion)] #se opta por el formato en guión, reduciendo los NA
                                                                                                       #a aquellas fechas que no coincidieron con ningún formato
datos$fecha_ultima_atencion <- fechas_formato_guion

fechas_originales <- read.csv("atenciones_salud_sucio.csv", stringsAsFactors = FALSE)$fecha_ultima_atencion[!duplicated(read.csv("atenciones_salud_sucio.csv"))]

fechas_originales[fechas_originales %in% c("Sin registro", "2027-15-40", "2030-01-01")] <- NA #para ver las fechas anteriores y calcular los NA nuevos por coerción
fecha_anterior_na <- sum(is.na(fechas_originales))

cat("NA generados por coerción: ", sum(is.na(datos$fecha_ultima_atencion)) - fecha_anterior_na)

datos$comuna <- as.factor(datos$comuna)
datos$diagnostico_principal <- as.factor(datos$diagnostico_principal)

#Para el tratamiento de valores faltantes se aplicará imputación a través de la mediana para datos cuantitativos 
#(a excepción del costo ya que es mala práctica inventar cobros)
#A diferencia de datos categóricos que se conservarán (fecha)

mediana_edad <- median(datos$edad, na.rm = TRUE) #se eliminaría más del 10% lo que sesgaría la muestra
mediana_dias <- median(datos$dias_hospitalizacion, na.rm = TRUE) 
mediana_consultas <- median(datos$cantidad_consultas, na.rm = TRUE)

datos$edad[is.na(datos$edad)] <- mediana_edad
datos$dias_hospitalizacion[is.na(datos$dias_hospitalizacion)] <- mediana_dias
datos$cantidad_consultas[is.na(datos$cantidad_consultas)] <- mediana_consultas

str(datos)
summary(datos)
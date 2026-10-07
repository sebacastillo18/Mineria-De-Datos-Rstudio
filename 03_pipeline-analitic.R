library(dplyr)

datos <- read.csv("atenciones_salud_sucio.csv", stringsAsFactors = FALSE)

datos <- datos[!duplicated(datos), ]
datos$comuna <- trimws(tolower(datos$comuna))
datos$diagnostico_principal <- trimws(tolower(datos$diagnostico_principal))

datos$comuna <- gsub("valparaisos", "valparaiso", datos$comuna)
datos$comuna <- gsub("maipus", "maipu", datos$comuna)
datos$comuna <- gsub("pte alto", "puente alto", datos$comuna)

datos$diagnostico_principal <- gsub("dm2|diabetes mellitus 2", "diabetes mellitus tipo 2", datos$diagnostico_principal)
datos$diagnostico_principal <- gsub("hta", "hipertensión arterial", datos$diagnostico_principal)
datos$diagnostico_principal <- gsub("ira", "infección respiratoria aguda", datos$diagnostico_principal)

datos$costo_atencion <- gsub("\\$", "", datos$costo_atencion)
datos$costo_atencion <- gsub(",", "", datos$costo_atencion)
datos$costo_atencion <- as.numeric(datos$costo_atencion)
datos$cantidad_consultas <- as.numeric(datos$cantidad_consultas)

datos$edad[datos$edad < 0 | datos$edad > 110] <- NA
datos$dias_hospitalizacion[datos$dias_hospitalizacion >= 300] <- NA

datos$edad[is.na(datos$edad)] <- median(datos$edad, na.rm = TRUE)
datos$dias_hospitalizacion[is.na(datos$dias_hospitalizacion)] <- median(datos$dias_hospitalizacion, na.rm = TRUE)
datos$cantidad_consultas[is.na(datos$cantidad_consultas)] <- median(datos$cantidad_consultas, na.rm = TRUE)


# Pipeline dplyr
datos_transformados <- datos %>%
  filter(!is.na(edad)) %>%
  mutate(
    grupo_etario = case_when(
      edad < 18 ~ "Menor de edad",
      edad < 65 ~ "Adulto",
      TRUE ~ "Adulto mayor"
    ),
    costo_std = as.numeric(scale(costo_atencion))
  ) %>%
  arrange(desc(dias_hospitalizacion))

resumen_gestion <- datos_transformados %>%
  group_by(comuna, grupo_etario) %>%
  summarise(
    total_pacientes = n(),
    promedio_hospitalizacion = round(mean(dias_hospitalizacion, na.rm = TRUE), 2),
    mediana_hospitalizacion = median(dias_hospitalizacion, na.rm = TRUE),
    costo_promedio_std = round(mean(costo_std, na.rm = TRUE), 3),
    .groups = "drop"
  )

#Visualización
print(head(resumen_gestion, 12))

cat("\n--- VERIFICACIÓN POST-NORMALIZACIÓN (costo_std) ---\n")
cat("Media:", round(mean(datos_transformados$costo_std, na.rm = TRUE), 4), "\n")
cat("Desviación estándar:", round(sd(datos_transformados$costo_std, na.rm = TRUE), 4), "\n")
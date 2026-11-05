## Exercise Facilities
## Start Date: 2026-06-01
## Current Date: 2026-10-27
## Database: 
  ## Censo de locales, sus actividades y terrazas de hosteleria y restauracion

# Libraries ----
library(dplyr)
library(readxl)
library(sf)
library(readr)
library(purrr)
library(tidyr)
library(foreign)

# exclusion algorithm
library(stringr)
library(stringi)
library(tidytext)
library(writexl)

## Raw data ----
## 2025
## PR raw data
load("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/Paula/CensoComerciosRaw_Nov25_final20251218.RData")
View(dta_raw)

instalaciones2025 <- dta_raw %>%
  filter(
    id_seccion %in% c("R", "P"),
    id_division %in% c("85", "93"),
    id_epigrafe %in% c(
      "855002",
      "931001",
      "931002",
      "931003",
      "931004",
      "931008",
      "931009"
    ))

## 2025
## Portal raw data 
nov2025 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2025.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
instalaciones2025 <- nov2025 %>%
  filter(
    id_seccion %in% c("R", "P"),
    id_division %in% c("85", "93"),
    id_epigrafe %in% c(
      "855002",
      "931001",
      "931002",
      "931003",
      "931004",
      "931008",
      "931009"
    )
  ) %>%
  mutate(
    repetido = if_else(
      duplicated(id_local),
      "rep",
      "N"
    )
  )

## 2023
## Portal raw data
nov2023 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2023.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
instalaciones2023 <- nov2023 %>%
  filter(
    id_seccion %in% c("R", "P"),
    id_division %in% c("85", "93"),
    id_epigrafe %in% c(
      "855002",
      "931001",
      "931002",
      "931003",
      "931004",
      "931008",
      "931009"
    )
  ) %>%
  mutate(
    repetido = if_else(
      duplicated(id_local),
      "rep",
      "N"
    )
  )


## 2021
## Portal raw data
nov2021 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2021.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
instalaciones2021 <- nov2021 %>%
  filter(
    id_seccion %in% c("R", "P"),
    id_division %in% c("85", "93"),
    id_epigrafe %in% c(
      "855002",
      "931001",
      "931002",
      "931003",
      "931004",
      "931008",
      "931009"
    )
  ) %>%
  mutate(
    repetido = if_else(
      duplicated(id_local),
      "rep",
      "N"
    )
  )

## 2019
## Portal raw data
nov2019 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2019.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

instalaciones2019 <- nov2019 %>%
  filter(
    id_seccion %in% c("R", "P"),
    id_division %in% c("85", "93"),
    id_epigrafe %in% c(
      "855002",
      "931001",
      "931002",
      "931003",
      "931004",
      "931008",
      "931009"
    )
  ) %>%
  mutate(
    repetido = if_else(
      duplicated(id_local),
      "rep",
      "N"
    )
  )

## 2017
## Portal raw data
nov2017 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2017.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

instalaciones2017 <- nov2017 %>%
  filter(
    id_seccion %in% c("R", "P"),
    id_division %in% c("85", "93"),
    id_epigrafe %in% c(
      "855002",
      "931001",
      "931002",
      "931003",
      "931004",
      "931008",
      "931009"
    )
  ) %>%
  mutate(
    repetido = if_else(
      duplicated(id_local),
      "rep",
      "N"
    )
  )

## Save filtered datasets to clean based on exercise facilities definition ----
write_excel_csv(instalaciones2017, "instalaciones2017.csv")
write_excel_csv(instalaciones2019, "instalaciones2019.csv")
write_excel_csv(instalaciones2021, "instalaciones2021.csv")
write_excel_csv(instalaciones2023, "instalaciones2023.csv")
write_excel_csv(instalaciones2025, "instalaciones2025.csv")

## Clean data ----
## Data cleaning of exercise facility definition done in Excel
  # use inclusion words filters 1 and 2

## IGNORE FOR NOW Load cleaned dataset ----
# 2025
inst2025 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/clean/instalaciones2025.xlsx")

## Creation of Exclusion Algorithm ----
names(inst2025)
table(inst2025$include, useNA = "ifany")

# Standardise the text without changing the original rotulo
inst2025 <- inst2025 %>%
  mutate(
    include = toupper(trimws(as.character(include))),
    rotulo_original = as.character(rotulo),
    rotulo_norm = rotulo_original %>%
      str_to_lower() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[^a-z0-9]+", " ") %>%
      str_squish()
  )

# Keep records with a usable label and known classification
inst2025_valid <- inst2025 %>%
  filter(
    !is.na(rotulo_norm),
    rotulo_norm != "",
    include %in% c("Y", "N", "SR")
  )

# Check the number of records in each group
table(inst2025_valid$include)

# Convert each label into individual words
palabras <- inst2025_valid %>%
  select(rotulo_norm, include) %>%
  mutate(id = row_number()) %>%
  unnest_tokens(
    output = palabra,
    input = rotulo_norm,
    token = "words"
  ) %>%
  distinct(id, include, palabra)

# Count how many records in each group contain each word
frecuencia_palabras <- palabras %>%
  count(palabra, include, name = "n") %>%
  pivot_wider(
    names_from = include,
    values_from = n,
    values_fill = 0,
    names_prefix = "n_"
  )

# Ensure all three classification columns exist
for (col in c("n_N", "n_Y", "n_SR")) {
  if (!col %in% names(frecuencia_palabras)) {
    frecuencia_palabras[[col]] <- 0L
  }
}

# Calculate useful metrics
total_N <- sum(inst2025_valid$include == "N")
total_Y <- sum(inst2025_valid$include == "Y")

frecuencia_palabras <- frecuencia_palabras %>%
  mutate(
    pct_N_records = ifelse(total_N > 0, n_N / total_N, NA_real_),
    pct_Y_records = ifelse(total_Y > 0, n_Y / total_Y, NA_real_),
    precision_N = ifelse(
      n_N + n_Y > 0,
      n_N / (n_N + n_Y),
      NA_real_
    )
  ) %>%
  arrange(desc(n_N), desc(precision_N))  

# Inspect the 50 most frequent words in N records
frecuencia_palabras %>%
  select(palabra, n_N, n_Y, n_SR,
         pct_N_records, pct_Y_records, precision_N) %>%
  slice_head(n = 50)

# Convert to geopackage
inst2025_sf <- st_as_sf(
  inst2025,
  coords = c("coordenada_x_local", "coordenada_y_local"),
  crs = 25830,
  remove = FALSE
)

# Export the complete layer
st_write(
  inst2025_sf,
  "instalaciones_ejercicio_2025.gpkg",
  layer = "instalaciones_2025",
  delete_layer = TRUE
)

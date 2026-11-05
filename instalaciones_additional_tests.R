## Start Date: 2026-06-01
## Current Date: 2026-10-08
## Database Instalaciones Deportivas Basicas y Centros Deportivos/Polideportivos

# Libraries ----
library(readxl)
library(writexl)
library(dplyr)
library(sf)
library(readr)
library(purrr)
library(tidyr)
library(foreign)
library(stringdist)

## Comparision dt2020 and dt2026 ----
dt2026 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/municipales 2026.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE,
                       locale = locale(encoding = "Latin1"))
dt2026 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/municip2026.xlsx")
dtluis <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/ESPACIOS DEPORTIVOS MUNICIPALES.xls", 
                     sheet = "212808-0-espacio-deporte")
dtluis <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/Instalaciones Deportivas Municipales Completo.xlsx")

## Comparison of dtluis (2020) and dt2026 ----
## check if PKs are unique
sum(duplicated(dtluis$PK))
sum(duplicated(dt2026$PK))

## Old datasets
nrow(dtluis)

## New
nrow(dt2026)

## Common facilities
common_pk <- intersect(dtluis$PK, dt2026$PK)

## added and removed
added_pk <- setdiff(dt2026$PK, dtluis$PK)
added_pk
removed_pk <- setdiff(dtluis$PK, dt2026$PK)
removed_pk

summary <- data.frame(
  old = nrow(dtluis),
  new = nrow(dt2026),
  common = length(common_pk),
  added = length(added_pk),
  removed = length(removed_pk)
)
summary

# Removed Facilities -
removed <- dtluis %>% 
  filter(PK %in% setdiff(dtluis$PK, dt2026$PK))

table(removed$`Tipo Instalación`)
table(removed$`TIPO2`)

# Added Facilities -
added <- dt2026 %>% 
  filter(PK %in% setdiff(dt2026$PK, dtluis$PK))

table(added$TIPO2)

## When attempted to extract public facilities from comercios database ----
comercios2020 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2020.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE,
                            locale = locale(encoding = "Latin1"))

comercios2020 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2020.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE,
                            locale = locale(encoding = "Latin1"))

comercios2021 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2021.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

comercios2023 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2023.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

comercios2025 <- read_delim("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/raw/actividades noviembre 2025.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Filter appropriate epigraphs
  ## Update with appropriate file year
comercios2021 <- comercios2021 %>%
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
  )

## Save filtered files
  ## Update with appropriate file name
write_excel_csv(comercios2021, "comercios2021f.csv")

## Cleaning process done in Excel
## Incorporate keyword algorithm to include potential centros deportivos
## Individual check of observations in google 
## Trying to capture public facilities ----
cd2020 <- read_csv("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/filtered/comercios2020f.csv")
cd2020 <- read_csv("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/filtered/comercios2020f.csv")
cd2021 <- read_csv("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/filtered/comercios2021f.csv")
cd2023 <- read_csv("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/filtered/comercios2023f.csv")
cd2025 <- read_csv("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/filtered/comercios2025f.csv")

## filter to capture those in the algorithm
cd2020 <- cd2020 %>% 
  filter(
    alg %in% c("Y")
  )

## sport centers included (for all years)
cd2020c <- cd2020 %>% 
  filter(
    cd_pd %in% c("Y")
  )
### Database Public Facilities
## Start Date: 2026-06-01
## Current Date: 2026-10-27

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

## Given difficulty of finding official governmental databases throught the Portal
## de Ayuntamiento that contained historical information, we resorted to converting
## PDFs shared by the board of directors of Villa de Vallecas which are official 
## census documents of the government. 

## From these PDFs, public facilities were extracted facilitated by AI and were 
## manually checked to ensure correct transition from PDF to excel format. 

## Each excel contains information on name, district, address, and potential
## description of the public facility. Coordinates were manually found for those 
## where the coordinates were not located on the government website using the 
## address in google maps to identify the nearest sport facility best associated 
## to the description of the facility.

## Coordinates found from google maps had to be transformed to 2830.

## Load Excels from PDFs ----
cd2015 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/CENTROS DEPORTIVOS + INSTALACIONES DEPORTIVAS BASICAS/censo_deportivo_madrid_2015.xlsx")
cd2017 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/CENTROS DEPORTIVOS + INSTALACIONES DEPORTIVAS BASICAS/censo_deportivo_madrid_2017.xlsx")
cd2019 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/CENTROS DEPORTIVOS + INSTALACIONES DEPORTIVAS BASICAS/censo_deportivo_madrid_2019.xlsx")
cd2020 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/CENTROS DEPORTIVOS + INSTALACIONES DEPORTIVAS BASICAS/censo_deportivo_madrid_2020.xlsx")
cd2021 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/CENTROS DEPORTIVOS + INSTALACIONES DEPORTIVAS BASICAS/censo_deportivo_madrid_2021.xlsx")
cd2023 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/CENTROS DEPORTIVOS + INSTALACIONES DEPORTIVAS BASICAS/censo_deportivo_madrid_2023.xlsx")
cd2025 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Centros Deportivos/CENTROS DEPORTIVOS + INSTALACIONES DEPORTIVAS BASICAS/censo_deportivo_madrid_2025.xlsx")

## Data Cleaning: Perform per year ----
havexy <- !is.na(cd2015$X) & !is.na(cd2015$Y)
reproy <- !havexy & 
  !is.na(cd2015$LATITUD) &
  !is.na(cd2015$LONGITUD)

cd2015$X_25830 <- NA_real_
cd2015$Y_25830 <- NA_real_

# Copy the existing projected coordinates
cd2015$X_25830[havexy] <- cd2015$X[havexy]
cd2015$Y_25830[havexy] <- cd2015$Y[havexy]

# Transform the geographic coordinates
puntos_geo <- st_as_sf(
  cd2015[reproy, ],
  coords = c("LONGITUD", "LATITUD"),
  crs = 4326,
  remove = FALSE
)

puntos_proyectados <- st_transform(puntos_geo, 25830)

# Extract the transformed coordinates
coords <- st_coordinates(puntos_proyectados)
cd2015$X_25830[reproy] <- coords[, 1]
cd2015$Y_25830[reproy] <- coords[, 2]

## convert to geopackage
cd2015_sf <- st_as_sf(
  cd2015,
  coords = c("X_25830", "Y_25830"),
  crs = 25830,
  remove = FALSE
)

cd2015_sf <- cd2015_sf %>% 
  select(TIPO, TIPO_NOMBRE, NOMBRE, DISTRITO, DISTRITONUM, `CLASE-VIA`, 
         CALLE, NUM, EQUIPAMIENTO, `DIRECCION_ORIGINAL`, X_25830, Y_25830,
         YEAR, geometry)

# Export: Perform per year 
st_write(
  cd2015_sf,
  "cd_idb2015.gpkg",
  layer = "cd_idb_2015",
  delete_layer = TRUE
)

# Excel (geometry is dropped): Perform per year
write_xlsx(cd2015_sf, "cd2015.xlsx")

## Overall database save ----
publicas <- rbind.data.frame(cd2017_sf, cd2019_sf, cd2021_sf, cd2023_sf, cd2025_sf)

st_write(
  publicas,
  "publicas.gpkg",
  layer = "publicas",
  delete_layer = TRUE
)

write_xlsx(publicas, "publicas.xlsx")

  # Sport courts all years 
ins_dep_basicas <- publicas %>% 
  filter(TIPO == 1)

st_write(
  ins_dep_basicas,
  "ins_dep_basicas.gpkg",
  layer = "ins_dep_basicas",
  delete_layer = TRUE
)

write_xlsx(ins_dep_basicas, "ins_dep_basicas.xlsx")

  # Public Sport Centres all years
centros_poli_dep <- publicas %>% 
  filter(TIPO == 2)

st_write(
  centros_poli_dep,
  "centros_poli_dep.gpkg",
  layer = "centros_poli_dep",
  delete_layer = TRUE
)

write_xlsx(centros_poli_dep, "centros_poli_dep.xlsx")
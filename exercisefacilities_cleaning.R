## Exercise Facilities
## Start Date: 2026-06-01
## Current Date: 2026-10-27
## Database: 
  ## Censo de locales, sus actividades y terrazas de hosteleria y restauracion

### Libraries ----
library(dplyr)
library(readxl)
library(sf)
library(readr)
library(purrr)
library(tidyr)
library(foreign)
library(writexl)

# exclusion algorithm
library(stringr)
library(stringi)
library(tidytext)

## Raw data ----
# Path
raw_dir <- file.path(
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/",
  "Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/",
  "Instalaciones Ejercicio/raw"
)

# Filter criteria
secciones  <- c("R", "P")
divisiones <- c("85", "93")
epigrafes  <- c("855002", "931001", "931002", "931003",
                "931004", "931008", "931009")

data_dir <- file.path(
  "C:/Users/juani/Universidad de Alcala/",
  "EVERYMOVE Intranet - Documents/Paquetes de trabajo/",
  "WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio"
)

output_dir <- file.path(data_dir, "filtered_data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## Function to filter databases ----
clean_instalaciones <- function(year, raw_dir) {
  
  file_path <- file.path(raw_dir, paste0("actividades noviembre ", year, ".csv"))
  
  read_delim(
    file_path,
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    show_col_types = FALSE
  ) %>%
    filter(
      id_seccion  %in% secciones,
      id_division %in% divisiones,
      id_epigrafe %in% epigrafes
    ) %>%
    mutate(
      repetido = if_else(duplicated(id_local), "rep", "N")
    )
}
years <- c(2017, 2019, 2021, 2023, 2025)

instalaciones_all <- setNames(
  lapply(years, function(y) clean_instalaciones(y, raw_dir)),
  years
)

# Save one file per year
for (y in years) {
  write_excel_csv(
    instalaciones_all[[as.character(y)]],
    file.path(output_dir, paste0("instalaciones", y, ".csv"))
  )
}

## Data cleaning ----
## Data cleaning of exercise facilities done in Excel for each year according to
## a process of inclusion and exclusion filters and subsequent manual checking
## of labels based on the exercise facilities definition. 
  # Inclusion words algorithm created by Paula during the validation study
  # Exclusion words algoritm created by Juanita based on the code below
  # the words and precision are based on a manual clean to then inform which words
  # are removed with what precision (100% being always when that word is present, 
  # is removed).

## Creation of the Exclusion Algorithm ----
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

## Load clean datasets ----
ie2017 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/clean/instalaciones2017.xlsx")
ie2019 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/clean/instalaciones2019.xlsx")
ie2021 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/clean/instalaciones2021.xlsx")
ie2023 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/clean/instalaciones2023.xlsx")
ie2025 <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/clean/instalaciones2025.xlsx")

# Load CARTOCIUDAD coordinates of those that are NA and 0
ie_cc <- read_excel("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Instalaciones Ejercicio/clean/coordenadas.xlsx")
output_dir <- file.path(data_dir, "final")

# Clean function ----
clean_installations <- function(data, year, ie_cc) {
  data <- data %>%
    filter(incluido %in% c("Y", "SR"))

  coord_vars <- c(
    "coordenada_x_local",
    "coordenada_y_local",
    "coordenada_x_agrupacion",
    "coordenada_y_agrup"
  )
  
  data <- data %>%
    mutate(
      across(
        all_of(coord_vars),
        ~ as.numeric(na_if(as.character(.x), "NA"))
      )
    )
  
  # Initialise final coordinate variables
  
  data$X_25830 <- NA_real_
  data$Y_25830 <- NA_real_
  
  # Identify valid local coordinates
  
  havexy <- !is.na(data$coordenada_x_local) &
    !is.na(data$coordenada_y_local) &
    data$coordenada_x_local != 0 &
    data$coordenada_y_local != 0
  
  # Identify valid grouping coordinates
  
  reproy <- !havexy &
    !is.na(data$coordenada_x_agrupacion) &
    !is.na(data$coordenada_y_agrup) &
    data$coordenada_x_agrupacion != 0 &
    data$coordenada_y_agrup != 0
  
  # Copy local coordinates
  
  data$X_25830[havexy] <-
    data$coordenada_x_local[havexy]
  
  data$Y_25830[havexy] <-
    data$coordenada_y_local[havexy]
  
  # Copy grouping coordinates
  
  data$X_25830[reproy] <-
    data$coordenada_x_agrupacion[reproy]
  
  data$Y_25830[reproy] <-
    data$coordenada_y_agrup[reproy]
  
  # Identify installations still missing coordinates
  
  reproy_cc <- is.na(as.numeric(data$X_25830)) |
    is.na(as.numeric(data$Y_25830)) |
    data$X_25830 == 0 |
    data$Y_25830 == 0
  
  # Prepare CARTOCIUDAD coordinates
  
  coords_cc <- ie_cc %>%
    filter(YEAR == year) %>%
    transmute(
      id_local = trimws(as.character(ID_LOCAL)),
      cc_latitud = as.numeric(LATITUD_WGS84_4326),
      cc_longitud = as.numeric(LONGITUD_WGS84_4326)
    )
  
  # Join CARTOCIUDAD coordinates
  
  data <- data %>%
    mutate(id_local = trimws(as.character(id_local))) %>%
    left_join(coords_cc, by = "id_local")
  
  reproy_cc_valid <- reproy_cc &
    !is.na(data$cc_latitud) &
    !is.na(data$cc_longitud) &
    data$cc_latitud != 0 &
    data$cc_longitud != 0

  # Identify valid CARTOCIUDAD coordinates
  
  reproy_cc_valid <- reproy_cc &
    !is.na(data$latitud) &
    !is.na(data$longitud) &
    data$latitud != 0 &
    data$longitud != 0
  
  # Transform geographic coordinates
  
  if (any(reproy_cc_valid)) {
    
    puntos_geo <- st_as_sf(
      data[reproy_cc_valid, ],
      coords = c("longitud", "latitud"),
      crs = 4326,
      remove = FALSE
    )
    
    puntos_proyectados <- st_transform(
      puntos_geo,
      crs = 25830
    )
    
    coords <- st_coordinates(puntos_proyectados)
    
    data$X_25830[reproy_cc_valid] <- coords[, 1]
    data$Y_25830[reproy_cc_valid] <- coords[, 2]
    
  }
  
  # Create spatial dataset
  
  data_sf <- st_as_sf(
    data,
    coords = c("X_25830", "Y_25830"),
    crs = 25830,
    remove = FALSE,
    na.fail = FALSE
  )
  
  return(data_sf)
  
}
years <- c(2017, 2019, 2021, 2023, 2025)
years <- c(2025)

# Store raw databases in a list
instalaciones_raw <- list(
  # `2017` = ie2017,
  # `2019` = ie2021,
  # `2021` = ie2021,
  # `2023` = ie2021,
  `2025` = ie2025
)

# Run function
instalaciones_clean <- setNames(
  lapply(
    years,
    function(y) {
      clean_installations(
        data = instalaciones_raw[[as.character(y)]],
        year = y,
        ie_cc = ie_cc
      )
    }
  ),
  years
)

# Save files
instalaciones_all <- bind_rows(
  lapply(instalaciones_clean, function(df) {
    df %>%
      mutate(across(-geometry, as.character))
  })
)

# Save combined spatial database
st_write(
  instalaciones_all,
  file.path(output_dir, "instalaciones_all_years.gpkg"),
  layer = "instalaciones_all",
  delete_dsn = TRUE
)

# 2. Save each year separately as a GeoPackage (with geometry)
for (year in names(instalaciones_clean)) {
  
  st_write(
    instalaciones_clean[[year]],
    file.path(output_dir, paste0("instalaciones_2_", year, ".gpkg")),
    layer = paste0("instalaciones_", year),
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
}

# Prepare Excel dataset
instalaciones_excel <- instalaciones_all %>%
  st_drop_geometry() %>%
  mutate(
    X_25830 = as.numeric(X_25830),
    Y_25830 = as.numeric(Y_25830)
  )

# Export
writexl::write_xlsx(
  instalaciones_excel,
  file.path(output_dir, "instalaciones_all_years.xlsx")
)

# Non-function format 2017 ----
# Filter to keep only Y and SR
ie2017 <- ie2017 %>% 
  filter(incluido %in% c("Y", "SR"))

havexy <- !is.na(ie2017$coordenada_x_local) & !is.na(ie2017$coordenada_y_local) &
  ie2017$coordenada_x_local != 0 &
  ie2017$coordenada_y_local != 0

reproy <- !havexy & 
  !is.na(ie2017$coordenada_x_agrupacion) &
  !is.na(ie2017$coordenada_y_agrup)

ie2017$X_25830 <- NA_real_
ie2017$Y_25830 <- NA_real_

## Fix the different kinds of errors in local coordinates
ie2017 <- ie2017 %>%
  mutate(
    X_local_corrected = case_when(
      coordenada_x_local >= 400000 &
        coordenada_x_local <= 500000 ~ as.numeric(coordenada_x_local),
      
      coordenada_x_local / 10 >= 400000 &
        coordenada_x_local / 10 <= 500000 ~ as.numeric(coordenada_x_local) / 10,
      
      coordenada_x_local / 100 >= 400000 &
        coordenada_x_local / 100 <= 500000 ~ as.numeric(coordenada_x_local) / 100,
      
      coordenada_x_local / 1000 >= 400000 &
        coordenada_x_local / 1000 <= 500000 ~ as.numeric(coordenada_x_local) / 1000,
      
      coordenada_x_local / 10000 >= 400000 &
        coordenada_x_local / 10000 <= 500000 ~ as.numeric(coordenada_x_local) / 10000,
      
      coordenada_x_local / 100000 >= 400000 &
        coordenada_x_local / 100000 <= 500000 ~ as.numeric(coordenada_x_local) / 100000,
      
      coordenada_x_local / 1000000 >= 400000 &
        coordenada_x_local / 1000000 <= 500000 ~ as.numeric(coordenada_x_local) / 1000000,
      
      coordenada_x_local / 10000000 >= 400000 &
        coordenada_x_local / 10000000 <= 500000 ~ as.numeric(coordenada_x_local) / 10000000,
      
      coordenada_x_local / 100000000 >= 400000 &
        coordenada_x_local / 100000000 <= 500000 ~ as.numeric(coordenada_x_local) / 100000000,
      
      coordenada_x_local / 1000000000 >= 400000 &
        coordenada_x_local / 1000000000 <= 500000 ~ as.numeric(coordenada_x_local) / 1000000000,
      
      TRUE ~ NA_real_
    ),
    
    Y_local_corrected = case_when(
      coordenada_y_local >= 4460000 &
        coordenada_y_local <= 4490000 ~ as.numeric(coordenada_y_local),
      
      coordenada_y_local / 10 >= 4460000 &
        coordenada_y_local / 10 <= 4490000 ~ as.numeric(coordenada_y_local) / 10,
      
      coordenada_y_local / 100 >= 4460000 &
        coordenada_y_local / 100 <= 4490000 ~ as.numeric(coordenada_y_local) / 100,
      
      coordenada_y_local / 1000 >= 4460000 &
        coordenada_y_local / 1000 <= 4490000 ~ as.numeric(coordenada_y_local) / 1000,
      
      coordenada_y_local / 10000 >= 4460000 &
        coordenada_y_local / 10000 <= 4490000 ~ as.numeric(coordenada_y_local) / 10000,
      
      coordenada_y_local / 100000 >= 4460000 &
        coordenada_y_local / 100000 <= 4490000 ~ as.numeric(coordenada_y_local) / 100000,
      
      coordenada_y_local / 1000000 >= 4460000 &
        coordenada_y_local / 1000000 <= 4490000 ~ as.numeric(coordenada_y_local) / 1000000,
      
      coordenada_y_local / 10000000 >= 4460000 &
        coordenada_y_local / 10000000 <= 4490000 ~ as.numeric(coordenada_y_local) / 10000000,
      
      coordenada_y_local / 100000000 >= 4460000 &
        coordenada_y_local / 100000000 <= 4490000 ~ as.numeric(coordenada_y_local) / 100000000,
      
      TRUE ~ NA_real_
    )
  )

# Fix grouped coordinates before joining to final variables
ie2017 <- ie2017 %>%
  mutate(
    x_agrup_corrected = as.numeric(coordenada_x_agrupacion) /
      10^(pmax(nchar(as.character(coordenada_x_agrupacion)) - 6, 0)),
    
    y_agrup_corrected = as.numeric(coordenada_y_agrup) /
      10^(pmax(nchar(as.character(coordenada_y_agrup)) - 7, 0))
  )

# Join fixed coordinates to final variables
ie2017 <- ie2017 %>%
  mutate(
    X_25830 = coalesce(X_local_corrected, x_agrup_corrected),
    Y_25830 = coalesce(Y_local_corrected, y_agrup_corrected)
  )

# Load found coordinates with CARTOCIUDAD
coords_cc <- ie_cc %>%
  filter(YEAR == 2017) %>% 
  select(
    ID_LOCAL,
    LATITUD_WGS84_4326,
    LONGITUD_WGS84_4326
  ) %>%
  rename(
    id_local = ID_LOCAL,
    latitud = LATITUD_WGS84_4326,
    longitud = LONGITUD_WGS84_4326
  )

ie2017 <- ie2017 %>%
  left_join(
    coords_cc,
    by = "id_local"
  )

reproy_valid <- reproy &
  !is.na(ie2017$latitud) &
  !is.na(ie2017$longitud)

# Transform the geographic coordinates
puntos_geo <- st_as_sf(
  ie2017[reproy_valid, ],
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)

puntos_proyectados <- st_transform(puntos_geo, 25830)

# Extract the transformed coordinates
coords <- st_coordinates(puntos_proyectados)

ie2017$X_25830[reproy_valid] <- coords[, 1]
ie2017$Y_25830[reproy_valid] <- coords[, 2]

## convert to geopackage
ie2017_sf <- st_as_sf(
  ie2017,
  coords = c("X_25830", "Y_25830"),
  crs = 25830,
  remove = FALSE
)

st_write(
  ie2017_sf,
  "ie2017.gpkg",
  layer = "ie2017",
  delete_layer = TRUE
)

# Excel (geometry is dropped): Perform per year
write_xlsx(ie2017_sf, "ie2017.xlsx")

# Non-function format 2019 ----
# Filter to keep only Y and SR
ie2019 <- ie2019 %>% 
  filter(incluido %in% c("Y", "SR"))

havexy <- !is.na(ie2019$coordenada_x_local) & !is.na(ie2019$coordenada_y_local) &
  ie2019$coordenada_x_local != 0 &
  ie2019$coordenada_y_local != 0

reproy <- !havexy & 
  !is.na(ie2019$coordenada_x_agrupacion) &
  !is.na(ie2019$coordenada_y_agrup)

ie2019$X_25830 <- NA_real_
ie2019$Y_25830 <- NA_real_

## fix the different kinds of errors in coordinates
ie2019 <- ie2019 %>%
  mutate(
    X_local_corrected = case_when(
      coordenada_x_local >= 400000 &
        coordenada_x_local <= 500000 ~ as.numeric(coordenada_x_local),
      
      coordenada_x_local / 10 >= 400000 &
        coordenada_x_local / 10 <= 500000 ~ as.numeric(coordenada_x_local) / 10,
      
      coordenada_x_local / 100 >= 400000 &
        coordenada_x_local / 100 <= 500000 ~ as.numeric(coordenada_x_local) / 100,
      
      coordenada_x_local / 1000 >= 400000 &
        coordenada_x_local / 1000 <= 500000 ~ as.numeric(coordenada_x_local) / 1000,
      
      coordenada_x_local / 10000 >= 400000 &
        coordenada_x_local / 10000 <= 500000 ~ as.numeric(coordenada_x_local) / 10000,
      
      coordenada_x_local / 100000 >= 400000 &
        coordenada_x_local / 100000 <= 500000 ~ as.numeric(coordenada_x_local) / 100000,
      
      coordenada_x_local / 1000000 >= 400000 &
        coordenada_x_local / 1000000 <= 500000 ~ as.numeric(coordenada_x_local) / 1000000,
      
      coordenada_x_local / 10000000 >= 400000 &
        coordenada_x_local / 10000000 <= 500000 ~ as.numeric(coordenada_x_local) / 10000000,
      
      coordenada_x_local / 100000000 >= 400000 &
        coordenada_x_local / 100000000 <= 500000 ~ as.numeric(coordenada_x_local) / 100000000,
      
      coordenada_x_local / 1000000000 >= 400000 &
        coordenada_x_local / 1000000000 <= 500000 ~ as.numeric(coordenada_x_local) / 1000000000,
      
      TRUE ~ NA_real_
    ),
    
    Y_local_corrected = case_when(
      coordenada_y_local >= 4460000 &
        coordenada_y_local <= 4490000 ~ as.numeric(coordenada_y_local),
      
      coordenada_y_local / 10 >= 4460000 &
        coordenada_y_local / 10 <= 4490000 ~ as.numeric(coordenada_y_local) / 10,
      
      coordenada_y_local / 100 >= 4460000 &
        coordenada_y_local / 100 <= 4490000 ~ as.numeric(coordenada_y_local) / 100,
      
      coordenada_y_local / 1000 >= 4460000 &
        coordenada_y_local / 1000 <= 4490000 ~ as.numeric(coordenada_y_local) / 1000,
      
      coordenada_y_local / 10000 >= 4460000 &
        coordenada_y_local / 10000 <= 4490000 ~ as.numeric(coordenada_y_local) / 10000,
      
      coordenada_y_local / 100000 >= 4460000 &
        coordenada_y_local / 100000 <= 4490000 ~ as.numeric(coordenada_y_local) / 100000,
      
      coordenada_y_local / 1000000 >= 4460000 &
        coordenada_y_local / 1000000 <= 4490000 ~ as.numeric(coordenada_y_local) / 1000000,
      
      coordenada_y_local / 10000000 >= 4460000 &
        coordenada_y_local / 10000000 <= 4490000 ~ as.numeric(coordenada_y_local) / 10000000,
      
      coordenada_y_local / 100000000 >= 4460000 &
        coordenada_y_local / 100000000 <= 4490000 ~ as.numeric(coordenada_y_local) / 100000000,
      
      TRUE ~ NA_real_
    )
  )

# Fix grouped coordinates before joining to final variables
ie2019 <- ie2019 %>%
  mutate(
    x_agrup_corrected = as.numeric(coordenada_x_agrupacion) /
      10^(pmax(nchar(as.character(coordenada_x_agrupacion)) - 6, 0)),

    y_agrup_corrected = as.numeric(coordenada_y_agrup) /
      10^(pmax(nchar(as.character(coordenada_y_agrup)) - 7, 0))
  )

# Join fixed coordinates to final variables
ie2019 <- ie2019 %>%
  mutate(
    X_25830 = coalesce(X_local_corrected, x_agrup_corrected),
    Y_25830 = coalesce(Y_local_corrected, y_agrup_corrected)
  )

# Load found coordinates with CARTOCIUDAD
coords_cc <- ie_cc %>%
  filter(YEAR == 2019) %>% 
  select(
    ID_LOCAL,
    LATITUD_WGS84_4326,
    LONGITUD_WGS84_4326
  ) %>%
  rename(
    id_local = ID_LOCAL,
    latitud = LATITUD_WGS84_4326,
    longitud = LONGITUD_WGS84_4326
  )

ie2019 <- ie2019 %>%
  left_join(
    coords_cc,
    by = "id_local"
  )

reproy_valid <- reproy &
  !is.na(ie2019$latitud) &
  !is.na(ie2019$longitud)

# Transform the geographic coordinates
puntos_geo <- st_as_sf(
  ie2019[reproy_valid, ],
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)

puntos_proyectados <- st_transform(puntos_geo, 25830)

# Extract the transformed coordinates
coords <- st_coordinates(puntos_proyectados)

ie2019$X_25830[reproy_valid] <- coords[, 1]
ie2019$Y_25830[reproy_valid] <- coords[, 2]

## convert to geopackage
ie2019_sf <- st_as_sf(
  ie2019,
  coords = c("X_25830", "Y_25830"),
  crs = 25830,
  remove = FALSE
)

st_write(
  ie2019_sf,
  "ie2019.gpkg",
  layer = "ie2019",
  delete_layer = TRUE
)

# Excel (geometry is dropped): Perform per year
write_xlsx(ie2019_sf, "ie2019.xlsx")

# Non-function format 2021 ----
# Filter to keep only Y and SR
ie2021 <- ie2021 %>% 
  filter(incluido %in% c("Y", "SR"))

havexy <- !is.na(ie2021$coordenada_x_local) & !is.na(ie2021$coordenada_y_local) &
  ie2021$coordenada_x_local != 0 &
  ie2021$coordenada_y_local != 0

reproy <- !havexy & 
  !is.na(ie2021$coordenada_x_agrupacion) &
  !is.na(ie2021$coordenada_y_agrup)

ie2021$X_25830 <- NA_real_
ie2021$Y_25830 <- NA_real_

ie2021 <- ie2021 %>%
  mutate(
    x_local_corrected = as.numeric(coordenada_x_local) /
      if_else(nchar(as.character(coordenada_x_local)) > 6,
              10^(nchar(as.character(coordenada_x_local)) - 6), 1),
    
    y_local_corrected = as.numeric(coordenada_y_local) /
      if_else(nchar(as.character(coordenada_y_local)) > 7,
              10^(nchar(as.character(coordenada_y_local)) - 7), 1),
    
    x_agrup_corrected = as.numeric(coordenada_x_agrupacion) /
      10^(pmax(nchar(as.character(coordenada_x_agrupacion)) - 6, 0)),
    
    y_agrup_corrected = as.numeric(coordenada_y_agrup) /
      10^(pmax(nchar(as.character(coordenada_y_agrup)) - 7, 0))
  )

# Copy the existing projected coordinates
ie2021$X_25830[havexy] <- ie2021$x_local_corrected[havexy]
ie2021$Y_25830[havexy] <- ie2021$y_local_corrected[havexy]

# Copy existing grouped coordinates where in local are unavailable
ie2021$X_25830[reproy] <- ie2021$x_agrup_corrected[reproy]
ie2021$Y_25830[reproy] <- ie2021$y_agrup_corrected[reproy]

coords_cc <- ie_cc %>%
  filter(YEAR == 2021) %>% 
  select(
    ID_LOCAL,
    LATITUD_WGS84_4326,
    LONGITUD_WGS84_4326
  ) %>%
  rename(
    id_local = ID_LOCAL,
    latitud = LATITUD_WGS84_4326,
    longitud = LONGITUD_WGS84_4326
  )

ie2021 <- ie2021 %>%
  left_join(
    coords_cc,
    by = "id_local"
  )

reproy_valid <- reproy &
  !is.na(ie2021$latitud) &
  !is.na(ie2021$longitud)

# Transform the geographic coordinates
puntos_geo <- st_as_sf(
  ie2021[reproy_valid, ],
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)4552

puntos_proyectados <- st_transform(puntos_geo, 25830)

# Extract the transformed coordinates
coords <- st_coordinates(puntos_proyectados)

ie2021$X_25830[reproy_valid] <- coords[, 1]
ie2021$Y_25830[reproy_valid] <- coords[, 2]

## convert to geopackage
ie2021_sf <- st_as_sf(
  ie2021,
  coords = c("X_25830", "Y_25830"),
  crs = 25830,
  remove = FALSE
)

st_write(
  ie2021_sf,
  "ie2021.gpkg",
  layer = "ie2021",
  delete_layer = TRUE
)

# Excel (geometry is dropped): Perform per year
write_xlsx(ie2021_sf, "ie2021.xlsx")

# Non-function format 2023 ----
# Filter to keep only Y and SR
ie2023 <- ie2023 %>% 
  filter(incluido %in% c("Y", "SR"))

havexy <- !is.na(ie2023$coordenada_x_local) & !is.na(ie2023$coordenada_y_local) &
  ie2023$coordenada_x_local != 0 &
  ie2023$coordenada_y_local != 0

reproy <- !havexy & 
  !is.na(ie2023$coordenada_x_agrupacion) &
  !is.na(ie2023$coordenada_y_agrup)

ie2023$X_25830 <- NA_real_
ie2023$Y_25830 <- NA_real_

# Copy the existing projected coordinates
ie2023$X_25830[havexy] <- ie2023$coordenada_x_local[havexy]
ie2023$Y_25830[havexy] <- ie2023$coordenada_y_local[havexy]

# Copy existing grouped coordinates where in local are unavailable
ie2023$X_25830[reproy] <- ie2023$coordenada_x_agrupacion[reproy]
ie2023$Y_25830[reproy] <- ie2023$coordenada_y_agrup[reproy]

coords_cc <- ie_cc %>%
  filter(YEAR == 2023) %>% 
  select(
    ID_LOCAL,
    LATITUD_WGS84_4326,
    LONGITUD_WGS84_4326
  ) %>%
  rename(
    id_local = ID_LOCAL,
    latitud = LATITUD_WGS84_4326,
    longitud = LONGITUD_WGS84_4326
  )

ie2023 <- ie2023 %>%
  left_join(
    coords_cc,
    by = "id_local"
  )

reproy_valid <- reproy &
  !is.na(ie2023$latitud) &
  !is.na(ie2023$longitud)

# Transform the geographic coordinates
puntos_geo <- st_as_sf(
  ie2023[reproy_valid, ],
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)

puntos_proyectados <- st_transform(puntos_geo, 25830)

# Extract the transformed coordinates
coords <- st_coordinates(puntos_proyectados)

ie2023$X_25830[reproy_valid] <- coords[, 1]
ie2023$Y_25830[reproy_valid] <- coords[, 2]

## convert to geopackage
ie2023_sf <- st_as_sf(
  ie2023,
  coords = c("X_25830", "Y_25830"),
  crs = 25830,
  remove = FALSE
)

st_write(
  ie2023_sf,
  "ie2023.gpkg",
  layer = "ie2023",
  delete_layer = TRUE
)

# Excel (geometry is dropped): Perform per year
write_xlsx(ie2023_sf, "ie2023.xlsx")

# Non-function format 2025 ----
# Filter to keep only Y and SR
ie2025 <- ie2025 %>% 
  filter(incluido %in% c("Y", "SR"))

havexy <- !is.na(ie2025$coordenada_x_local) & !is.na(ie2025$coordenada_y_local) &
  ie2025$coordenada_x_local != 0 &
  ie2025$coordenada_y_local != 0

reproy <- !havexy & 
  !is.na(ie2025$coordenada_x_agrupacion) &
  !is.na(ie2025$coordenada_y_agrup)

ie2025$X_25830 <- NA_real_
ie2025$Y_25830 <- NA_real_

# Copy the existing projected coordinates
ie2025$X_25830[havexy] <- ie2025$coordenada_x_local[havexy]
ie2025$Y_25830[havexy] <- ie2025$coordenada_y_local[havexy]

# Copy existing grouped coordinates where in local are unavailable
ie2025$X_25830[reproy] <- ie2025$coordenada_x_agrupacion[reproy]
ie2025$Y_25830[reproy] <- ie2025$coordenada_y_agrup[reproy]

coords_cc <- ie_cc %>%
  filter(YEAR == 2025) %>% 
  select(
    ID_LOCAL,
    LATITUD_WGS84_4326,
    LONGITUD_WGS84_4326
  ) %>%
  rename(
    id_local = ID_LOCAL,
    latitud = LATITUD_WGS84_4326,
    longitud = LONGITUD_WGS84_4326
  )

ie2025 <- ie2025 %>%
  left_join(
    coords_cc,
    by = "id_local"
  )

reproy_valid <- reproy &
  !is.na(ie2025$latitud) &
  !is.na(ie2025$longitud)

# Transform the geographic coordinates
puntos_geo <- st_as_sf(
  ie2025[reproy_valid, ],
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)

puntos_proyectados <- st_transform(puntos_geo, 25830)

# Extract the transformed coordinates
coords <- st_coordinates(puntos_proyectados)

ie2025$X_25830[reproy_valid] <- coords[, 1]
ie2025$Y_25830[reproy_valid] <- coords[, 2]

## convert to geopackage
ie2025_sf <- st_as_sf(
  ie2025,
  coords = c("X_25830", "Y_25830"),
  crs = 25830,
  remove = FALSE
)

st_write(
  ie2025_sf,
  "ie2025.gpkg",
  layer = "ie2025",
  delete_layer = TRUE
)

# Excel (geometry is dropped): Perform per year
write_xlsx(ie2025_sf, "ie2025.xlsx")

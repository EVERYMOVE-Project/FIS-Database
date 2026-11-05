### Walkability Index Madrid 2017-2025
## Data Cleaning Script
## Start Date: 2026-07-15
## Current Date: 2026-10-27
### Libraries ----
library(dplyr)
library(readxl)
library(sf)
library(readr)
library(purrr)
library(tidyr)
library(foreign)

## Functions ----
clean_population <- function(year, data_dir) {
  # 1. Import the raw database -------------------------------
  
  file_path <- file.path(
    data_dir,
    paste0("pob", year, ".csv")
  )
  
  # Different settings for 2019
  if (year == 2019) {
    
    pob <- read_delim(
      file_path,
      delim = ";",
      col_names = TRUE,
      locale = locale(decimal_mark = ","),
      show_col_types = FALSE
    )
    
  } else {
    
    pob <- read_delim(
      file_path,
      delim = ";",
      col_names = TRUE,
      locale = locale(
        decimal_mark = ",",
        encoding = "Latin1"
      ),
      show_col_types = FALSE
    )
    
  }
  # 2. Apply the appropriate cleaning rules -----------------
  
  if (year %in% c(2017, 2019)) {
    
    # Older structure: Sección
    
    pob <- pob %>%
      filter(
        `Edad (grupos quinquenales)` == "Total",
        Sexo == "Ambos Sexos"
      ) %>%
      mutate(
        cpro = substr(Sección, 1, 2),
        cmun = substr(Sección, 3, 5),
        dist = substr(Sección, 6, 7),
        secc = substr(Sección, 8, 10)
      ) %>%
      rename(
        pob = Total,
        secc0 = Sección
      ) %>%
      filter(cmun == "079") %>%
      select(
        secc0, pob
      )
    
  } else if (year %in% c(2021, 2023, 2025)) {
    
    # Newer structure: Secciones
    pob <- pob %>%
      filter(
        Municipios == "28079 Madrid"
      ) %>%
      mutate(
        cpro = substr(Secciones, 1, 2),
        cmun = substr(Secciones, 3, 5),
        dist = substr(Secciones, 6, 7),
        secc = substr(Secciones, 8, 10),
        secc0 = substr(Secciones, 1, 10)
      ) %>%
      filter(cmun == "079") %>%
      rename(pob = Total) %>%
      select(
        secc0, pob
      )
    
  } 
  # 3. Standardise the output -------------------------------
  pob <- pob %>%
    mutate(
      secc0 = as.numeric(secc0)
    )
  return(pob)
}
clean_residential <- function(file_path) {
  res <- read_delim(
    file_path,
    delim = ",",
    col_names = TRUE,
    locale = locale(decimal_mark = ",", encoding = "Latin1"),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  res <- res %>% 
    select(cpro, cmun, dist, secc, t19_1) %>% 
    mutate(
      across(c(cpro, cmun, dist, secc), as.integer), 
      secc0 = as.numeric(paste0(
        sprintf("%02d", cpro),
        sprintf("%03d", cmun),
        sprintf("%02d", dist),
        sprintf("%03d", secc)
      )),
      res = parse_number(t19_1, locale = locale(decimal_mark = ","))
    ) %>% 
    filter(cpro == 28, cmun == 79) %>% 
    select(secc0, res)
  res
  
}
clean_retail <- function(year, retail_dir) {
  
  file_path <- file.path(
    retail_dir,
    paste0("retail_", year, ".gpkg")
  )
  
  retail <- st_read(file_path, quiet = TRUE) %>%
    st_drop_geometry() %>%
    transmute(
      secc0  = as.numeric(CUSEC),
      retail = mean
    )
  retail
}
clean_sections <- function(year, secc_dir) {
  
  file_path <- file.path(secc_dir, secc_files[[as.character(year)]])
  
  secc <- st_read(file_path, quiet = TRUE) %>%
    filter(substr(CUSEC, 1, 2) == "28", substr(CUSEC, 3, 5) == "079") %>%
    mutate(secc0 = as.numeric(CUSEC))
  
  # 2021+ files have no Shape_area / Shape_len: calculate them
  if (!"Shape_area" %in% names(secc)) {
    secc <- secc %>%
      mutate(
        Shape_area = as.numeric(st_area(geometry)),
        Shape_len  = as.numeric(st_length(st_boundary(geometry)))
      )
  }
  
  secc <- secc %>% select(secc0, Shape_area, Shape_len)
  return(secc)
}

# Data cleaning ----
data_dir <- file.path(
  "C:/Users/juani/Universidad de Alcala/",
  "EVERYMOVE Intranet - Documents/Paquetes de trabajo/",
  "WP2 - Entorno/Recursos Urbanos/Caminabilidad"
)

output_dir <- file.path(data_dir, "clean_data")
## 1. Population ----
years <- c(2017, 2019, 2021, 2023, 2025)

population_all <- setNames(
  lapply(
    years,
    function(y) clean_population(y, data_dir)
  ),
  years
)

for (year in years) {
  saveRDS(
    population_all[[as.character(year)]],
    file.path(output_dir, paste0("pob", year, "_clean.rds"))
  )
  write_csv(
    population_all[[as.character(year)]],
    file.path(output_dir, paste0("pob", year, "_clean.csv"))
  )
  
}

population_long <- bind_rows(population_all, .id = "year") %>%
  mutate(year = as.integer(year))

saveRDS(population_long, file.path(output_dir, "pob_all_years.rds"))
write_csv(population_long, file.path(output_dir, "pob_all_years.csv"))

## 2. Dwellings ----
res_path <- file.path(data_dir, "res2021.csv")
res <- clean_residential(res_path)

saveRDS(res, file.path(output_dir, "res_clean.rds"))
write_csv(res, file.path(output_dir, "res_clean.csv"))
## 3. Retails ----
retail_dir <- file.path(
  data_dir,
  "QGIS exploratory/RETAIL DENSITY/street network buffers"
)

retail_all <- setNames(
  lapply(years, function(y) clean_retail(y, retail_dir)),
  years
)

retail_long <- bind_rows(retail_all, .id = "year") %>%
  mutate(year = as.integer(year))

saveRDS(retail_long, file.path(output_dir, "retail_all_years.rds"))

## 4. Intersections ----
int_path <- file.path(
  data_dir,
  "QGIS Exploratory/INTERSECTION DENSITY COUNT/numint2025.gpkg"
)

int <- st_read(int_path, quiet = TRUE) %>%
  st_drop_geometry() %>%
  transmute(
    secc0 = as.numeric(CUSEC),
    int   = count
  )

saveRDS(int, file.path(output_dir, "int2025_clean.rds"))
write_csv(int, file.path(output_dir, "int2025_clean.csv"))
## 5. Census sections ----
secc_dir <- file.path(data_dir, "QGIS exploratory/SECCIONADO")

secc_files <- c(
  "2017" = "seccionado_2017/SECC_CE_20170101.shp",
  "2019" = "seccionado_2019/SECC_CE_20190101.shp",
  "2021" = "seccionado_2021/SECC_CE_20210101.shp",
  "2023" = "seccionado_2023/SECC_CE_20230101.shp",
  "2025" = "seccionado_2025/SECC_CE_20250101.shp"
)

secc_all <- setNames(
  lapply(years, function(y) clean_sections(y, secc_dir)),
  years
)

secc_long <- bind_rows(
  lapply(secc_all, st_drop_geometry), .id = "year"
) %>%
  mutate(year = as.integer(year)) %>% 
  select(year, secc0, Shape_area, Shape_len)

saveRDS(secc_long, file.path(output_dir, "secc_all_years.rds"))
# Geometries kept separately, one per year, for mapping
saveRDS(secc_all, file.path(output_dir, "secc_geometries_by_year.rds"))

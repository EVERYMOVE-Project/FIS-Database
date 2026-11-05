### Walkability Index Madrid 2017-2025
## Data Calculations
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
library(mapSpain)
library(mapview)

## Directories ----
data_dir <- file.path(
  "C:/Users/juani/Universidad de Alcala/",
  "EVERYMOVE Intranet - Documents/Paquetes de trabajo/",
  "WP2 - Entorno/Recursos Urbanos/Caminabilidad"
)

output_dir <- file.path(data_dir, "clean_data")

pob_long    <- readRDS(file.path(output_dir, "pob_all_years.rds"))
res         <- readRDS(file.path(output_dir, "res_clean.rds"))
retail_long <- readRDS(file.path(output_dir, "retail_all_years.rds"))
int         <- readRDS(file.path(output_dir, "int2025_clean.rds"))
secc_long   <- readRDS(file.path(output_dir, "secc_all_years.rds"))
## Join all datasets ----
walk_long <- pob_long %>%
  left_join(secc_long,   by = c("secc0", "year")) %>%
  left_join(retail_long, by = c("secc0", "year")) %>%
  left_join(res,         by = "secc0") %>%     
  left_join(int,         by = "secc0")         

## Calculations ----
walk_long <- walk_long %>%
  mutate(
    pob_dens = pob / (Shape_area / 1e6),
    res_dens = res / (Shape_area / 1e6)
  ) %>%
  group_by(year) %>%
  mutate(
    pob_dens_z = as.numeric(scale(pob_dens)),
    res_dens_z = as.numeric(scale(res_dens)),
    retail_z   = as.numeric(scale(retail)),
    int_z      = as.numeric(scale(int)),
    
    walkability   = 0.25 * pob_dens_z + 0.25 * res_dens_z +
      0.25 * retail_z   + 0.25 * int_z,
    walkability_z = as.numeric(scale(walkability))
  ) %>%
  ungroup()

## NAs ----
walk_long %>% filter(is.na(walkability_z)) %>% count(year)
walk_long %>%
  filter(is.na(walkability_z)) %>%
  summarise(across(c(pob_dens, res_dens, retail, int), ~ sum(is.na(.x))), .by = year)

## Export ----
# Per year:
years_walk <- sort(unique(walk_long$year))

for (y in years_walk) {
  
  walk_y <- walk_long %>%
    filter(year == y)
  
  write_csv(
    walk_y,
    file.path(output_dir, paste0("walkability_", y, "_with_NA.csv"))
  )
  
  walk_y %>%
    filter(!is.na(walkability_z)) %>%
    select(secc0, walkability_z) %>%
    write_csv(file.path(output_dir, paste0("walkability_", y, "_qgis.csv")))
}

# Walkability Index all Years
  # NAs included
saveRDS(walk_long, file.path(, "walkability_long_with_NA.rds"))
write_csv(walk_long, file.path(output_dir, "walkability_long_with_NA.csv"))

# Clean, one column per year
  ## NAs included
walk_wide <- walk_long %>%
  select(secc0, year, walkability_z) %>%
  pivot_wider(names_from = year, names_prefix = "walk", values_from = walkability_z)

write_csv(walk_wide, file.path(output_dir, "walkability_wide_with_NA.csv")) 
walk_qgis <- walk_wide %>% drop_na()
write_csv(walk_qgis, file.path(output_dir, "walkability_qgis.csv"))

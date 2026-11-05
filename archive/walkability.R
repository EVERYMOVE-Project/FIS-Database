## Start Date: 2026-07-15
## Current Date: 2026-09-28
## Walkability 

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

library(lubridate) # service area consultation

### Dataset Cleaning ----
# 1. Population ----
# 2017, 2021, 2023, 2025
pob <- read_delim(
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/pob2025.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(decimal_mark = ",", encoding = "Latin1")
)

# 2019 
pob <- read_delim(
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/pob2019.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(decimal_mark = ",")
)

# creating new identifier variables 2017, 2019
pob <- pob %>%
  filter(`Edad (grupos quinquenales)` == "Total") %>% 
  filter(Sexo == "Ambos Sexos") %>% 
  mutate(
    cpro = substr(Sección, 1, 2), # create to filter by municipality
    cmun = substr(Sección, 3, 5),
    dist = substr(Sección, 6, 7),
    secc = substr(Sección, 8, 10)
  ) %>%
  relocate(cpro, cmun, dist, secc, .before = Sección) %>% 
  rename(pob = Total, secc0 = Sección) %>% 
  filter(cmun == "079") %>% 
  select(-Sexo, -`Edad (grupos quinquenales)`) %>% 
  mutate(secc0 = as.numeric(secc0))

pob <- pob %>%
  select(-cpro, -cmun, -dist, -secc) # remove

# creating new identifier variables 2021, 2023, 2025
pob <- pob %>%
  filter(Municipios == "28079 Madrid") %>% 
  mutate(
    cpro = substr(Secciones, 1, 2), # create to filter by municipality
    cmun = substr(Secciones, 3, 5),
    dist = substr(Secciones, 6, 7),
    secc = substr(Secciones, 8, 10),
    secc0 = substr(Secciones, 1, 10)
  ) %>%
  relocate(cpro, cmun, dist, secc, secc0, .before = Secciones) %>% 
  rename(pob = Total) %>% 
  filter(cmun == "079") %>% 
  select(-Sexo, -Nacionalidad, -Periodo, -Provincias, -Municipios, -Secciones) %>% 
  mutate(secc0 = as.numeric(secc0))

pob <- pob %>%
  select(-cpro, -cmun, -dist, -secc) # remove

# check duplicates
sum(duplicated(pob$secc0))

# 2. Dwellings ----
# Only have residential census from 2011 and 2021
# Use res2021 for all years
res <- read_delim(
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/res2021.csv",
  delim = ",",
  col_names = TRUE,
  locale = locale(decimal_mark = ",", encoding = "Latin1")
)

# # keep relevant variables 2017, 2019
# # cpro, cmun, dist, secc, t17_1 (occupied dwellings)
# res <- res %>% 
#   select(cpro, cmun, dist, secc, t17_1)
# 
# # create combined census section variable: Sección
# res <- res %>% 
#   mutate(Sección = paste0(
#     sprintf("%02d", as.numeric(cpro)),
#     sprintf("%03d", as.numeric(cmun)),
#     sprintf("%02d", as.numeric(dist)),
#     sprintf("%03d", as.numeric(secc))
#   )) %>%
#   relocate(Sección, .after = secc) %>% 
#   rename(secc0 = Sección, res = t17_1) %>% 
#   mutate(secc0 = as.numeric(secc0)) %>% 
#   filter(cmun == "079") 
# 
# res <- res %>%
#   select(-cpro, -cmun, -dist, -secc) # remove

# keep relevant variables 2021, 2023, 2025
# cpro, cmun, dist, secc, t19_1 (main dwellings)
res <- res %>% 
  select(cpro, cmun, dist, secc, t19_1)

# create combined census section variable: Sección
res <- res %>% 
  mutate(Sección = paste0(
    sprintf("%02d", as.numeric(cpro)),
    sprintf("%03d", as.numeric(cmun)),
    sprintf("%02d", as.numeric(dist)),
    sprintf("%03d", as.numeric(secc))
  )) %>%
  relocate(Sección, .after = secc) %>% 
  rename(secc0 = Sección, res = t19_1) %>% 
  mutate(secc0 = as.numeric(secc0)) %>% 
  filter(cpro == "28") %>% 
  filter(cmun == "079") 

res <- res %>%
  select(-cpro, -cmun, -dist, -secc) # remove

# check duplicates
sum(duplicated(res$secc0))

# 3. Retails ----
loc <- read_delim( # change for each year
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/loc2017.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(decimal_mark = ",", encoding = "Latin1")
)

# adapt census section label to other dataframes
loc <- loc %>% 
  mutate(
    dist = sprintf("%02d", as.numeric(id_distrito_local)),
    secc = sprintf("%03d", as.numeric(desc_seccion_censal_local)))

# dataframe with count of retails per census section
loc <- loc %>% 
  count(dist, secc, name = "local") %>% 
  mutate(
    cpro = "28",
    cmun = "079",
    secc0 = paste0(cpro, cmun, dist, secc)) %>%
  mutate(secc0 = as.numeric(secc0)) %>% 
  relocate(cpro, cmun, dist, secc, secc0, .before = local) %>% 
  filter(!is.na(secc0)) # identified one row with no values for id_distrito_local etc., but is cpro 27 therefore does not affect analysis, exclude

loc <- loc %>%
  select(-cpro, -cmun, -dist, -secc) # remove

# check duplicates
sum(duplicated(loc$secc0))

# 3.5 Retails 2 ----
## Change per year of interest
retails <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/QGIS exploratory/RETAIL DENSITY/street network buffers/2025/media_comercios_ccss_1-21.gpkg")

retails <- retails %>% 
  rename(secc0 = CUSEC) %>%
  mutate(secc0 = as.numeric(secc0))

retails <- retails %>% 
  select(secc0, mean)

# 4. Street Connectivity ----
int <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/QGIS Exploratory/INTERSECTION DENSITY COUNT/numint2025.gpkg")

int <- int %>% 
  rename(secc0 = CUSEC) %>%
  mutate(secc0 = as.numeric(secc0))

int <- int %>%
  select(secc0, count)

# 5. Census Section ----
# Adapt file location for each year
  # to capture area of census sections
secc <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/QGIS exploratory/SECCIONADO/seccionado_2025/SECC_CE_20250101.shp")

# 2017 and 2019
# clean secc dataframe with:
  # CUSEC
  # Shape_area in m2
  # Shape_len in m
  # geometry
secc <- secc %>% 
  filter(substr(CUSEC, 1, 2) == "28") %>% 
  filter(CMUN == "079") %>%
  select(CUSEC, Shape_area, Shape_len, geometry) %>%
  rename(secc0 = CUSEC) %>%
  mutate(secc0 = as.numeric(secc0))

# 2021, 2023, 2025
secc <- secc %>% 
  filter(substr(CUSEC, 1, 2) == "28") %>% 
  filter(CMUN == "079") %>%
  select(CUSEC, geometry) %>%
  rename(secc0 = CUSEC) %>%
  mutate(secc0 = as.numeric(secc0),
         Shape_area = as.numeric(st_area(.)),
         Shape_len = as.numeric(st_length(st_boundary(.)))
  )
         
### Join Datasets ---- 
# check all secc0 identifiers are same type
listdf <- list(pob, res, retails, int, secc)
map(listdf, ~ class(.x$secc0))

# join dataframes
walk2017 <- reduce(listdf, left_join, by = "secc0")
walk2019 <- reduce(listdf, left_join, by = "secc0")
walk2021 <- reduce(listdf, left_join, by = "secc0")
walk2023 <- reduce(listdf, left_join, by = "secc0")
walk2025 <- reduce(listdf, left_join, by = "secc0")

### Calculations 2017 ----
### Calculate density variables
walk2017 <- walk2017 %>%
  mutate(
    pob_dens = pob / (Shape_area/1e6),
    res_dens = res / (Shape_area/1e6)
  ) 

### Standardise density variables
walk2017 <- walk2017 %>%
  mutate(
    pob_dens_z = scale(pob_dens)[,1],
    res_dens_z = scale(res_dens)[,1],
    retail_dens_z = scale(mean)[,1],
    int_dens_z = scale(count)[,1]
  )

### Equal weights (0.25 each)
walk2017 <- walk2017 %>%
  mutate(
    w_pob = 0.25 * pob_dens_z,
    w_res = 0.25 * res_dens_z,
    w_retail = 0.25 * retail_dens_z,
    w_int = 0.25 * int_dens_z)

### Walkability index
walk2017 <- walk2017 %>%
  mutate(
    walkability = w_pob + w_res + w_retail + w_int
  ) 

summary(walk2017$walkability)
hist(walk2017$walkability)

### Standardise walkability
walk2017 <- walk2017 %>%
  mutate(
    walkability_z_score = scale(walkability)[,1]
  ) %>% 
  filter(!is.na(walkability_z_score))

summary(walk2017$walkability_z_score)
hist(walk2017$walkability_z_score)

### Calculations 2019 ----
### Calculate density variables
walk2019 <- walk2019 %>%
  mutate(
    pob_dens = pob / (Shape_area/1e6),
    res_dens = res / (Shape_area/1e6)
  ) 

### Standardise density variables
walk2019 <- walk2019 %>%
  mutate(
    pob_dens_z = scale(pob_dens)[,1],
    res_dens_z = scale(res_dens)[,1],
    retail_dens_z = scale(mean)[,1],
    int_dens_z = scale(count)[,1]
  )

### Equal weights (0.25 each)
walk2019 <- walk2019 %>%
  mutate(
    w_pob = 0.25 * pob_dens_z,
    w_res = 0.25 * res_dens_z,
    w_retail = 0.25 * retail_dens_z,
    w_int = 0.25 * int_dens_z)

### Walkability index
walk2019 <- walk2019 %>%
  mutate(
    walkability = w_pob + w_res + w_retail + w_int
  ) 

summary(walk2019$walkability)
hist(walk2019$walkability)

### Standardise walkability
walk2019 <- walk2019 %>%
  mutate(
    walkability_z_score = scale(walkability)[,1]
  ) %>%
  filter(!is.na(walkability_z_score))

summary(walk2019$walkability_z_score)
hist(walk2019$walkability_z_score)

### Calculations 2021 ----
### Calculate density variables
walk2021 <- walk2021 %>%
  mutate(
    pob_dens = pob / (Shape_area/1e6),
    res_dens = res / (Shape_area/1e6)
  ) 

### Standardise density variables
walk2021 <- walk2021 %>%
  mutate(
    pob_dens_z = scale(pob_dens)[,1],
    res_dens_z = scale(res_dens)[,1],
    retail_dens_z = scale(mean)[,1],
    int_dens_z = scale(count)[,1]
  )

### Equal weights (0.25 each)
walk2021 <- walk2021 %>%
  mutate(
    w_pob = 0.25 * pob_dens_z,
    w_res = 0.25 * res_dens_z,
    w_retail = 0.25 * retail_dens_z,
    w_int = 0.25 * int_dens_z)

### Walkability index
walk2021 <- walk2021 %>%
  mutate(
    walkability = w_pob + w_res + w_retail + w_int
  ) 

summary(walk2021$walkability)
hist(walk2021$walkability)

### Standardise walkability
walk2021 <- walk2021 %>%
  mutate(
    walkability_z_score = scale(walkability)[,1]
  ) %>%
  filter(!is.na(walkability_z_score))

summary(walk2021$walkability_z_score)
hist(walk2021$walkability_z_score)

### Calculations 2023 ----
### Calculate density variables
walk2023 <- walk2023 %>%
  mutate(
    pob_dens = pob / (Shape_area/1e6),
    res_dens = res / (Shape_area/1e6)
  ) 

### Standardise density variables
walk2023 <- walk2023 %>%
  mutate(
    pob_dens_z = scale(pob_dens)[,1],
    res_dens_z = scale(res_dens)[,1],
    retail_dens_z = scale(mean)[,1],
    int_dens_z = scale(count)[,1]
  )

### Equal weights (0.25 each)
walk2023 <- walk2023 %>%
  mutate(
    w_pob = 0.25 * pob_dens_z,
    w_res = 0.25 * res_dens_z,
    w_retail = 0.25 * retail_dens_z,
    w_int = 0.25 * int_dens_z)

### Walkability index
walk2023 <- walk2023 %>%
  mutate(
    walkability = w_pob + w_res + w_retail + w_int
  ) 

summary(walk2023$walkability)
hist(walk2023$walkability)

### Standardise walkability
walk2023 <- walk2023 %>%
  mutate(
    walkability_z_score = scale(walkability)[,1]
  ) %>%
  filter(!is.na(walkability_z_score))

summary(walk2023$walkability_z_score)
hist(walk2023$walkability_z_score)

### Calculations 2025 ----
### Calculate density variables
walk2025 <- walk2025 %>%
  mutate(
    pob_dens = pob / (Shape_area/1e6),
    res_dens = res / (Shape_area/1e6),
  )

### Standardise density variables
walk2025 <- walk2025 %>%
  mutate(
    pob_dens_z = scale(pob_dens)[,1],
    res_dens_z = scale(res_dens)[,1],
    retail_dens_z = scale(mean)[,1],
    int_dens_z = scale(count)[,1]
  )

### Equal weights (0.25 each)
walk2025 <- walk2025 %>%
  mutate(
    w_pob = 0.25 * pob_dens_z,
    w_res = 0.25 * res_dens_z,
    w_retail = 0.25 * retail_dens_z,
    w_int = 0.25 * int_dens_z)

### Walkability index
walk2025 <- walk2025 %>%
  mutate(
    walkability = w_pob + w_res + w_retail + w_int
  )

summary(walk2025$walkability)
hist(walk2025$walkability)

### Standardise walkability
walk2025na <- walk2025 %>%
  mutate(
    walkability_z_score = scale(walkability)[,1]
  ) 

walk2025 <- walk2025na %>% 
  filter(!is.na(walkability_z_score))

summary(walk2025$walkability_z_score)
hist(walk2025$walkability_z_score)

# check missingness
walk2025 %>%
  summarise(
    missing_pob = sum(is.na(pob_dens_z)),
    missing_res = sum(is.na(res_dens_z)),
    missing_retail = sum(is.na(retail_dens_z)),
    missing_intersections = sum(is.na(int_dens_z)),
    missing_walkability = sum(is.na(walkability_z_score))
  )

### Save ----
write_csv(walk2017, "walkability_2017.csv")
write_csv(walk2019, "walkability_2019.csv")
write_csv(walk2021, "walkability_2021.csv")
write_csv(walk2023, "walkability_2023.csv")
write_csv(walk2025, "walkability_2025.csv")

# walkability index dataframe all years
walkability_all <- list(
  walk2017 %>% select(secc0, walkability_z_score) %>% rename(walk2017 = walkability_z_score),
  walk2019 %>% select(secc0, walkability_z_score) %>% rename(walk2019 = walkability_z_score),
  walk2021 %>% select(secc0, walkability_z_score) %>% rename(walk2021 = walkability_z_score),
  walk2023 %>% select(secc0, walkability_z_score) %>% rename(walk2023 = walkability_z_score),
  walk2025 %>% select(secc0, walkability_z_score) %>% rename(walk2025 = walkability_z_score)
) %>%
  reduce(left_join, by = "secc0")

write_csv(walkability_all, "walkability.csv")

dt <- read_delim(
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/FIS-Database-2026/walkability.csv",
  delim = ",",
  col_names = TRUE,
  locale = locale(decimal_mark = ",", encoding = "Latin1")
)

### Review NAs ----
# NAs in separate databases
res %>%
  filter(is.na(res)) %>%
  select(secc0)

na_pob  <- pob  %>% filter(is.na(pob)) %>% select(secc0) %>% mutate(source = "pob")
na_res  <- res  %>% filter(is.na(res)) %>% select(secc0) %>% mutate(source = "res")
na_retails  <- retails  %>% filter(is.na(mean)) %>% select(secc0) %>% mutate(source = "retails")
na_int  <- int  %>% filter(is.na(count)) %>% select(secc0) %>% mutate(source = "int")

na_all <- bind_rows(na_pob, na_res, na_retails, na_int)
na_all

# census sections that are in original that are not in new walkability2017
censusreview <- c(
  "2807902112","2807902111","2807904131","2807903100","2807905104",
  "2807905103","2807908182","2807908183","2807908181","2807908186",
  "2807908187","2807908188","2807908185","2807908184","2807909092",
  "2807910221","2807911195","2807911196","2807911197","2807911198",
  "2807913217","2807914094","2807914095","2807916126","2807916127",
  "2807916128","2807916129","2807917115","2807917116","2807918066",
  "2807918067","2807918068","2807920123","2807920124","2807920125",
  "2807920126","2807921033","2807919052"
)

# census sections that are in new walkability2017 that are not in original
censusreview <- c(
  "2807903062","2807907112","2807910104","2807910196","2807911084",
  "2807911159","2807912021","2807912036","2807913064","2807914001",
  "2807914023","2807914078","2807915139","2807916062","2807920084"
)

table_na <- data.frame(secc0 = censusreview) %>%
  mutate(
    pob_NA  = !(secc0 %in% pob$secc0) | secc0 %in% pob$secc0[is.na(pob$pob)],
    res_NA  = !(secc0 %in% res$secc0) | secc0 %in% res$secc0[is.na(res$res)],
    loc_NA  = !(secc0 %in% loc$secc0) | secc0 %in% loc$secc0[is.na(loc$local)],
    con_NA  = !(secc0 %in% con$secc0) | secc0 %in% con$secc0[is.na(con$`_mean`)]
  )
# where TRUE means yes there is an NA or missing
table_na

# all rows in walkability_all where across all years NA
walkability_all %>%
  filter(
    if_all(-c(secc0), is.na)
  )

# count NAs per year
walkability_all %>%
  filter(
    if_all(c(walk2017, walk2019, walk2021, walk2023, walk2025), is.na)
  ) %>%
  nrow()

walkability_all %>%
  summarise(
    across(
      c(walk2017, walk2019, walk2021, walk2023, walk2025),
      ~ sum(is.na(.))
    )
  )

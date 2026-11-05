### Walkability Index Madrid 2017-2025
## Additional Testing
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
library(lubridate) # service area consultation


### Correlation Test Pedro-2017 and New-2017 ----
# load 2017 intersection density kernel
walk2017 <- read_csv("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/Gullon et al/walkability_seccion_2017.csv")
con2017 <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/int2-2017.shp")

con2017 <- con2017 %>%
  mutate(
    con_z = scale(X_mean)[,1],
    con_z_w = 0.25 * con_z,
    con_km2 = X_mean * 1000000
  )

con2017 <- con2017 %>% 
  select(CUSEC, X_mean, con_z, con_z_w, con_km2) %>% 
  rename(secc0 = CUSEC) %>% 
  mutate(secc0 = as.numeric(secc0))

# crude kernel density
summary(con2017$X_mean)
summary(walk2017$`_mean`)

# standardised mean
summary(con2017$con_z)
summary(walk2017$con_dens_z)

# weighted 0.25
summary(con2017$con_z_w)
summary(walk2017$w_con) 

# join for correlation
comparison_df <- walk2017 %>%
  inner_join(con2017, by = "secc0")

# spearman rank correlation
corr_test <- cor.test(comparison_df$con_dens_z, comparison_df$con_z, 
                      method = "spearman", use = "complete.obs")
corr_test

## Correlation Test of 2017-OSM and 2026-OSM ----
# in QGIS perform "unir atributos por localizacion" to identify number of intersections per ccss
# load data
numint2017 <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/numint-ccss_2017.shp")
numint2017 <- numint2017 %>% 
  mutate(
    code = as.factor(code)
  )
  
numint2017 <- numint2017 %>% 
  group_by(CUSEC) %>% 
  summarise(int = n())

# standardise number of intersections
numint2017 <- numint2017 %>%
  mutate(
    int_z = scale(`int`)[,1]
  )

# weighted number of intersections
numint2017 <- numint2017 %>%
  mutate(
    int_z_w = 0.25*int_z
  )

numint2026 <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/numint-ccss_2025.shp")
numint2026 <- numint2026 %>% 
  group_by(CUSEC) %>% 
  summarise(int = n())

# standardise number of intersections
numint2026 <- numint2026 %>%
  mutate(
    int_z = scale(`int`)[,1]
  )

# weighted number of intersections
numint2026 <- numint2026 %>%
  mutate(
    int_z_w = 0.25*int_z
  )

combine_numint <- numint2017 %>%
  inner_join(numint2026 %>% st_drop_geometry(), by = "CUSEC")

# spearman rank correlation
corr_test <- cor.test(combine_numint$int_z.x, combine_numint$int_z.y, 
                      method = "spearman", use = "complete.obs")

corr_test #0.814 

## Correlation Test between walkability index 2025 with 2025 int.dens. and walkability index 2025 with 2017 int.dens. ----
database_final <- walk2025 %>%
  select(secc0, walkability_z_score) %>%
  left_join(
    walk2025_2 %>%
      select(secc0, walkability_z_score),
    by = "secc0"
  ) 

corr_test <- cor.test(database_final$walkability_z_score.x, database_final$walkability_z_score.y,
                      method = "spearman", use = "complete.obs")
corr_test

### Correlation Kernel 2025 and 2017 ----
kernelint2025 <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/QGIS exploratory/mean kernel3-2025.shp")
kernelint2017 <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/QGIS exploratory/mean kernel3-2017.shp")

kernelint2017 <- kernelint2017 %>%
  mutate(
    int_z = scale(X_meanmean)[,1],
    int_z_w = 0.25 * int_z
  )

kernelint2025 <- kernelint2025 %>%
  mutate(
    int_z = scale(X_mean)[,1],
    int_z_w = 0.25 * int_z
  )

hist(kernelint2025$int_z)
hist(kernelint2017$int_z)
hist(con2017$con_)

summary(walk2017$con_dens_z)
summary(kernelint2017$int_z)
summary(kernelint2025$int_z)

kernelint2017 <- kernelint2017 %>% 
  rename(secc0 = CUSEC) %>% 
  mutate(secc0 = as.numeric(secc0))

# join for correlation
comparison_df <- walk2017 %>%
  inner_join(kernelint2017, by = "secc0")

# spearman rank correlation
corr_test <- cor.test(comparison_df$con_dens_z, comparison_df$int_z, 
                      method = "spearman", use = "complete.obs")
corr_test

### Retails Service Area Consultation ----
portales <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/QGIS exploratory/PORTALES GEOPORTAL/ndp_vig_his.shp")

portales <- portales %>% 
  mutate(
    FECHA_DE_A = as.Date(FECHA_DE_A),
    FECHA_DE_B = as.Date(FECHA_DE_B)
  ) %>% 
  filter(TIPO_NDP == 'Portal')

years <- c(2017, 2019, 2021, 2023, 2025, 2026)

resumen_portales <- bind_rows(lapply(years, function(y) {
  
  inicio <- as.Date(paste0(y, "-01-01"))
  fin <- as.Date(paste0(y, "-12-31"))
  siguiente <- as.Date(paste0(y + 1, "-01-01"))
  
  # Exclude the anomalous date only from 2017 movement counts
  fecha_inicio_movimientos <- if (y == 2017) {
    as.Date("2017-01-01")
  } else {
    inicio
  }
  
  tibble(
    year = y,
    
    created = sum(
      portales$FECHA_DE_A >= fecha_inicio_movimientos &
        portales$FECHA_DE_A < siguiente &
        !(y == 2017 & portales$FECHA_DE_A == as.Date("2017-05-04")),
      na.rm = TRUE
    ),
    
    deactivated = sum(
      portales$FECHA_DE_B >= fecha_inicio_movimientos &
        portales$FECHA_DE_B < siguiente &
        !(y == 2017 & portales$FECHA_DE_B == as.Date("2017-05-04")),
      na.rm = TRUE
    ),
    
    # Excluding any changes on may 4th
    active_year_end = sum(
      !is.na(portales$FECHA_DE_A) &
        portales$FECHA_DE_A <= fin &
        (is.na(portales$FECHA_DE_B) |
           portales$FECHA_DE_B > fin) &
        if (y == 2017) {
          portales$FECHA_DE_A != as.Date("2017-05-04") &
            (is.na(portales$FECHA_DE_B) |
               portales$FECHA_DE_B != as.Date("2017-05-04"))
        } else {
          TRUE
        },
      na.rm = TRUE
    )
  )
  
})) %>%
  mutate(net_change = created - deactivated)
resumen_portales

resumen_portales <- bind_rows(lapply(years, function(y) {
  
  inicio <- as.Date(paste0(y, "-01-01"))
  fin <- as.Date(paste0(y, "-12-31"))
  siguiente <- as.Date(paste0(y + 1, "-01-01"))
  
  tibble(
    year = y,
    
    created = sum(
      portales$FECHA_DE_A >= inicio &
        portales$FECHA_DE_A < siguiente,
      na.rm = TRUE
    ),
    
    deactivated = sum(
      portales$FECHA_DE_B >= inicio &
        portales$FECHA_DE_B < siguiente,
      na.rm = TRUE
    ),
    
    active_year_end = sum(
      portales$FECHA_DE_A <= fin &
        (is.na(portales$FECHA_DE_B) |
           portales$FECHA_DE_B > fin),
      na.rm = TRUE
    )
  )
})) %>%
  mutate(net_change = created - deactivated)

# checking if any replicate portales in database
portales %>%
  filter(
    TIPO_NDP == "PORTAL",
    is.na(FECHA_DE_B)
  ) %>%
  count(COD_NDP, name = "n_records") %>%
  filter(n_records > 1) %>%
  arrange(desc(n_records))

### Retail database for the commercial density calculation ----
# Cleaning the comercios database 2017 
loc <- read_delim( # change for each year
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/loc2023.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(decimal_mark = ",", encoding = "Latin1")
)

# adapt census section label to other dataframes
loc <- loc %>% 
  mutate(
    dist = sprintf("%02d", as.numeric(id_distrito_local)),
    secc = sprintf("%03d", as.numeric(desc_seccion_censal_local)))

str(loc$coordenada_x_local)
str(loc$coordenada_y_local)
str(loc$coordenada_x_agrupacion)
str(loc$coordenada_y_agrupacion)

unique(loc$coordenada_x_local)
unique(loc$coordenada_y_local)
unique(loc$coordenada_x_agrupacion)
unique(loc$coordenada_y_agrupacion)

# Cleaning the comercios database 2017, 2019, 2021
locclean <- loc %>% 
  rename(coordenada_y_agrupacion = coordenada_y_agrup) %>% 
  mutate(
    X_coordinates = if_else(coordenada_x_local == 0, coordenada_x_agrupacion, coordenada_x_local),
    Y_coordinates = if_else(coordenada_y_local == 0, coordenada_y_agrupacion, coordenada_y_local)
  ) 

# Cleaning the comercios database 2023, 2025 
locclean <- loc %>% 
  mutate(
    X_coordinates = if_else(coordenada_x_local == "0.0", coordenada_x_agrupacion, coordenada_x_local),
    Y_coordinates = if_else(coordenada_y_local == "0.0", coordenada_y_agrupacion, coordenada_y_local))

locmaster <- locclean

# Convert coordinates to a spatial object
loc_sf <- locmaster %>% 
  filter(!is.na(X_coordinates), 
         !is.na(Y_coordinates)
         ) %>% 
  st_as_sf(coords = c("X_coordinates", "Y_coordinates"), crs = 25830)

# Change for each year to save
st_write(loc_sf, "loc_madrid_2023.gpkg", layer = "loc")

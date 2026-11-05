## October 1, 2026

# intersection density test
### Libraries ----
library(dplyr)
library(readxl)
library(sf)
library(readr)
library(purrr)
library(tidyr)
library(foreign)
library(mapSpain)

### Load new 2017 kernel ----
# load 2017 intersection density kernel
con2017 <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/int2-2017.shp")

con2017 <- st_read("C:/Users/Juanita Mora/OneDrive - Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/int2-2017.shp")

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

# load walk2017
walk2017 <- read_delim(
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/FIS-Database-2026/walkability_2017.csv",
  delim = ",",
  col_names = TRUE)

### Correlation test of 2017-pedro and 2017-new ----
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

comparison_df %>% 
  select(secc0, con_dens_z, con_z)

## Number intersections per ccss ----

# in QGIS perform "unir atributos por localizacion" to identify number of intersections per ccss

# load data
numint2017 <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/numint-ccss_2017.shp")

numint2026 <- numint2026 %>% 
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

### Correlation test of 2017-OSM and 2026-OSM ----
combine_numint <- numint2017 %>%
  inner_join(numint2026 %>% st_drop_geometry(), by = "CUSEC")

# spearman rank correlation
corr_test <- cor.test(combine_numint$int_z.x, combine_numint$int_z.y, 
                      method = "spearman", use = "complete.obs")

corr_test #0.814 

### Correlation Kernel 2025 and 2027 ----
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

comparison_df %>% 
  select(secc0, con_dens_z, con_z)


### Libraries ----

### Load data ----
### 1. Poblacion ----
pob <- read_delim(
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/pob2025.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(decimal_mark = ",", encoding = "Latin1")
)

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
  select(-Sexo, -Nacionalidad, -Periodo, -Provincias, -Municipios, -Secciones, -cpro, -cmun, -dist, -secc) %>% 
  mutate(secc0 = as.numeric(secc0))

### 2. Seccion Censal ----
secc <- st_read("C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/Recursos Urbanos/Caminabilidad/seccionado/seccionado_2025/SECC_CE_20250101.shp")

secc <- secc %>% 
  filter(substr(CUSEC, 1, 2) == "28") %>% 
  filter(CMUN == "079") %>%
  select(CUSEC, geometry) %>%
  rename(secc0 = CUSEC) %>%
  mutate(secc0 = as.numeric(secc0),
         Shape_area = as.numeric(st_area(.)),
         Shape_len = as.numeric(st_length(st_boundary(.)))
  )

### Renta media y mediana ----
renta <- read_delim(
  "C:/Users/juani/Universidad de Alcala/EVERYMOVE Intranet - Documents/Paquetes de trabajo/WP2 - Entorno/renta.csv",
  delim = ";",
  col_names = TRUE,
  locale = locale(decimal_mark = ",")
)

renta_media <- renta %>% 
  filter(Municipios == "28079 Madrid") %>% 
  filter(Periodo == "2023") %>% 
  filter(`Indicadores de renta media y mediana` == "Renta neta media por persona") %>% 
  mutate(
    cpro = substr(Secciones, 1, 2), # create to filter by municipality
    cmun = substr(Secciones, 3, 5),
    dist = substr(Secciones, 6, 7),
    secc = substr(Secciones, 8, 10),
    secc0 = substr(Secciones, 1, 10)
  ) %>%
  filter(cmun == "079") %>% 
  rename(rentamediap = Total) %>%
  mutate(secc0 = as.numeric(secc0),
         rentamediap = as.numeric(gsub("\\.", "", rentamediap))) %>%  
  relocate(secc0, .before = rentamediap) %>% 
  select(-Distritos, -`Indicadores de renta media y mediana`, -Municipios, -Secciones, -cpro, -cmun, -dist, -secc, -Periodo)
  
renta_mediana <- renta %>% 
  filter(Municipios == "28079 Madrid") %>% 
  filter(Periodo == "2023") %>% 
  filter(`Indicadores de renta media y mediana` == "Mediana de la renta por unidad de consumo") %>% 
  mutate(
    cpro = substr(Secciones, 1, 2), # create to filter by municipality
    cmun = substr(Secciones, 3, 5),
    dist = substr(Secciones, 6, 7),
    secc = substr(Secciones, 8, 10),
    secc0 = substr(Secciones, 1, 10)
  ) %>%
  filter(cmun == "079") %>% 
  rename(medianarentaconsumo = Total) %>%
  mutate(secc0 = as.numeric(secc0),
         medianarentaconsumo = as.numeric(gsub("\\.", "", medianarentaconsumo))) %>% 
  relocate(secc0, .before = medianarentaconsumo) %>% 
  select(-Distritos, -`Indicadores de renta media y mediana`, -Municipios, -Secciones, -cpro, -cmun, -dist, -secc, -Periodo)

### Unir bases por seccion censal ----
# check all secc0 identifiers are same type
listdf <- list(secc, pob, renta_media, renta_mediana)
map(listdf, ~ class(.x$secc0))

# join dataframes
renta_sscc <- reduce(listdf, left_join, by = "secc0") 

# save 
write_csv(renta_sscc, "renta_sscc.csv")
save(renta_sscc, file = "renta_sscc.RData")

# base con las secciones censales a 2025, población, superficie, renta media por persona y renta mediana equivalente.
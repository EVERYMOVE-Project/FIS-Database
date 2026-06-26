## Start Date: 2026-06-01
## Current Date: 2026-06-10
## Database Instalaciones Deportivas Basicas y Polideportivos

# Libraries ----
library(readxl)
library(dplyr)

# Load data ----
dt2026 <- read_excel("INSTALACIONES DEPORTIVAS BASICAS MUNICIPALES + POLIDEPORTIVOS 2026.xlsx")
dt <- read_excel("Instalaciones Deportivas Municipales Completo.xlsx")

## dt removing excluded
dt <- dt %>% 
  filter(MANTENER == 1)

# Comparison of dt and dt2026 ----

## check if PKs are unique
sum(duplicated(dt$PK))
sum(duplicated(dt2026$PK))

## Old dataset 541 facilities
nrow(dt)

## New dataset 677 facilities
nrow(dt2026)

## Common facilities 491 facilities
common_pk <- intersect(dt$PK, dt2026$PK)

## added and removed
added <- anti_join(dt2026, dt, "PK")
removed <- anti_join(dt, dt2026, by = "PK")

added_pk <- setdiff(dt2026$PK, dt$PK)
removed_pk <- setdiff(dt$PK, dt2026$PK)

n_added <- nrow(added) # 186
n_removed <- nrow(removed) # 50

# Summary ----
summary <- data.frame(
  old = nrow(dt),
  new = nrow(dt2026),
  common = length(common_pk),
  added = length(added_pk),
  removed = length(removed_pk)
)
summary

# Added Facilities ----
added <- dt2026 %>% 
  filter(PK %in% setdiff(dt2026$PK, dt$PK))

table(added$TIPO) # 177 instalaciones + 9 polideportivos

# can't know which type were removed

# New dt2026 ----
dt2026 <- dt2026 %>% 
  select(-`DESCRIPCION-ENTIDAD`, -DESCRIPCION, -ACCESIBILIDAD, 
     -`CONTENT-URL`, -PLANTA, -PUERTA, -ESCALERAS, -ORIENTACION,
     -FAX, -TIPO2)

View(dt2026)
saveRDS(dt2026, file = "instalaciones2026.RData")

# https://github.com/EVERYMOVE-Project/FIS-Database
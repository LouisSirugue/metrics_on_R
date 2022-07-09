
library(tidyverse)
library(readxl)
library(foreign)
library(sf)
library(raster)
select <- dplyr::select
library(ncdf4)

setwd("D:/Postgrad/Thèse/data-analysis-course-gh-pages/data-analysis-course-gh-pages/lecture15/data/")

# Revenu disponible par unité de consommation médian
####################################################

# 2012 : https://www.insee.fr/fr/statistiques/1895078
# 2013 : https://www.insee.fr/fr/statistiques/2388572
# 2014 : https://www.insee.fr/fr/statistiques/3126432
# 2015 : https://www.insee.fr/fr/statistiques/3560121
# 2016 : https://www.insee.fr/fr/statistiques/4190004
# 2017 : https://www.insee.fr/fr/statistiques/4507225?sommaire=4507229

med <- read_xlsx(path = paste0("revenu/2017.xlsx"), 
                 sheet = "DEP", skip = 5) %>%
              select(CODGEO, LIBGEO, MED17)

for (i in 16:12) {
  med <- med %>% 
    left_join(read_xls(path = paste0("revenu/", 2000 + i, ".xls"), 
                       sheet = "DEP", skip = 5) %>%
                select(CODGEO, LIBGEO, paste0("MED", i)))
}

# Population légale
###################

# 2012 : https://www.insee.fr/fr/statistiques/2119595?sommaire=2119686
# 2013 : https://www.insee.fr/fr/statistiques/2387611?sommaire=2119504
# 2014 : https://www.insee.fr/fr/statistiques/2525755?sommaire=2525768
# 2015 : https://www.insee.fr/fr/statistiques/3545833?sommaire=3292701
# 2016 : https://www.insee.fr/fr/statistiques/3677785?sommaire=3677855
# 2017 : https://www.insee.fr/fr/statistiques/4265429?sommaire=4265511

pop <- read_xls(path = paste0("population/2017.xls"), 
                sheet = "Départements", skip = 7) %>%
  rename(CODGEO = `Code département`, 
         LIBGEO = `Nom du département`, 
         POP17 = `Population totale`) %>%
  select(CODGEO, LIBGEO, POP17)

for (i in 16:12) {
  pop <- pop %>%
    left_join(read_xls(path = paste0("population/", 2000 + i, ".xls"), 
                       sheet = "Départements", skip = 7) %>%
                rename(CODGEO = `Code département`, 
                       LIBGEO = `Nom du département`, 
                       POP = `Population totale`) %>%
                select(CODGEO, LIBGEO, POP) %>%
                rename_at("POP", function(x){paste0(x, i)}))
} 

# Taux de chômage
#################

# https://www.insee.fr/fr/statistiques/series/102760732

chom <- read_xlsx("chomage.xlsx") %>% 
  filter(grepl("département", Libellé)) %>%
  mutate(LIBGEO = gsub("Taux de chômage localisé par département - ", "", Libellé)) %>%
  select(-c(`2021-T2`, Libellé, idBank, Période, `Dernière mise à jour`)) %>%
  mutate_all(as.character) %>%
  pivot_longer(-LIBGEO, names_to = "trim", values_to = "chom") %>%
  mutate(an = substr(trim, 3, 4)) %>%
  filter(an %in% 12:17) %>%
  group_by(LIBGEO, an) %>%
  summarise(CHOM = mean(as.numeric(chom))) %>%
  ungroup() %>%
  pivot_wider(values_from = CHOM, names_from = an) %>% 
  rename_at(vars(-LIBGEO), function(x){paste0("CHOM", x)}) 

# Familles monoparentales
#########################

# 2012 : https://www.insee.fr/fr/statistiques/2028569
# 2013 : https://www.insee.fr/fr/statistiques/2386664
# 2014 : https://www.insee.fr/fr/statistiques/3137412
# 2015 : https://www.insee.fr/fr/statistiques/3627367
# 2016 : https://www.insee.fr/fr/statistiques/4228428
# 2017 : https://www.insee.fr/fr/statistiques/4799268

mono <- read_xlsx("mono/2017.xlsx", sheet = "IRIS", skip = 5) %>%
          group_by(DEP) %>%
          summarise(MONO17 = sum(C17_FAMMONO, na.rm = T) / sum(C17_FAM, na.rm = T))

for (i in 16:12) {
  mono <- mono %>% 
    left_join(read_xls(path = paste0("mono/", 2000 + i, ".xls"), 
                       sheet = "IRIS", skip = 5) %>%
                group_by(DEP) %>%
                summarise(MONO = sum(get(paste0("C", i, "_FAMMONO")), na.rm = T) / 
                            sum(get(paste0("C", i, "_FAM")), na.rm = T)) %>%
                rename_at("MONO", function(x){paste0(x, i)}))
}
  
mono <- mono %>% rename(CODGEO = DEP)
            
# Other variables

other <- tibble(CODGEO = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", 
                           "16", "17", "18", "19", "21", "22", "23", "24", "25", "26", "27", "28", "29", "2A", "2B", 
                           "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", 
                           "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", 
                           "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", 
                           "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",  
                           "90", "91", "92", "93", "94", "95"),
                metrop = c(   0,    0,    0,    0,    0,    1,    0,    0,    0,    0,    0,    0,    1,    0,    0,    
                              0,    0,    0,    0,    1,    0,    0,    0,    0,    0,    0,    0,    1,    0,    0,
                              0,    0,    0,    1,    1,    1,    0,    1,    1,    0,    0,    0,    1,    0,    0,
                              1,    0,    0,    0,    0,    0,    0,    0,    0,    1,    0,    0,    1,    0,    1,
                              0,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0,    0,    0,    0,
                              1,    1,    0,    0,    0,    0,    0,    0,    1,    0,    0,    0,    0,    0,    0,
                              0,    0,    0,    0,    0,    0),
                fleuve = c(   1,    0,    1,    0,    0,    0,    1,    1,    0,    0,    0,    0,    1,    1,    1,    
                              0,    0,    1,    1,    0,    0,    0,    1,    0,    1,    1,    0,    0,    0,    0,
                              1,    1,    1,    1,    0,    0,    0,    1,    1,    0,    1,    1,    1,    1,    1,
                              1,    1,    1,    0,    1,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0,
                              0,    0,    0,    1,    1,    1,    0,    1,    1,    1,    0,    1,    0,    1,    1,
                              1,    1,    1,    1,    0,    0,    0,    1,    0,    1,    0,    0,    0,    1,    0,
                              0,    1,    1,    1,    1,    1))

# PM2.5 
#######

#https://sites.wustl.edu/acag/datasets/surface-pm2-5/

# Import data
dep_shp <-  st_transform(read_sf("pollution/DEPARTEMENT.shp") %>% 
  filter(nchar(INSEE_DEP) == 2) %>%
  select(INSEE_DEP, geometry), "EPSG:4326")

pm_data <- raster("pollution/2017.nc")
crs(pm_data) <- "EPSG:4326"
names(pm_data) <- "pm2.5"
pm_data <- crop(pm_data, extent(dep_shp))
pm_data <- mask(pm_data, dep_shp)
pm_data[is.na(pm_data)] <- 0

pol <- dep_shp %>% 
  mutate(POL17 = extract(x = pm_data, y = ., fun = mean)) %>%
  st_drop_geometry()

for (i in 16:12) {
  pm_data <- raster(paste0("pollution/", 2000 + i, ".nc"))
  crs(pm_data) <- "EPSG:4326"
  names(pm_data) <- "pm2.5"
  pm_data[is.na(pm_data)] <- 0
  pm_data <- crop(pm_data, extent(dep_shp))
  pm_data <- mask(pm_data, dep_shp)
  
  pol <- pol %>%
    left_join(dep_shp %>% 
                mutate(POL = extract(x = pm_data, y = ., fun = mean))  %>% 
                st_drop_geometry() %>%
                rename_at("POL", function(x){paste0(x, i)}))
  
}

pol <- pol %>% rename(CODGEO = INSEE_DEP)

# Merge everything
##################

dep_data <- chom %>%
  left_join(med) %>%
  left_join(mono) %>%
  left_join(pol) %>%
  left_join(pop)

dep_data <- dep_data %>%
  pivot_longer(-c(LIBGEO, CODGEO), names_to = "varyear", values_to = "values") %>%
  mutate(varname = substr(varyear, 1, nchar(varyear) - 2),
         year = 2000 + as.numeric(substr(varyear, nchar(varyear) - 1, nchar(varyear)))) %>%
  left_join(other) %>%
  select(-c(varyear, CODGEO)) %>%
  pivot_wider(names_from = "varname", values_from = values) %>%
  na.omit() %>%
  rename(Department = LIBGEO,
         Year = year,
         Metropole = metrop,
         `Main river` = fleuve,
         `Unemployment rate` = CHOM,
         `Median income` = MED,
         `Share single parents` = MONO,
         `PM2.5 concentration` = POL,
         Population = POP) %>%
  mutate(`Log population` = log(Population),
         `Log median income` = log(`Median income`),
         `Share single parents` = 100 * `Share single parents`)

dep_data <- dep_data %>%
  mutate(Department = ifelse(Department == "Ardèche", "Ardeche", Department),
         Department = ifelse(Department == "Ariège", "Ariege", Department),
         Department = ifelse(Department == "Bouches-du-Rhône", "Bouches-du-Rhone", Department),
         Department = ifelse(Department == "Corrèze", "Correze", Department),
         Department = ifelse(Department == "Côte-d'Or", "Cote-d'Or", Department),
         Department = ifelse(Department == "Côtes-d'Armor", "Cotes-d'Armor", Department),
         Department = ifelse(Department == "Deux-Sèvres", "Deux-Sevres", Department),
         Department = ifelse(Department == "Drôme", "Drome", Department),
         Department = ifelse(Department == "Finistère", "Finistere", Department),
         Department = ifelse(Department == "Hautes-Pyrénées", "Hautes-Pyrenees", Department),
         Department = ifelse(Department == "Haute-Saône", "Haute-Saone", Department),
         Department = ifelse(Department == "Hérault", "Herault", Department),
         Department = ifelse(Department == "Isère", "Isere", Department),
         Department = ifelse(Department == "Lozère", "Lozere", Department),
         Department = ifelse(Department == "Nièvre", "Nievre", Department),
         Department = ifelse(Department == "Puy-de-Dôme", "Puy-de-Dome", Department),
         Department = ifelse(Department == "Pyrénées-Atlantiques", "Pyrenees-Atlantiques", Department),
         Department = ifelse(Department == "Pyrénées-Orientales", "Pyrenees-Orientales", Department),
         Department = ifelse(Department == "Rhône", "Rhone", Department),
         Department = ifelse(Department == "Saône-et-Loire", "Saone-et-Loire", Department),
         Department = ifelse(Department == "Vendée", "Vendee", Department))

# Remplacer les noms de départements

write.csv(dep_data, "dep_data.csv", row.names = F)

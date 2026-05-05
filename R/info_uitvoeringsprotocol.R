# dummy_data


# project_gebied <- "EP"
# project_activiteiten <- "1a"
# uitvoering_start <-  "2026-06-01"
# uitvoering_eind <-  "2026-07-31"

# Start echte script

library(tidyverse)
library(readxl)

bestand_up <- "data/opzet_data_input_ingevuld.xlsx"

gebieden <- read_excel(bestand_up, sheet = "gebieden") 
werkzaamheden <- read_excel(bestand_up, sheet = "werkzaamheden") 
soorten <- read_excel(bestand_up, sheet = "soorten") 
gebied_soorten <- read_excel(bestand_up, sheet = "gebied_soorten") 

periode_sel <- interval(as_date(uitvoering_start), as_date(uitvoering_eind))

maatregelen <- 
  read_excel(bestand_up, sheet = "maatregelen") %>% 
  select(1:4)

algemene_maatregelen <- 
  read_excel(bestand_up, sheet = "algemene_maatregelen") %>% 
  select(-contains("omschrijving")) %>% 
  left_join(maatregelen, by = join_by(maatregel_code))

soortspecifieke_maatregelen <- 
  read_excel(bestand_up, sheet = "soortspecifieke_maatregelen") %>% 
  select(-contains("omschrijving")) %>% 
  left_join(maatregelen, by = join_by(maatregel_code)) %>% 
  mutate(periode = interval(start = make_date(year = year(Sys.Date()), month = month(periode_begin), day = day(periode_begin)),
                            end   = make_date(year = year(Sys.Date()), month = month(periode_eind) , day = day(periode_eind)))
         )

activiteit_sel <- 
  werkzaamheden %>% filter(werk_code %in% project_activiteiten) %>% 
  pull(werk_omschrijving)

soorten_sel <-
  gebied_soorten %>% 
  filter(gebied_code %in% project_gebied) %>% 
  pull(soort)


algemene_maatregelen_sel <- 
  algemene_maatregelen %>% 
  filter(werk_code %in% project_activiteiten) %>% 
  select(fase, maatregel_omschrijving)

soortspecifieke_maatregelen_sel <-
  soortspecifieke_maatregelen %>% 
  filter(werk_code %in% project_activiteiten,
         soort %in% soorten_sel,
         int_overlaps(periode_sel, periode)
         
         )  %>% 
  select(soort, fase, maatregel_omschrijving)
  
# dummy_data

if(interactive()){
project_gebied <- c("EP", "GZ")
project_activiteiten <- "1a"
uitvoering_start <-  "2026-01-01"
uitvoering_eind <-  "2026-12-31"
}
# Start echte script

library(tidyverse)
library(readxl)
library(glue)

bestand_up <- "data/opzet_data_input_ingevuld.xlsx"

gebieden <- read_excel(bestand_up, sheet = "gebieden") 
werkzaamheden <- read_excel(bestand_up, sheet = "werkzaamheden") 
soorten <- read_excel(bestand_up, sheet = "soorten") 
gebied_soorten <- read_excel(bestand_up, sheet = "gebied_soorten") 

project_gebieden_sel <- 
  gebieden %>% 
  filter(gebied_code %in% project_gebied) %>% 
  pull(gebied_omschrijving) %>% 
  glue_collapse(sep = ", ", last = " en ")

periode_sel <- interval(as_date(uitvoering_start), as_date(uitvoering_eind))

maatregelen <- 
  read_excel(bestand_up, sheet = "maatregelen") %>% 
  select(1:4)

algemene_maatregelen <- 
  read_excel(bestand_up, sheet = "algemene_maatregelen") %>% 
  filter_out(is.na(maatregel_code)) %>% 
  select(-contains("omschrijving")) %>% 
  left_join(maatregelen, by = join_by(maatregel_code))

soortspecifieke_maatregelen <- 
  read_excel(bestand_up, sheet = "soortspecifieke_maatregelen") %>% 
  filter_out(is.na(maatregel_code)) %>% 
  select(-contains("omschrijving")) %>% 
  left_join(maatregelen, by = join_by(maatregel_code)) %>% 
  mutate(periode_begin = make_date(year = year(Sys.Date()), month = month(periode_begin), day = day(periode_begin)),
         periode_eind  = make_date(year = year(Sys.Date()), month = month(periode_eind) , day = day(periode_eind)),
         # een extra jaar toevoegen als de jaargrens wordt gepasseerd
         periode_eind = if_else(periode_begin > periode_eind, periode_eind + period(1, "year"), periode_eind)
         ) %>% 
  mutate(periode = interval(periode_begin, periode_eind))

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
  mutate(maatregel_code = str_pad(maatregel_code, width = 3, side = "right")) %>% 
  mutate(maatregel_tekst_basis = glue("- {maatregel_omschrijving} ({maatregel_code})")) %>% 
  group_by(maatregel_type) %>% 
  summarise(maatregel_tekst = glue_collapse(maatregel_tekst_basis, sep = "\n")) %>% 
  ungroup() %>% 
  mutate(maatregel_tekst = glue("### {maatregel_type} \n\n{maatregel_tekst}")) %>% 
  pull(maatregel_tekst) %>% 
  glue_collapse(sep = "\n\n")


# soorten_sel <- c("Naakte lathyrus", "Rugstreeppad") # temp
  
soortspecifieke_maatregelen_sel <-
  soortspecifieke_maatregelen %>% 
  filter(werk_code %in% project_activiteiten,
         soort %in% soorten_sel,
         int_overlaps(periode_sel, periode)
         
         ) %>% 
    arrange(periode_begin) %>% 
    mutate(maatregel_tekst_basis = glue("- {maatregel_omschrijving} ({maatregel_code})")) %>% 
    select(-maatregel_omschrijving, -maatregel_code) %>%  # tijdelijk voor meer overzicht
    group_by(periode_begin, periode_eind, periode, maatregel_type, maatregel_tekst_basis) %>% #View("basis")
    summarise(soorten = glue_collapse(soort, sep = ", " )) %>% # gaat ervan uit dat een maatregel in een bepaalde periode voor een soort maar eenmaal wordt genoemd.
    group_by(soorten, periode_begin, periode_eind, periode, maatregel_type) %>% 
    summarise(maatregel_tekst = glue_collapse(maatregel_tekst_basis, sep = "\n")) %>% 
    ungroup() %>% 
    mutate(maatregel_tekst = glue("Voor de soort(en): **{soorten}**\n\n{maatregel_tekst}")) %>% 
    group_by(periode_begin, periode_eind, periode, maatregel_type) %>% 
    summarise(maatregel_tekst = glue_collapse(maatregel_tekst, sep = "\n\n")) %>% 
    ungroup() %>% 
    mutate(maatregel_tekst = glue("In de periode van **{periode_begin} tot {periode_eind}**\n\n{maatregel_tekst}")) %>% 
    group_by(maatregel_type) %>% 
    summarise(maatregel_tekst = glue_collapse(maatregel_tekst, sep = "\n\n")) %>% 
    ungroup() %>% 
    mutate(maatregel_tekst = glue("### {maatregel_type} \n\n{maatregel_tekst}")) %>% 
    pull(maatregel_tekst) %>% 
    glue_collapse(sep = "\n\n")
  
    
    
    
    
  
    
  
  
  
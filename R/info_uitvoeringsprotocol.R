
# Setup -------------------------------------------------------------------

# dummy_data

# uitgebreid
if(interactive()){
  project_gebied <- c("SN")
  project_subgebied <- c("EP", "GZ")
  project_activiteiten <- "1a"
  uitvoering_start <-  "2026-01-01"
  uitvoering_eind <-  "2026-12-31"
  project_habitatbenadering <- TRUE
}

# simpel
if(interactive()){
  project_gebied <- c("SN")
  project_subgebied <- c("EP")
  project_activiteiten <- "1a"
  uitvoering_start <-  "2026-06-01"
  uitvoering_eind <-  "2026-08-31"
  project_habitatbenadering <- TRUE
}


library(tidyverse)
library(readxl)
library(glue)

# Inlezen data ------------------------------------------------------------

bestand_up <- 
  tibble(up_bestanden = list.files("data/", pattern = "^maatregelen_uitvoeringsprotocol", full.names = TRUE)) %>% 
  mutate(datum = ymd(str_extract(up_bestanden, "\\d{4}-\\d{2}-\\d{2}"))) %>% 
  filter(datum == max(datum, na.rm = TRUE)) %>% 
  pull(up_bestanden)


gebieden <- read_excel(bestand_up, sheet = "gebieden") 
werkzaamheden <- read_excel(bestand_up, sheet = "werkzaamheden") 
soorten <- read_excel(bestand_up, sheet = "soorten") 
gebied_soorten <- 
  read_excel(bestand_up, sheet = "gebied_soorten") %>% 
  filter_out(is.na(gebied_code))

maatregelen <- 
  read_excel(bestand_up, sheet = "maatregelen") %>% 
  mutate(habitatbenadering = case_when(
    str_to_upper(habitatbenadering) == "JA" ~ TRUE, 
    str_to_upper(habitatbenadering) == "NEE" ~ FALSE,
    .default = NA)) %>%
  # select(1:4) %>% 
  mutate(maatregel_nr = as.numeric(str_extract(maatregel_code, "\\d+")),
         maatregel_letters = str_extract(maatregel_code, "\\D+")) %>% 
  mutate(fase_nr = recode_values(fase, "voorbereiding" ~ 1, "uitvoering" ~ 2)) %>% 
  arrange(fase_nr, maatregel_type, maatregel_letters, maatregel_nr) %>% 
  mutate(maatregel_type = glue("{fase} - {maatregel_type}") |> str_to_sentence() |> fct_inorder()) 

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
         periode_eind  = if_else(periode_begin > periode_eind, periode_eind + period(1, "year"), periode_eind),
         # een extra jaar toevoegen als de periode is verstreken
         periode_begin = if_else(Sys.Date() > periode_eind, periode_begin + period(1, "year"), periode_begin),
         periode_eind  = if_else(periode_begin > periode_eind, periode_eind + period(1, "year"), periode_eind)
         ) %>% 
  mutate(periode = interval(periode_begin, periode_eind))


# Selectie van soorten en maatregelen -------------------------------------

project_gebieden_sel <- 
  gebieden %>% 
  filter(gebied_code %in% project_gebied) %>% 
  select(gebied_omschrijving) %>% 
  distinct() %>% 
  pull(gebied_omschrijving) %>% 
  glue_collapse(sep = ", ", last = " en ")

project_subgebieden_sel <- 
  gebieden %>% 
  filter(gebied_code %in% project_gebied, subgebied_code %in% project_subgebied) %>% 
  select(subgebied_omschrijving) %>% 
  distinct() %>% 
  pull(subgebied_omschrijving) %>% 
  glue_collapse(sep = ", ", last = " en ")


periode_sel <- interval(as_date(uitvoering_start), as_date(uitvoering_eind))

activiteit_sel <- 
  werkzaamheden %>% 
  filter(werk_code %in% project_activiteiten) %>% 
  pull(werk_omschrijving)

soorten_sel <-
  gebied_soorten %>% 
  filter(gebied_code %in% project_gebied,
         is.na(subgebied_code) | subgebied_code %in% project_subgebied,
         !is.na(soort)) %>% 
  pull(soort)


algemene_maatregelen_sel <-
  algemene_maatregelen %>% 
  filter(werk_code %in% project_activiteiten) %>% 
  filter_out(habitatbenadering == !project_habitatbenadering) %>% 
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
  filter_out(habitatbenadering == !project_habitatbenadering) %>% 
    arrange(periode_begin) %>% 
    mutate(maatregel_tekst_basis = glue("- {maatregel_omschrijving} ({maatregel_code})")) %>% 
    # select(-maatregel_omschrijving, -maatregel_code) %>%  # tijdelijk voor meer overzicht
    group_by(periode_begin, periode_eind, periode, maatregel_type, maatregel_nr, maatregel_tekst_basis) %>% 
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
  
    
    
    
    
  
    
  
  
  
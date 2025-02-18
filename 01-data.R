
# 0 Packages --------------------------------------------------------------

library(tidyverse)
library(here)
library(fs)
library(readxl)
library(magrittr)
library(janitor)

# 0 Global ----------------------------------------------------------------

source("paths.R")

# 1 Data ------------------------------------------------------------------
# 1.1 TL3 -----------------------------------------------------------------

tl3 <- read_excel(
  path = paste(dir_dataraw, "/OECD Territorial grid and Regional typologies - September 2020.xlsx", sep=""),
  sheet = "Typology metro-non metro", skip = 10
 ) %>% 
  clean_names() %>% 
  mutate(code = factor(code, c("MR-L", "MR-M", "NMR-M", "NMR-S", "NMR-R", "..")))

uscounty_tl3 <- read.csv(
  path(dir_dataraw, "USA/usa_tl3_counties_mapping.csv")
) %>% 
  mutate(
    county_code = as.character(county_code),
    tl3_id = as.character(tl3_id)
  )

# 1.1 Canada --------------------------------------------------------------

canada <- read.csv(
  path(dir_dataraw, "CAN/98-400-X2016292_English_CSV_data.csv"),
  header = TRUE
)

canada %<>% janitor::clean_names() 

canada_2016 <- canada %>%  # i_census_year = 2016
  filter(
    dim_labour_force_status_3 == "Employed",
    dim_age_13a == "Total - Age",
    dim_sex_3 == "Total - Sex",
    geo_level == "2"
  ) %>% 
  rename(naics = dim_industry_north_american_industry_classification_system_naics_2012_427a) %>% 
  mutate(
    naics_id = gsub('\\D','', naics), # replaces non-digits with blancs
    # naics_id = as.numeric(str_extract(naics, "[0-9]+")),
    naics_description = gsub('\\d','', naics),       # replaces digits with blanks
    naics_description2 = str_extract(naics, "[aA-zZ]+"),
    naics_digit = nchar(naics_id) # count characters in string
  ) %>% 
  filter(
    naics_digit == "3",
    naics_id > 310,
    naics_id < 340
  )  %>% 
  mutate(
    reg_id = str_c("CA", geo_code_por)
  ) %>% 
  left_join(
    .,
    tl3[,c("code", "reg_id", "metro_non_metro_typology")],
    by = c("reg_id")
  ) %>% 
  group_by(naics_id, code) %>% 
  mutate(sum = sum(dim_class_of_worker_7a_member_id_1_total_class_of_worker_note_13)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(dim_class_of_worker_7a_member_id_1_total_class_of_worker_note_13)) %>% 
  ungroup() %>% 
  mutate(share = sum/total)



# 1.2 UK ------------------------------------------------------------------

uk <- read.csv(
  path(dir_dataraw, "GBR/2564823154689671.csv"),
  skip = 7
) %>% 
  clean_names()

uk_2019 <- uk %>% 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "nace_sector",
    values_to = "employment"
  ) %>% 
  mutate(
    area = gsub("nuts316:","", area),
    reg_id = str_sub(area, 1, 5),
    nace_id = parse_number(nace_sector)
  ) %>% 
  filter(
    nace_id > 9,
    nace_id < 34
  ) %>% 
  left_join(
    .,
    tl3[,c("code", "reg_id", "metro_non_metro_typology")],
    by = c("reg_id")
  ) %>% 
  group_by(nace_id, code) %>% 
  mutate(sum = sum(employment)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(employment)) %>% 
  ungroup() %>% 
  mutate(share = sum/total)

# 1.3 Italy ---------------------------------------------------------------

italy <- read.csv(
  path(dir_dataraw, "ITA/DICA_ASIAULP - Local units and persons employed - full dataset.csv"),
  sep = "|", header = TRUE
)

italy_2019 <- italy %>% 
  filter(
    D2 == "LUEMPDAA", # Data.type # number of persons employed of local units of active enterprises (annual average values)
    D4 == "TOTAL", # Size.class.of.persons.employed # Total
    D5 == "2019" # Select.time
  ) %>% 
  rename(reg_id  = ï..D1) %>% 
  mutate(digit = nchar(D3)) %>% 
  filter(digit == "2")

italy_2019 <- left_join(
  italy_2019,
  tl3,
  by = c("reg_id")
) %>% 
  filter(tl == "3")

italy_2019_2 <- italy_2019  %>%
  mutate(D3 = as.numeric(D3)) %>% 
  filter(D3 < 34, D3 > 9) %>% 
  group_by(D3, code) %>% 
  mutate(sum = sum(Value)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(Value)) %>% 
  ungroup() %>% 
  mutate(share = sum/total)



# 1.4 Korea ---------------------------------------------------------------

korea <- read_excel(
  path(dir_dataraw, "KOR/Number_of_workers_by_province__industry_and_status_of_workers_20220714230128.xlsx")
) %>% 
  clean_names() %>% 
  filter(
    by_status_of_workers == "Total", 
    by_administrative_divisions != "Whole country"
  ) %>% 
  rename(regional_name = by_administrative_divisions)

korea_tl3 <- korea %>% 
  left_join(
    .,
    tl3,
    by = c("regional_name")
  ) %>% 
  mutate(
    x2019 = as.numeric(x2019),
    x2019 = if_else(is.na(x2019), 0, x2019)
  ) %>% 
  group_by(by_industry, code) %>% 
  mutate(sum = sum(x2019)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(x2019)) %>% 
  ungroup() %>% 
  mutate(share = sum/total)


# 1.5 Norway --------------------------------------------------------------

norway <- read_excel(
  path(dir_dataraw, "NOR/08536_20220717-084741.xlsx"),
  skip = 4
) %>% 
  clean_names() %>% 
  select(-starts_with("x")) %>% 
  drop_na(region)

norway_2019 <- norway %>% 
  mutate(
    reg_id = case_when(
      region == "01 Østfold (-2019)" ~ "NO031",              
      region == "02 Akershus (-2019)" ~ "NO012",             
      region == "03 Oslo" ~ "NO011",                        
      region == "04 Hedmark (-2019)" ~ "NO021",              
      region == "05 Oppland (-2019)" ~ "NO022",              
      region == "06 Buskerud (-2019)" ~ "NO032",            
      region == "07 Vestfold (-2019)" ~ "NO033",             
      region == "08 Telemark (-2019)" ~ "NO034",             
      region == "09 Aust-Agder (-2019)" ~ "NO041",          
      region == "10 Vest-Agder (-2019)" ~ "NO042",           
      region == "11 Rogaland" ~ "NO043",                     
      region == "12 Hordaland (-2019)" ~ "NO051",           
      region == "14 Sogn og Fjordane (-2019)" ~ "NO052",     
      region == "15 Møre og Romsdal" ~ "NO053",              
      region == "50 Trøndelag - Trööndelage" ~ "NO060",     
      region == "16 Sør-Trøndelag (-2017)" ~ "",        
      region == "17 Nord-Trøndelag (-2017)" ~ "",       
      region == "18 Nordland - Nordlánnda" ~ "NO071",       
      region == "19 Troms - Romsa (-2019)" ~ "NO072",        
      region == "20 Finnmark - Finnmárku (-2019)" ~ "NO073", 
      region == "21 Svalbard"  ~ ""  
    )
  ) %>% 
  left_join(
    .,
    tl3[,c("code", "reg_id", "metro_non_metro_typology")],
    by = c("reg_id")
  ) %>% 
  group_by(nace_sector, code) %>% 
  mutate(sum = sum(both_sexes)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(both_sexes)) %>% 
  ungroup() %>% 
  mutate(share = sum/total)

# 1.6 Portugal ------------------------------------------------------------

portugal <- read_excel(
  path(dir_dataraw, "PRT/brVsznI3Vnr3qSvzQ_jhJjjo6o1Uwriu4cXEQPgr_51989.xls"),
  skip = 8
) %>% 
  clean_names() %>% 
  select(-x5, -x7)

portugal_2019 <- portugal %>% 
  left_join(
    .,
    tl3[,c("code", "reg_id", "metro_non_metro_typology", "regional_name")],
    by = c("regional_name")
  ) %>% 
  group_by(nace_sector, code) %>% 
  mutate(sum = sum(x2019)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(x2019)) %>% 
  ungroup() %>% 
  mutate(share = sum/total)

# 1.7 US ------------------------------------------------------------------

us <- read_excel(
  path(dir_dataraw, "USA/county_3digitnaics_2019.xlsx"), skip = 2
) %>% 
  clean_names()

us_2019 <- us %>% 
  mutate(naics = as.numeric(naics)) %>% 
  filter(
    enterprise_size == "1: Total",
    naics > 310,
    naics < 340
  ) %>% 
  mutate(
    state = as.numeric(state),
    state = as.character(state),
    county_code = str_c(state, county)
  ) %>% 
  left_join(
    .,
    uscounty_tl3[,c("county_code", "tl3_id", "reg_name")],
    by = c("county_code")
  ) %>% 
  rename(reg_id = tl3_id) %>% 
  left_join(
    .,
    tl3[,c("code", "reg_id", "metro_non_metro_typology")],
    by = c("reg_id")
  ) %>% 
  group_by(naics, code) %>% 
  mutate(sum = sum(employment)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(employment)) %>% 
  ungroup() %>% 
  mutate(share = sum/total)



# 3 Save ------------------------------------------------------------------

write_rds(italy_2019_2, path(dir_data, "italy_2019_2.rds"))
write_rds(korea_tl3, path(dir_data, "korea_tl3.rds"))
write_rds(canada_2016, path(dir_data, "canada_2016.rds"))
write_rds(us_2019, path(dir_data, "us_2019.rds"))
write_rds(uk_2019, path(dir_data, "uk_2019.rds"))
write_rds(norway_2019, path(dir_data, "norway_2019.rds"))
write_rds(portugal_2019, path(dir_data, "portugal_2019.rds"))

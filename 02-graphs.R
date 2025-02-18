
# 0 Packages --------------------------------------------------------------

library(tidyverse)
library(here)
library(fs)
library(patchwork)
library(magrittr)

# 0 Global ----------------------------------------------------------------

source("paths.R")

pal <- c(
  "#52CB77", "#F72C25", "#9D695A", "#44355B",
  "#b082a1", "#6b5fc1", "#90A8C3", "#ECA72C", 
  "#757575"
)

# 1 Data ------------------------------------------------------------------

italy_2019_2 <- read_rds(path(dir_data, "italy_2019_2.rds"))
korea_tl3 <- read_rds(path(dir_data, "korea_tl3.rds"))
canada_2016 <- read_rds(path(dir_data, "canada_2016.rds"))
us_2019 <- read_rds(path(dir_data, "us_2019.rds"))
uk_2019 <- read_rds(path(dir_data, "uk_2019.rds"))
norway_2019 <- read_rds(path(dir_data, "norway_2019.rds"))
portugal_2019 <- read_rds(path(dir_data, "portugal_2019.rds"))

tech <- tibble::tribble(
  ~common_sectors,        ~tech,   
  "Food",                 "Resource/Low", 
  "Textile",              "Low", 
  "Fossil",               "Resource/Low", 
  "Chemicals",            "High", 
  "Metals",               "Resource/Low", 
  "Non-metallic mineral", "Resource/Low", 
  "Paper",                "Low", 
  "Equipment",            "High",
  "Other",                "Low", 
) %>% 
  mutate(
    tech = factor(tech, levels = c(
      "High", "Low", "Resource/Low"
    ))
  )

# 2 Wrangle ---------------------------------------------------------------
# 2.1 Canada --------------------------------------------------------------

canada_2016 %<>% 
  mutate(common_sectors = case_when(
    naics_description == " Food manufacturing" ~ "Food",                                        
    naics_description == " Beverage and tobacco product manufacturing" ~ "Food",                 
    naics_description == " Textile mills" ~ "Textile",                                             
    naics_description == " Textile product mills" ~ "Textile",                                     
    naics_description == " Clothing manufacturing" ~ "Textile",                                    
    naics_description == " Leather and allied product manufacturing" ~ "Textile",                  
    naics_description == " Wood product manufacturing" ~ "Paper",                                
    naics_description == " Paper manufacturing"  ~ "Paper",                                      
    naics_description == " Printing and related support activities" ~ "Paper",                   
    naics_description == " Petroleum and coal products manufacturing" ~ "Fossil",                 
    naics_description == " Chemical manufacturing" ~ "Chemicals",                                    
    naics_description == " Plastics and rubber products manufacturing" ~ "Chemicals",                
    naics_description == " Non-metallic mineral product manufacturing" ~ "Non-metallic mineral",                
    naics_description == " Primary metal manufacturing" ~ "Metals",                               
    naics_description == " Fabricated metal product manufacturing" ~ "Metals",                    
    naics_description == " Machinery manufacturing" ~ "Equipment",                                   
    naics_description == " Computer and electronic product manufacturing" ~ "Equipment",             
    naics_description == " Electrical equipment, appliance and component manufacturing" ~ "Equipment",
    naics_description == " Transportation equipment manufacturing" ~ "Equipment",                    
    naics_description == " Furniture and related product manufacturing" ~ "Other",               
    naics_description == " Miscellaneous manufacturing" ~ "Equipment"
   ),
   common_sectors = factor(common_sectors, levels = c(
     "Food", "Textile", "Fossil", "Chemicals", "Metals", "Non-metallic mineral",
     "Paper", "Equipment", "Other"
   ))
  ) %>% 
  group_by(common_sectors, code) %>% 
  mutate(sum = sum(dim_class_of_worker_7a_member_id_1_total_class_of_worker_note_13)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(dim_class_of_worker_7a_member_id_1_total_class_of_worker_note_13)) %>% 
  ungroup() %>% 
  mutate(share = sum/total) %>% 
  left_join(., tech, by = c("common_sectors"))


# 2.2 Italy ---------------------------------------------------------------

italy_2019_2 %<>% 
  mutate(
    Nace.2007 = as.character(Nace.2007),
    common_sectors = case_when(
      Nace.2007 == "manufacture of leather and related products" ~ "Textile",                                                                                   
      Nace.2007 == "manufacture of wood and of products of wood and cork, except furniture, manufacture of articles of straw and plaiting materials"  ~ "Paper",
      Nace.2007 == "manufacture of paper and paper products" ~ "Paper",                                                                                       
      Nace.2007 == "printing and reproduction of recorded media"  ~ "Paper",                                                                                  
      Nace.2007 == "manufacture of coke and refined petroleum products" ~ "Fossil",                                                                            
      Nace.2007 == "manufacture of chemicals and chemical products" ~ "Chemicals",                                                                                
      Nace.2007 == "manufacture of rubber and plastic products" ~ "Chemicals",                                                                                    
      Nace.2007 == "manufacture of food products"  ~ "Food",                                                                                                
      Nace.2007 == "manufacture of textiles" ~ "Textile",                                                                                                       
      Nace.2007 == "manufacture of wearing apparel" ~ "Textile",                                                                                                
      Nace.2007 == "manufacture of basic pharmaceutical products and pharmaceutical preparations" ~ "Chemicals",                                                  
      Nace.2007 == "manufacture of other non-metallic mineral products" ~ "Non-metallic mineral",                                                                            
      Nace.2007 == "manufacture of furniture"  ~ "Other",                                                                                                      
      Nace.2007 == "manufacture of tobacco products"  ~ "Other",                                                                                               
      Nace.2007 == "manufacture of electrical equipment and of non-electric domestic appliances"   ~ "Equipment",                                                  
      Nace.2007 == "other manufacturing"   ~ "Other",                                                                                                          
      Nace.2007 == "manufacture of machinery and equipment n.e.c."  ~ "Equipment",                                                                                 
      Nace.2007 == "manufacture of basic metals"  ~ "Metals",                                                                                                   
      Nace.2007 == "manufacture of computer, electronic and optical products"  ~ "Equipment",                                                                      
      Nace.2007 == "manufacture of beverages"   ~ "Food",                                                                                                     
      Nace.2007 == "repair and installation of machinery and equipment"   ~ "Equipment",                                                                           
      Nace.2007 == "manufacture of other transport equipment"   ~ "Equipment",                                                                                     
      Nace.2007 == "manufacture of fabricated metal products, except machinery and equipment"   ~ "Metals",                                                     
      Nace.2007 == "manufacture of motor vehicles, trailers and semi-trailers"  ~ "Other"
   ),
   common_sectors = factor(common_sectors, levels = c(
     "Food", "Textile", "Fossil", "Chemicals", "Metals", "Non-metallic mineral",
     "Paper", "Equipment", "Other"
   ))
  ) %>% 
  group_by(common_sectors, code) %>% 
  mutate(sum = sum(Value)) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  mutate(total = sum(Value)) %>% 
  ungroup() %>% 
  mutate(share = sum/total)  %>% 
  left_join(., tech, by = c("common_sectors"))


# 2.3 Korea ---------------------------------------------------------------

korea_tl3 %<>% 
  mutate(
    common_sectors = case_when(
    by_industry == "Manufacture of food products" ~ "Food",                                                                
    by_industry == "Manufacture of beverages" ~ "Food",                                                                     
    by_industry == "Manufacture of tobacco products" ~ "Other",                                                              
    by_industry == "Manufacture of textiles, except apparel" ~ "Textile",                                                      
    by_industry == "Manufacture of wearing apparel, clothing accessories and fur articles" ~ "Textile",                         
    by_industry == "Manufacture of leather, luggage and footwear" ~ "Textile",                                                 
    by_industry == "Manufacture of wood and of products of wood and cork; except furniture" ~ "Paper",                       
    by_industry == "Manufacture of pulp, paper and paper products" ~ "Paper",                                                
    by_industry == "Printing and reproduction of recorded media" ~ "Paper",                                                  
    by_industry == "Manufacture of coke, briquettes and refined petroleum products" ~ "Fossil",                               
    by_industry == "Manufacture of chemicals and chemical products; except pharmaceuticals and medicinal chemicals" ~ "Chemicals",
    by_industry == "Manufacture of pharmaceuticals, medicinal chemical and botanical products" ~ "Chemicals",                    
    by_industry == "Manufacture of rubber and plastics products" ~ "Chemicals",                                                  
    by_industry == "Manufacture of other non-metallic mineral products" ~ "Non-metallic mineral",                                           
    by_industry == "Manufacture of basic metals" ~ "Metals",                                                                  
    by_industry == "Manufacture of fabricated metal products, except machinery and furniture" ~ "Metals",                     
    by_industry == "Manufacture of electronic components, computer; visual, sounding and communication equipment" ~ "Equipment", 
    by_industry == "Manufacture of medical, precision and optical instruments, watches and clocks" ~ "Equipment",                
    by_industry == "Manufacture of electrical equipment" ~ "Equipment",                                                          
    by_industry == "Manufacture of other machinery and equipment" ~ "Equipment",                                                 
    by_industry == "Manufacture of motor vehicles, trailers and semitrailers"  ~ "Equipment",                                    
    by_industry == "Manufacture of other transport equipment" ~ "Equipment",                                                     
    by_industry == "Manufacture of furniture" ~ "Other",                                                                     
    by_industry == "Other manufacturing" ~ "Other",                                                                          
    by_industry == "Maintenance and repair services of industrial machinery and equipment" ~ "Equipment",
    ),
    common_sectors = factor(common_sectors, levels = c(
      "Food", "Textile", "Fossil", "Chemicals", "Metals", "Non-metallic mineral",
      "Paper", "Equipment", "Other"
    ))
  )  %>% 
  left_join(., tech, by = c("common_sectors"))


# 2.4 Norway --------------------------------------------------------------

norway_2019 %<>% 
  mutate(
    common_sectors = case_when(
      nace_sector == "10 Food products" ~ "Food",                     
      nace_sector == "11 Beverages" ~ "Food",                        
      nace_sector == "12 Tobacco products" ~ "Other",                  
      nace_sector == "13 Textiles" ~ "Textile",                         
      nace_sector == "14 Wearing apparel" ~ "Textile",                   
      nace_sector == "15 Leather and leather products" ~ "Textile",     
      nace_sector == "16 Wood and wood products" ~ "Paper",            
      nace_sector == "17 Paper and paper products" ~ "Paper",         
      nace_sector == "18 Printing and reproduction" ~ "Paper",         
      nace_sector == "19 Refined petroleum products" ~ "Fossil",       
      nace_sector == "20 Chemicals, chemical products" ~ "Chemicals",      
      nace_sector == "21 Pharmaceuticals" ~ "Chemicals",                  
      nace_sector == "22 Rubber and plastic products" ~ "Chemicals",       
      nace_sector == "23 Other non-metal mineral products" ~ "Non-metallic mineral", 
      nace_sector == "24 Basic metals" ~ "Metals",                      
      nace_sector == "25 Fabricated metal prod." ~ "Metals",           
      nace_sector == "26 Electronic and optical products" ~ "Equipment",   
      nace_sector == "27 Electrical equipment" ~ "Equipment",             
      nace_sector == "28 Machinery and equipment" ~ "Equipment",           
      nace_sector == "29 Motor vehicles etc." ~ "Equipment",              
      nace_sector == "30 Other transport equipment" ~ "Equipment",         
      nace_sector == "31 Furniture" ~ "Other",                        
      nace_sector == "32 Other manufacturing" ~ "Other",               
      nace_sector == "33 Repair, installation of machinery" ~ "Equipment"
    ),
    common_sectors = factor(common_sectors, levels = c(
      "Food", "Textile", "Fossil", "Chemicals", "Metals", "Non-metallic mineral",
      "Paper", "Equipment", "Other"
    ))
  )  %>% 
  left_join(., tech, by = c("common_sectors"))


# 2.5 Portugal ------------------------------------------------------------

portugal_2019 %<>% 
  mutate(
    common_sectors = case_when(
      nace_sector == "Manufacture of food products" ~ "Food",                     
      nace_sector == "Manufacture of beverages" ~ "Food",                        
      nace_sector == "Manufacture of tobacco products" ~ "Other",                  
      nace_sector == "Manufacture of textiles" ~ "Textile",                         
      nace_sector == "Manufacture of wearing apparel" ~ "Textile",                   
      nace_sector == "Manufacture of leather and related products" ~ "Textile",     
      nace_sector == "Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials" ~ "Paper",            
      nace_sector == "Manufacture of paper and paper products" ~ "Paper",         
      nace_sector == "Printing and reproduction of recorded media" ~ "Paper",         
      nace_sector == "Manufacture of coke, refined petroleum products and fuels briquettes" ~ "Fossil",       
      nace_sector == "Manufacture of chemicals, chemical products and man-made fibres, except pharmaceutical products" ~ "Chemicals",      
      nace_sector == "Manufacture of basic pharmaceutical products and pharmaceutical preparations" ~ "Chemicals",                  
      nace_sector == "Manufacture of rubber and plastic products" ~ "Chemicals",       
      nace_sector == "Manufacture of other non-metallic mineral products" ~ "Non-metallic mineral", 
      nace_sector == "Manufacture of basic metals" ~ "Metals",                      
      nace_sector == "Manufacture of fabricated metal products, except machinery and equipment" ~ "Metals",           
      nace_sector == "Manufacture of computer, communication equipment, electronic and optical products" ~ "Equipment",   
      nace_sector == "Manufacture of electrical equipment" ~ "Equipment",             
      nace_sector == "Manufacture of machinery and equipment n.e.c." ~ "Equipment",           
      nace_sector == "Manufacture of motor vehicles, trailers, semi-trailers and parts and accessories for motor vehicles" ~ "Equipment",              
      nace_sector == "Manufacture of other transport equipment" ~ "Equipment",         
      nace_sector == "Manufacture of furniture" ~ "Other",                        
      nace_sector == "Other manufacturing activities" ~ "Other",               
      nace_sector == "Repair, maintenance and installation of machinery and equipment" ~ "Equipment"
    ),
    common_sectors = factor(common_sectors, levels = c(
      "Food", "Textile", "Fossil", "Chemicals", "Metals", "Non-metallic mineral",
      "Paper", "Equipment", "Other"
    ))
  ) %>% 
  left_join(., tech, by = c("common_sectors"))


# 2.5 UK ------------------------------------------------------------------

uk_2019 %<>% 
  mutate(
    common_sectors = case_when(
      nace_sector == "x10_manufacture_of_food_products" ~ "Food",                                                                                                 
      nace_sector == "x11_manufacture_of_beverages" ~ "Food",                                                                                                     
      nace_sector == "x12_manufacture_of_tobacco_products" ~ "Other",                                                                                              
      nace_sector == "x13_manufacture_of_textiles" ~ "Textile",                                                                                                      
      nace_sector == "x14_manufacture_of_wearing_apparel" ~ "Textile",                                                                                               
      nace_sector == "x15_manufacture_of_leather_and_related_products" ~ "Textile",                                                                                  
      nace_sector == "x16_manufacture_of_wood_and_of_products_of_wood_and_cork_except_furniture_manufacture_of_articles_of_straw_and_plaiting_materials" ~ "Paper",
      nace_sector == "x17_manufacture_of_paper_and_paper_products" ~ "Paper",                                                                                      
      nace_sector == "x18_printing_and_reproduction_of_recorded_media" ~ "Paper",                                                                                  
      nace_sector == "x19_manufacture_of_coke_and_refined_petroleum_products" ~ "Fossil",                                                                           
      nace_sector == "x20_manufacture_of_chemicals_and_chemical_products" ~ "Chemicals",                                                                               
      nace_sector == "x21_manufacture_of_basic_pharmaceutical_products_and_pharmaceutical_preparations" ~ "Chemicals",                                                 
      nace_sector == "x22_manufacture_of_rubber_and_plastic_products" ~ "Chemicals",                                                                                   
      nace_sector == "x23_manufacture_of_other_non_metallic_mineral_products" ~ "Non-metallic mineral",                                                                           
      nace_sector == "x24_manufacture_of_basic_metals" ~ "Metals",                                                                                                  
      nace_sector == "x25_manufacture_of_fabricated_metal_products_except_machinery_and_equipment" ~ "Metals",                                                      
      nace_sector == "x26_manufacture_of_computer_electronic_and_optical_products" ~ "Equipment",                                                                      
      nace_sector == "x27_manufacture_of_electrical_equipment" ~ "Equipment",                                                                                          
      nace_sector == "x28_manufacture_of_machinery_and_equipment_n_e_c" ~ "Equipment",                                                                                 
      nace_sector == "x29_manufacture_of_motor_vehicles_trailers_and_semi_trailers" ~ "Equipment",                                                                     
      nace_sector == "x30_manufacture_of_other_transport_equipment" ~ "Equipment",                                                                                     
      nace_sector == "x31_manufacture_of_furniture" ~ "Other",                                                                                                     
      nace_sector == "x32_other_manufacturing" ~ "Other",                                                                                                          
      nace_sector == "x33_repair_and_installation_of_machinery_and_equipment" ~ "Equipment"  
    ),
    common_sectors = factor(common_sectors, levels = c(
      "Food", "Textile", "Fossil", "Chemicals", "Metals", "Non-metallic mineral",
      "Paper", "Equipment", "Other"
    ))
  ) %>% 
  left_join(., tech, by = c("common_sectors"))


# 2.6 US ------------------------------------------------------------------

us_2019 %<>% 
  mutate(
    common_sectors = case_when(
      naics_description == "Printing and Related Support Activities" ~ "Paper",                   
      naics_description == "Fabricated Metal Product Manufacturing" ~ "Metals",                     
      naics_description == "Food Manufacturing" ~ "Food",                                          
      naics_description == "Beverage and Tobacco Product Manufacturing" ~ "Food",                  
      naics_description == "Textile Product Mills" ~ "Textile",                                       
      naics_description == "Wood Product Manufacturing" ~ "Paper",                                  
      naics_description == "Paper Manufacturing" ~ "Paper",                                         
      naics_description == "Chemical Manufacturing" ~ "Chemicals",                                      
      naics_description == "Plastics and Rubber Products Manufacturing" ~ "Chemicals",                  
      naics_description == "Nonmetallic Mineral Product Manufacturing" ~ "Non-metallic mineral",                   
      naics_description == "Machinery Manufacturing" ~ "Equipment",                                     
      naics_description == "Computer and Electronic Product Manufacturing" ~ "Equipment",               
      naics_description == "Transportation Equipment Manufacturing" ~ "Equipment",                      
      naics_description == "Furniture and Related Product Manufacturing" ~ "Other",                 
      naics_description == "Miscellaneous Manufacturing" ~ "Other",                                 
      naics_description == "Primary Metal Manufacturing" ~ "Metals",                                 
      naics_description == "Textile Mills" ~ "Textile",                                               
      naics_description == "Apparel Manufacturing" ~ "Textile",                                       
      naics_description == "Leather and Allied Product Manufacturing" ~ "Textile",                    
      naics_description == "Electrical Equipment, Appliance, and Component Manufacturing" ~ "Equipment",
      naics_description == "Petroleum and Coal Products Manufacturing" ~ "Fossil"
    ),
    common_sectors = factor(common_sectors, levels = c(
      "Food", "Textile", "Fossil", "Chemicals", "Metals", "Non-metallic mineral",
      "Paper", "Equipment", "Other"
    ))
  ) %>% 
  left_join(., tech, by = c("common_sectors"))


# 2.7 Group ---------------------------------------------------------------

can <- canada_2016 %>%
  mutate(iso3 = "CAN") %>% 
  distinct(iso3, common_sectors, code, metro_non_metro_typology, sum, total, share) %>% 
  drop_na(code)

ita <- italy_2019_2 %>% 
  mutate(iso3 = "ITA") %>% 
  mutate(sum = sum(Value))
  distinct(iso3, common_sectors, code, metro_non_metro_typology, sum, total, share)

# 3 Graphs by country -----------------------------------------------------

a <- canada_2016 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = common_sectors)) +
  geom_bar(stat = "unique", position = position_fill(reverse = TRUE)) +
  labs(
    title = "Canada",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

b <- italy_2019_2 %>% 
  ggplot(aes(x = code, y = share, fill = common_sectors)) +
  geom_bar(stat = "unique", position = position_fill(reverse = TRUE)) +
  labs(
    title = "Italy",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

c <- korea_tl3 %>% 
  ggplot(aes(x = code, y = share, fill = common_sectors)) +
  geom_bar(stat = "unique", position = position_fill(reverse = TRUE)) +
  labs(
    title = "South Korea",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

d <- portugal_2019 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = common_sectors)) +
  geom_bar(stat = "unique", position = position_fill(reverse = TRUE)) +
  labs(
    title = "Portugal",
    x = element_blank(),
    y = element_blank()
  ) +
 scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0))
  
g <- norway_2019 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = common_sectors)) +
  geom_bar(stat = "unique", position = position_fill(reverse = TRUE)) +
  labs(
    title = "Norway",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

e <- uk_2019 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = common_sectors)) +
  geom_bar(stat = "unique", position = position_fill(reverse = TRUE)) +
  labs(
    title = "United Kingdom",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

f <- us_2019 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = common_sectors)) +
  geom_bar(stat = "unique", position = position_fill(reverse = TRUE)) +
  labs(
    title = "United States",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 


(a + b) / (c + d) / (e + f) + plot_layout(guides = "collect") &
  theme(
    axis.text = element_text(size = 10, colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(color = "black"),
    #axis.ticks.length = unit(-0.15, "cm"),
    plot.title = element_text(size = 12, colour = "black", hjust = 0.5),
    #plot.margin = margin(t = 4, 1, 1, 1, "lines"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#E9E9E9"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.key = element_rect(color = "#E9E9E9", fill = "#E9E9E9"),
    legend.background = element_rect(fill = "#E9E9E9"),
    legend.box.background = element_rect(color = "#E9E9E9", fill = "#E9E9E9"),
    legend.box.margin = margin(0, 0.5, 0, 0.5, "cm"),
    legend.key.size = unit(0.7, "line"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )

ggsave(
  path(dir_fig, "empl_shares.png"),
  height = 20, width = 18, units = "cm"
)


# 4 Graphs average --------------------------------------------------------

a <- canada_2016 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = tech)) +
  geom_bar(stat = "unique") +
  labs(
    title = "Canada",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

b <- italy_2019_2 %>% 
  ggplot(aes(x = code, y = share, fill = tech)) +
  geom_bar(stat = "unique") +
  labs(
    title = "Italy",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

c <- korea_tl3 %>% 
  ggplot(aes(x = code, y = share, fill = tech)) +
  geom_bar(stat = "unique") +
  labs(
    title = "South Korea",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

d <- portugal_2019 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = tech)) +
  geom_bar(stat = "unique") +
  labs(
    title = "Portugal",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0))

g <- norway_2019 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = tech)) +
  geom_bar(stat = "unique") +
  labs(
    title = "Norway",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

e <- uk_2019 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = tech)) +
  geom_bar(stat = "unique") +
  labs(
    title = "United Kingdom",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 

f <- us_2019 %>% 
  drop_na(code) %>% 
  ggplot(aes(x = code, y = share, fill = tech)) +
  geom_bar(stat = "unique") +
  labs(
    title = "United States",
    x = element_blank(),
    y = element_blank()
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) 


(a + b) / (c + d) / (e + f) + plot_layout(guides = "collect") &
  theme(
    axis.text = element_text(size = 10, colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(color = "black"),
    #axis.ticks.length = unit(-0.15, "cm"),
    plot.title = element_text(size = 12, colour = "black", hjust = 0.5),
    #plot.margin = margin(t = 4, 1, 1, 1, "lines"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#E9E9E9"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.key = element_rect(color = "#E9E9E9", fill = "#E9E9E9"),
    legend.background = element_rect(fill = "#E9E9E9"),
    legend.box.background = element_rect(color = "#E9E9E9", fill = "#E9E9E9"),
    legend.box.margin = margin(0, 0.5, 0, 0.5, "cm"),
    legend.key.size = unit(0.7, "line"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) 
  

ggsave(
  path(dir_fig, "empl_tech.png"),
  height = 20, width = 18, units = "cm"
)



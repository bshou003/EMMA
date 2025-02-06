library(tidyverse)
library(readxl)
library(GGally)
all.2023.anions <- read_excel("~/Documents/Data/2023_Data/Ion.Data/BryceTetonH2O_052024.xlsx", sheet = 1, skip = 3) %>% 
  subset(select =c(1:8)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Anion")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("Nalgene")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("inbre")) ~ "lab",
    str_detect(SITE, regex("Field")) ~ "lab",
    str_detect(SITE, regex("Peak")) ~ "lab",
    str_detect(SITE, regex("Inbre")) ~ "lab",
    str_detect(SITE, regex("Whirl")) ~ "lab",
    str_detect(SITE, regex("GV")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG")

june.2024.anions <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 1, skip = 3) %>% 
  subset(select =c(1:8)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Anion")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("SO")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    str_detect(SITE, regex("Snow")) ~ "lab",
    str_detect(SITE, regex("Field")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG")

july.2024.anions <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 3, skip = 3) %>% 
  subset(select =c(1:8)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Anion")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("SO")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    str_detect(SITE, regex("SP")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG")    

august.2024.anions <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 5, skip = 3) %>% 
  subset(select =c(1:8)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Anion")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("SO")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG") 

august.2024.anions.leftover <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 9, skip = 3) %>% 
  subset(select =c(1:8)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Anion")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("SO")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG") 

september.2024.anions <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 7, skip = 3) %>% 
  subset(select =c(1:8)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Anion")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("SO")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG") 

anions <- all.2023.anions %>% 
  bind_rows(june.2024.anions) %>% 
  bind_rows(july.2024.anions) %>% 
  bind_rows(august.2024.anions) %>% 
  bind_rows(august.2024.anions.leftover) %>% 
  bind_rows(september.2024.anions) %>% 
  separate(SITE, c("location", "Event")) %>% 
  subset(select = -c(Setting.Type,bromide)) %>% 
  mutate(Event = as.numeric(Event)) #no observable bromide

anions.nitrate <- anions %>% 
  mutate(nitrate.15 = nitrate / 0.015)

anions.longer <- anions %>% 
  pivot_longer(c(fluoride:sulfate), names_to = "anion", values_to = "ppm") %>% 
  group_by(anion) %>% 
  mutate(ppm = as.numeric(ppm))

####Cations###
all.2023.cations <- read_excel("~/Documents/Data/2023_Data/Ion.Data/BryceTetonH2O_052024.xlsx", sheet = 2, skip = 3) %>% 
  subset(select =c(1:6)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Cation")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("Nalgene")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("inbre")) ~ "lab",
    str_detect(SITE, regex("Field")) ~ "lab",
    str_detect(SITE, regex("Peak")) ~ "lab",
    str_detect(SITE, regex("Inbre")) ~ "lab",
    str_detect(SITE, regex("Whirl")) ~ "lab",
    str_detect(SITE, regex("GV")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG")

june.2024.cations <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 2, skip = 3) %>% 
  subset(select =c(1:6)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Cation")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("Hi")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    str_detect(SITE, regex("Snow")) ~ "lab",
    str_detect(SITE, regex("Field")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG")

july.2024.cations <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 4, skip = 3) %>% 
  subset(select =c(1:6)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Cation")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("Ca")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    str_detect(SITE, regex("SP")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG")    

august.2024.cations <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 6, skip = 3) %>% 
  subset(select =c(1:6)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Cation")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("SO")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG")

august.2024.cations.leftover <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 10, skip = 3) %>% 
  subset(select =c(1:6)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Cation")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("SO")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG") 

september.2024.cations <- read_excel("~/Documents/Data/2024_Data/Ion.Data/BryceTetonH2O_names_adjusted.xlsx", sheet = 8, skip = 3) %>% 
  subset(select =c(1:6)) %>% 
  rename(SITE = ...1) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("DI")) ~ "lab",
    str_detect(SITE, regex("Cation")) ~ "lab",
    str_detect(SITE, regex("Standard")) ~ "lab",
    str_detect(SITE, regex("QC")) ~ "lab",
    str_detect(SITE, regex("SO")) ~ "lab",
    str_detect(SITE, regex("MDL")) ~ "lab",
    str_detect(SITE, regex("FB")) ~ "lab",
    TRUE ~ "RG")) %>% 
  filter(Setting.Type == "RG") 

cations <- all.2023.cations %>% 
  bind_rows(june.2024.cations) %>% 
  bind_rows(july.2024.cations) %>% 
  bind_rows(august.2024.cations.leftover) %>% 
  bind_rows(september.2024.cations) %>% 
  separate(SITE, c("location", "Event")) %>% 
  subset(select = -c(Setting.Type)) %>% 
  mutate(Event = as.numeric(Event))

cations.longer <- cations %>% 
  pivot_longer(c(Sodium:Calcium), names_to = "cation", values_to = "ppm") %>% 
  group_by(cation) %>% 
  mutate(ppm = as.numeric(ppm))


ggplot(data = cations.longer, aes(x= location,y=ppm )) +
  geom_boxplot() +
  facet_wrap(~cation, scales = "free_y")

####Isotopes####
iso<-read_csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") %>% 
  filter(Setting.Type == "Rock Glacier") %>% 
  separate(SITE, c("location", "event")) %>% 
  subset(select = -c(seq_position, on, event, Original_name, Setting.Type, month, year))

####Bind###
c.a.i <- cations %>% 
  merge(anions) %>% 
  merge(iso) %>% 
  subset(select = -c(phosphate))


ggpairs(c.a.i,col = 3:15,, aes(colour = location) )

data(iris)
ggpairs(iris,                 # Data frame
        columns = 1:4,        # Columns
        aes(color = Species,  # Color by group (cat. variable)
            alpha = 0.5))     # Transparency
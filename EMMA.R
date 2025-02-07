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
  subset(select = -c(Setting.Type)) %>% 
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


####Calculate m.eq for anions####
#Conversion from Hem, John David. Study and interpretation of the chemical characteristics of natural water. 
#No. 1473. US Government Printing Office, 1970.
fd <- 0.05264
cld <- 0.02821
no2d <-0.02174
brd <-0.01251
no3d <-0.01613
po4d<-0.01031
so4d<-0.02082

#Calculating meq with the above values and by the full method
fl <- all.2023.anions$fluoride* fd
fl2 <- (all.2023.anions$fluoride*1)/18.998
cl<- all.2023.anions$chloride*cld
cl2<- (all.2023.anions$chloride*1)/35.453
no2 <- all.2023.anions$nitrite * no2d
no22 <- (all.2023.anions$nitrite * 1) / 46.005
br <- all.2023.anions$bromide * brd
br2 <- (all.2023.anions$bromide * 1) /79.904
no3 <- all.2023.anions$nitrate * no3d
no32 <- (all.2023.anions$nitrate*1)/62.0049
po4 <- all.2023.anions$phosphate * po4d
po42 <- (all.2023.anions$phosphate * 3) / 94.9714
so4<- all.2023.anions$sulfate * so4d
so42 <- (all.2023.anions$sulfate * 2)/96.06

#Filtering out samples less than 1 ppm
anion.meq <- anions %>% 
  filter(nitrate <= 1)
anion.meq <- anion.meq %>% 
  reframe(location = location,
          Event = as.character(Event),
          fl = fluoride* fd,
          cl= chloride*cld,
          no2 = nitrite * no2d,
          br =bromide * brd,
          no3 = nitrate * no3d,
          po4 = phosphate * po4d,
          so4= sulfate * so4d,
          nitrate = nitrate,
          nitrate.15 = nitrate / 0.014)
#adding the meq to check if it goes above the resin value
anion.meq<-anion.meq %>% 
 dplyr::mutate("SUM_RQ" = rowSums((anion.meq[,3:9]), na.rm = TRUE)) %>% 
  mutate(Event = as.numeric(Event)) %>% 
  filter(Event >=10)
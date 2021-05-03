# double check working directory
getwd()
# sick

########################################
library(tidyverse)
library(RColorBrewer)
##
par(mar=c(3,4,2,2))
display.brewer.all(colorblindFriendly = T)
display.brewer.pal(8, "Dark2")
my_pal <- brewer.pal(8, "Dark2")

########################################

# load data
# CA
evt_ca <- read.csv("./EVT_Percentages/US200EVT_CA.csv")
tnc_ca <- read.csv("./EVT_Percentages/US200EVT_CA_TNC.csv")

# add ACRES column based on COUNT
# COUNT is number of 30x30 m pixels in raster, or 900 m^2
# add PERCENT column
evt_ca <- evt_ca %>%
  rename(COUNT_CA = COUNT) %>%
  arrange(desc(COUNT_CA)) %>%
  mutate(ACRES_CA = round(COUNT_CA * 900 / 4046.86),
         PERCENT_CA = round(ACRES_CA / sum(ACRES_CA) * 100, 4),
         RANK_CA = 1:pull(tally(evt_ca)))
tnc_ca <- tnc_ca %>%
  rename(COUNT_TNC = COUNT) %>%
  arrange(desc(COUNT_TNC)) %>%
  mutate(ACRES_TNC = round(COUNT_TNC * 900 / 4046.86),
         PERCENT_TNC = round(ACRES_TNC / sum(ACRES_TNC) *100, 4),
         RANK_TNC = 1:pull(tally(tnc_ca)))

# join to single data frame and replace NAs with 0
ca <- left_join(evt_ca, tnc_ca) %>%
  replace(is.na(.), 0)

#################################################
# summarize each evt by evt_phys
phys_ca <- ca %>% group_by(EVT_PHYS) %>%
    summarise(phys_percent_ca = sum(PERCENT_CA),
            phys_percent_tnc = sum(PERCENT_TNC))

#Change EVT_PHYS from factor to character 
phys_ca$EVT_PHYS <- as.character(phys_ca$EVT_PHYS)


# rename and subset some shit
phys_sub_ca <- phys_ca %>% mutate(phys = case_when(
  str_detect(EVT_PHYS, "Developed") ~ "Developed",
  str_detect(EVT_PHYS, "Exotic") ~ "Exotic",
  str_detect(EVT_PHYS, "Quarries") ~ "Quarries and Mining Land")) %>%
  mutate(phys = if_else(is.na(phys), EVT_PHYS, phys)) %>%
  group_by(phys) %>%
  summarise(phys_percent_ca = sum(phys_percent_ca),
            phys_percent_tnc = sum(phys_percent_tnc))

# make tidy
phys_tidy_ca <- phys_sub_ca %>%
  pivot_longer(!phys, names_to = "location", values_to = "percent") %>%
  mutate(location = if_else(location == "phys_percent_ca", true = "CA", false = "TNC"))

phys_filter_ca <- phys_tidy_ca %>%
  filter(!phys %in% c("Snow-Ice", "Open Water", "Quarries and Mining Land"))
  

# double bar chart
ggplot(data = phys_filter_ca, aes(x = phys, y = percent)) +
  geom_bar(aes(fill = location), stat = "identity", position = position_dodge2(reverse = T)) +
  ylim(c(0, 40)) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = "LANDFIRE EVTs of California",
       subtitle = "comparison with TNC ownership - grouped by EVT_PHYS",
       x = "",
       y = "Percent (%)",
       fill = "") +
  theme_light() +
  scale_fill_manual(values = c(CA = my_pal[6], TNC = my_pal[5]))
ggsave("./MW_plots/top10evts_OwnedByTNC_ca.jpg",
       width = 6,
       height = 4,
       units = "in")
  
##################################################################
# diverging bar chart based on CA evts over 1%

evt1_ca <- ca %>% filter(PERCENT_CA >= 1) %>%
  dplyr::select(EVT_NAME, PERCENT_CA, PERCENT_TNC) %>%
  mutate(diff = PERCENT_TNC - PERCENT_CA,
         color_score = if_else(diff > 0, "TNC over", "TNC under")) %>%
  filter(EVT_NAME != "Open Water")

tevt_ca <- evt1_ca %>% slice_max(n = 5, order_by = diff) 
bevt_ca <- evt1_ca %>% slice_min(n = 5, order_by = diff)
top2bot_ca <- bind_rows(tevt_ca, bevt_ca) %>%
  arrange(desc(diff)) %>%
  mutate(EVT_NAME = factor(EVT_NAME, levels = EVT_NAME))

# plot diverging bar
ggplot(data = top2bot_ca, aes(x = EVT_NAME, y = diff)) +
  geom_bar(aes(fill = color_score), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  #scale_y_continuous(breaks = seq(-10, 20, by = 5)) +
  labs(title = "LANDFIRE EVTs of California - TNC representation",
       subtitle = "Difference between % TNC ownership and % total CA \nTop and bottom 5 for CA EVTs over 1%",
       x = "",
       y = "Percent difference (%)",
       fill = "") +
  theme_light() +
  scale_fill_manual(values = c(my_pal[3], my_pal[4]))
ggsave("./MW_plots/top5bot_TNCrepresentation_ca.jpg",
       width = 10,
       height = 4,
       units = "in")

####################
####################

# load data
# MAL
evt_mal <- read.csv("./EVT_Percentages/US200EVT_MAL.csv")
tnc_mal <- read.csv("./EVT_Percentages/US200EVT_MAL_TNC.csv")

# add ACRES column based on COUNT
# COUNT is number of 30x30 m pixels in raster, or 900 m^2
# add PERCENT column
evt_mal <- evt_mal %>%
  rename(COUNT_MAL = COUNT) %>%
  arrange(desc(COUNT_MAL)) %>%
  mutate(ACRES_MAL = round(COUNT_MAL * 900 / 4046.86),
         PERCENT_MAL = round(ACRES_MAL / sum(ACRES_MAL) * 100, 4),
         RANK_MAL = 1:pull(tally(evt_mal)))
tnc_mal <- tnc_mal %>%
  rename(COUNT_TNC = COUNT) %>%
  arrange(desc(COUNT_TNC)) %>%
  mutate(ACRES_TNC = round(COUNT_TNC * 900 / 4046.86),
         PERCENT_TNC = round(ACRES_TNC / sum(ACRES_TNC) *100, 4),
         RANK_TNC = 1:pull(tally(tnc_mal)))

# join to single data frame and replace NAs with 0
mal <- left_join(evt_mal, tnc_mal) %>%
  replace(is.na(.), 0)

#################################################
# summarize each evt by evt_phys
phys_mal <- mal %>% group_by(EVT_PHYS) %>%
  summarise(phys_percent_mal = sum(PERCENT_MAL),
            phys_percent_tnc = sum(PERCENT_TNC))

#Change EVT_PHYS from factor to character 
phys_mal$EVT_PHYS <- as.character(phys_mal$EVT_PHYS)


# rename and subset some shit
phys_sub_mal <- phys_mal %>% mutate(phys = case_when(
  str_detect(EVT_PHYS, "Developed") ~ "Developed",
  str_detect(EVT_PHYS, "Exotic") ~ "Exotic",
  str_detect(EVT_PHYS, "Quarries") ~ "Quarries and Mining Land")) %>%
  mutate(phys = if_else(is.na(phys), EVT_PHYS, phys)) %>%
  group_by(phys) %>%
  summarise(phys_percent_mal = sum(phys_percent_mal),
            phys_percent_tnc = sum(phys_percent_tnc))

# make tidy
phys_tidy_mal <- phys_sub_mal %>%
  pivot_longer(!phys, names_to = "location", values_to = "percent") %>%
  mutate(location = if_else(location == "phys_percent_mal", true = "MAL", false = "TNC"))

phys_filter_mal <- phys_tidy_mal %>%
  filter(!phys %in% c("Snow-Ice", "Open Water", "Quarries and Mining Land"))


# double bar chart
ggplot(data = phys_filter_mal, aes(x = phys, y = percent)) +
  geom_bar(aes(fill = location), stat = "identity", position = position_dodge2(reverse = T)) +
  ylim(c(0, 40)) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = "LANDFIRE EVTs of Mid-Atlantic Region",
       subtitle = "comparison with TNC ownership - grouped by EVT_PHYS",
       x = "",
       y = "Percent (%)",
       fill = "") +
  theme_light() +
  scale_fill_manual(values = c(my_pal[1], my_pal[5]))
ggsave("./MW_plots/top10evts_OwnedByTNC_mal.jpg",
       width = 6,
       height = 4,
       units = "in")

##################################################################
# diverging bar chart based on MAL evts over 1%

evt1_mal <- mal %>% filter(PERCENT_MAL >= 1) %>%
  dplyr::select(EVT_NAME, PERCENT_MAL, PERCENT_TNC) %>%
  mutate(diff = PERCENT_TNC - PERCENT_MAL,
         color_score = if_else(diff > 0, "TNC over", "TNC under")) %>%
  filter(EVT_NAME != "Open Water")

tevt_mal <- evt1_mal %>% slice_max(n = 5, order_by = diff) 
bevt_mal <- evt1_mal %>% slice_min(n = 5, order_by = diff)
top2bot_mal <- bind_rows(tevt_mal, bevt_mal) %>%
  arrange(desc(diff)) %>%
  mutate(EVT_NAME = factor(EVT_NAME, levels = EVT_NAME))

# plot diverging bar
ggplot(data = top2bot_mal, aes(x = EVT_NAME, y = diff)) +
  geom_bar(aes(fill = color_score), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  #scale_y_continuous(breaks = seq(-10, 20, by = 5)) +
  labs(title = "LANDFIRE EVTs of Mid-Atlantic region - TNC representation",
       subtitle = "Difference between % TNC ownership and % total MAL \nTop and bottom 5 for MAL EVTs over 1%",
       x = "",
       y = "Percent difference (%)",
       fill = "") +
  theme_light() +
  scale_fill_manual(values = c(my_pal[3], my_pal[4]))
ggsave("./MW_plots/top5bot_TNCrepresentation_mal.jpg",
       width = 10,
       height = 4,
       units = "in")

################
################

# load data
# New York
evt_ney <- read.csv("./EVT_Percentages/US200EVT_NEY.csv")
tnc_ney <- read.csv("./EVT_Percentages/US200EVT_NEY_TNC.csv")

# add ACRES column based on COUNT
# COUNT is number of 30x30 m pixels in raster, or 900 m^2
# add PERCENT column
evt_ney <- evt_ney %>%
  rename(COUNT_NEY = COUNT) %>%
  arrange(desc(COUNT_NEY)) %>%
  mutate(ACRES_NEY = round(COUNT_NEY * 900 / 4046.86),
         PERCENT_NEY = round(ACRES_NEY / sum(ACRES_NEY) * 100, 4),
         RANK_NEY = 1:pull(tally(evt_ney)))
tnc_ney <- tnc_ney %>%
  rename(COUNT_TNC = COUNT) %>%
  arrange(desc(COUNT_TNC)) %>%
  mutate(ACRES_TNC = round(COUNT_TNC * 900 / 4046.86),
         PERCENT_TNC = round(ACRES_TNC / sum(ACRES_TNC) *100, 4),
         RANK_TNC = 1:pull(tally(tnc_ney)))

# join to single data frame and replace NAs with 0
ney <- left_join(evt_ney, tnc_ney) %>%
  replace(is.na(.), 0)

#################################################
# summarize each evt by evt_phys
phys_ney <- ney %>% group_by(EVT_PHYS) %>%
  summarise(phys_percent_ney = sum(PERCENT_NEY),
            phys_percent_tnc = sum(PERCENT_TNC))

#Change EVT_PHYS from factor to character 
phys_ney$EVT_PHYS <- as.character(phys_ney$EVT_PHYS)


# rename and subset some shit
phys_sub_ney <- phys_ney %>% mutate(phys = case_when(
  str_detect(EVT_PHYS, "Developed") ~ "Developed",
  str_detect(EVT_PHYS, "Exotic") ~ "Exotic",
  str_detect(EVT_PHYS, "Quarries") ~ "Quarries and Mining Land")) %>%
  mutate(phys = if_else(is.na(phys), EVT_PHYS, phys)) %>%
  group_by(phys) %>%
  summarise(phys_percent_ney = sum(phys_percent_ney),
            phys_percent_tnc = sum(phys_percent_tnc))

# make tidy
phys_tidy_ney <- phys_sub_ney %>%
  pivot_longer(!phys, names_to = "location", values_to = "percent") %>%
  mutate(location = if_else(location == "phys_percent_ney", true = "NY", false = "TNC"))

phys_filter_ney <- phys_tidy_ney %>%
  filter(!phys %in% c("Snow-Ice", "Open Water", "Quarries and Mining Land"))


# double bar chart
ggplot(data = phys_filter_ney, aes(x = phys, y = percent)) +
  geom_bar(aes(fill = location), stat = "identity", position = position_dodge2(reverse = T)) +
  ylim(c(0, 40)) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = "LANDFIRE Existing Vegetation Types of New York",
       subtitle = "comparison with TNC ownership - grouped by EVT_PHYS",
       x = "",
       y = "Percent (%)",
       fill = "") +
  theme_light() +
  scale_fill_manual(values = c(NY = my_pal[2], TNC = my_pal[5]))
ggsave("./MW_plots/top10evts_OwnedByTNC_ney.jpg",
       width = 6,
       height = 4,
       units = "in")

##################################################################
# diverging bar chart based on NEY evts over 1%

evt1_ney <- ney %>% filter(PERCENT_NEY >= 1) %>%
  dplyr::select(EVT_NAME, PERCENT_NEY, PERCENT_TNC) %>%
  mutate(diff = PERCENT_TNC - PERCENT_NEY,
         color_score = if_else(diff > 0, "TNC over", "TNC under")) %>%
  filter(EVT_NAME != "Open Water")

tevt_ney <- evt1_ney %>% slice_max(n = 5, order_by = diff) 
bevt_ney <- evt1_ney %>% slice_min(n = 5, order_by = diff)
top2bot_ney <- bind_rows(tevt_ney, bevt_ney) %>%
  arrange(desc(diff)) %>%
  mutate(EVT_NAME = factor(EVT_NAME, levels = EVT_NAME))

# plot diverging bar
ggplot(data = top2bot_ney, aes(x = EVT_NAME, y = diff)) +
  geom_bar(aes(fill = color_score), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  #scale_y_continuous(breaks = seq(-10, 20, by = 5)) +
  labs(title = "LANDFIRE EVTs of New York - TNC representation",
       subtitle = "Difference between % TNC ownership and % total MAL \nTop and bottom 5 for NY EVTs over 1%",
       x = "",
       y = "Percent difference (%)",
       fill = "") +
  theme_light() +
  scale_fill_manual(values = c(my_pal[3], my_pal[4]))
ggsave("./MW_plots/top5bot_TNCrepresentation_ney.jpg",
       width = 10,
       height = 4,
       units = "in")

########################################################
########################################################

# load data
# CONUS
evt_us <- read.csv("./EVT_Percentages/US200EVT_CONUS.csv")
tnc_us <- read.csv("./EVT_Percentages/US200EVT_CONUS_TNC.csv")

# add ACRES column based on COUNT
# COUNT is number of 30x30 m pixels in raster, or 900 m^2
# add PERCENT column
evt_us <- evt_us %>%
  rename(COUNT_US = COUNT) %>%
  arrange(desc(COUNT_US)) %>%
  mutate(ACRES_US = round(COUNT_US * 900 / 4046.86),
         PERCENT_US = round(ACRES_US / sum(ACRES_US) * 100, 4),
         RANK_US = 1:pull(tally(evt_us)))
tnc_us <- tnc_us %>%
  rename(COUNT_TNC = COUNT) %>%
  arrange(desc(COUNT_TNC)) %>%
  mutate(ACRES_TNC = round(COUNT_TNC * 900 / 4046.86),
         PERCENT_TNC = round(ACRES_TNC / sum(ACRES_TNC) *100, 4),
         RANK_TNC = 1:pull(tally(tnc_us)))

# join to single data frame and replace NAs with 0
us <- left_join(evt_us, tnc_us) %>%
  replace(is.na(.), 0)

#################################################
# summarize each evt by evt_phys
phys_us <- us %>% group_by(EVT_PHYS) %>%
  summarise(phys_percent_us = sum(PERCENT_US),
            phys_percent_tnc = sum(PERCENT_TNC))

#Change EVT_PHYS from factor to character 
phys_us$EVT_PHYS <- as.character(phys_us$EVT_PHYS)


# rename and subset some shit
phys_sub_us <- phys_us %>% mutate(phys = case_when(
  str_detect(EVT_PHYS, "Developed") ~ "Developed",
  str_detect(EVT_PHYS, "Exotic") ~ "Exotic",
  str_detect(EVT_PHYS, "Quarries") ~ "Quarries and Mining Land")) %>%
  mutate(phys = if_else(is.na(phys), EVT_PHYS, phys)) %>%
  group_by(phys) %>%
  summarise(phys_percent_us = sum(phys_percent_us),
            phys_percent_tnc = sum(phys_percent_tnc))

# make tidy
phys_tidy_us <- phys_sub_us %>%
  pivot_longer(!phys, names_to = "location", values_to = "percent") %>%
  mutate(location = if_else(location == "phys_percent_us", true = "CONUS", false = "TNC"))

phys_filter_us <- phys_tidy_us %>%
  filter(!phys %in% c("Snow-Ice", "Open Water", "Quarries and Mining Land"))


# double bar chart
ggplot(data = phys_filter_us, aes(x = phys, y = percent)) +
  geom_bar(aes(fill = location), stat = "identity", position = position_dodge2(reverse = T)) +
  ylim(c(0, 40)) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = "LANDFIRE Existing Vegetation Types of United States",
       subtitle = "comparison with TNC ownership - grouped by EVT_PHYS",
       x = "",
       y = "Percent (%)",
       fill = "") +
  theme_light() +
  scale_fill_manual(values = c(CONUS = my_pal[8], TNC = my_pal[5]))
ggsave("./MW_plots/top10evts_OwnedByTNC_conus.jpg",
       width = 6,
       height = 4,
       units = "in")

##################################################################
# diverging bar chart based on NEY evts over 1%

evt1_us <- us %>% filter(PERCENT_US >= 1) %>%
  dplyr::select(EVT_NAME, PERCENT_US, PERCENT_TNC) %>%
  mutate(diff = PERCENT_TNC - PERCENT_US,
         color_score = if_else(diff > 0, "TNC over", "TNC under")) %>%
  filter(EVT_NAME != "Open Water")

tevt_us <- evt1_us %>% slice_max(n = 5, order_by = diff) 
bevt_us <- evt1_us %>% slice_min(n = 5, order_by = diff)
top2bot_us <- bind_rows(tevt_us, bevt_us) %>%
  arrange(desc(diff)) %>%
  mutate(EVT_NAME = factor(EVT_NAME, levels = EVT_NAME))

# plot diverging bar
ggplot(data = top2bot_us, aes(x = EVT_NAME, y = diff)) +
  geom_bar(aes(fill = color_score), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  #scale_y_continuous(breaks = seq(-10, 20, by = 5)) +
  labs(title = "LANDFIRE EVTs of Continental United States - TNC representation",
       subtitle = "Difference between % TNC ownership and % total CONUS \nTop and bottom 5 for CONUS EVTs over 1%",
       x = "",
       y = "Percent difference (%)",
       fill = "") +
  theme_light() +
  scale_fill_manual(values = c(my_pal[3], my_pal[4]))
ggsave("./MW_plots/top5bot_TNCrepresentation_conus.jpg",
       width = 10,
       height = 4,
       units = "in")







################################################################## NOT DONE YET, WAITING 6 HOURS TO DOWNLOAD EVT200
# percent protected map using evt140
library(raster)
library(sf)
library(tmap)

ca_shp <- st_read("D:/Shapefiles/california/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") %>%
  st_union() %>%
  st_sf()

lf_evt_ca <- raster("D:/LANDFIRE/US_140EVT/Grid/us_140evt/hdr.adf") %>%
  crop(ca_shp) %>%
  mask(ca_shp)

protect_ca <- ca %>% mutate(protect_percent = round(COUNT_TNC / COUNT_CA * 100, 4)) %>%
  dplyr::select(EVT_NAME, protect_percent) %>%
  rename(CLASSNAME = EVT_NAME)

levels(lf_evt_ca)[[1]] %>% left_join(protect_ca)

levels(lf_evt_ca)[[1]]

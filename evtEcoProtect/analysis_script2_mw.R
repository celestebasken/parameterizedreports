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
evt_ca <- read.csv("evtEcoProtect/US200EVT_CA.csv")
tnc_ca <- read.csv("evtEcoProtect/US200EVT_CA_TNC.csv")

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
ggsave("top10evts_OwnedByTNC_ca.jpg",
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
ggsave("top5bot_TNCrepresentation_ca.jpg",
       width = 10,
       height = 4,
       units = "in")

####################
####################

library(readxl)
library(RColorBrewer)
library(tmap)
library(dplyr)
library(tidyr)
library(magrittr)
library(colorspace)
library(countrycode)
library(reactable)
library(htmltools)

###Create CEM Campaign Matrix####

eu <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE") %>%
  countrycode("iso2c", "iso3c")

cem_campaigns <- read_xlsx("~/CEM/CEM Matrix/2021 June Matrix.xlsx", "Campaigns") %>%
  pivot_longer(!(iso2:iso3), names_to = "campaign", values_to = "participation")

cem_campaigns %<>% 
  replace_na(list(participation = 0)) %>%
  mutate(participation = participation * rep(1:length(unique(cem_campaigns$campaign)), 
                                             length(unique(cem_campaigns$iso3))))

eu_campaigns <- tibble(iso3 = eu) %>% 
  full_join(cem_campaigns %>% 
              filter(iso2 == "EC") %>% 
              select(campaign, participation), by = character()) %>%
  mutate(participation_eu = participation + length(unique(cem_campaigns$campaign)) + 1) %>%
  select(-participation)

data("World")

cem_campaigns_full <-
  expand_grid(iso3 = World$iso_a3, 
              campaign = unique(cem_campaigns$campaign)) %>%
  left_join(cem_campaigns, by = c("iso3", "campaign")) %>%
  left_join(eu_campaigns, by = c("iso3", "campaign")) %>%
  replace_na(list(participation = 0, participation_eu = 0)) %>%
  mutate(participation = ifelse(iso3 %in% eu,
                                ifelse(participation == 0, participation_eu, participation),
                                participation))

world_cem_campaigns <- World %>%
  left_join(cem_campaigns_full, by = c("iso_a3" = "iso3")) %>%
  filter(iso_a3 != "ATA", continent != "Seven seas (open ocean)") #%>%
#mutate(participation = factor(participation, levels = unique(participation)))

###Generate maps####

# Large qualitative palette
qual_pal <- c(brewer.pal(brewer.pal.info['Set3', 'maxcolors'], 'Set3'),
              brewer.pal(brewer.pal.info['Set2', 'maxcolors'], 'Set2'))
pal <- qual_pal[1:length(unique(cem_campaigns$campaign))]
pal_eu <- lighten(pal, 0.6, method = "relative", space = "HLS")

pal <- c("#FCFCFC", pal, "#FCFCFC", pal_eu)

tm_shape(world_cem_campaigns %>% 
           mutate(color = pal[participation + 1])) +
  tm_polygons(col = "color",
              #palette = pal, style = "cat",
              legend.show = FALSE) +
  tm_facets(by = "campaign",
            nrow = 5) +
  tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA)

### Animated Campaign Map####

 campgif = tm_shape(world_cem_campaigns %>%
            mutate(color = pal [participation +1])) +
   tm_polygons(col = "color", 
               #palette = pal, style = "cat",
               legend.show = FALSE) +
   tm_facets(along = "campaign",
             nrow = 5, free.coords = FALSE) +
   tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA)
 tmap_animation(campgif, filename = "camp_anim.gif", delay = 75)
 
 
 
 ###matrix Chart####
 #to take campaigns and put them in a chart with flag#
 
 
 
 
   
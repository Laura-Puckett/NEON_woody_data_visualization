#-------------------------------------------------------
# Description
#-------------------------------------------------------

# Author Laura Puckett
# Date 11/26/2021
# Purpose: Join and filter NEON woody vegetation data for visualization 
# in a shiny app. 

#-------------------------------------------------------
# Setup
#-------------------------------------------------------
library(neonUtilities); library(dplyr); library(ggplot2)

#-------------------------------------------------------
# Load datasets of interest and select columns
#-------------------------------------------------------
veglist = readRDS('./veglist.Rdata')

veg_ind = veglist$vst_apparentindividual %>%
  dplyr::select(siteID, plotID, height, individualID, plantStatus,
                stemDiameter, date, eventID)

veg_loc = veglist$vst_mappingandtagging %>%
  dplyr::select(domainID, siteID, plotID, individualID, taxonID, scientificName) %>%
  unique()

plot_info = veglist$vst_perplotperyear %>% 
  select(eventID, siteID, plotID, plotType) %>% 
  unique()

rm(veglist) # unload the large file now that it's no longer needed

#-----------------------------------------
# Join datasets
#-----------------------------------------
veg = veg_ind %>%
  inner_join(veg_loc, by = c("individualID", "siteID", "plotID")) %>%
  inner_join(plot_info, by = c("siteID", "plotID", "eventID"))

#-----------------------------------------
# Filter Dataset
#-----------------------------------------
veg = veg %>%
  # filter for Live trees
  filter(substr(plantStatus, 1,4) == "Live") %>% 
  # filter for tree measurements (which start at 10cm)
  filter(stemDiameter > 10) %>%
  filter(is.na(height) == F & is.na(stemDiameter) == F) %>%
  # remove outliers
  group_by(taxonID) %>%
  mutate(height_lower =  boxplot(height, plot=FALSE)$stats[1],
         height_upper = boxplot(height, plot=FALSE)$stats[5],
         diam_lower =  boxplot(stemDiameter, plot=FALSE)$stats[1],
         diam_upper = boxplot(stemDiameter, plot=FALSE)$stats[5]) %>%
  filter(height > height_lower & height < height_upper,
         stemDiameter > diam_lower & stemDiameter < diam_upper) %>%
  dplyr::select(-height_lower, -height_upper, -diam_lower, -diam_upper) %>%
  ungroup()

# Select columns
veg = veg %>%
  dplyr::select(siteID, stemDiameter, height, plantStatus, scientificName, taxonID, date, individualID)

#-----------------------------------------
# Get species list for shiny app
#-----------------------------------------

# Find top 20 species by observation count
species = veg %>% 
  group_by(taxonID, scientificName) %>%
  summarize(obs_count = n()) %>%
  arrange(desc(obs_count)) %>%
  head(20) %>%
  select(taxonID, scientificName) %>%
  ungroup() %>%
  mutate(num = row_number()) %>%
  as.data.frame()

#-----------------------------------------
# Write Output
#-----------------------------------------
saveRDS(veg, './veg_data_for_shiny.Rdata')
saveRDS(species, './species.Rdata')

library(data.table)
library(readxl)
library(tidyverse)


## Import
raw.data <- read_excel("data_raw/NestedQueries_ExportToRegina_20230616.xlsx",
                       .name_repair = "universal") %>%
  as.data.frame()

## Draft for single watershed
North <- raw.data[raw.data$Row.Labels %like% "North", ] %>%
  separate(Row.Labels, into = c("Watershed", "Subreach"), sep = ",")


## Table 1: Summary of Change in Tree Canopy, X Watershed
unit.area <- table %>% 
  select(Watershed, Subreach, Sum.of.Area_Acre)

## Canopy changes
canopy.cover <- table %>%
  select(Watershed, Subreach, Sum.of.Area_Acre, Sum.of.CanopyAcre2013, Sum.of.CanopyAcre2019) %>%
  mutate(Percent_Total2013 = Sum.of.CanopyAcre2013 / Sum.of.Area_Acre) %>%
  mutate(Percent_Total2019 = Sum.of.CanopyAcre2019 / Sum.of.Area_Acre) %>%
  mutate(CanopyAcreChange = Sum.of.CanopyAcre2019 - Sum.of.CanopyAcre2013) %>%
  mutate(Change_in_Canopy = Percent_Total2019 - Percent_Total2013)

## Shoreline Clearing
shoreline.clearing <- table %>%
  select(Watershed, Subreach, Sum.of.P_ClearingFootprint_Acre_12_22)

## Shoreline Enhancement
shoreline.enhancement <- table %>%
  select(Watershed, Subreach, Sum.of.P_MitigationRestoration_Acre_12_22)

## Permitted dnr clearing
DNR.clearing <- table %>%
  select(Watershed, Subreach, Sum.of.ForestPracticesAcre_13_19)

## Unaccounted differences
other.change <- table %>%
  select(Watershed, Subreach, Sum.of.Area_Acre,
         Sum.of.P_ClearingFootprint_Acre_12_22, Sum.of.ForestPracticesAcre_13_19) %>%
  left_join(canopy.cover %>% select(1:3, CanopyAcreChange)) %>%
  rowwise() %>%
  select(Watershed, Subreach, CanopyAcreChange, 
         Sum.of.P_ClearingFootprint_Acre_12_22, Sum.of.ForestPracticesAcre_13_19) %>%
  mutate(UnaccountedChange = 
           (CanopyAcreChange - (Sum.of.P_ClearingFootprint_Acre_12_22 + Sum.of.ForestPracticesAcre_13_19))) %>%
  mutate(Change_in_CanopyPerc = (UnaccountedChange / CanopyAcreChange))


## Table 2: Summary of Change in Impervious Surface Cover, X Watershed

impervious.cover <- table %>%
  select(Watershed, Subreach, Sum.of.Area_Acre, Sum.of.Area_Acre,
         Sum.of.ImperviousAcre2013, Sum.of.ImperviousAcre2019) %>%
  mutate(Percent_Total2013 = Sum.of.ImperviousAcre2013 / Sum.of.Area_Acre) %>%
  mutate(Percent_Total2019 = Sum.of.ImperviousAcre2019 / Sum.of.Area_Acre) %>%
  mutate(ImpervAcreChange = Sum.of.ImperviousAcre2019 - Sum.of.ImperviousAcre2013) %>%
  mutate(Change_in_Imperv = Percent_Total2019 - Percent_Total2013)


#TODO: name all the columns once



## Table 1: Summary of Change in Tree Canopy, X Watershed
cleaned.data <- raw.data %>% 
  filter(!str_detect(Row.Labels, "Grand")) %>%
  separate_wider_delim(Row.Labels, names = c("Watershed", "Subreach"), delim = ",")


table1 <- cleaned.data %>%
  select(Watershed, Subreach, Unit_Area = Sum.of.Area_Acre, Canopy_Cover2013 = Sum.of.CanopyAcre2013,
         Canopy_Cover2019 = Sum.of.CanopyAcre2019, Permitted_Shoreline_Clearing = Sum.of.P_ClearingFootprint_Acre_12_22,
         Permitted_Shoreline_Enhancement = Sum.of.P_MitigationRestoration_Acre_12_22, 
         Permitted_DNR = Sum.of.ForestPracticesAcre_13_19) %>%
  rowwise() %>%
  mutate(Percent_Canopy2013 = (Canopy_Cover2013 / Unit_Area) * 100,
         Percent_Canopy2019 = (Canopy_Cover2019 / Unit_Area) * 100) %>%
  mutate(Canopy_Change_Acres = Canopy_Cover2019 - Canopy_Cover2013) %>%
  mutate(Canopy_Change_Percent = Percent_Canopy2019 - Percent_Canopy2013) %>%
  mutate(Unaccounted_Canopy_Change_Acres = 
           (Canopy_Change_Acres - (Permitted_Shoreline_Clearing + Permitted_DNR))) %>%
  mutate(Unaccounted_Canopy_Change_Percent = (Unaccounted_Canopy_Change_Acres / Canopy_Change_Acres)) %>%
  mutate(Permitted_Shoreline_Clearing_Percent = (Permitted_Shoreline_Clearing / Canopy_Change_Acres) * 100) %>%
  mutate(Permitted_Shoreline_Enhancement_Percent = (Permitted_Shoreline_Enhancement / Canopy_Change_Acres) * 100) %>%
  mutate(Permitted_DNR_Percent = (Permitted_DNR / Canopy_Change_Acres) * 100) %>%
  # select(Watershed, Subreach, Unit_Area, Canopy_Cover2013, Percent_Canopy2013, Canopy_Cover2019, Percent_Canopy2019,
  #        Canopy_Change_Acres, Canopy_Change_Percent, Permitted_Shoreline_Clearing, Permitted_Shoreline_Enhancement,
  #        Permitted_DNR, Unaccounted_Canopy_Change_Acres, Unaccounted_Canopy_Change_Percent) %>%
  mutate(across(where(is.numeric), round, digits = 2))

good.columns <- table1 %>%
  mutate(Canopy_Total2013 = paste(Canopy_Cover2013, " (", Percent_Canopy2013, "%)", sep = "")) %>%
  mutate(Canopy_Total2019 =  paste(Canopy_Cover2019, " (", Percent_Canopy2019, "%)", sep = "")) %>%
  mutate(Canopy_Change_Total = paste(Canopy_Change_Acres, " (", Canopy_Change_Percent, "%)", sep = "")) %>%
  mutate(Permitted_Shoreline_Total = paste(Permitted_Shoreline_Clearing, " (", Percent_Canopy2013, "%)", sep = "")) %>%
  mutate(Unaccounted_Change_Total = paste(Unaccounted_Canopy_Change_Acres, " (", Unaccounted_Canopy_Change_Percent, "%)", sep = "")) %>%
  select(Watershed, Subreach, Unit_Area, contains("Total"))
  
export.table <- good.columns %>%
  group_by(Watershed) %>%
  group_split()
names(export.table) <- unique(cleaned.data$Watershed)
  


## Table 2: Summary of Change in Impervious Surface Cover, X Watershed
# 
# table2 <- cleaned.data %>%
#   select(Watershed, Subreach, Unit_Area = Sum.of.Area_Acre, Impervious_Cover2013 = Sum.of.ImperviousAcre2013,
#          Impervious_Cover2019 = Sum.of.ImperviousAcre2019, Permitted_Shoreline_Clearing = Sum.of.P_ClearingFootprint_Acre_12_22,
#          Permitted_Shoreline_Enhancement = Sum.of.P_MitigationRestoration_Acre_12_22, 
#          Permitted_DNR = Sum.of.ForestPracticesAcre_13_19) %>%
#   mutate(Percent_Total2013 = Sum.of.ImperviousAcre2013 / Sum.of.Area_Acre) %>%
#   mutate(Percent_Total2019 = Sum.of.ImperviousAcre2019 / Sum.of.Area_Acre) %>%
#   mutate(ImpervAcreChange = Sum.of.ImperviousAcre2019 - Sum.of.ImperviousAcre2013) %>%
#   mutate(Change_in_Imperv = Percent_Total2019 - Percent_Total2013)




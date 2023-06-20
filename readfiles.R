
#TODO: name all the columns once



allen <- table1[[1]]
  


## Table 2: Summary of Change in Impervious Surface Cover, X Watershed

table2 <- cleaned.data %>%
  select(Watershed, Subreach, Unit_Area = Sum.of.Area_Acre, Impervious_Cover2013 = Sum.of.ImperviousAcre2013,
         Impervious_Cover2019 = Sum.of.ImperviousAcre2019, Permitted_Shoreline_Clearing = Sum.of.P_ClearingFootprint_Acre_12_22,
         Permitted_Shoreline_Enhancement = Sum.of.P_MitigationRestoration_Acre_12_22, 
         Permitted_DNR = Sum.of.ForestPracticesAcre_13_19) %>%
  mutate(Percent_Total2013 = Sum.of.ImperviousAcre2013 / Sum.of.Area_Acre) %>%
  mutate(Percent_Total2019 = Sum.of.ImperviousAcre2019 / Sum.of.Area_Acre) %>%
  mutate(ImpervAcreChange = Sum.of.ImperviousAcre2019 - Sum.of.ImperviousAcre2013) %>%
  mutate(Change_in_Imperv = Percent_Total2019 - Percent_Total2013)




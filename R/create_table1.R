
# Clean and tidy the imported data
cleaned.data <- raw.data %>% 
  filter(!str_detect(Row.Labels, "Grand")) %>%
  separate_wider_delim(Row.Labels, names = c("Watershed", "Subreach"), delim = ",")

# Select and rename columns
fixed.columns <- cleaned.data %>%
  select(Watershed, Subreach, 
         Area = Sum.of.Area_Acre, 
         Canopy2013 = Sum.of.CanopyAcre2013,
         Canopy2019 = Sum.of.CanopyAcre2019, 
         Shoreline_Cleared = Sum.of.P_ClearingFootprint_Acre_12_22,
         Shoreline_Enhanced = Sum.of.P_MitigationRestoration_Acre_12_22, 
         DNR_Cleared = Sum.of.ForestPracticesAcre_13_19,
         Impervious2013 = Sum.of.ImperviousAcre2013,
         Impervious2019 = Sum.of.ImperviousAcre2019)

# Table 1: Summary of Change in Tree Canopy, X Watershed ------------------
table1 <- fixed.columns %>%
  rowwise() %>%
  mutate(across(contains("Canopy"), ~ ((.x / Area) * 100), 
                .names = "Percent_{col}")) %>%
  mutate(Canopy_Change_Acres = Canopy2019 - Canopy2013,
         Canopy_Change_Percent = Percent_Canopy2019 - Percent_Canopy2013) %>%
  mutate(across(matches("Shoreline|DNR"), ~ ((.x / Canopy_Change_Acres) * 100),
                .names = "{col}_Percent")) %>%
  mutate(Other_Canopy_Change_Acres = 
           (Canopy_Change_Acres - (Shoreline_Cleared + DNR_Cleared))) %>%
  mutate(Other_Canopy_Change_Percent = (Other_Canopy_Change_Acres / Canopy_Change_Acres)) %>%
  mutate(across(where(is.numeric), round, digits = 2)) %>%
  mutate_all(~ifelse(is.nan(.), 0, .))

paste_columns <- function(colA, colB) {
  newcol <- paste(colA, " (", colB, "%)", sep = "")
}

final.columns <- table1 %>%
  mutate(Canopy_Total2013 = paste_columns(Canopy2013, Percent_Canopy2013),
         Canopy_Total2019 =  paste_columns(Canopy2019, Percent_Canopy2019),
         Canopy_Change_Total = paste_columns(Canopy_Change_Acres, Canopy_Change_Percent),
         Shoreline_TotalCleared = paste_columns(Shoreline_Cleared, Percent_Canopy2013),
         Shoreline_TotalEnhanced = paste_columns(Shoreline_Enhanced, Shoreline_Enhanced_Percent),
         DNR_Total = paste_columns(DNR_Cleared, DNR_Cleared_Percent),
         Unaccounted_Total = paste_columns(Other_Canopy_Change_Acres, Other_Canopy_Change_Percent)) %>%
  select(Watershed, Subreach, Area, contains("Total"))
  
export.table <- final.columns %>%
  group_by(Watershed) %>%
  group_split()
names(export.table) <- unique(cleaned.data$Watershed)
  

# Table 2: Change in Impervious Cover and Supporting Metrics ------------------

table2 <- fixed.columns %>%
  rowwise() %>%
  mutate(across(contains("Impervious"), ~ ((.x / Area) * 100), 
                .names = "Percent_{col}")) %>%
  mutate(Impervious_Change_Acres = Impervious2019 - Impervious2013,
         Impervious_Change_Percent = Percent_Impervious2013 - Percent_Impervious2019) %>%
  mutate(across(matches("Shoreline|DNR"), ~ ((.x / Impervious_Change_Acres) * 100),
                .names = "{col}_Percent")) %>%
  mutate(Other_Impervious_Change_Acres = 
           (Impervious_Change_Acres - (Shoreline_Cleared + DNR_Cleared))) %>%
  mutate(Other_Impervious_Change_Percent = (Other_Impervious_Change_Acres / Impervious_Change_Acres)) %>%
  mutate(across(where(is.numeric), round, digits = 2)) %>%
  mutate_all(~ifelse(is.nan(.), 0, .))

final.columns2 <- table2 %>%
  mutate(Impervious_Total2013 = paste_columns(Impervious2013, Percent_Impervious2013),
         Impervious_Total2019 =  paste_columns(Impervious2019, Percent_Impervious2019),
         Impervious_Change_Total = paste_columns(Impervious_Change_Acres, Impervious_Change_Percent),
         Shoreline_TotalCleared = paste_columns(Shoreline_Cleared, Percent_Impervious2013),
         Shoreline_TotalEnhanced = paste_columns(Shoreline_Enhanced, Shoreline_Enhanced_Percent),
         DNR_Total = paste_columns(DNR_Cleared, DNR_Cleared_Percent),
         Unaccounted_Total = paste_columns(Other_Impervious_Change_Acres, Other_Impervious_Change_Percent)) %>%
  select(Watershed, Subreach, Area, contains("Total"))

export.table2 <- final.columns2 %>%
  group_by(Watershed) %>%
  group_split()
names(export.table2) <- unique(cleaned.data$Watershed)




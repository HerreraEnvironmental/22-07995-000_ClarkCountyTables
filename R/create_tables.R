## Pasting function
paste_columns <- function(colA, colB) {
  newcol <- paste(colA, " (", colB, "%)", sep = "")
}

# Begin table creation ----------------------------------------------------

# Clean and tidy the imported data
raw.data$Watershed <- gsub("/", " ", raw.data$Watershed) # Remove / character


# Select and rename columns
fixed.columns <- raw.data %>%
  select(Watershed, Reach, 
         Area = Area_Acre, 
         Canopy2013 = CanopyAcre2013,
         Canopy2019 = CanopyAcre2019, 
         Impervious2013 = ImperviousAcre2013,
         Impervious2019 = ImperviousAcre2019,
         Shoreline_Cleared = P_ClearingFootprint_Acre_12_22,
         Shoreline_Enhanced = P_MitigationRestoration_Sqft_12_22, 
         P_NonMitigation = P_NonMitigationRestoration_SqFT_19_22,
         DNR_Cleared = ForestPracticesAcre_13_19,
         P_OverWaterStructure = P_OverWaterStructure_Acre_12_22,
         P_ImperviousFootprint = P_NewImperviousFootprint_Acre_12_22)


# Table 1: Summary of Change in Tree Canopy, X Watershed ------------------
table1 <- fixed.columns %>%
  rowwise() %>%
  mutate(across(contains("Canopy"), ~ ((.x / Area) * 100), 
                .names = "Percent_{col}")) %>%
  mutate(Canopy_Change_Acres = Canopy2019 - Canopy2013,
         Canopy_Change_Percent = Percent_Canopy2019 - Percent_Canopy2013) %>%
  mutate(P_NonMitigation_Percent = (P_NonMitigation/Canopy_Change_Acres * 100)) %>%
  mutate(Shoreline_Cleared_Percent = (Shoreline_Cleared / Canopy_Change_Acres) * 100) %>%
  mutate(Shoreline_Enhanced_Percent = (Shoreline_Enhanced / Canopy_Change_Acres) * 100) %>%
  mutate(DNR_Cleared_Percent = (DNR_Cleared / abs(Canopy_Change_Acres)) * 100) %>%
  mutate(Unaccounted_Canopy_Change_Acres = ifelse(Canopy_Change_Acres < 0, 
           (abs(Canopy_Change_Acres) - (DNR_Cleared + Shoreline_Cleared)), 
           (Canopy_Change_Acres) - (DNR_Cleared + Shoreline_Cleared))) %>%
  mutate(Unaccounted_Canopy_Change_Acres =
           ifelse(Unaccounted_Canopy_Change_Acres < 0, 
                  0, Unaccounted_Canopy_Change_Acres)) %>%
  mutate(Unaccounted_Canopy_Change_Percent = (Unaccounted_Canopy_Change_Acres / abs(Canopy_Change_Acres))) %>%

  mutate(Unaccounted_Canopy_Change_Percent = ifelse(Unaccounted_Canopy_Change_Percent < 0, 
                                                    0, Unaccounted_Canopy_Change_Percent)) %>%
  mutate(across(where(is.numeric), round, digits = 1)) %>%
  mutate_all(~ifelse(is.nan(.), 0, .))

## Create non-SMA totals
watershed.split1 <- table1 %>%
  group_by(Watershed) %>%
  group_split() 
names(watershed.split1) <- unique(table1$Watershed)

watershed.totals1 <- map(watershed.split1, ~ colSums(.x[!str_detect(.x$Reach, "Non-SMA"), -c(1:2)]) %>%
                          as.data.frame() %>%
                          rownames_to_column() %>%
                          pivot_wider(names_from = 1, values_from = 2) %>%
                          mutate(Watershed = "Total for Non-SMA Areas in Watershed:",
                                 Reach = NA) %>%
                          select(Watershed, Reach, everything()))
names(watershed.totals1) <- unique(table1$Watershed)

bound.watershed.totals1 <- bind_rows(watershed.totals1, .id = "Watershed") %>%
  filter(!str_detect(Watershed, "Grand")) %>%
  mutate(Reach = "Non-SMA Totals")

final <- table1 %>%
  rbind(bound.watershed.totals1)

## Select and rename columns
final.columns1 <- final %>%
  select(Watershed, Reach, everything()) %>%
  mutate(Canopy_Cover_2013_AcresPTotalUnit = paste_columns(Canopy2013, Percent_Canopy2013),
         Canopy_Cover_2019_AcresPTotalUnit =  paste_columns(Canopy2019, Percent_Canopy2019),
         Change_in_Canopy_Cover2013_2019 = paste_columns(Canopy_Change_Acres, Canopy_Change_Percent),
         Permitted_Shoreline_Clearing_in_SMA_Acres = paste_columns(Shoreline_Cleared, Shoreline_Cleared_Percent),
         Permitted_Shoreline_Compensatory_Migration2012_2022= paste_columns(Shoreline_Enhanced, Shoreline_Enhanced_Percent),
         Permitted_Shoreline_Restoration2019_2022 = paste_columns(P_NonMitigation, P_NonMitigation_Percent),
         Permitted_Clearing_from_DNR_Forestry2013_2021 = paste_columns(DNR_Cleared, DNR_Cleared_Percent),
         Change_in_Canopy_Cover_Not_Accounted = paste_columns(Unaccounted_Canopy_Change_Acres, Unaccounted_Canopy_Change_Percent)) %>%
  select(Watershed, Reach, Area, Canopy_Cover_2013_AcresPTotalUnit:Change_in_Canopy_Cover_Not_Accounted)
  
## Export to final rendering step
export.table1 <- final.columns1 %>%
  group_by(Watershed) %>%
  group_split()
names(export.table1) <- unique(final.columns1$Watershed)
  

# Table 2: Change in Impervious Cover and Supporting Metrics ------------------

table2 <- fixed.columns %>%
  rowwise() %>%
  mutate(across(contains("Impervious"), ~ ((.x / Area) * 100), 
                .names = "Percent_{col}")) %>%
  mutate(Impervious_Change_Acres = Impervious2019 - Impervious2013,
         Impervious_Change_Percent = Percent_Impervious2019 - Percent_Impervious2013) %>%
  mutate(P_ImperviousFootprint_Percent = (P_ImperviousFootprint / abs(Impervious_Change_Acres)) * 100) %>%
  mutate(P_OverWaterStructure_Percent = (P_OverWaterStructure / abs(Impervious_Change_Acres)) * 100) %>%
  mutate(Shoreline_Enhanced_Percent = (Shoreline_Enhanced / abs(Impervious_Change_Acres)) * 100) %>%
  mutate(P_NonMitigation_Percent = (P_NonMitigation / abs(Impervious_Change_Acres)) * 100) %>%
  mutate(Unaccounted_Impervious_Change_Acres = ifelse(Impervious_Change_Acres < 0, 
                                                           (abs(Impervious_Change_Acres) - (P_ImperviousFootprint - P_OverWaterStructure)), 
                                                           (Impervious_Change_Acres - (P_OverWaterStructure + P_ImperviousFootprint)))) %>%
  mutate(Unaccounted_Impervious_Change_Acres =
                    ifelse(Unaccounted_Impervious_Change_Acres < 0, 
                           0, Unaccounted_Impervious_Change_Acres)) %>%
  mutate(Unaccounted_Impervious_Change_Percent = (Unaccounted_Impervious_Change_Acres / abs(Impervious_Change_Acres))) %>%
  mutate(Unaccounted_Impervious_Change_Percent = ifelse(Unaccounted_Impervious_Change_Percent < 0, 
                                                             0, Unaccounted_Impervious_Change_Percent)) %>%
  mutate(across(where(is.numeric), round, digits = 1)) %>%
  mutate_all(~ifelse(is.nan(.), 0, .))

## Create non-SMA totals
watershed.split2 <- table2 %>%
  group_by(Watershed) %>%
  group_split() 
names(watershed.split2) <- unique(table2$Watershed)

watershed.totals2 <- map(watershed.split2, ~ colSums(.x[!str_detect(.x$Reach, "Non-SMA"), -c(1:2)]) %>%
                          as.data.frame() %>%
                          rownames_to_column() %>%
                          pivot_wider(names_from = 1, values_from = 2) %>%
                          mutate(Watershed = "Total for Non-SMA Areas in Watershed:",
                                 Reach = NA) %>%
                          select(Watershed, Reach, everything()))
names(watershed.totals2) <- unique(table2$Watershed)

bound.watershed.totals2 <- bind_rows(watershed.totals2, .id = "Watershed") %>%
  filter(!str_detect(Watershed, "Grand")) %>%
  mutate(Reach = "Non-SMA Totals")

final2 <- table2 %>%
  rbind(bound.watershed.totals2)


## Select and rename columns
final.columns2 <- final2 %>%
  select(Watershed, Reach, everything()) %>%
  mutate(Impervious_Cover_2013_Acres = paste_columns(Impervious2013, Percent_Impervious2013),
         Impervious_Cover_2019_Acres =  paste_columns(Impervious2019, Percent_Impervious2019),
         Change_in_Impervious_Cover2013_2019 = paste_columns(Impervious_Change_Acres, Impervious_Change_Percent),
         Permitted_Impervious_Surface_in_SMA_Acres = paste_columns(P_ImperviousFootprint, P_ImperviousFootprint_Percent),
         Permitted_Overwater_Structures_in_SMA = paste_columns(P_OverWaterStructure, P_OverWaterStructure_Percent),
         Permitted_Shoreline_Compensatory_Mitigation = paste_columns(Shoreline_Enhanced, Shoreline_Enhanced_Percent),
         Permitted_Shoreline_Restoration = paste_columns(P_NonMitigation, P_NonMitigation_Percent),
         Change_in_Impervious_Cover_Not_Accounted = paste_columns(Unaccounted_Impervious_Change_Acres, Unaccounted_Impervious_Change_Percent)) %>%
  select(Watershed, Reach, Area, Impervious_Cover_2013_Acres:Change_in_Impervious_Cover_Not_Accounted)

## Export to final rendering step
export.table2 <- final.columns2 %>%
  group_by(Watershed) %>%
  group_split()
names(export.table2) <- unique(final.columns2$Watershed)




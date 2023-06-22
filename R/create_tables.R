## Pasting function
paste_columns <- function(colA, colB) {
  newcol <- paste(colA, " (", colB, "%)", sep = "")
}

## Comments from Lauren: 6/21/23 call

# 	The % reported with each of these numbers should be the relative contribution of the restoration/enhancement action
# to the change in canopy or impervious cover in the region. 
# 	Ex: if 500 acres impervious lost between 2013 and 2019, and 0 acres of 
# Shoreline_TotalEnhanced action taken, the % reported should be (0/500 = 0%) 
## see north fork ~500 acres line for example



# 	Shoreline_TotalCleared (percentage value is pulling the percentage value for “Canopy_Total2013”). 
# This % should be the contribution of Shoreline_TotalCleared to the overall change in canopy
# 	Ex: if 100 acres of canopy were lost between 2013 and 2019, and Shoreline_TotalCleared was 5,
# the reported % for Shoreline_TotalCleared should be 5%)

# if 15 acres of canopy were cleared, and none of that came from permits, then the Shoreline_TotalCleared would be 0
# if 15 were cleared, and 10 of that came from permits (Pclearing footprint acre aka shoreline cleared), 10/15 (60% ) would be the output.

# DNR_cleared_percent total and unaccounted total are ok, just multiply by -1. DNR should be positive, and unaccounted total
# should also be positivie. 

# If the amount of clearing from Shoreline_TotalCleared and DNR_Total is greater than the Canopy_Change_Total, 
## can we return a value of 0.0 (0.0%)? 

# 	Instead of the “Shoreline_TotalCleared” and “DNR_Total” columns in the impervious cover reporting tables,
# we should have columns reporting the area and % contribution to change 
# in impervious cover from “P_OverWaterStructure” and “P_NewImperviousFootprint” 
## increasing impervious coverage. Both of these add impervious cover. These each have their own columns. 

## TODO CHECK SHORELINE ENHANCED PERCETN --BUGFIX

# Begin table creation ----------------------------------------------------

# Clean and tidy the imported data
raw.data$Watershed <- gsub("/", " ", raw.data$Watershed) # Remove / character


# Select and rename columns
fixed.columns <- raw.data %>%
  filter(str_detect(Watershed, "North")) %>%
  select(Watershed, Reach, 
         Area = Area_Acre, 
         Canopy2013 = CanopyAcre2013,
         Canopy2019 = CanopyAcre2019, 
         Impervious2013 = ImperviousAcre2013,
         Impervious2019 = ImperviousAcre2019,
         Shoreline_Cleared = P_ClearingFootprint_Acre_12_22,
         Shoreline_Enhanced = P_MitigationRestoration_Sqft_12_22, ## Shoreline Enhanced Total
         P_NonMitigation = P_NonMitigationRestoration_SqFT_19_22,
         DNR_Cleared = ForestPracticesAcre_13_19,
         P_OverWaterStructure = P_OverWaterStructure_Acre_12_22,
         P_ImperviousFootprint = P_NewImperviousFootprint_Acre_12_22)

## What/where is the shoreline restoration column?
## o	We will keep the “Shoreline_TotalEnhanced” column and add a “Shoreline_TotalRestoration” column. 
## Shoreline_TotalEnhanced is a catchall column for pasting together.
## 




# Table 1: Summary of Change in Tree Canopy, X Watershed ------------------
table1 <- fixed.columns %>%
  rowwise() %>%
  mutate(across(contains("Canopy"), ~ ((.x / Area) * 100), 
                .names = "Percent_{col}")) %>%
  mutate(Canopy_Change_Acres = Canopy2019 - Canopy2013,
         Canopy_Change_Percent = Percent_Canopy2019 - Percent_Canopy2013) %>%
  mutate(P_NonMitigation_Percent = (P_NonMitigation/Canopy_Change_Acres * 100)) %>%
  mutate(across(matches("Shoreline"), ~ ((.x / Canopy_Change_Acres) * 100),
                .names = "{col}_Percent")) %>%
  mutate(across(matches("DNR"), ~ ((.x / abs(Canopy_Change_Acres)) * 100),
                .names = "{col}_Percent")) %>%
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

## Select and rename columns
final.columns1 <- table1 %>%
  mutate(Canopy_Cover_2013_AcresPTotalUnit = paste_columns(Canopy2013, Percent_Canopy2013),
         Canopy_Cover_2019_AcresPTotalUnit =  paste_columns(Canopy2019, Percent_Canopy2019),
         Change_in_Canopy_Cover2013_2019 = paste_columns(Canopy_Change_Acres, Canopy_Change_Percent),
         Permitted_Shoreline_Clearing_in_SMA_Acres = paste_columns(Shoreline_Cleared, Shoreline_Cleared_Percent),
         Permitted_Shoreline_Compensatory_Migration2012_2022= paste_columns(Shoreline_Enhanced, Shoreline_Enhanced_Percent),
         Permitted_Shoreline_Restoration2019_2022 = paste_columns(P_NonMitigation, P_NonMitigation_Percent),
         Permitted_Clearing_from_DNR_Forestry2013_2021 = paste_columns(DNR_Cleared, DNR_Cleared_Percent),
         Change_in_Canopy_Cover_Not_Accounted = paste_columns(Unaccounted_Canopy_Change_Acres, Unaccounted_Canopy_Change_Percent)) %>%
  select(Watershed, Reach, Area, Canopy_Cover_2013_AcresPTotalUnit:Change_in_Canopy_Cover_Not_Accounted)
  
export.table1 <- final.columns1 %>%
  group_by(Watershed) %>%
  group_split()
names(export.table1) <- unique(cleaned.data$Watershed)
  

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
  mutate(across(where(is.numeric), round, digits = 1)) %>%
  mutate_all(~ifelse(is.nan(.), 0, .))

final.columns2 <- table2 %>%
  mutate(Impervious_Total2013 = paste_columns(Impervious2013, Percent_Impervious2013),
         Impervious_Total2019 =  paste_columns(Impervious2019, Percent_Impervious2019),
         Impervious_Change_Total = paste_columns(Impervious_Change_Acres, Impervious_Change_Percent),
         Shoreline_TotalCleared = paste_columns(Shoreline_Cleared, Percent_Impervious2013),
         Shoreline_TotalEnhanced = paste_columns(Shoreline_Enhanced, Shoreline_Enhanced_Percent),
         DNR_Total = paste_columns(DNR_Cleared, DNR_Cleared_Percent),
         Unaccounted_Total = paste_columns(Other_Impervious_Change_Acres, Other_Impervious_Change_Percent)) %>%
  select(Watershed, Reach, Area, contains("Total"))

export.table2 <- final.columns2 %>%
  group_by(Watershed) %>%
  group_split()
names(export.table2) <- unique(cleaned.data$Watershed)




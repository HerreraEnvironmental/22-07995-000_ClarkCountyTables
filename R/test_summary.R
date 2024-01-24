## Pick up from final.columnsN for this test run

## groupsplit
North <- table1 %>%
  filter(str_detect(Watershed, "North")) 

## apply to each table in the groupsplit 
test <- colSums(North[!str_detect(North$Reach, "Non-SMA"), -c(1:2)]) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_wider(names_from = 1, values_from = 2) %>%
  mutate(Watershed = "Total for Non-SMA Areas in Watershed:",
         Reach = NA) %>%
  select(Watershed, Reach, everything())


final <- North %>%
  rbind(test)






         


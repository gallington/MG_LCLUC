# summaries of management changes

# using base_mgmt and adding DVs for the open-ended answers

base_mgmtDVs <- base_mgmt %>%
  mutate(next5fencingDV = case_when(
    grepl("Fenc",   howChngNext5Y) ~ 1,
    .default = 0)) %>%
  mutate(across(next5fencingDV, as.factor))
  mutate(()) ## ADD NEXT ONES RE: LSK IMPROVEMENT
  # AND PLANTING GRASS
  
  mutate(across(next5fencingDV, as.factor))
#  grepl("Impro",  model) ~ "Improve",  
#  grepl("Toyota", model) ~ "Toyota",
#  grepl("Toyota", model) ~ "Toyota",
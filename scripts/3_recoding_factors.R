
sv<- readRDS("../data/MGsurvey.RDS")

sv %>% tabyl(impact_herd_pract_how) 

sv %>% tabyl(impact_herd_pract_how, show_na = FALSE)
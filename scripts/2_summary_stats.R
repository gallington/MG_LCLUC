# basic summary stats for inital investigation

install.packages("kableExtra")
library(kableExtra)
library(janitor)
library(skimr)

tabyl(sb$outmigYN)
skim(sb)



# demographic changes
base_movement %>% count(whoHerds, sort = TRUE)
base_movement %>% summarize(medLabor = median(laborCountmig, na.rm=TRUE),
                            maxLabor = max(laborCountmig, na.rm= TRUE),
                            minLabor = min(laborCountmig, na.rm = TRUE))

# Q: how many hh have had out-migration? -------
base_movement %>% count(outmigYN)

# Q: what changes to managment? ----------
base_mgmt %>% count(mgmtChng5Y)
base_mgmt %>% count(mgmtChngNext5Y)
base_mgmt %>% count(howChng5Y, sort = TRUE)
howChng5Yr <- base_mgmt %>% count(howChng5Y, sort = TRUE)
howChngNext5Yr <- base_mgmt %>% count(howChngNext5Y, sort = TRUE)

# distance and # times moving
base_mgmt %>% summarize(median_LY = median(moveLY, na.rm = TRUE))
base_mgmt %>% summarize(median_TY = median(moveTY, na.rm = TRUE))

ggplot(base_mgmt, aes(x = moveLY))+
  geom_density(color = "lightblue")+
  geom_density(aes(x = moveTY), color = "blue")

write.csv(howChngNext5Y, file = "./exports/changeNext5Y.csv")
write.csv(howChng5Yr, file = "./exports/changePast5Y.csv")

# how many said Y changed 5yr ago and Y plan to change in future?
base_mgmt %>%
  group_by(mgmtChng5Y, mgmtChngNext5Y)%>%
  summarise(n=n())%>%
  spread(mgmtChngNext5Y, n) %>%
  kable()

# how many said they plan to change but there are limits?
    # see below

# create a search string 
#DVs for fencing------------
# need to get a df with Ref # for those who change 5ya and those who want to change in future
# and cross-ref these to figure out how many people who fenced in the past plan on more fencing
# vs. just new people wanting to fence
    # mgmtChng5Y == "Yes", howChng5Yr == contains "fencing" # Make a DV so can get a count
    # crosstab w/
    # mgmtChngNext5Y == "Yes", Dv for past_fenced = Y

# Playing with setting up Dummy Variables and/or an ordinal var that combines them...

## By setting them this way, they can approximate incremental impacts akin to ordered
# mvmt <- mutate(sb, pastFenced = case_when(howChng5Y CONTAINS== fenc* ~ 0,   # No fencing in past = 0
#                                      
#                                           Rule == 1 | Rule == 2 ~ 1,   # Y fencing in past 5yr = 1
#                                      TRUE ~ NA_real_))            # else NA



sv_fence <- sv %>% # sv is the combined total social and veg data from 1_data_imprt
  mutate(next5fencingDV = case_when(
    # some have two answers, so will have to split those up into diff columns
    grepl("Fenc",   howChngNext5Y) ~ 1,
    .default = 0)) %>%
  mutate(across(next5fencingDV, as.factor))
#  grepl("Impro",  model) ~ "Improve",  
#  grepl("Toyota", model) ~ "Toyota",
#  grepl("Toyota", model) ~ "Toyota",

# how many said want to fence in next 5?
dim(filter(sv_fence, next5fencingDV == 1)) # off from the excel count by 1

# group hh by concated_loc then fenced Y/N then list mean veg params
sv_fence %>%
  group_by(concated_loc, next5fencingDV)%>%
  summarise(n=n(),
            veg_cov = mean(cov23median),
            veg_chng = mean(cov_chng)) %>%
  spread(next5fencingDV, n) %>%
  kable()

# mean veg change or veg cover in hh where fence = Y v. N
ggplot(sv_fence, aes(x= cov_chng, group = next5fencingDV))+
  geom_density()
# no diff

# Calculate the median for each group 
medians <- sv_fence %>% 
  group_by(next5fencingDV) %>%
  summarize(median_value = median(cov23median, na.rm = TRUE))
# NO DUH THEY AREN'T DIFFERENT BC THEY ARE IN THE SAME BAG AND THE COV VALUES ARE SUMMARIZED TO THE BAG. 
  # Instead calculate the prop of hh per bag that want to fence and use that as a continuous...

f1<- sv_fence %>% 
  filter(next5fencingDV == 1) %>%
  group_by(concated_loc) %>%
  #group_by(next5fencingDV) %>%
  summarize(countFenceY = n()) # yields a count of hh that want to fence per bag
# now I need a total # per bag
f2<- sv_fence %>% 
  group_by(concated_loc) %>%
  #group_by() %>%
  summarize(countbaghh = n()) 

# veg per bag
fv<- sv_fence %>% 
  group_by(concated_loc) %>%
#  summarize(mean(cov23mean))
#  summarize(mean(cov_chng))
  summarise(mean())

# Create the density plot with a vertical line for the median of each group
ggplot(sv_fence, aes(x= cov23median, group = next5fencingDV, color = next5fencingDV))+
  geom_density() +
  abline(v = )

# Create the density plot with a vertical line for the median of each group
ggplot(data, aes(x = value, fill = group, color = group)) +
  geom_density(alpha = 0.3, na.rm = TRUE) +
  geom_vline(data = medians, aes(xintercept = median_value, color = group),
             linetype = "dashed", size = 1) +
  labs(title = "Density Plot with Median Lines by Group", x = "Value", y = "Density") +
  theme_minimal()



#mvmt summaries-------
# get summary data for table in order
base2_movement <- na.omit(base_movement) # need this to do the basic summary stats or get NAs


detsLY <- base2_movement %>% dplyr::summarize(
  N = n(),
  min = min(movedLY), avg = mean(movedLY), max = max(movedLY)
)

detsTY <- base2_movement %>% dplyr::summarize(
  N = n(),
  min = min(movedTY), avg = mean(movedTY), max = max(movedTY)
)

boxplot(base2_movement$movedTY , base2_movement$movedLY, data = base2_movement)

# KW test wants it in long form w Distance in one column and YEar in another
#kruskal.test(base2_movement$movedTY ~ base2_movement$movedLY, var.equal=TRUE, data = base2_movement)                    

#dplyr & tidyr: Crosstabs
#To turn our summary data into a crosstab or contingency table, we need variable A (class) to be listed by row, and variable B (cyl) to be listed by column.
#We can achieve this by including the spread() command, to create columns for each cyl value, with n as the crosstab response value.

# limits to changing
base_mgmt %>% count(chngCantMake, sort = TRUE)
#View(base_mgmt %>% count(limitToChng, sort = TRUE))

# those who plan to change but there are limits
base_mgmt %>%
  group_by(mgmtChngNext5Y, chngCantMake)%>%
  summarise(n=n())%>%
  spread(chngCantMake, n) %>%
  kable()
# 88 plan to change and see limits to whether they can
# use those and THEN filter the DV = Y that remain?
# see if get the same things as the one below:
base_mgmt %>%
  mutate(next5fencingDV = case_when(
    # some have two answers, so will have to split those up into diff columns
    grepl("Fenc",   howChngNext5Y) ~ 1,
    .default = 0)) %>%
  mutate(across(next5fencingDV, as.factor)) %>%
  filter(mgmtChngNext5Y == "Yes") %>%
  filter(chngCantMake == "Yes") %>% 
  filter(next5fencingDV == 1) %>%
  count(limitToChng, sort = TRUE)
# SAME RESULTS.

# filter those who want to fence DV = Y, then ChangeCant= YES, then count responses to why can't 
base_mgmt %>%
  mutate(next5fencingDV = case_when(
    # some have two answers, so will have to split those up into diff columns
    grepl("Fenc",   howChngNext5Y) ~ 1,
    .default = 0)) %>%
  mutate(across(next5fencingDV, as.factor)) %>%
  filter(next5fencingDV == 1) %>%
  filter(chngCantMake == "Yes") %>% 
  count(limitToChng, sort = TRUE)
  
str(base_mgmt)


#Q tenure status-----
base_tenure %>% count(wcTenure, wcContract, sort = TRUE)
base_tenure %>% count(wpTenure, wpContract, sort = TRUE)
base_tenure %>% count(wcspcSame)

# this yield wcTenure by row & wcContract by column
base_tenure %>%
  group_by(wcTenure, wcContract)%>%
  summarise(n=n())%>%
  spread(wcContract, n) %>%
  kable()

# wcTenure by wpTenure

base_tenure %>%
  group_by(wcTenure, wpTenure)%>%
  summarise(n=n())%>%
  spread(wpTenure, n) %>%
  kable()

# Comparison of number of times moved based on whether they have the same sp and wtr camp
# wcspcSame==Yes, median # of times move per year vs.wcspcSame == Yes
 
str(base_movement)
str(base_tenure)
ten_mov <- left_join(base_movement, base_tenure)
# Yes same
ten_mov %>% filter(wcspcSame == "Yes") %>% summarise(mean(!is.na(movedTY))) #0.974 
ten_mov %>% filter(wcspcSame == "Yes") %>% summarise(median(!is.na(movedTY))) #1                   

# No not same
ten_mov %>% filter(wcspcSame == "No") %>% summarise(mean(!is.na(movedTY))) #1
ten_mov %>% filter(wcspcSame == "No") %>% summarise(median(movedTY))  #3

# overall 
mean(!is.na(ten_mov$movedTY))
median(!is.na(ten_mov$movedTY))
# MESSAGE: No diff


# need to get a df with Ref # for those who change 5ya and those who want to change in future
# and cross-ref these to figure out how many people who fenced in the past plan on more fencing
# vs. just new people wanting to fence


#Q: pasture conditions--------
base_mgmt %>% group_by(pastureChng, pastureChngHow)%>%
  summarise(n=n())%>%
  spread(pastureChngHow, n) %>%
  kable()

#another test for Jessie

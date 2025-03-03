#This script creates the following subset dfs:
  # base_demog
  # base_movement
  # base_mgmt
  # base_

# Also combines the above and adds in summary veg change data for survey soums/bags

#NOTE: If bringing in new data exported from QGis remember to adjust
  # place names so that they match
  # e.g. #sb values: "Dundgovi"    "Govi-Sumber" "Tuv aimag"  
         #veg values: "Dundgobi"   "Gobisumber" "Tuv"



#SET ENV------- 
##pkgs-----
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(dplyr)
library(rvest)
library(ggplot2) 
library(rlang) 
library(readr)


##set wd ----------------------------------------------------------------------
setwd("C:/Users/jessi/OneDrive - Cornell University/Documents/MSPhD/AllingtonLab_LCLUC/RStudio_WorkingDirectory")

#CODE-------------------------------------------------------------------------

##IMPORT DATA FILE ------------------------------------
# this is the latest file from the SUMR proj, dated 16 Aug 2024:
original_file <- read_csv("MG_LCLUC_Household_Survey_TRANSLATED_02172024.csv", 
                           col_names = TRUE, 
                           trim_ws = TRUE)

names(original_file) <- gsub("\\.x$", "", names(original_file))
view(original_file)


str(original_file)
original_file <- as.data.frame(original_file)
original_file <- original_file %>% rename(Ref = '_1_idInfo_survey_ref_') %>%
                                   mutate(across(Ref, as.factor))
##Subsetting ----- 
# the full spreadsheet into smaller dataframes--- 
# and renaming the super long col names 
# see the codebook to cross-reference names to data types

###base_demog----
base_demog <- dplyr::select(original_file,
                    Ref = 'Ref',
                    Aimag = '_2_idInfo_aimag_',
                    Soum = '_3_idInfo_soum_',
                    bag = '_4_idInfo_bagCode_',
                  #### demographics: -----
                    hhSize = '_1_HHDems_num_HHmembers_',
                    '_2_HHDems_hoh_age_',
                    '_3_HHDems_hoh_sex_',
                    '_4_HHDems_hoh_education_',
                  kidsCamp = '_5_HHDems_num_HHmems_sub16_camp_tot_',
                  kidsSoum = '_5_a_HHDems_num_HHmems_sub16_soum_',
                  kidsAimag ='_5_b_HHDems_num_HHmems_sub16_aimag_',
                  kidsUB = '_5_c_HHDems_num_HHmems_sub16_ub_',
                  yaCamp = '_6_HHDems_num_HHmems_16to30_camp_tot_',
                  yaSoum = `_6_a_HHDems_num_HHmems_16to30_soum_`, 
                  yaAimag =  `_6_b_HHDems_num_HHmems_16to30_aimag_`, 
                  yaUB =  `_6_c_HHDems_num_HHmems_16to30_ub_`,
                  adCamp = '_7_HHDems_num_HHmems_30to60_camp_tot_',
                  adSoum = `_7_a_HHDems_num_HHmems_30to60_soum_`, 
                  adAimag = `_7_b_HHDems_num_HHmems_30to60_aimag_`, 
                  adUB = `_7_c_HHDems_num_HHmems_30to60_ub_`,
                  oldCamp = '_8_HHDems_num_HHmems_60plus_camp_tot_',
                  oldSoum =`_8_a_HHDems_num_HHmems_60plus_soum_`, 
                  oldAimag = `_8_b_HHDems_num_HHmems_60plus_aimag_`, 
                  oldUB = `_8_c_HHDems_num_HHmems_60plus_ub_`, 
                  khotAil_num_hh =  '_9_HHDems_num_HH_khotAil_',
                  khotAil_num_ppl =  '_10_HHDems_num_people_khotAil_',
                  khotAil_stay_tog =  '_11_HHDems_do_HH_stayTogether_'
                    )
view(base_demog)

###base_demog_aimag----
base_demog <- mutate(base_demog, newAimag = case_when(Aimag == "Tuv aimag" ~ "Tuv", 
                                      Aimag == "Govi-Sumber" ~ "Govisumber",
                                      Aimag == "Dundgovi" ~ "Dundgovi"
                      )) %>%
              mutate(across(newAimag, as.factor)) %>%
              mutate(across(c(1:4), as.factor)) %>%
              mutate(concated_loc = paste(newAimag, Soum, bag, sep = '_')
              )  
view(base_demog)

###base_movement--------                   
base_movement <- dplyr::select(original_file,
                     Ref, 
                     whoHerds = '_1_labor_HHmems_movesAnimals_daily_',
                     whoHerdsmig = '_2_labor_HHmems_seasonalMig_toCamp_',
                     laborCountmig = '_2_a_labor_HHmems_seasonalMig_toCamp_num_',
                     daily_fw_time = '_2_b_labor_HHmems_durAutmnWinter_',
                     daily_ss_time = '_2_c_labor_HHmems_durSpringSummer_',
                     outmigYN = '_3_labor_HHmems_leftCamp_',
                     mig_reason = '_3_a_labor_HHmems_leftCamp_reason_' ,
                     movedLY = '_5_herdMgmt_lastYr_timesMoved_camp_',
                     movedTY = '_4_herdMgmt_thisYr_timesMoved_camp_',
                     perm_move = '_3_d_labor_permMove_',	# 3. d. Is this a permanent move?
                     return_plan = '_3_e_labor_returnPlan_',	#3. e. Do they plan to return? 
                     move_reasons = '_4_labor_HHmems_moveReason_',	#4. What are the reasons for relocation to soum/aimag center/UB
                     impacted_labor = '_5_labor_HHmems_mig_impHerdLabor_',	# 5. Has the migration in household members to soum center/UB impacted herding labor?:
                     impact_how = '_5_a_labor_herdLabor_ifYes_how_',	# 5.a. If YES, How has the number of people available to help with herding changed?
                     impact_herding_pract = '_6_labor_HHmems_mig_impHerdPractices_',	# 6. Has the migration in household members to soum center/UB impacted herding practices ?  
                     impact_herd_pract_how =  '_6_a_labor_herdPractices_ifYes_how_',	# 6. a. If YES, what has changed?
                     hired_labor =  '_7_labor_hireLabor_',	# 7. Do you hire extra labor to help with herding?
                     hired_labor_daily =  '_7_a_labor_hireLabor_dailyHerdMvmts_',	# 7. a. Do they assist with daily herd movements?
                     hired_labor_seas_mvmt =  '_7_b_labor_hireLabor_moveToPasture_',	# 7. b. Do they assist with moving to spring/summer/fall/winter pastures?
                     hired_labor_otor =  '_7_c_labor_hireLabor_assistOtor_',	# 7. c. Do they assist with otor? 
                     hired_labor_other =  '_8_labor_hireLabor_otherHusbandry_',	# 8. Do you hire extra labor to help with other husbandry activities?
                     hired_labor_other_how =  '_8_a_labor_ifYes_how_text_'	# 8.a. If Yes, what do they help with
                     ) %>%
  mutate(across(c(1:3,7:8,1:13), as.factor))
view(base_movement)
  

#base2_movement <- na.omit(base_movement). # need this to do the basic summary stats or get NAs

#str(base2_movement)
#base2_movement <- base2_movement %>%  mutate(across(Ref, as.factor))

###tenure----- 
base_tenure <- dplyr::select(original_file,
                      Ref,       
                      wcTenure = '_1_tenure_HH_winterCamp_',
                      wcContract = '_2_tenure_HH_winterCamp_contract_',
#                      '_2_a_tenure_ifYes_whoHolds_contract_',
                      wpTenure = '_3_tenure_HH_winterPasture_',
                      wpContract = '_4_tenure_HH_winterPasture_contract_',
                      wcspcSame = '_5_tenure_isWinterCamp_springCamp_', # are winter and spring camps the same?
                      scTenure = '_6_tenure_HH_springCamp_',
                      scContract = '_7_tenure_HH_springCamp_contract_',
                      spContract = '_8_tenure_HH_springPasture_contract_'
)
base_tenure %<>% mutate(across(c(2:9), as.factor))
view(base_tenure)




###altLivelihoods-------
# _1_a_altLivelihoods_HHmems_nonHerdingWork_
# _1_a_i_altLivelihoods_IfYes_who_
# _1_a_i_altLivelihoods_IfYes_what_
# _1_b_altLivelihoods_otherInc_
# _1_b_altLivelihoods_otherInc_pension_
# _1_b_altLivelihoods_otherInc_herdingLivestock_
# _1_b_altLivelihoods_otherInc_salary_
# _1_b_altLivelihoods_otherInc_govAllowance_
# _1_b_altLivelihoods_otherInc_govAllowance_childBenefit_
# _1_b_altLivelihoods_otherInc_hourlyWage_
# _1_b_altLivelihoods_otherInc_remits_
# _1_b_altLivelihoods_otherInc_crafts_
# _1_b_altLivelihoods_otherInc_investments_
# _1_b_altLivelihoods_otherInc_agencyAid_
# _1_b_altLivelihoods_otherInc_other_
# _1_b_i_other_text_
# _2_altLivelihoods_loans_whereToGet_
# _3_altLivelihoods_loans_howOften_yr_
# _4_altLivelihoods_loans_minMax_
# _4_altLivelihoods_loans_min_millions_
# _4_altLivelihoods_loans_max_millions_
# _5_altLivelihoods_finNeed_difTime_
# _5_altLivelihoods_finNeed_difTime_winter_
# _5_altLivelihoods_finNeed_difTime_autumn_
# _5_altLivelihoods_finNeed_difTime_lunarNY_
# _5_altLivelihoods_finNeed_difTime_spring_
# _5_altLivelihoods_finNeed_difTime_summer_
# _5_altLivelihoods_finNeed_difTime_yearround_





###herd mgmt-------
base_mgmt <- dplyr::select(original_file, 
                      Ref,
                      # _1_herdMgmt_dailyHerd_dist_km_
                      # _1_a_herdMgmt_dailyHerd_summer_dist_km_
                      # _1_b_herdMgmt_dailyHerd_winter_dist_km_
                      # _2_herdMgmt_summerCamp_distFrom_km_
                      # _3_herdMgmt_winterCamp_distFrom_km_
                      moveTY = '_4_herdMgmt_thisYr_timesMoved_camp_',
                      moveLY = '_5_herdMgmt_lastYr_timesMoved_camp_',
                      distLY = '_6_herdMgmt_lastYr_avgMoveDist_km_',
                      # _7_herdMgmt_lastYr_totMoveDist_km_
                      distPast = '_8_herdMgmt_10yrsAgo_avgMoveDist_km_',
                      # _9_herdMgmt_lastYr_otor_yn_
                      # _9_a_ifYes_
                      # _10_herdMgmt_thisYr_otor_yn_
                      # _11_herdMgmt_thisYr_winterPasture_res_yn_
                      # _12_herdMgmt_lastYr_winterPasture_res_yn_
                      # _13_herdMgmt_thisYr_springPasture_res_yn_
                      # _14_herdMgmt_lastYr_springPasture_res_yn_
                      # _15_herdMgmt_thisYr_dzudPasture_res_yn_
                      # _16_herdMgmt_lastYr_dzudPasture_res_yn_
                      # _17_herdMgmt_past5yrs_resPasture_grazed_yn_
                      # _18_a_herdMgmt_10yrsAgo_dailyHerd_means_
                      # _18_a_herdMgmt_10yrsAgo_dailyHerd_means_walk_km_
                      # _18_a_herdMgmt_10yrsAgo_dailyHerd_means_horseCamel_km_
                      # _18_a_herdMgmt_10yrsAgo_dailyHerd_means_car_km_
                      # _18_a_herdMgmt_10yrsAgo_dailyHerd_means_motorbike_km_
                      # _18_b_herdMgmt_5yrsAgo_dailyHerd_means_
                      # _18_b_herdMgmt_5yrsAgo_dailyHerd_means_walk_km_
                      # _18_b_herdMgmt_5yrsAgo_dailyHerd_means_horseCamel_km_
                      # _18_b_herdMgmt_5yrsAgo_dailyHerd_means_car_km_
                      # _18_b_herdMgmt_5yrsAgo_dailyHerd_means_motorbike_km_
                      # _18_c_herdMgmt_lastYr_dailyHerd_means_
                      # _18_c_herdMgmt_lastYr_dailyHerd_means_walk_km_
                      # _18_c_herdMgmt_lastYr_dailyHerd_means_horseCamel_km_
                      # _18_c_herdMgmt_lastYr_dailyHerd_means_car_km_
                      # _18_c_herdMgmt_lastYr_dailyHerd_means_motorbike_km_
                      dailyDist10Y = '_19_herdMgmt_10yrsAgo_dailyHerd_distTrav_km_',
                      # _19_a_herdMgmt_10yrsAgo_winter_dailyHerd_distTrav_km_
                      # _19_b_herdMgmt_10yrsAgo_summer_dailyHerd_distTrav_km_
                      dailyDist5Y =  '_20_herdMgmt_5yrsAgo_dailyHerd_distTrav_km_',
                      # _20_a_herdMgmt_5yrsAgo_winter_dailyHerd_distTrav_km_
                      # _20_b_herdMgmt_5yrsAgo_summer_dailyHerd_distTrav_km_
                      dailyDistLY = '_21_herdMgmt_lastYr_dailyHerd_distTrav_km_',
                      # _21_a_herdMgmt_lastYr_winter_dailyHerd_distTrav_km_
                      # _21_b_herdMgmt_lastYr_summer_dailyHerd_distTrav_km_
                      mgmtChng5Y =  '_22_herdMgmt_past5yrs_mgmtPractices_chgd_yn_', # In the past 5 years, have you changed any of your herd or pasture management practices?
                      howChng5Y = '_22_a_herdMgmt_ifYes_how_',
                      mgmtChngNext5Y = '_23_herdMgmt_next5Yrs_mgmtPractices_chgs_yn_',
                      howChngNext5Y =  '_23_a_herdMgmt_ifYes_how_',
                      chngCantMake = '_24_herdMgmt_mgmtPractices_chgs_cantMake_',
                      limitToChng = '_25_a_herdMgmt_ifYes_lmtngFactor_',
                      pastureChng = '_1_resUse_past3yrs_pastureCon_chg_',
                      pastureChngHow = '_1_a_resUse_past3yrs_pastureCon_chg_deg_'
                      )
str(base_mgmt)
# have to upate factor column codes as add elements to df"
base_mgmt %<>% mutate(across(c(1, 9:16), as.factor))
view(base_mgmt)


###livestock_fodder-------
base_lsk <- dplyr::select(original_file, 
                           Ref,
                          camel23 = '_1_livestock_EOY_2023_camel_tot_',
                          cow23 = '_2_livestock_EOY_2023_cow_tot_',
                          horse23 = '_3_livestock_EOY_2023_horse_tot_',
                          sheep23 = '_4_livestock_EOY_2023_sheep_tot_',
                          goat23 = '_5_livestock_EOY_2023_goat_tot_',
                          camel5Y = '_1_1_livestock_EOY_5yrsAgo_camel_tot_',
                          cow5Y = '_2_1_livestock_EOY_5yrsAgo_cow_tot_',
                          horse5Y = '_3_1_livestock_EOY_5yrsAgo_horse_tot_',
                          sheep5Y = '_4_1_livestock_EOY_5yrsAgo_sheep_tot_',
                          goat5Y = '_5_1_livestock_EOY_5yrsAgo_goat_tot_',
                          # FODDER Qs
                          '_7_livestock_lastYr_suppFodder_yn_',
                          '_8_livestock_thisYr_suppFodder_yn_',
                          '_9_livestock_vegForageShifts_longTerm_yn_',
                          '_9_a_livestock_vegForageShifts_ifSo_quanQual_',
                          '_9_b_livestock_vegForageShifts_ifSo_chgType_',
                          # Herd change
                          '_10_livestock_past5yrs_herdSzChg_',
                          #'_10_a_livestock_past5Yrs_herdSzChg_inc_reason_',
                          #'_10_a_livestock_past5Yrs_herdSzChg_inc_births_',
                          #'_10_a_livestock_past5Yrs_herdSzChg_inc_prchsdMore_',
                          #'_10_a_livestock_past5Yrs_herdSzChg_inc_gift_',
                          #'_10_a_livestock_past5Yrs_herdSzChg_inc_other_',
                         ## '_10_a_i_livestock_past5Yrs_herdSzChg_inc_reason_other_', #error
                          ##'_10_a_i_livestock_past5Yrs_herdSzChg_inc_reason_other_text_', # error
                          #'_10_b_livestock_past5yrs_herdSzChg_dec_reason_', 
                          #'_10_b_i_livestock_past5yrs_herdSzChg_dec_reason_other_text_',
                          '_11_livestock_nextYr_herdSzChg_plans_yn_',
                          '_11_a_livestock_nextYr_herdSzChg_plans_ifYes_what_',
                          ##'_11_b_livestock_nextYr_herdSzChg_plans_ifYes_why_' # error
)
str(base_lsk)
base_lsk %<>% mutate(across(c(1, 9:16), as.factor))

View(base_lsk)



#Combined_base dfs-------
sb <-  base_demog%>% 
  left_join(base_mgmt) %>%
  left_join(base_movement) %>%
      left_join(base_tenure)
view(sb)


#Veg change from QGis------- summarized by SOUM???
veg <- read_csv("./data/veg_cov_chng_export.csv", 
                col_types = c("fffnddddddn"))

veg <- as_tibble(veg)

veg <- veg %>% 
  dplyr::select(c(1:3, 5:6, 8:9, 11)) %>%
  rename(
      Aimag = '_2_idInfo_aimag_', 
      Soum = '_3_idInfo_soum_',
      bag = '_4_idInfo_bagCode_',
       cov23mean = '_cov23mean',
       cov23median = '_cov23medi',
       cov19mean = '_cov19mean',
       cov19median = '_cov19medi',
       cov_chng = 'cov_chng_19_23'
       )    %>%
  mutate(newAimag = case_when(Aimag == "Tuv" ~ "Tuv", 
                              Aimag == "Gobisumber" ~ "Govisumber",
                              Aimag == "Dundgobi" ~ "Dundgovi"
       )) %>%
  mutate(concated_loc = paste(newAimag, Soum, bag, sep = '_')
         ) %>%
  mutate(across(9:10, as.factor))

str(veg)
str(sb)    

sv <- sb %>% left_join(veg, by = "concated_loc") %>% mutate(across(concated_loc, as.factor))
str(sv)

saveRDS(sv, "./data/MGsurvey.RDS")

#extras----------                    
# library(purrr)
# ## calculate how many NAs there are in each variable 
# base_movement %>%
# map(is.na) %>% map(sum)    


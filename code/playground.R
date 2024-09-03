col <- "q43"
df <- survey6

index <- which(survey_codebook$q == col)
#Parses the content from the "options" column of survey_codebook, splitting it into integers using the regex pattern "[:punct:]", representing answer choices.
values <- as.integer(unlist(stringr::str_split(survey_codebook$options[index], pattern = "[:punct:]")))
#Similarly extracts labels for answer options from the "choices" column in survey_codebook.
tags <- unlist(str_split(survey_codebook$choices[index], "; "))
#Determines if there are "not applicable" labels in the answer options by checking the tags. recode_na is a logical value (TRUE or FALSE) indicating whether these values need to be recoded.
recode_na <- any(str_detect(tags, "not applicable"))
#If any of the labels in tags is null or if recode_na is NA (meaning it wasn't set to TRUE in the previous step), then recode_na is explicitly set to FALSE. This ensures that recode_na is either TRUE or FALSE and not NA.
if(any(is.null(tags), is.na(recode_na))) {
  recode_na <- FALSE 
}
# This conditional block checks if the values in the values vector are exactly equal to the vector c(1, 2). If they are, it proceeds with the following steps:
if(all.equal(values, c(1,2)) == TRUE) {
  df[[col]] <- 2 - as.integer(df[[col]]) #recoding responses to 0,1
  values <- (2 - values) #recoding answer choices column to reflect change above
  
  # In this block, it further checks if recode_na is TRUE (meaning there were "not applicable" labels) and if the values vector is equal to c(1, 2). If both conditions are met, it performs the following actions: basically we are changing 1 to 1, and 2 to 0
  if(recode_na & all.equal(values, c(1,2)) == TRUE) {
    df[[col]][df[[col]] == -1] <- 2 ##unsure here...
    values[values == -1] <- 2
  }
}

named <- setNames(values, tags) %>% na.omit #re-mapping choices (text) to the re-coded choices (numbers)

if(is.null(tags)) {
  named <- FALSE #where re-mapping didn't work, call those FALSE in named
}


if(col %in% c(likert, simple)) {
  out <- df[c("responseid", col)] %>%
    mutate_at(vars(col), ~labelled(as.integer(.), named)) ###leaving off here###
  
  # multiple answer  
} else if(col %in% mavr){
  
  sym_col <- sym(col)
  out <- df[c("responseid", col)] %>%
    fastDummies::dummy_cols(col, split = ",", ignore_na = TRUE) %>%
    tidytext::unnest_tokens(output = !!sym_col, input = col, token = stringr::str_split,  pattern = ",") %>%
    mutate_at(col, ~factor(as.integer(.), levels = values, labels = tags)) %>%
    group_by(responseid) %>% mutate_at(col, ~paste(., collapse = ";")) %>%
    distinct %>% mutate(across(where(is.character), ~na_if(., "NA")))

#######

library(data.table)

old <- fread("data/output/wrangled.csv")
new <- fread("data/output/wrangled_new.csv")

nrow(old) == nrow(new)
ncol(old) == ncol(new)

names(old)[!names(old) %in% names(new)]
names(new)[!names(new) %in% names(old)]

old_cols <- names(old)[names(old) %in% names(new)]
new_cols <- names(new)[names(new) %in% names(old)]

# old_cols <- sort(names(old)[names(old) %in% names(new)])
# new_cols <- sort(names(new)[names(new) %in% names(old)])

old_subset <- old[, ..old_cols]
new_subset <- old[, ..new_cols]

ncol(old_subset) == ncol(new_subset)
names(old_subset)[!names(old_subset) %in% names(new_subset)]
names(new_subset)[!names(new_subset) %in% names(old_subset)]

identical(old, new)
all.equal(old, new)

identical(old_subset, new_subset)
all.equal(old_subset, new_subset)


q30 <- file[str_detect(elements, "QID116")][[1]]
q32 <- file[str_detect(elements, "QID35")][[1]]
q31 <- file[str_detect(elements, "QID34")][[1]]
q38 <- file[str_detect(elements, "QID87")][[1]]

unlisted_30 = unlist(lapply(q30$Payload$Choices, function(element) trimws(element$Display)))
unlisted_32 = unlist(lapply(q32$Payload$Choices, function(element) trimws(element$Display)))
unlisted_31 = unlist(lapply(q31$Payload$Choices, function(element) trimws(element$Display)))
unlisted_38 = unlist(lapply(q38$Payload$Choices, function(element) trimws(element$Display)))

part_31 = unlisted_31
part_32 = unlisted_32
part_30 = unlisted_30
part_38 = unlisted_38

unlisted_31 = unlist(lapply(q31$Payload$Answers, function(element) trimws(element$Display)))
unlisted_32 = unlist(lapply(q32$Payload$Answers, function(element) trimws(element$Display)))
unlisted_30 = unlist(lapply(q30$Payload$Answers, function(element) trimws(element$Display)))
unlisted_38 = unlist(lapply(q38$Payload$Answers, function(element) trimws(element$Display)))

names(q31$Payload)
test <- "Q31_"
part <- lapply(q32$Payload$ChoiceDataExportTags, function(element) gsub(test, "", element)) %>% unlist()

names(part_31) <- part

(temp_30 = names(part_30))
(temp_31 = names(part_31))
(temp_38 = names(part_38))
# 
# ## Placeholders
# survey6 <- survey6 %>% mutate(gc = 99, distributionchannel = 99)
# 
# wrangled <- survey6 %>% 
#   mutate(completion = rowSums(!is.na(survey6))/ncol(survey6)) %>%
#   # make new variables
#   mutate(
#     #### location variables ####
#     # create binary for all of the boroughs
#     bronx_bi = borough == "bronx",
#     brooklyn_bi = borough == "brooklyn",
#     queens_bi = borough == "queens",
#     manhattan_bi = borough == "manhattan",
#     staten_island_bi = borough == "staten island",
#     
#     #### transform binary variables ####  #add new variables that need binaries##
#     #across(c(q24_8, q25_9), ~ifelse(. == 3, NA_integer_, 2 - .), .names = "{.col}_bi"),
#     
#     # for the disability question, 1 is cognitive disability, 2 is physical disability, 3 is neither, 4 prefer not to say. based ont that now we are creating 4 binary variables below: 
#     dis = ifelse(disab == 1 | disab == 2, 1, 0),
#     cog_dis = ifelse(disab == 1, 1, 0),
#     phys_dis = ifelse(disab == 2, 1, 0),
#     both_dis = ifelse(disab == 1 & disab == 2, 1, 0),
#     
#     # east asian or south asian, for the spring 2023 survey, we divided asian into east asian and south asian, but still we need a general asian binary variable just for the convenience of weighting, because we are not 100% sure that we could find the census data including explicit data on both south asian and east asian
#     asian = ifelse(race_e_asian == 1 | race_s_asian == 1, 1, 0),
#     
#     #### income variables ####
#     # income change variables, creating binary variabls for positive changed income and negative changed income
#     inc_neg = ifelse(inc_ch == 2, 1, 0),
#     inc_pos = ifelse(inc_ch == 1, 1, 0),
#     inc_same = ifelse(inc_ch == 3, 1, 0), 
#     
#     # inc_drop_pov = q13 > poverty_line & q14 <= poverty_line,
#     
#     # The code checks these income categories to determine whether individuals or families fall into the "below poverty" category based on the following conditions:
#     
#     #q15 == 1 | q15 == 2: If the income category is either 1 or 2, the individual or family is classified as "below poverty." This means they have an income below $36,500.
#     
#     #q15 == 3: If the income category is 3, the code checks additional conditions involving the composition of the family:
#     
#     #q14_2 == 1 & q14_3 + q14_4 + q14_5 >= 1: If there is exactly one adult (q14_2 == 1) and at least one child aged 0-2, 3-5, or 6-17 in the family, the individual or family is classified as "below poverty."
#     #q14_2 == 1 & q14_1 >= 1: If there is exactly one adult (q14_2 == 1) and at least one senior (q14_1 >= 1) in the family, they are classified as "below poverty."
#     #q14_2 > 1 & q14_3 + q14_4 + q14_5 == 0: If there are multiple adults (q14_2 > 1) and no children aged 0-2, 3-5, or 6-17 in the family, they are classified as "below poverty."
#     #q14_2 > 1 & q14_1 == 0: If there are multiple adults (q14_2 > 1) and no seniors in the family, they are classified as "below poverty."
#     #q15 == 4: If the income category is 4, the code checks more complex conditions involving the composition of the family:
#     
#     #q14_2 == 1 & q14_1 >= 1: If there is exactly one adult (q14_2 == 1) and at least one senior in the family, they are classified as "below poverty."
#     #q14_2 == 1 & q14_3 + q14_4 + q14_5 + q14_1 >= 3 & q14_1 >= 1: If there is exactly one adult and at least three family members, including at least one senior, they are classified as "below poverty."
#     #q14_2 == 1 & q14_3 + q14_4 + q14_5 + q14_1 >= 4 & q14_3 + q14_4 + q14_5 >= 3: If there is exactly one adult and at least four family members, including at least three children aged 0-2, 3-5, or 6-17, they are classified as "below poverty."
#     #q14_2 == 2 & q14_3 + q14_4 + q14_5 + q14_1 >= 2: If there are two adults and at least two family members, including at least two children or seniors, they are classified as "below poverty."
#     #q14_2 >= 3: If there are three or more adults in the family, they are classified as "below poverty."
#     below_poverty_bi = ifelse(income == 1 | income == 2 | 
#                                 income == 3 & hh_ad_18_64 == 1 & hh_ch_6_17 + hh_ch_3_5 + hh_ch_0_2 >= 1 | 
#                                 income == 3 & hh_ad_18_64 == 1 & hh_sn_65 >= 1 | 
#                                 income == 3 & hh_ad_18_64 > 1 & hh_ch_6_17 + hh_ch_3_5 + hh_ch_0_2 == 0 | 
#                                 income == 3 & hh_ad_18_64 > 1 & hh_sn_65 == 0 | 
#                                 income == 4 & hh_ad_18_64 == 1 & hh_sn_65 >= 1 | 
#                                 income == 4 & hh_ad_18_64  == 1 & hh_ch_6_17 + hh_ch_3_5 + hh_ch_0_2 + hh_sn_65 >= 3 & hh_sn_65 >= 1 |
#                                 income == 4 & hh_ad_18_64 == 1 & hh_ch_6_17 + hh_ch_3_5 + hh_ch_0_2 + hh_sn_65 >= 4 & hh_ch_6_17 + hh_ch_3_5 + hh_ch_0_2 >= 3 | 
#                                 income == 4 & hh_ad_18_64 == 2 &  hh_ch_6_17 + hh_ch_3_5 + hh_ch_0_2 + hh_sn_65 >= 2 | 
#                                 income == 4 & hh_ad_18_64 >= 3, 1, 0),
#     
#     inc_dist = case_when(
#       income <= poverty_line ~ 1, 
#       income <= median_inc ~ 2,
#       TRUE ~ 3), # label them with haven
#     
#     
#     # above median income both
#     inc_ab_med = income > median_inc,
#     inc_be_med = income <= median_inc,
#     
#     #### employment variables ####
#     # creating a work-in-person binary variable
#     wrk_in = wrk %in% c(1, 2, 3)
#     
#     # work full-time; 
#     #work part-time; 
#     #freelance or consultant; 
#     #gig worker (uber, lyft, instacart, etc.); 
#     #small business owner; 
#     #homemaker; 
#     #student; 
#     #retired; 
#     #unemployed; 
#     #other
#     emp_status_before = case_when(
#       str_detect(emp_before, "unemp") ~ "unemployed",
#       str_detect(emp_before, "full|part|free|gig|small|home") ~ "employed",
#       str_detect(emp_before, "student") ~ "student",
#       str_detect(emp_before, ";") ~ "multiple_other",
#       TRUE ~ emp_before), # students will be categorized
#     
#     emp_status_after = case_when(
#       str_detect(emp_after, "unemp") ~ "unemployed",
#       str_detect(emp_after, "full|part|free|gig|small|home") ~ "employed",
#       str_detect(emp_after, "student") ~ "student",
#       str_detect(emp_after, ";") ~ "multiple_other",
#       TRUE ~ emp_after),
#     
#     emp_change = emp_before != emp_after,
#     
#     #
#     emp_before_part_time = case_when(
#       emp_before_un == 0 & any(emp_before_ft == 1, emp_before_pt == 1, emp_before_fl == 1) ~ 1, #, q18_10 == 1
#       emp_before_un == 1 ~ 0
#     ),
#     
#     emp_after_part_time = case_when(
#       emp_after_un == 0 & any(emp_after_ft == 1, emp_after_pt == 1, emp_after_fl == 1) ~ 1, #, q19_10 == 1 # q19_9 == 0 & 
#       TRUE ~ 0 #q19_9 == 1 ~ 0
#     ),
#     #unemployed = emp_after == "unemployed", # newly unemployed # emp_un_after
#     
#     #### mental health variables ####
#     ## q36_1 unable to control
#     ## q36_2 confident about your ability
#     ## q36_3 going your way
#     ## q36_4 difficulties piling up
#     
#     across(c(q33_2, q33_3),
#            ~labelled(5-.,5-attributes(final_clean6$q33_3)$labels)),
#     across(c(q33_1, q33_4),
#            ~labelled(.-1,attributes(final_clean6$q33_4)$labels-1)),
#     stress_score = rowSums(across(starts_with("stress_"))),
#     stress_bi = mean(stress_score, na.rm = TRUE) < stress_score,
#     
#     
#     #### health variables ####
#     ## q22_5 indicate no insurance, so we are creating an ins_has by subtracts the value of q29_5 from 1.
#     ins_has = ins_none == 0,
#     
#     #### household variables ####
#     ## q5 = age
#     decade = floor(as.numeric(age)/10),
#     
#     hh_size = rowSums(across(starts_with("hh"))),
#     hh_ad_one = hh_sn_65 + hh_ad_18_64 == 1,
#     
#     hh_sn_65_bi = hh_sn_65 >= 1, # fixed 
#     hh_ch_6_17_bi = hh_ch_6_17 >= 1,
#     hh_ch_3_5_bi = hh_ch_3_5 >= 1,
#     hh_ch_0_2_bi = hh_ch_0_2 >= 1,
#     hh_ch_0_17_bi = if_any(c(hh_ch_6_17, hh_ch_3_5, hh_ch_0_2), ~. >= 1),
#     hh_ch_sn = if_all(c(hh_sn_65_bi, hh_ch_0_17_bi), ~. >= 1),
#     #hh_ch_0_17_bi = q26_3 >=1 | q26_4 >=1,
#     
#     #### education variables #### creating education binary variables
#     sch_level_cat = labelled::to_character(sch_level),
#     sch_level_cat = case_when(
#       str_detect(sch_level_cat, "high school|some college") ~ as.numeric(sch_level),
#       str_detect(sch_level_cat, "bach|ass") ~ 4,
#       str_detect(sch_level_cat, "doct|mast|prof") ~ 5
#     ),
#     
#     sch_bach = ifelse(sch_level_cat <= 3, 0, 1),
#     
#     #### discrimination variables ####
#     discrim_bi = discrim %in% c(1, 2),
#     
#     #### abuse or violence variables ####
#     exp_ab_or_vi = str_detect(exp, "yes"), # binary variable describing if the respondent has ever experienced either physical abuse or mental abuse 
#     exp_ab_and_vi = str_detect(exp, "verbal abuse") & str_detect(exp, "physical violence"), # binary variable describing if the respondent has ever experienced both physical abuse and mental abuse 
#     
#     # at least two concerns about insecurity
#     #fin_unstable = q21_3 == 1 & q21_7 == 1 | q21_7 == 1 & q21_8 == 1 |
#     #  q21_8 == 1 & q21_3 == 1,
#     # q22_2 diff_worr - worried about running out of food
#     # q22_3 diff_ran_out
#     # q22_7 diff_bill
#     # q22_8 diff_rent
#     
#     
#     # worried about running out or ran out of food
#     #This checks if there is at least one "1" (likely indicating a positive response) in either q25_1 or q24_1.
#     #q24_1 binary assessing if they have difficulty affording food, q25_1 assessing if they have diffifulty accessing food
#     #If there is a "1" in either of these variables, food_insec will be assigned the value TRUE (or 1), indicating that the respondent is worried about running out or has run out of food.
#     food_insec = if_any(c(diff_access_food, diff_afford_food), ~.==1), 
#     
#     #This code checks if there is at least one "1" (likely indicating a positive response) in either q24_4 or q25_4.
#     #q24_4 binary assessing if they have difficulty paying rent or mortgage, q25_4 assessing if they have diffifulty accessing finding housing
#     #If there is a "1" in either of these variables, house_insec will be assigned the value TRUE (or 1), indicating that the respondent is worried about housing
#     house_insec = if_any(c(diff_afford_rent, diff_access_hous), ~.== 1),
#     
#     #This code checks if there is at least one "1" (likely indicating a positive response) in either q24_1 or q24_3 q24_4.
#     #q24_1 binary assessing if they have difficulty affording food, q24_3 assessing if they have diffifulty paying utilities,q24_4 binary assessing if they have difficulty paying rent or mortgage,
#     #If there is a "1" in either of these variables, fin_insec will be assigned the value TRUE (or 1), indicating that the respondent is worried about financial problems
#     fin_insec = if_any(c(diff_afford_food, diff_afford_util, diff_afford_rent), ~.== 1),
#     
#     # accessing resources
#     #resources: accessing to Food\Social services and/or benefits (e.g. food stamps)\Transportation\Finding housing\Internet and/or technology.If there is a "1" in either of these variables, res_insec will be assigned the value TRUE (or 1), indicating that the respondent is worried about resource accessing problems
#     res_insec = if_any(c(diff_access_food, diff_access_ss, diff_access_trans, diff_access_hous, diff_access_internet), ~.==1),
#     
#     
#     #### rating variables ####
#     
#     # #federal government
#     # rate_gov_fed_bad = q31_1 == 4 | q31_1 == 5,
#     # #state government
#     # rate_gov_sta_bad = q31_2 == 4 | q31_2 == 5,
#     # #City government
#     # rate_gov_cit_bad = q31_3 == 4 | q31_3 == 5,
#     # #rate all levell
#     # rate_gov_all_good = if_all(starts_with("q31"), ~.<3),
#     # rate_gov_all_bad = if_all(starts_with("q31"), ~(.==4 | .== 5)),
#     # rate_gov_all_ave = round(rowMeans(6 - across(starts_with("q31"))), digits = 4),
#     
#     across(c(rate_neigh_pub, rate_neigh_rec, rate_neigh_code, rate_neigh_pol, rate_neigh_pre), ~ . < 3, .names = "{.col}_good"),
#     across(c(rate_neigh_pub, rate_neigh_rec, rate_neigh_code, rate_neigh_pol, rate_neigh_pre), ~ . > 3, .names = "{.col}_bad"),
#     
#     #### access variables ####
#     lr_fam = if_any(starts_with("lr_") & ends_with("ff"), ~.==1),
#     lr_gov = if_any(starts_with("lr_") & ends_with("gov"), ~.==1),
#     lr_fb = if_any(starts_with("lr_") & ends_with("fb"), ~.==1),
#     lr_np = if_any(starts_with("lr_") & ends_with("np"), ~.==1),
#     lr_uni = if_any(starts_with("lr_") & ends_with("uni"), ~.==1),
#     lr_emp = if_any(starts_with("lr_") & ends_with("emp"), ~.==1),
#     
#     
#     
#     race_census = ifelse(race != "white (non-hispanic or latinx)", 
#                          str_replace_all(race, 
#                                          c(".*hispanic or latinx.*" = "hispanic or latinx",
#                                            ".*;.*" = "two or more races",
#                                            "prefer not to answer" = NA_character_)), 
#                          "white (non-hispanic or latinx)"))
# 
# 
# wrangled <- mutate(wrangled, inc_dist = ifelse(income == 1 | income == 2, 1, ifelse(income == 3, 2, 3)))
# wrangled <- mutate(wrangled, inc_ab_med = ifelse(income == 4 | income == 5, 1, 0))
# wrangled <- mutate(wrangled, inc_be_med = ifelse(income == 3 | income == 2 | income == 1, 1, 0))
# 
# wrangled <- mutate(wrangled, hh_ch_3_5_bi = ifelse(hh_ch_3_5 == 0, 0, 1))
# 
# wrangled <- mutate(wrangled, discrim_bi = ifelse(discrim == 1 | discrim == 2, 1, ifelse(discrim == 3, 0, NA)))
# #creating binary variable food_insec
# wrangled <- mutate(wrangled, food_insec = ifelse(diff_access_food == 1 | diff_afford_food == 1, 1, ifelse(diff_access_food == 0 & diff_afford_food == 0, 0, NA)))
# 
# #creating binary variable house_insec
# wrangled <- mutate(wrangled, house_insec = ifelse(diff_access_hous == 1 | diff_afford_rent == 1, 1, ifelse(diff_access_hous == 0 & diff_afford_rent == 0, 0 , NA)))
# 
# #creating binary variable sev_food_insec
# wrangled <- mutate(wrangled, sev_food_insec = ifelse(food_insec == 1 & insecure_food_ran_out == 1 | food_insec == 1 & insecure_food_worr == 1, 1, ifelse(food_insec == 0 & insecure_food_ran_out == 0 & insecure_food_ran_out == 0, 0, NA)))
# 
# #creating binary variable sev_hous_insec
# wrangled <- mutate(wrangled, sev_hous_insec = ifelse(house_insec == 1 & insecure_hous == 1 | house_insec == 1 & insecure_evict == 1, 1, ifelse(house_insec == 0 & insecure_hous == 0 & insecure_evict == 0, 0, NA)))
# 
# # converts categorical variables with values "TRUE" and "FALSE" into a binary numeric variable where "TRUE" is represented as 1, "FALSE" is represented as 0, and other values are represented as missing (NA). 
# wrangled <- mutate(wrangled, rate_neigh_code_good = ifelse(rate_neigh_code_good == "TRUE", 1, ifelse(rate_neigh_code_good == "FALSE", 0, NA)))
# wrangled <- mutate(wrangled, rate_neigh_code_bad = ifelse(rate_neigh_code_bad == "TRUE", 1, ifelse(rate_neigh_code_bad == "FALSE", 0, NA)))
# 
# #check if there is a column named "NA_idk" in the wrangled dataset and return the index (position) of the column if it exists.
# which(colnames(wrangled) == "NA_idk") # none
# 
# #add neighborhoods names 
# wrangled$bronx <- wrangled$zip %>% recode("10453" = "Central Bronx", "10457" = "Central Bronx", "10460" = "Central Bronx", "10458" = "Bronx Park and Fordham", "10467" = "Bronx Park and Fordham", "10468" = "Bronx Park and Fordham", "10451" = "High Bridge and Morrisania", "10452" = "High Bridge and Morrisania", "10456"= "High Bridge and Morrisania", "10454" = "Hunts Point and Mott Haven", "10455" = "Hunts Point and Mott Haven", "10459" = "Hunts Point and Mott Haven", "10474" = "Hunts Point and Mott Haven", "10463" = "Kingsbridge and Riverdale", "10471" = "Kingsbridge and Riverdale", "10466" = "Northeast Bronx", "10469" = "Northeast Bronx", "10470" = "Northeast Bronx", "10475" = "Northeast Bronx", "10461" = "Southeast Bronx", "10462" = "Southeast Bronx", "10464" = "Southeast Bronx", "10465" = "Southeast Bronx", "10472" = "Southeast Bronx", "10473"= "Southeast Bronx")
# 
# wrangled$bk <- wrangled$zip %>% recode("11212" = "Central Brooklyn", "11213" = "Central Brooklyn", "11216" = "Central Brooklyn", "11233" = "Central Brooklyn", "11238" = "Central Brooklyn", "11209" = "Southwest Brooklyn", "11214" = "Southwest Brooklyn", "11228" = "Southwest Brooklyn", "11204" = "Borough Park", "11218" = "Borough Park", "11219" = "Borough Park", "11230" = "Borough Park", "11234" = "Canarsie and Flatlands", "11236" = "Canarsie and Flatlands", "11239" = "Canarsie and Flatlands", "11223" = "Southern Brooklyn", "11224" = "Southern Brooklyn", "11229" = "Southern Brooklyn", "11235" = "Southern Brooklyn", "11201" = "Northwest Brooklyn", "11205" = "Northwest Brooklyn", "11215" = "Northwest Brooklyn", "11217" = "Northwest Brooklyn", "11231" = "Northwest Brooklyn", "11203" = "Flatbush", "11210" = "Flatbush", "11225" = "Flatbush", "11226" = "Flatbush", "11207" = "East New York and New Lots", "11208" = "East New York and New Lots", "11211" = "Greenpoint", "11222" = "Greenpoint", "11220" = "Sunset Park", "11232" = "Sunset Park", "11206" = "Bushwick and Williamsburg", "11221" = "Bushwick and Williamsburg", "11237" = "Bushwick and Williamsburg")
# 
# 
# wrangled$mh <- wrangled$zip %>% recode("10026" = "Central Harlem", "10027" = "Central Harlem", "10030" = "Central Harlem", "10037" = "Central Harlem", "10039" = "Central Harlem", "10001" = "Chelsea and Clinton", "10011" = "Chelsea and Clinton", "10018" = "Chelsea and Clinton", "10019" = "Chelsea and Clinton", "10020" = "Chelsea and Clinton", "10036" = "Chelsea and Clinton", "10029" = "East Harlem", "10035" = "East Harlem", "10010" = "Gramercy Park and Murray Hill", "10016" = "Gramercy Park and Murray Hill", "10017" = "Gramercy Park and Murray Hill", "10022" = "Gramercy Park and Murray Hill", "10012" = "Greenwich Village and Soho", "10013" = "Greenwich Village and Soho", "10014" = "Greenwich Village and Soho", "10004" = "Lower Manhattan", "10005" = "Lower Manhattan", "10006" = "Lower Manhattan", "10007" = "Lower Manhattan", "10038" = "Lower Manhattan", "10280" = "Lower Manhattan", "10002" = "Lower East Side", "10003" = "Lower East Side", "10009" = "Lower East Side", "10021" = "Upper East Side", "10028" = "Upper East Side", "10044" = "Upper East Side", "10065" = "Upper East Side", "10075" = "Upper East Side", "10128" = "Upper East Side", "10023" = "Upper West Side", "10024" = "Upper West Side", "10025" = "Upper West Side", "10031" = "Inwood and Washington Heights", "10032" = "Inwood and Washington Heights", "10033" = "Inwood and Washington Heights", "10034" = "Inwood and Washington Heights", "10040" = "Inwood and Washington Heights")
# 
# wrangled$qu <- wrangled$zip %>% recode("11361" = "Northeast Queens", "11362" = "Northeast Queens", "11363" = "Northeast Queens", "11364" = "Northeast Queens", "11354" = "North Queens", "11355" = "North Queens", "11356" = "North Queens", "11357" = "North Queens", "11358" = "North Queens", "11359" = "North Queens", "11360" = "North Queens", "11365" = "Central Queens", "11366" = "Central Queens", "11367" = "Central Queens", "11412" = "Jamaica", "11423" = "Jamaica", "11432" = "Jamaica", "11433" = "Jamaica", "11434" = "Jamaica", "11435" = "Jamaica", "11436" = "Jamaica", "11101" = "Northwest Queens", "11102" = "Northwest Queens", "11103" = "Northwest Queens", "11104" = "Northwest Queens", "11105" = "Northwest Queens", "11106" = "Northwest Queens", "11374" = "West Central Queens", "11375" = "West Central Queens", "11379" = "West Central Queens", "11385" = "West Central Queens", "11691" = "Rockaways", "11692" = "Rockaways", "11693" = "Rockaways", "11694" = "Rockaways", "11695" = "Rockaways", "11697" = "Rockaways", "11004" = "Southeast Queens", "11005" = "Southeast Queens", "11411" = "Southeast Queens", "11413" = "Southeast Queens", "11422" = "Southeast Queens", "11426" = "Southeast Queens", "11427" = "Southeast Queens", "11428" = "Southeast Queens", "11429" = "Southeast Queens", "11414" = "Southwest Queens", "11415" = "Southwest Queens", "11416" = "Southwest Queens", "11417" = "Southwest Queens", "11418" = "Southwest Queens", "11419" = "Southwest Queens", "11420" = "Southwest Queens", "11421" = "Southwest Queens", "11368" = "West Queens", "11369" = "West Queens", "11370" = "West Queens", "11372" = "West Queens", "11373" = "West Queens", "11377" = "West Queens", "11378" = "West Queens")
# 
# wrangled$si <- wrangled$zip %>% recode("10302" = "Port Richmond", "10303" = "Port Richmond", "10310" = "Port Richmond", "10306" = "South Shore", "10307" = "South Shore", "10308" = "South Shore", "10309" = "South Shore", "10312" = "South Shore", "10301" = "Stapleton and St. George", "10304" = "Stapleton and St. George", "10305" = "Stapleton and St. George", "10314" = "Mid-Island")
# 
# 
# wrangled <- wrangled %>% mutate(neighborhood = coalesce(bronx, bk, mh, qu, si))
############################################################################
############################################################################
###### Linking profils to distall outcomes
############################################################################
############################################################################

# creating DB with all needed variables (covarations and outcomes) for MPLUS

library(tidyverse)
library(here)
# library(config)
library(MplusAutomation)
source('1_funcs.R')
#########################################
# Read in and combine all of the conversation level liwc files
# df_lpa <- get_combined_data()
# df_auxVars <- read.csv('/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/cross_study_analyses/profile_data_3_L1_and_3_L2_combined_trait_role.csv') %>%
#   select(total_word_count,L2,provider_id,study,file,starts_with('cahps_'))
df_rias <- haven::read_dta(here::here('merged_profile_all_Sep8-23.dta'))
# skimr::skim(df_lpa)
# skimr::skim(df_auxVars)
skimr::skim(df_rias)

mergedResults <- read.csv('FIXED_NewProfiles_10-27-2023.csv')
# cross-walk of old and new profiles
# crossWalk <- df_rias %>%
#   select(L1,L2,file,study) %>%
#   full_join(mergedResults, by = c('file','study')) %>%
#   select(1:6)
# 
# crossWalk %>%
#   rename('New_L1' = 'm4_l1',
#          'New_L2' = 'm4_l2',
#          'Old_L1' = 'L1',
#          'Old_L2' = 'L2') %>%
#   janitor::clean_names() %>%
#   select(starts_with('new') | starts_with('old')) %>%
#   drop_na() %>%
#   gtsummary::tbl_cross(row = old_l1,col = new_l1, percent = 'column')


df_finalAux <- df_rias %>%
  left_join(mergedResults, by = c('file','study','provider_id')) %>%
  select(
    study,provider_id,
    # L2, L1,
    m4_l2, m4_l1,
    l2_1_prob_4,l2_2_prob_4,l2_3_prob_4,l2_4_prob_4,
    l1_1_prob_4,l1_2_prob_4,l1_3_prob_4,l1_4_prob_4,
    drives, cognition, affect, social, wc, big_words, tot_wc, starts_with('cahps_'),
    verbdom, ptcent1, posd, emod, negd, chitd, rapportx, infomedd, infopsyd, bcd, turndur, turndens,
    # global affect measures
    resptd, symd, engagd, warmd, intd,
    patient_sex, provider_sex,
    patient_race,provider_race
    ) %>%
  mutate(
    across(starts_with('cahps_'),~ ifelse(.x == 4, 1, 0)),
    across(c(drives, cognition, affect, social, wc, big_words, tot_wc), ~ c(scale(.,center = TRUE, scale = TRUE)))
  ) %>%
  rename(
    # tot_wc = total_word_count,
    listen = cahps_listen_carefully_all,
    und_stnd = cahps_understand_all,
    respect = cahps_respect_all,
    spnd_tm = cahps_spend_time_all
  )

# make composite and study standardized mean affect score
df_finalAux <- df_finalAux %>%
  rowwise() %>%
  mutate(
    tot_aff = sum(resptd, symd, engagd, warmd, intd) / 5
  ) %>%
  ungroup() %>%
  group_by(study) %>%
  mutate(
    tot_aff_sd = as.numeric(scale(tot_aff, center = TRUE, scale = TRUE))
    ) %>%
  ungroup()

haven::write_sav(df_finalAux, 'NEWprofile_auxVarWithRias_LG_withTotAf_11-02-2023.sav')

#write.csv(df_finalAux, 'NewProfiles_10-26-2023.csv')

mergedResults %>% select()

df_finalAux %>% select(l2_1_prob_4,l2_2_prob_4,l2_3_prob_4,l2_4_prob_4,ptcent1) %>%
  pivot_longer(cols = starts_with("l2"),names_to = 'Profile',values_to = 'Probability') %>%
  filter(ptcent1 < 40) %>%
  ggplot(aes(ptcent1,Probability, color = Profile)) + geom_point() + ggthemes::theme_tufte()+ facet_wrap(~Profile)

df_finalAux %>% select(l2_1_prob_4,l2_2_prob_4,l2_3_prob_4,l2_4_prob_4,verbdom) %>%
  pivot_longer(cols = starts_with("l2"),names_to = 'Profile',values_to = 'Probability') %>%
  # filter(ptcent1 < 40) %>%
  ggplot(aes(verbdom,Probability, color = Profile)) + geom_point() + ggthemes::theme_tufte()+ facet_wrap(~Profile)

df_finalAux %>% select(l2_1_prob_4,l2_2_prob_4,l2_3_prob_4,l2_4_prob_4,turndur) %>%
  pivot_longer(cols = starts_with("l2"),names_to = 'Profile',values_to = 'Probability') %>%
  # filter(ptcent1 < 40) %>%
  ggplot(aes(turndur,Probability, color = Profile)) + geom_point() + ggthemes::theme_tufte() + facet_wrap(~Profile)

df_finalAux %>% select(l2_1_prob_4,l2_2_prob_4,l2_3_prob_4,l2_4_prob_4,turndens) %>%
  pivot_longer(cols = starts_with("l2"),names_to = 'Profile',values_to = 'Probability') %>%
  # filter(ptcent1 < 40) %>%
  ggplot(aes(turndens,Probability, color = Profile)) + geom_point() + ggthemes::theme_tufte()+ facet_wrap(~Profile)

df_finalAux %>% select(l2_1_prob_4,l2_2_prob_4,l2_3_prob_4,l2_4_prob_4,tot_aff_sd) %>%
  pivot_longer(cols = starts_with("l2"),names_to = 'Profile',values_to = 'Probability') %>%
  # filter(ptcent1 < 40) %>%
  ggplot(aes(tot_aff_sd,Probability, color = Profile)) + geom_point() + ggthemes::theme_tufte()+ facet_wrap(~Profile)

df_finalAux %>% select(l1_1_prob_4,l1_2_prob_4,l1_3_prob_4,l1_4_prob_4,tot_aff_sd) %>%
  pivot_longer(cols = starts_with("l1"),names_to = 'Profile',values_to = 'Probability') %>%
  # filter(ptcent1 < 40) %>%
  ggplot(aes(tot_aff_sd,Probability, color = Profile)) + geom_point() + ggthemes::theme_tufte()+ facet_wrap(~Profile)

k <- 1
outcome <- 'respect'
df_finalAux %>% select(!!sym(glue::glue('l2_{k}_prob_4')),respect) %>%
  drop_na() %>%
  mutate(
    cuts = cut(!!sym(glue::glue('l2_{k}_prob_4')), breaks = 10, labels = FALSE)
  ) %>% #select(cuts,l2_4_prob_4) %>% table()
  group_by(cuts) %>%
  summarize(
    high_respect = sum(respect == 1, na.rm = TRUE),
    low_respect = sum(respect == 0, na.rm = TRUE)
  ) %>%
  pivot_longer(names_to = 'respect',values_to = 'count',cols = ends_with('respect')) %>%
  ggplot(aes(x = cuts, y = count, fill = respect)) + 
  geom_bar(position="stack", stat="identity")
  
  
  pivot_longer(cols = starts_with("l1"),names_to = 'Profile',values_to = 'Probability') %>%
  # filter(ptcent1 < 40) %>%
  ggplot(aes(tot_aff_sd,Probability, color = Profile)) + geom_point() + ggthemes::theme_tufte()+ facet_wrap(~Profile)
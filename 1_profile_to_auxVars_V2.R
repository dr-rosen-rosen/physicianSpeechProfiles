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

df_age <- read.csv(here::here('age_df.csv')) %>%
  select(file,study,patient_age)

df_finalAux <- df_rias %>%
  left_join(df_age,by = c('file','study')) %>%
  left_join(mergedResults, by = c('file','study','provider_id')) %>%
  select(
    study,provider_id,file,
    # L2, L1,
    m4_l2, m4_l1,
    l2_1_prob_4,l2_2_prob_4,l2_3_prob_4,l2_4_prob_4,
    l1_1_prob_4,l1_2_prob_4,l1_3_prob_4,l1_4_prob_4,
    drives, cognition, affect, social, wc, big_words, tot_wc, starts_with('cahps_'),
    verbdom, ptcent1, posd, emod, negd, chitd, rapportx, infomedd, infopsyd, bcd, turndur, turndens,
    # global affect measures
    resptd, symd, engagd, warmd, intd,
    patient_sex, provider_sex,
    patient_race,provider_race,
    patient_age
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
  ) %>%
  mutate(
    patient_race = case_match(patient_race,
                              # c('.tive Hawaiian or Other Pacific Islander',
                              #   'American Indian/Alaska .tive','Asian','Other') ~ 'other',
                              # c('','.','Unknown') ~ 'unknown',
                              # .default = patient_race
                              'Black/African American' ~ 'black',
                              'White/Caucasian' ~ 'white',
                              .default = 'other/unknown'
                              ),
    provider_race = case_match(provider_race,
                               # c('','.') ~ 'unknown',
                               # .default = provider_race
                               'Black/African American' ~ 'black',
                               'White/Caucasian' ~ 'white',
                               'Asian' ~ 'asian',
                               .default = 'other/unknown'
                               )
  ) %>%
  mutate(
    patient_sex = case_match(patient_sex,
                             # c('','.') ~ 'unknown',
                             # c('4','Transgender male') ~ 'other',
                             # .default = patient_sex
                             'Female' ~ 'female',
                             'Male' ~ 'male',
                             .default = 'other/unknown'
                             ),
    provider_sex = case_match(provider_sex,
                              # c('','.') ~ 'unknown',
                              # .default = provider_sex
                              'Female' ~ 'female',
                              'Male' ~ 'male',
                              .default = 'other/unknown'
                              )
  )
skimr::skim(df_finalAux)
# df_finalAux %>% select(study,patient_sex,provider_sex) %>%
df_finalAux %>% select(study,patient_race,patient_sex,patient_age) %>%
  gtsummary::tbl_summary(by = study) %>%
  gtsummary::add_overall() |> 
  gtsummary::as_gt() |> 
  gt::gtsave(filename = "patient_demo.docx")

tf <- tempfile("provider_demo", fileext = ".docx")
df_finalAux %>% select(provider_id,study,provider_race,provider_sex) %>%
  distinct() |>
  select(-provider_id) |>
  gtsummary::tbl_summary(by = study) %>%
  gtsummary::add_overall() |> 
  gtsummary::as_gt() |> 
  gt::gtsave(filename = "provider_demo.docx")

df_finalAux |> filter(patient_race != 'other/unkown', provider_race != 'other/unknown',
                      patient_sex != 'other/unknown', provider_sex != 'other/unkown')
# table(df_finalAux$patient_race)
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

# reode L2 lables to match their dominant L1 profile
df_finalAux <- df_finalAux %>%
  mutate(
    l2_1_prob_4_rc = l2_3_prob_4,
    l2_2_prob_4_rc = l2_4_prob_4,
    l2_3_prob_4_rc = l2_1_prob_4,
    l2_4_prob_4_rc = l2_2_prob_4
  ) %>%
  mutate(
    m4_l2_rc = case_match(m4_l2,
                          1 ~ 3,
                          2 ~ 4,
                          3 ~ 1,
                          4 ~ 2,
                          .default = NA)
  )

haven::write_sav(
  # df_finalAux, 
  # 'NEWprofile_auxVarWithRias_LG_withTotAf_01-31-2024.sav'
  df_finalAux  |> filter(patient_race != 'other/unknown', provider_race != 'other/unknown',
                         patient_sex != 'other/unknown', provider_sex != 'other/unknown'), 
  'NEWprofile_auxVarWithRias_LG_withNoOTHER_03-27-2024.sav'
  )

test <- df_finalAux  |> filter(patient_race != 'other/unknown', provider_race != 'other/unknown',
                               patient_sex != 'other/unknown', provider_sex != 'other/unknown')

table(test$patient_race)
#write.csv(df_finalAux, 'NewProfiles_10-26-2023.csv')

t <- df_finalAux %>% select(patient_sex, provider_sex,
                       patient_race,provider_race) 

df_finalAux %>% select(m4_l1,m4_l2_rc,verbdom,study) %>%
  # mutate(respect = as.factor(respect)) %>%
  gtsummary::tbl_summary(by = study) %>%
  gtsummary::add_overall() %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(path = 'Profiles_and_respect_by_study.docx')


cont_rias_vars <- c('verbdom', 'ptcent1','turndur','turndens')
m.data <- df_finalAux |>
  select(study,tot_wc,
         ends_with("_rc"),
         starts_with("l1_"),
         m4_l2_rc,m4_l1,
         respect,listen,und_stnd,spnd_tm, # caps
         verbdom, ptcent1,turndur,turndens, # continuous rias variables
         # posd, emod, negd, chitd, rapportx, infomedd, infopsyd,bcd,
         patient_sex,provider_sex,
         patient_race,provider_race,
         patient_age,provider_id
         ) |>
  mutate(across(ends_with("_sex") | ends_with("_race"), ~ as.factor(.x))) |>
  mutate(study = as.factor(study), m4_l2_rc = as.factor(m4_l2_rc), m4_l1 = as.factor(m4_l1)) |>
  # filter(study == 'ECHO3') %>%
  drop_na() |>
  mutate(tot_wc = datawizard::standardize(tot_wc,
                                          robust = FALSE,
                                          two_sd = FALSE,
                                          weights = NULL,
                                          reference = NULL,
                                          center = NULL,
                                          scale = NULL,
                                          verbose = TRUE)) #|>
  # mutate()
  # mutate(across(.cols = starts_with('l2_'), ~ datawizard::standardize(.x,
  #                                                                        robust = FALSE,
  #                                                                        two_sd = FALSE,
  #                                                                        weights = NULL,
  #                                                                        reference = NULL,
  #                                                                        center = NULL,
  #                                                                        scale = NULL,
  #                                                                        verbose = TRUE))) |>
  # mutate(across(.cols = all_of(cont_rias_vars), ~ datawizard::standardize(.x,
  #                                                                     robust = FALSE,
  #                                                                     two_sd = FALSE,
  #                                                                     weights = NULL,
  #                                                                     reference = NULL,
  #                                                                     center = NULL,
  #                                                                     scale = NULL,
  #                                                                     verbose = TRUE)))

skimr::skim(m.data)

m.0 <- lme4::glmer(respect ~ # respect,listen,und_stnd,spnd_tm
                     # l2_1_prob_4_rc + l2_2_prob_4_rc + l2_3_prob_4_rc + l2_4_prob_4_rc +
                     # m4_l2_rc +
                     m4_l1 +
                     tot_wc + 
                     # patient_sex +
                     provider_sex +
                     patient_race + provider_race +
                     provider_race +
                     patient_age +
                     study +
                     # (1|study/provider_id),
                     (1|provider_id),
              # family=binomial(link="logit"), 
              family = 'binomial',
              data = m.data, 
              control=lme4::glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
lme4::allFit(m.0)
sjPlot::tab_model(m.0)


m.1 <- lme4::lmer(turndur ~ # verbdom, ptcent1,turndur,turndens
                     # l2_1_prob_4_rc + l2_2_prob_4_rc + l2_3_prob_4_rc + l2_4_prob_4_rc +
                     # m4_l2_rc +
                     m4_l1 +
                     tot_wc + 
                     # patient_sex + 
                     provider_sex +
                     patient_race + provider_race +
                     provider_race +
                     patient_age +
                     (1|study/provider_id),
                   # (1|provider_id),
                   # family=binomial(link="logit"), 
                   # family = 'binomial',
                   data = m.data
                   )
lme4::allFit(m.1)
sjPlot::tab_model(m.1)

m.0 <- lme4::glmer(spnd_tm ~ # respect,listen,und_stnd,spnd_tm
                     # l2_1_prob_4_rc + l2_2_prob_4_rc + l2_3_prob_4_rc + l2_4_prob_4_rc +
                     # m4_l2_rc +
                     m4_l1 +
                     tot_wc + 
                     # patient_sex + 
                     provider_sex +
                     patient_race + provider_race +
                     provider_race +
                     patient_age +
                     (1|study/provider_id),
                   # (1|provider_id),
                   # family=binomial(link="logit"), 
                   family = 'binomial',
                   data = m.data, 
                   control=lme4::glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e5)))
lme4::allFit(m.0)
sjPlot::tab_model(m.0)


table(t$patient_sex)
table(df_finalAux$patient_race)

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
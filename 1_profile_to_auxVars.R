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
df_lpa <- get_combined_data()
df_auxVars <- read.csv('/Users/mrosen44/Johns\ Hopkins/Salar\ Khaleghzadegan\ -\ Patient\ _Provider_Communication_Projects/cross_study_analyses/profile_data_3_L1_and_3_L2_combined_trait_role.csv') %>%
  select(total_word_count,L2,provider_id,study,file,starts_with('cahps_'))
df_rias <- haven::read_dta(here('merged_profile_all_Sep8-23.dta'))
# skimr::skim(df_lpa)
# skimr::skim(df_auxVars)
skimr::skim(df_rias)

df_rias <- df_rias %>%
  left_join(df_auxVars[c('total_word_count','file','study','L2')], by = c('file','study')) %>%
  select(study,provider_id,L2,row_num,drives, cognition, affect, social, wc, big_words, total_word_count, starts_with('cahps_'),
         verbdom, ptcent1, posd, emod, negd, chitd, rapportx, infomedd, infopsyd, bcd, turndur, turndens,
         # global affect measures
         resptd, symd, engagd, warmd, intd) %>%
  mutate(
    across(starts_with('cahps_'),~ ifelse(.x == 4, 1, 0)),
    across(c(drives, cognition, affect, social, wc, big_words, total_word_count), ~ c(scale(.,center = TRUE, scale = TRUE)))
  ) %>%
  rename(
    tot_wc = total_word_count,
    listen = cahps_listen_carefully_all,
    und_stnd = cahps_understand_all,
    respect = cahps_respect_all,
    spnd_tm = cahps_spend_time_all
  )

# make composite and study standardized mean affect score
df_rias <- df_rias %>%
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

haven::write_sav(df_rias, 'profile_auxVarWithRias_LG_withTotAf.sav')

mPlus_df <- right_join(df_lpa, df_auxVars, by = c('file','study')) %>%
  select(PCP_ID,row_num,drives, cognition, affect, social, wc, big_words, total_word_count,starts_with('cahps_')) %>%
  mutate(
    across(starts_with('cahps_'),~ ifelse(.x == 4, 1, 0))#,
    # across(c(drives, cognition, affect, social, wc, big_words), ~ c(scale(.,center = TRUE, scale = TRUE)))
    ) %>%
  rename(
    tot_wc = total_word_count,
    listen = cahps_listen_carefully_all,
    und_stnd = cahps_understand_all,
    respect = cahps_respect_all,
    spnd_tm = cahps_spend_time_all
    
  )
# head(mPlus_df)
skimr::skim(mPlus_df)

haven::write_sav(mPlus_df, 'profile_auxVar_LG_raw.sav')

test <- haven::read_sav('data1.sav')
test <- test %>%
  rename(int_class = 'clu#') %>%
  pivot_longer(
    cols = drives:big_words,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(int_class,variable) %>%
  summarize(m_value = mean(value, na.rm = TRUE)) %>%
  mutate(int_class = as.factor(int_class)) %>%
  mutate(int_class = as.factor(int_class),
         variable = ordered(variable, levels = c("drives", "cognition", "affect", "analytic", "clout", "authentic", "wc", "big_words"))) %>%
  ggplot(aes(x = variable, y = m_value, color = int_class, group = int_class)) + 
  geom_point() + geom_line() + ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set2")
  

mlLPA_results %>% 
  select(-starts_with("V")) %>%
  pivot_longer(
    cols = drives:big_words,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(int_class,variable) %>%
  summarize(m_value = mean(value)) %>%
  mutate(int_class = as.factor(int_class),
         variable = ordered(variable, levels = c("drives", "cognition", "affect", "analytic", "clout", "authentic", "wc", "big_words"))) %>%
  ggplot(aes(x = variable, y = m_value, color = int_class, group = int_class)) + 
  geom_point() + geom_line() + ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set2")

mPlus_df %>%
  prepareMplusData(
    filename = 'mplus_analyses/profile_to_auxVars/profile_to_auxVars.dat'
  )
names(mPlus_df)

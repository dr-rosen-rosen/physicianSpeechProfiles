############################################################################
############################################################################
###### Code for Generating Physician Speech Profiles
############################################################################
############################################################################
library(tidyverse)
library(here)
library(config)
library(MplusAutomation)
#########################################
# Read in and combine all of the conversation level liwc files
df_lpa_echo1 <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/ECHO1_Study/ECHO_LSM_MLM_V3.csv') %>%
  mutate(
    provider_id = str_sub(File, 1, 4),
    study = 'ECHO1') %>%
  filter(Speaker == 'D') %>%
  janitor::clean_names() %>%
  select(-c(x,segment,speaker,text))
table(df_lpa_echo1$provider_id)

df_lpa_echo3 <- read.csv(here('/Users/mrosen44/Documents/Data_Analysis_Local/Patient_MD_Com/LIWC-22 Results - final_echo3_conversational - LIWC Analysis.csv')) %>%
  mutate(
    provider_id = str_sub(file_id, -2),
    speaker = case_match(speaker, 'doctor' ~ 'D',.default = speaker),
    study = 'ECHO3') %>%
  filter(speaker == 'D') %>%
  janitor::clean_names()%>%
  rename('file' = 'file_id') %>%
  select(-c(x,speaker,text)) %>%
  select(-contains('x_')) # not sure where all the extra columns are coming from, but they are empty


df_lpa_bb <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/blackbox_study/blackbox_conv_liwc.csv') %>%
  mutate(
    provider_id = str_sub(File, 1, 3), # need to update with correct parsing of file_id
    Speaker = case_match(Speaker, 'pcp' ~ 'D',.default = Speaker),
    study = 'BB') %>%
  filter(Speaker == 'D') %>%
  janitor::clean_names() %>%
  select(-c(x,segment,speaker,text))

table(df_lpa_bb$provider_id)
df_lpa <- rbind(df_lpa_bb,df_lpa_echo1,df_lpa_echo3)
df_lpa[,2:120] <- sapply(df_lpa[,2:120],as.numeric)
skimr::skim(df_lpa)
names(df_lpa_echo1)
names(df_lpa_bb)
##################################################################################
# Following six steps from Mäkikangas et al., 2018

# select variables
# preliminary analysis
test_structure_drives <- lme4::lmer(drives ~ 1 + (1|provider_id), data = df_lpa)
test_structure_cognition <- lme4::lmer(cognition ~ 1 + (1|provider_id), data = df_lpa)
test_structure_affect <- lme4::lmer(affect ~ 1 + (1|provider_id), data = df_lpa)
test_structure_social <- lme4::lmer(social ~ 1 + (1|provider_id), data = df_lpa)
sjPlot::tab_model(test_structure_drives,test_structure_cognition,test_structure_affect,test_structure_social)


##################################################################################
# Run tidyLPA?
# https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html
library(tidyLPA)
lpa_fit <- df_lpa %>%
  #select(Analytic, Clout, Authentic) %>%
  # select(Drives, Cognition, Affect, Social) %>%
  select(drives, cognition, affect, social, wc, wps, big_words) %>%
  # select(Cognition, Affect, Social, Physical, Perception) %>%
  # select(WC,Analytic,Clout,Authentic,Tone,WPS,BigWords) %>%
  single_imputation() %>%
  scale() %>%
  # estimate_profiles(2:15,
  #                   variances = c("equal", "varying"),
  #                   covariances = c("zero", "varying")) %>%
  # compare_solutions(statistics = c("AIC", "BIC")) #%>%
  estimate_profiles(3,
                    variances = 'varying',
                    covariances = 'varying')
get_estimates(lpa_fit)
lpa_fit %>%
  plot_profiles() +
  ggthemes::theme_tufte()
##################################################################################
# Prep data for Mplus analysis
library(MplusAutomation)
# https://cran.r-project.org/web/packages/MplusAutomation/vignettes/vignette.html
df_lpa %>%
  select(Cognition, Affect, Social, Physical, Perception, provider_id) %>%
  prepareMplusData(
    filename = 'ECHO_mPlus.dat'
  )
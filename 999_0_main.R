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
    provider_id = paste0('ECHO1_',(str_sub(File, 1, 4))),
    study = 'ECHO1') %>%
  filter(Speaker == 'D') %>%
  janitor::clean_names() %>%
  select(-c(x,segment,speaker,text))
table(df_lpa_echo1$provider_id)

df_lpa_echo3 <- read.csv(here('/Users/mrosen44/Documents/Data_Analysis_Local/Patient_MD_Com/LIWC-22 Results - final_echo3_conversational - LIWC Analysis.csv')) %>%
  mutate(
    provider_id = paste0('ECHO3_',(str_sub(file_id, -2))),
    speaker = case_match(speaker, 'doctor' ~ 'D',.default = speaker),
    study = 'ECHO3') %>%
  filter(speaker == 'D') %>%
  janitor::clean_names()%>%
  rename('file' = 'file_id') %>%
  select(-c(x,speaker,text, column_id,segment)) %>%
  select(-contains('x_')) # not sure where all the extra columns are coming from, but they are empty
table(df_lpa_echo3$provider_id)

df_lpa_bb <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/blackbox_study/blackbox_conv_liwc.csv') %>%
  mutate(
    provider_id = paste0('BB_',str_sub(File, 1, 3)), # need to update with correct parsing of file_id
    Speaker = case_match(Speaker, 'pcp' ~ 'D',.default = Speaker),
    study = 'BB') %>%
  filter(Speaker == 'D') %>%
  janitor::clean_names() %>%
  select(-c(x,segment,speaker,text))

table(df_lpa_bb$provider_id)

df_lpa_mar <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/maripohsa_study/maripohsa_conv_liwc.csv') %>%
  mutate(
    provider_id = paste0('MAR_',str_sub(File, 1, 2)),
    Speaker = case_match(Speaker, 'doctor' ~ 'D',.default = Speaker),
    study = 'MAR'
  ) %>%
  filter(Speaker == 'D') %>%
  janitor::clean_names() %>%
  select(-c(x,segment,speaker,text))

table(df_lpa_mar$provider_id)

df_lpa <- rbind(df_lpa_bb,df_lpa_echo1,df_lpa_echo3,df_lpa_mar) %>%
  relocate(study,provider_id)
df_lpa$provider_id <- factor(df_lpa$provider_id, ordered = FALSE)
df_lpa[,4:120] <- sapply(df_lpa[,4:120],as.numeric)
skimr::skim(df_lpa)

# write.csv(df_lpa,'combined_MD_liwc_for_FA.csv')
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
  estimate_profiles(2,
                    variances = 'varying',
                    covariances = 'varying')
get_estimates(lpa_fit)
lpa_fit %>%
  plot_profiles() +
  ggthemes::theme_tufte()
##################################################################################
# Preliminary data anlaysis

# interactions per physician?
df_lpa %>%
  group_by(provider_id) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(count) %>%
  select(count) %>%
  ggplot(aes(x=count)) + geom_histogram(bins =100) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  labs(title = '# of interactions by PCPs', y = '# of PCPs', x = '# of interactions') +
  geom_vline(xintercept=4, color = "red")
  

df_lpa_md <- df_lpa %>%
  group_by(provider_id) %>% filter(n() > 4) %>% ungroup()
print(length(unique(df_lpa_md$provider_id)))
# outliers?
df_lpa_md <- df_lpa_md %>%
  select(drives, cognition, affect, social, wc, wps, big_words) %>%
  careless::mahad() %>% #+ geom_hline(yintercept = 40, color = "red")
  cbind(df_lpa_md) %>%
  rename('mahad' = '.') %>%
  filter(mahad < 40)

# df_lpa_md %>%
#   group_by(provider_id) %>%
#   summarize(count = n()) %>%
#   ungroup() %>%
#   arrange(count) 

# inter-correlations; vars are supposed to be uncorrelated
df_lpa_md %>%
  select(drives, cognition, affect, social, wc, wps,big_words) %>%
  corrtable::correlation_matrix()
  # corrplot::corrplot()

# ICC's 
for (var in c('drives', 'cognition', 'affect', 'social', 'wc', 'wps','big_words')) {
  f <- as.formula(paste(var,'~ provider_id'))
  ICC1 <- multilevel::ICC1(aov(f,data=df_lpa_md))
  print(paste0(var,': ',ICC1))
}
##################################################################################
# Prep data for Mplus analysis
library(MplusAutomation)
# https://cran.r-project.org/web/packages/MplusAutomation/vignettes/vignette.html

ml_lpa_df <- df_lpa_md %>%
  select(drives, cognition, affect, social, wc, big_words, wps, provider_id)

df_prep <- df_lpa_md %>%
  select(drives, cognition, affect, social, wc, big_words, wps, provider_id) %>%
  # mutate_if(is.numeric,scale) %>% % this sets attributes that prep_mplus has a problem with.
  prepareMplusData(
    filename = 'mplus_analyses/MD_profile_cmb_mPlus.dat'
  )

#### Automating MPLUS models

class_str <- character()

ml_lpa1_6 <- lapply(1:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[drives cognition affect social wc wps big_words];
drives cognition affect social wc wps big_words;")
    } else {
      class_str <<- paste(class_str,
                         glue::glue("%C#{x}%
[drives cognition affect social wc wps big_words];
drives cognition affect social wc wps big_words;"
                                    ), sep="\n")
    }
    print(class_str)
  }
  

  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("P{k}_lpa__freeVarenum;"),
    VARIABLE = glue::glue("CLASSES = c({k});"),
    DEFINE = "STANDARDIZE drives cognition affect social wc wps big_words;",
    ANALYSIS = "TYPE = MIXTURE;
    ESTIMATOR=MLR;
    STARTS=1000 50;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = glue::glue('%OVERALL%\n',class_str),
    OUTPUT = "",
    PLOT = "type = plot3;",
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )

  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                            dataout = glue::glue("mplus_analyses/prof_{k}_ml_lpa_freeVar_enum.dat"),
                                            modelout = glue::glue("mplus_analyses/prof_{k}_ml_lpa_freeVar_enum.inp"),
                                            check = TRUE, run = TRUE, hashfilename = FALSE)
}
  )

output_enum <- readModels(here("mplus_analyses"), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

 enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title, pattern = 'lpa__freeVarenum')) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(Title)
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks= scales::pretty_breaks()) + labs(title='Means and variances only')

# extract posterior probabilities
pp1 <- as.data.frame(output_enum[["prof_2_ml_lpa_enum.out"]]
                     [["gh5"]]
                     [["means_and_variances_data"]]
                     [["estimated_probs"]]
                     [["values"]]
                     [seq(2, 14, 2),]) #seq("from","to","by")

################################
######## Exmploring first ml-lpa
################################

mlLPA_results <- read.table('mplus_analyses/mlLPA2.dat') %>%
  rename(
    drives = V1,
    cognition = V2,
    affect = V3,
    social = V4,
    wc = V5,
    wps = V6,
    big_words = V7,
    # V8 = prov_class_1_prob,
    # V9 = prov_class_2_prob,
    # V10 = prov_class_3_prob,
    # V11 = ,
    # V12 = ,
    # V13 = ,
    # prov_class = V14,
    # int_class = V15,
    # cmb_class = V16,
    # mplus_prov_id = V17
    prov_class = V24,
    int_class = V25,
    cmb_class = V26,
    mplus_prov_id = V27
  )

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
         variable = ordered(variable, levels = c("affect", "drives","social","cognition","big_words","wc","wps"))) %>%
  ggplot(aes(x = variable, y = m_value, color = int_class, group = int_class)) + 
  geom_point() + geom_line() + ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set2")

table(mlLPA_results$int_class)

l2_summary <- mlLPA_results %>% 
  select(last_col(offset = 3):last_col(),-cmb_class) %>%
  group_by(mplus_prov_id,prov_class) %>%
  summarize(
    prop_prof_1 = sum(int_class == 1) / n(),
    prop_prof_2 = sum(int_class == 2) / n(),
    prop_prof_3 = sum(int_class == 3) / n(),
    prop_prof_4 = sum(int_class == 4) / n()
  ) %>%
  ungroup() %>%
  group_by(prov_class) %>%
  summarize(
    mean_prop_prof_1 = mean(prop_prof_1),
    mean_prop_prof_2 = mean(prop_prof_2),
    mean_prop_prof_3 = mean(prop_prof_3),
    mean_prop_prof_4 = mean(prop_prof_4),) %>%
  ungroup()

l2_summary <- mlLPA_results %>%
  select(mplus_prov_id,prov_class) %>%
  distinct() %>%
  group_by(prov_class) %>%
  summarize(count = n()) %>%
  full_join(l2_summary, by = 'prov_class')

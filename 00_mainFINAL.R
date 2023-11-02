############################################################################
############################################################################
###### Code for Generating Physician Speech Profiles
###### ACROSS ALL DATASETS W/O transcript legnth/turn restrictions
############################################################################
############################################################################
library(tidyverse)

# Read in new combined data set



df_lpa <- read.csv('/Users/mrosen44/Documents/Data_Analysis_Local/Patient_MD_Com/LIWC-22 Results - FINAL_cmbd_conv - LIWC Analysis.csv')

df_lpa <- df_lpa %>%
  filter(speaker == 'clinician') %>%
  separate_wider_delim(file_study_id,delim = '-',names = c("file","study")) %>%
  mutate(
    provider_id = case_match(study,
                      'BB' ~  paste0('BB_',str_sub(file, 1, 3)),
                      'ECHO1' ~ paste0('ECHO1_',(str_sub(file, 1, 4))),
                      'ECHO3' ~ paste0('ECHO3_',(str_sub(file, -2))),
                      'MAR' ~ paste0('MAR_',str_sub(file, 1, 2)),
                      .default = NA
                      ),
    row_num = row_number()
  ) %>% janitor::clean_names()

names(df_lpa)
table(df_lpa$study)

##################################################################################
# Following six steps from Mäkikangas et al., 2018


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
  dplyr::select(drives, cognition, affect, social, big_words, wps) %>%
  # dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc) %>%
  careless::mahad() %>% #+ geom_hline(yintercept = 40, color = "red")
  cbind(df_lpa_md) %>%
  rename('mahad' = '.') %>%
  filter(mahad < 40)

# inter-correlations; vars are supposed to be uncorrelated
df_lpa_md %>%
  dplyr::select(drives, cognition, affect, social, big_words, wc) %>%
  modelsummary::datasummary_correlation()

# ICC's 
for (var in c('drives', 'cognition', 'affect', 'social', 'wps', 'big_words')) {
  f <- as.formula(paste(var,'~ provider_id'))
  ICC1 <- multilevel::ICC1(aov(f,data=df_lpa_md))
  print(paste0(var,': ',ICC1))
}

##################################################################################
# Prep data for Mplus analysis
library(MplusAutomation)
# https://cran.r-project.org/web/packages/MplusAutomation/vignettes/vignette.html

out_f_dir <- 'FINAL_Runs'

ml_lpa_df <- df_lpa_md %>%
  rename('PCP_ID' = 'provider_id') %>%
  dplyr::select(drives, cognition, affect, social, big_words, wc)
# dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc, PCP_ID, row_num)

df_prep <- df_lpa_md %>%
  rename('PCP_ID' = 'provider_id') %>%
  dplyr::select(drives, cognition, affect, social, big_words, wc) %>%
  # dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc, PCP_ID, row_num) %>%
  prepareMplusData(
    filename = glue::glue('mplus_analyses/{out_f_dir}/MD_profile_cmb_mPlus.dat')
  )

#### Automating MPLUS models

class_str <- character()

ml_lpa1_10 <- lapply(7:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[drives cognition affect social big_words wc];
drives cognition affect social big_words wc;")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[drives cognition affect social big_words wc];
drives cognition affect social big_words wc;"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L1_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("CLASSES = c({k});"),
    # USEVARIABLES = "drives cognition affect social big_words wc;",
    DEFINE = "STANDARDIZE drives cognition affect social big_words wc;",
    ANALYSIS = "TYPE = MIXTURE;
    ESTIMATOR=MLR;
    STARTS=1000 50;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = glue::glue('%OVERALL%\n',class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    # PLOT = "type = plot3;",
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/{out_f_dir}/L1_prof_{k}_ml_lpa_freeVar_enum.dat"),
                                              modelout = glue::glue("mplus_analyses/{out_f_dir}/L1_prof_{k}_ml_lpa_freeVar_enum.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)

output_enum <- readModels(here::here(glue::glue("mplus_analyses/{out_f_dir}")), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

l1_k_profiles_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title, pattern = 'L1_')) %>%
  # filter(str_detect(Title, pattern = 'L2', negate = TRUE)) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L1_P_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title='Model fit by number of interaction (L1) profiles',
    x = 'Number of interaction (L1) profiles',
    y = 'Model fit statistic value',
    color = 'Fit statistic')

################################
######## Check for fit for different number of L2 vars
################################

out_f_dir <- 'FINAL_Runs'

ml_lpa_df <- df_lpa_md %>%
  rename('PCP_ID' = 'provider_id') %>%
  dplyr::select(drives, cognition, affect, social, big_words, wc,row_num,PCP_ID)
# dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc, PCP_ID, row_num)

df_prep <- df_lpa_md %>%
  rename('PCP_ID' = 'provider_id') %>%
  dplyr::select(drives, cognition, affect, social, big_words, wc,row_num,PCP_ID) %>%
  # dplyr::select(drives, cognition, affect, social, big_words, pcp_to_pt_wc, PCP_ID, row_num) %>%
  MplusAutomation::prepareMplusData(
    filename = glue::glue('mplus_analyses/{out_f_dir}/MD_profile_cmb_mPlus.dat')
  )
class_str <- character()

k <- 4 # L1 profiles
ml_lpa2 <- lapply(1:10, function(j) # L2 profiles
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[drives cognition affect social big_words wc];
drives cognition affect social big_words wc;")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[drives cognition affect social big_words wc];
drives cognition affect social big_words wc;"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L2_{j}_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = BC({j}) c({k});\nCLUSTER IS PCP_ID;\nWITHIN ARE drives cognition affect social big_words wc;\nBETWEEN ARE BC;"),
    DEFINE = "STANDARDIZE drives cognition affect social big_words wc;",
    ANALYSIS = "TYPE = MIXTURE TWOLEVEL;
    ESTIMATOR=MLR;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = glue::glue('%WITHIN%\n%OVERALL%\n%BETWEEN%\n%OVERALL%\nc ON BC;\nMODEL c:\n%WITHIN%\n',class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    # PLOT = "type = plot3;",
    SAVEDATA = glue::glue("file=mlLpa_L2_{j}_L1_{k}.dat;\nsave=cprob;\nTECH4 IS tech4.dat;"),
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/{out_f_dir}/L2_{j}_P_{k}_ml_lpa_freeVar.dat"),
                                              modelout = glue::glue("mplus_analyses/{out_f_dir}/L2_{j}_P_{k}_ml_lpa_freeVar.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)


output_enum <- readModels(here::here(glue::glue("mplus_analyses/{out_f_dir}")), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

k <- 4
l2_k_range_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title,pattern = 'L1_', negate = TRUE)) %>%
  filter(str_detect(Title, pattern = glue::glue('_P_{k}_'))) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L2_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title='Model fit by number of clinician (L2) classes \nfor a 4 L1 profile model',
    x = 'Number of clinician (L2) classes',
    y = 'Model fit statistic value',
    color = 'Fit statistic')

library(patchwork)

l1_k_profiles_plot + l2_k_range_plot + plot_layout(guides = "collect")  & theme(legend.position = 'bottom') & plot_annotation(tag_levels = 'A')
# test <- MplusAutomation::getSavedata_Fileinfo(glue::glue('mplus_analyses/psych_process/L2_{L2}_P_{L1}_ml_lpa_freeVar.out')) 
mlLPA_results <- MplusAutomation::readModels(glue::glue("mplus_analyses/{out_f_dir}/L2_{k}_P_4_ml_lpa_freeVar.out"), what="savedata")$savedata

# L! bar charts faceted by profile
l1_by_liwc_plot <- mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  dplyr::select(DRIVES:WC, L1) %>%
  # rename('row_num' = ROW_NUM) %>%
  # select(-starts_with("V")) %>%
  pivot_longer(
    cols = DRIVES:WC,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    L1 = as.factor(L1),
    variable = ordered(tolower(variable), levels = c("affect", "drives","social","cognitio","big_word","wc"))
    ) %>%
  group_by(L1, variable) %>%
  summarize(
    m = mean(value),
    sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, fill = variable)) + geom_col() + facet_grid(~L1) + 
  ggthemes::theme_tufte() + theme(
    legend.position="bottom",
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) + labs(fill = 'Linguistic dimension',
           y = 'Mean standardized value',
           x = 'Linguistic dimension',
           title = 'Linguistic dimensions by interaction (L1) profile')

# L1 line plots
# mlLPA_results %>% 
#   rename('L1' = 'C',
#          'L2' = 'BC') %>%
#   janitor::clean_names() %>%
#   select(drives:big_word, l1) %>%
#   pivot_longer(
#     cols = drives:big_word,
#     names_to = "variable",
#     values_to = "value"
#   ) %>%
#   mutate(l1 = as.factor(l1),
#          variable = ordered(variable, levels = c("affect", "drives","social","cognitio","big_word","wc"))) %>%
#   group_by(l1, variable) %>%
#   summarize(
#     m = mean(value)) %>%
#   ungroup() %>%
#   ggplot(aes(x = variable, y = m, color = l1, group = l1)) + 
#   geom_point() + geom_line() + ggthemes::theme_tufte() +
#   scale_fill_brewer(palette = "Set2")

# L2 profiles by L1 composition
l2_by_l1_plot <- mlLPA_results %>% 
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(l1,l2) %>%
  mutate(
    l1 = as.factor(l1),
    l2 = as.factor(l2)
  ) %>%
  group_by(l2,l1) %>%
  summarize(l1_count = n())%>%
  ungroup() %>%
  group_by(l2) %>%
  mutate(l1_perc = l1_count / sum(l1_count)) %>%
  ungroup() %>%
  ggplot(aes(fill=l1, y=l1_perc, x=l2)) + 
  geom_bar(position="fill", stat="identity") + ggthemes::theme_tufte() + 
  theme(legend.position="bottom") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Frequency of L1 interaction profiles in L2 clinician classes',
    x = 'L2 clinician class',
    y = 'Percentage of L1 profiles',
    fill = 'L1 profile'
  )

l1_by_l2_tbl <- mlLPA_results %>% 
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(l1,l2) %>%
  mutate(
    l1 = as.factor(l1),
    l2 = as.factor(l2)
  ) %>%
  gtsummary::tbl_cross(
    row = l1,
    col = l2, 
    percent = 'column',
    label = list(l1 = 'L1 profiles', l2 = 'L2 classes')) %>%
  gtsummary::bold_labels() %>%
  gtsummary::as_gt() %>%
  gt::tab_header(title = 'Frequency of L1 interaction profiles by L2 clinician classes') %>%
  bstfun::as_ggplot()

l1_by_liwc_plot / (l2_by_l1_plot + l1_by_l2_tbl) + plot_annotation(tag_levels = 'A')

# Merging new and old data sets
L1 <- 4
mergedResults <- mlLPA_results %>% 
  mutate(
    L2_1_prob_4 = rowSums(select(.,(num_range("CPROB", 1:4)))),
    L2_2_prob_4 = rowSums(select(.,(num_range("CPROB", 5:8)))),
    L2_3_prob_4 = rowSums(select(.,(num_range("CPROB", 9:12)))),
    L2_4_prob_4 = rowSums(select(.,(num_range("CPROB", 13:16)))),
    L1_1_prob_4 = rowSums(select(.,num_range("CPROB",c(1,5,9,13)))),
    L1_2_prob_4 = rowSums(select(.,num_range("CPROB",c(2,6,10,14)))),
    L1_3_prob_4 = rowSums(select(.,num_range("CPROB",c(3,7,11,15)))),
    L1_4_prob_4 = rowSums(select(.,num_range("CPROB",c(4,8,12,16))))
  ) %>%
  select(BC,C,ROW_NUM,PCP_ID,starts_with('L')) %>%
  rename("M{L1}_L1" := 'C',
         "M{L1}_L2":=  'BC') %>%
  janitor::clean_names() %>%
  full_join(df_lpa_md, by = 'row_num') %>%
  select(starts_with('m4'),ends_with('_4'),file, study, provider_id) 

file_metrics <- read.csv('/Users/mrosen44/Documents/Data_Analysis_Local/Patient_MD_Com/cross_study_matching_accomodation_10-19-2023.csv')
mergedResults <- mergedResults %>%
  left_join(file_metrics, by = c('file','study'))

mergedResults %>%
  rename('L1' = 'm4_l1',
         'L2' = 'm4_l2') %>%
    janitor::clean_names() %>%
    select(l1,l2,study) %>%
    mutate(
      l1 = as.factor(l1),
      l2 = as.factor(l2),
      study = as.factor(study)
    ) %>%
    gtsummary::tbl_cross(row = l2,col = study, percent = 'column')

# haven::read_sav('NEWprofile_auxVarWithRias_LG_withTotAf_10-26-2023.sav')

mergedResults %>% write.csv('FIXED_NewProfiles_10-27-2023.csv')



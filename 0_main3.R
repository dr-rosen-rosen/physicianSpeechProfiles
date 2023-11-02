############################################################################
############################################################################
###### Code for Generating Physician Speech Profiles
###### Focusing on summary measures
############################################################################
############################################################################
library(tidyverse)
library(here)
library(config)
library(MplusAutomation)
source('1_funcs.R')
#########################################
# Read in and combine all of the conversation level liwc files
df_lpa <- get_combined_data()
skimr::skim(df_lpa)

# write.csv(df_lpa,'combined_MD_liwc_for_FA.csv')
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
  select(drives, cognition, affect, analytic, clout, authentic, wc, big_words) %>%
  careless::mahad() %>% #+ geom_hline(yintercept = 40, color = "red")
  cbind(df_lpa_md) %>%
  rename('mahad' = '.') %>%
  filter(mahad < 50)

# inter-correlations; vars are supposed to be uncorrelated
df_lpa_md %>%
  select(drives, cognition, affect, analytic, clout, authentic, wc, big_words) %>%
  corrtable::correlation_matrix()
  # corrplot::corrplot()

# ICC's 
for (var in c('drives', 'cognition', 'affect', 'analytic', 'clout', 'authentic', 'wc','big_words')) {
  f <- as.formula(paste(var,'~ provider_id'))
  ICC1 <- multilevel::ICC1(aov(f,data=df_lpa_md))
  print(paste0(var,': ',ICC1))
}
##################################################################################
# Prep data for Mplus analysis

ml_lpa_df <- df_lpa_md %>%
  select(drives, cognition, affect, analytic, clout, authentic, wc, big_words, PCP_ID)

df_prep <- df_lpa_md %>%
  select(drives, cognition, affect, analytic, clout, authentic, wc, big_words, PCP_ID) %>%
  # mutate_if(is.numeric,scale) %>% % this sets attributes that prep_mplus has a problem with.
  prepareMplusData(
    filename = 'mplus_analyses/cmbd_vars/MD_profile_cmb_mPlus.dat'
  )

#### Automating MPLUS models

class_str <- character()

ml_lpa1_6 <- lapply(1:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[drives, cognition, affect, analytic, clout, authentic, wc, big_words];
drives, cognition, affect, analytic, clout, authentic, wc, big_words;")
    } else {
      class_str <<- paste(class_str,
                         glue::glue("%C#{x}%
[drives, cognition, affect, analytic, clout, authentic, wc, big_words];
drives, cognition, affect, analytic, clout, authentic, wc, big_words;"
                                    ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("P{k}_lpa__freeVarenum;"),
    VARIABLE = glue::glue("CLASSES = c({k});"),
    DEFINE = "STANDARDIZE drives, cognition, affect, analytic, clout, authentic, wc, big_words;",
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
                                            dataout = glue::glue("mplus_analyses/cmbd_vars/prof_{k}_ml_lpa_freeVar_enum.dat"),
                                            modelout = glue::glue("mplus_analyses/cmbd_vars/prof_{k}_ml_lpa_freeVar_enum.inp"),
                                            check = TRUE, run = TRUE, hashfilename = FALSE)
}
  )

output_enum <- readModels(here("mplus_analyses/cmbd_vars"), quiet = TRUE)
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
# filter(str_detect(Title, pattern = 'lpa__freeVarenum')) %>%
pivot_longer(
  !Title, names_to = 'criteria', values_to = 'value'
) %>%
mutate(
  num_profiles = readr::parse_number(Title)
) %>%
filter(criteria != 'LL') %>%
ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
geom_point() + geom_line() +
scale_x_continuous(breaks= scales::pretty_breaks()) + labs(title='Means and variances')


################################
######## Check for fit for different number of L2 vars
################################
class_str <- character()

k <- 4
ml_lpa1_6 <- lapply(2:6, function(j)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[drives, cognition, affect, analytic, clout, authentic, wc, big_words];
drives, cognition, affect, analytic, clout, authentic, wc, big_words;")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[drives, cognition, affect, analytic, clout, authentic, wc, big_words];
drives, cognition, affect, analytic, clout, authentic, wc, big_words;"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L2{j}_P{k}_lpa__freeVarenum_L2;"),
    VARIABLE = glue::glue("CLASSES = BC({j}) c({k});\nCLUSTER IS PCP_ID;\nWITHIN ARE drives, cognition, affect, analytic, clout, authentic, wc, big_words;\nBETWEEN ARE BC;"),
    DEFINE = "STANDARDIZE drives, cognition, affect, analytic, clout, authentic, wc, big_words;",
    ANALYSIS = "TYPE = MIXTURE TWOLEVEL;
    ESTIMATOR=MLR;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = glue::glue('%WITHIN%\n%OVERALL%\n%BETWEEN%\n%OVERALL%\nc ON BC;\nMODEL c:\n%WITHIN%\n',class_str),
    OUTPUT = "",
    PLOT = "type = plot3;",
    SAVEDATA = glue::glue("file=mlLpa_{k}L1_{j}L2.dat; save=cprob; TECH4 IS tech4.dat;"),
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/cmbd_vars/prof_{k}_ml_lpa_freeVar_enumL2_{j}.dat"),
                                              modelout = glue::glue("mplus_analyses/cmbd_vars/prof_{k}_ml_lpa_freeVar_enum2_{j}.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)


output_enum <- readModels(here("mplus_analyses/cmbd_vars"), quiet = TRUE)
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
  filter(str_detect(Title, pattern = '_L2')) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(Title)
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() +
  scale_x_continuous(breaks= scales::pretty_breaks()) + labs(title='3 L1 Profiles')




################################
######## Exmploring first ml-lpa
################################
mlLPA_results <- read.table('mplus_analyses/cmbd_vars/mlLpa_4L1_4L2.dat') %>%
  rename(
    drives = V1,
    cognition = V2,
    affect = V3,
    analytic = V4,
    clout = V5,
    authentic = V6,
    # tone = V4,
    wc = V7,
    #wps = V6,
    big_words = V8,
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
    prov_class = V37,
    int_class = V38,
    cmb_class = V39,
    mplus_prov_id = V40
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
         variable = ordered(variable, levels = c("drives", "cognition", "affect", "analytic", "clout", "authentic", "wc", "big_words"))) %>%
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
    mean_prop_prof_4 = mean(prop_prof_4),
    ) %>%
  ungroup()

l2_summary <- mlLPA_results %>%
  select(mplus_prov_id,prov_class) %>%
  distinct() %>%
  group_by(prov_class) %>%
  summarize(count = n()) %>%
  full_join(l2_summary, by = 'prov_class')

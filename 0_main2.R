############################################################################
############################################################################
###### Code for Generating Physician Speech Profiles
############################################################################
############################################################################
library(tidyverse)
library(here)
library(config)
library(MplusAutomation)
library(ggdist)
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
  select(analytic, clout, authentic, tone, wc, big_words) %>%
  careless::mahad() %>% #+ geom_hline(yintercept = 40, color = "red")
  cbind(df_lpa_md) %>%
  rename('mahad' = '.') %>%
  filter(mahad < 40)

# inter-correlations; vars are supposed to be uncorrelated
df_lpa_md %>%
  select(analytic, clout, authentic, tone, wc, big_words) %>%
  corrtable::correlation_matrix()
  # corrplot::corrplot()

# ICC's 
for (var in c('analytic', 'clout', 'authentic', 'tone', 'wc','big_words')) {
  f <- as.formula(paste(var,'~ provider_id'))
  ICC1 <- multilevel::ICC1(aov(f,data=df_lpa_md))
  print(paste0(var,': ',ICC1))
}
##################################################################################
# Prep data for Mplus analysis

ml_lpa_df <- df_lpa_md %>%
  select(analytic, clout, authentic, tone, wc, big_words, PCP_ID)

df_prep <- df_lpa_md %>%
  select(analytic, clout, authentic, tone, wc, big_words, PCP_ID) %>%
  # mutate_if(is.numeric,scale) %>% % this sets attributes that prep_mplus has a problem with.
  prepareMplusData(
    filename = 'mplus_analyses/sum_vars/MD_profile_cmb_mPlus.dat'
  )

#### Automating MPLUS models

class_str <- character()

ml_lpa1_6 <- lapply(1:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[analytic clout authentic tone wc big_words];
analytic clout authentic tone wc big_words;")
    } else {
      class_str <<- paste(class_str,
                         glue::glue("%C#{x}%
[analytic clout authentic tone wc big_words];
analytic clout authentic tone wc big_words;"
                                    ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L1_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("CLASSES = c({k});"),
    DEFINE = "STANDARDIZE analytic clout authentic tone wc big_words;",
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
                                            dataout = glue::glue("mplus_analyses/sum_vars/L1_prof_{k}_ml_lpa_freeVar_enum.dat"),
                                            modelout = glue::glue("mplus_analyses/sum_vars/L1_prof_{k}_ml_lpa_freeVar_enum.inp"),
                                            check = TRUE, run = TRUE, hashfilename = FALSE)
}
  )

output_enum <- readModels(here("mplus_analyses/sum_vars"), quiet = TRUE)
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
filter(str_detect(Title, pattern = 'L1_')) %>%
pivot_longer(
  !Title, names_to = 'criteria', values_to = 'value'
) %>%
mutate(
  num_profiles = readr::parse_number(str_remove(Title,'L1_P_'))
) %>%
filter(criteria != 'LL') %>%
ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
geom_point() + geom_line() +
scale_x_continuous(breaks= scales::pretty_breaks()) + labs(title='Means and variances')


################################
######## Check for fit for different number of L2 vars
################################
class_str <- character()

k <- 7
ml_lpa1_6 <- lapply(1:10, function(j)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[analytic clout authentic tone wc big_words];
analytic clout authentic tone wc big_words;")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[analytic clout authentic tone wc big_words];
analytic clout authentic tone wc big_words;"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L2_{j}_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("CLASSES = BC({j}) c({k});\nCLUSTER IS PCP_ID;\nWITHIN ARE analytic clout authentic tone wc big_words;\nBETWEEN ARE BC;"),
    DEFINE = "STANDARDIZE analytic clout authentic tone wc big_words;",
    ANALYSIS = "TYPE = MIXTURE TWOLEVEL;
    ESTIMATOR=MLR;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = glue::glue('%WITHIN%\n%OVERALL%\n%BETWEEN%\n%OVERALL%\nc ON BC;\nMODEL c:\n%WITHIN%\n',class_str),
    OUTPUT = "",
    PLOT = "type = plot3;",
    SAVEDATA = glue::glue("file=mlLpa_L2_{j}_L1_{k}.dat; save=cprob; TECH4 IS tech4.dat;"),
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/sum_vars/L2_{j}_P_{k}_ml_lpa_freeVar.dat"),
                                              modelout = glue::glue("mplus_analyses/sum_vars/L2_{j}_P_{k}_ml_lpa_freeVar.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)


output_enum <- readModels(here("mplus_analyses/sum_vars"), quiet = TRUE)
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
  geom_point() + geom_line() +
  scale_x_continuous(breaks= scales::pretty_breaks()) + labs(title=glue::glue('{k} L1 Profiles'))




################################
######## Exmploring first ml-lpa
################################

L1 <- 3
L2 <- 3

mlLPA_results <- read.table(glue::glue('mplus_analyses/sum_vars/mlLpa_L2_{L2}_L1_{L1}.dat')) %>%
  rename(
    analytic = V1,
    clout = V2,
    authentic = V3,
    tone = V4,
    wc = V5,
    #wps = V6,
    big_words = V6,
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
    cols = analytic:big_words,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(int_class,variable) %>%
  summarize(m_value = mean(value)) %>%
  mutate(int_class = as.factor(int_class),
         variable = ordered(variable, levels = c("analytic", "clout","authentic","tone","big_words","wc"))) %>%
  ggplot(aes(x = variable, y = m_value, color = int_class, group = int_class)) + 
  geom_point() + geom_line() + ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set2")


# https://mjskay.github.io/ggdist/articles/dotsinterval.html

mlLPA_results %>% 
  select(-starts_with("V")) %>%
  pivot_longer(
    cols = analytic:big_words,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(int_class = as.factor(int_class),
              variable = ordered(variable, levels = c("analytic", "clout","authentic","tone","big_words","wc"))) %>%
  #ggplot(aes(x = int_class, y = value)) + geom_boxplot() + facet_wrap(~variable) + ggpubr::stat_compare_means()
  ggplot(aes(y = int_class, x = value, fill = int_class)) +
  facet_wrap(~variable) +
  #scale_y_discrete(limits = rev) +
  # ggplot(aes(y = as.factor(race), x = rw.rLSM.D, fill = as.factor(cdspeak))) +
  #xlim(0,1) + 
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") #+
  #labs(title = "Distribtion of linguistic style matching measures",fill = "Matching measure", y = 'Matching measure', x = 'Matching score')


  
  

table(mlLPA_results$int_class)

l2_summary <- mlLPA_results %>% 
  select(last_col(offset = 3):last_col(),-cmb_class) %>%
  group_by(mplus_prov_id,prov_class) %>%
  summarize(
    prop_prof_1 = sum(int_class == 1) / n(),
    prop_prof_2 = sum(int_class == 2) / n(),
    prop_prof_3 = sum(int_class == 3) / n(),
    prop_prof_4 = sum(int_class == 4) / n(),
    prop_prof_5 = sum(int_class == 5) / n(),
    prop_prof_6 = sum(int_class == 6) / n(),
    prop_prof_7 = sum(int_class == 7) / n()
  ) %>%
  ungroup() %>%
  group_by(prov_class) %>%
  summarize(
    mean_prop_prof_1 = mean(prop_prof_1),
    mean_prop_prof_2 = mean(prop_prof_2),
    mean_prop_prof_3 = mean(prop_prof_3),
    mean_prop_prof_4 = mean(prop_prof_4),
    mean_prop_prof_5 = mean(prop_prof_5),
    mean_prop_prof_6 = mean(prop_prof_6),
    mean_prop_prof_7 = mean(prop_prof_7)
    ) %>%
  ungroup()

l2_summary <- mlLPA_results %>%
  select(mplus_prov_id,prov_class) %>%
  distinct() %>%
  group_by(prov_class) %>%
  summarize(count = n()) %>%
  full_join(l2_summary, by = 'prov_class')

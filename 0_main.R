############################################################################
############################################################################
###### Code for Generating Physician Speech Profiles
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
  select(drives, cognition, affect, social, wc, wps, big_words) %>%
  careless::mahad() %>% #+ geom_hline(yintercept = 40, color = "red")
  cbind(df_lpa_md) %>%
  rename('mahad' = '.') %>%
  filter(mahad < 40)

# inter-correlations; vars are supposed to be uncorrelated
df_lpa_md %>%
  select(drives, cognition, affect, social, wc, big_words) %>%
  modelsummary::datasummary_correlation()

# ICC's 
for (var in c('drives', 'cognition', 'affect', 'social', 'wc', 'big_words')) {
  f <- as.formula(paste(var,'~ provider_id'))
  ICC1 <- multilevel::ICC1(aov(f,data=df_lpa_md))
  print(paste0(var,': ',ICC1))
}
##################################################################################
# Prep data for Mplus analysis
library(MplusAutomation)
# https://cran.r-project.org/web/packages/MplusAutomation/vignettes/vignette.html

ml_lpa_df <- df_lpa_md %>%
  select(drives, cognition, affect, social, wc, big_words, PCP_ID, row_num)

df_prep <- df_lpa_md %>%
  select(drives, cognition, affect, social, wc, big_words, PCP_ID, row_num) %>%
  prepareMplusData(
    filename = 'mplus_analyses/psych_process/MD_profile_cmb_mPlus.dat'
  )

#### Automating MPLUS models

class_str <- character()

ml_lpa1_10 <- lapply(1:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[drives cognition affect social wc big_words];
drives cognition affect social wc big_words;")
    } else {
      class_str <<- paste(class_str,
                         glue::glue("%C#{x}%
[drives cognition affect social wc big_words];
drives cognition affect social wc big_words;"
                                    ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L1_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("CLASSES = c({k});"),
    DEFINE = "STANDARDIZE drives cognition affect social wc big_words;",
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
                                            dataout = glue::glue("mplus_analyses/psych_process/L1_prof_{k}_ml_lpa_freeVar_enum.dat"),
                                            modelout = glue::glue("mplus_analyses/psych_process/L1_prof_{k}_ml_lpa_freeVar_enum.inp"),
                                            check = TRUE, run = TRUE, hashfilename = FALSE)
}
  )

output_enum <- readModels(here("mplus_analyses/psych_process"), quiet = TRUE)
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
# filter(str_detect(Title, pattern = 'L2', negate = TRUE)) %>%
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

k <- 5 # L1 profiles
ml_lpa2 <- lapply(5:5, function(j) # L2 profiles
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[drives cognition affect social wc big_words];
drives cognition affect social wc big_words;")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[drives cognition affect social wc big_words];
drives cognition affect social wc big_words;"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L2_{j}_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = BC({j}) c({k});\nCLUSTER IS PCP_ID;\nWITHIN ARE drives cognition affect social wc big_words;\nBETWEEN ARE BC;"),
    DEFINE = "STANDARDIZE drives cognition affect social wc big_words;",
    ANALYSIS = "TYPE = MIXTURE TWOLEVEL;
    ESTIMATOR=MLR;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = glue::glue('%WITHIN%\n%OVERALL%\n%BETWEEN%\n%OVERALL%\nc ON BC;\nMODEL c:\n%WITHIN%\n',class_str),
    OUTPUT = "",
    PLOT = "type = plot3;",
    SAVEDATA = glue::glue("file=mlLpa_L2_{j}_L1_{k}.dat;\nsave=cprob;\nTECH4 IS tech4.dat;"),
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("mplus_analyses/psych_process/L2_{j}_P_{k}_ml_lpa_freeVar.dat"),
                                              modelout = glue::glue("mplus_analyses/psych_process/L2_{j}_P_{k}_ml_lpa_freeVar.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)


output_enum <- readModels(here("mplus_analyses/psych_process"), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

k <- 5
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
  scale_x_continuous(breaks= scales::pretty_breaks()) + labs(title=glue::glue('Range of L2 classes for {k} interaction profiles'))




################################
######## Exmploring first ml-lpa
################################
L1 <- 3
L2 <- 3
out_f_dir <- 'psych_process'
mlLPA_results <- MplusAutomation::readModels(glue::glue("mplus_analyses/{out_f_dir}/L2_{L2}_P_{L1}_ml_lpa_freeVar.out"), what="savedata")$savedata





# write.csv(cmb_data, 'profile_data_3_L1_and_3_L2.csv')

#######################################################
####### Creating dataset with multiple class models linked to original data

L1 <- 3
L2 <- 3

mlLPA_results <- MplusAutomation::readModels(glue::glue("mplus_analyses/psych_process/L2_{L2}_P_{L1}_ml_lpa_freeVar.out"), what="savedata")$savedata

x <- mlLPA_results %>%
  mutate(
    L2_1_prob = rowSums(select(.,(num_range("CPROB", 1:3)))),
    L2_2_prob = rowSums(select(.,(num_range("CPROB", 4:6)))),
    L2_3_prob = rowSums(select(.,(num_range("CPROB", 7:9)))),
    L1_1_prob = rowSums(select(.,num_range("CPROB",c(1,4,7)))),
    L1_2_prob = rowSums(select(.,num_range("CPROB",c(2,5,8)))),
    L1_3_prob = rowSums(select(.,num_range("CPROB",c(3,6,9))))
  ) %>%
  select(BC,C,ROW_NUM,PCP_ID,starts_with('L')) %>%
  rename("M{L1}_L1" := 'C',
         "M{L1}_L2":=  'BC') %>%
  janitor::clean_names() %>%
  full_join(df_lpa_md, by = 'row_num') %>%
  select(1:15, -mahad, -row_num, -PCP_ID,-pcp_id)

write.csv(x,'profilesByFileWithProbabilities.csv')
table(y$study,y$m3_l2)

y <- x %>%
  select(starts_with("l2_"),m3_l2,study,provider_id) %>%
  group_by(provider_id) %>%
  # mutate(l21_prob_mean = mean(l2_1_prob)) %>%
  mutate(across(starts_with("l2_"),mean)) %>%
  ungroup() %>% distinct()

write.csv(y,'leve2ClassesWithProbabilities.csv')
table(x$study,x$m3_l1)

hist(x$L2_1_prob)


profiles_5 <- mlLPA_results %>%
  relocate("M{L1}_L1" := 'C',
         "M{L1}_L2" :=  'BC',
         ROW_NUM) %>%
  janitor::clean_names() %>%
  select(1:3) 

cmb_data <- df_lpa_md %>%
  full_join(profiles_3, by = 'row_num') %>%
  full_join(profiles_4, by = 'row_num') %>%
  full_join(profiles_5, by = 'row_num') %>%
  select(matches('m[0-9]_*'), study, file)

write.csv(cmb_data, 'combined_profiles_06-08-2023.csv')






table(mlLPA_results$int_class)

l2_summary <- mlLPA_results %>% 
  select(last_col(offset = 3):last_col(),-cmb_class) %>%
  group_by(mplus_prov_id,prov_class) %>%
  summarize(
    prop_prof_1 = sum(int_class == 1) / n(),
    prop_prof_2 = sum(int_class == 2) / n(),
    prop_prof_3 = sum(int_class == 3) / n(),
    prop_prof_4 = sum(int_class == 4) / n(),
    prop_prof_5 = sum(int_class == 5) / n()
  ) %>%
  ungroup() %>%
  group_by(prov_class) %>%
  summarize(
    mean_prop_prof_1 = mean(prop_prof_1),
    mean_prop_prof_2 = mean(prop_prof_2),
    mean_prop_prof_3 = mean(prop_prof_3),
    mean_prop_prof_4 = mean(prop_prof_4),
    mean_prop_prof_5 = mean(prop_prof_5)
    ) %>%
  ungroup()

l2_summary <- mlLPA_results %>%
  select(mplus_prov_id,prov_class) %>%
  distinct() %>%
  group_by(prov_class) %>%
  summarize(count = n()) %>%
  full_join(l2_summary, by = 'prov_class')


################################
######## Plots
################################

# L1 raincloud plots
# https://mjskay.github.io/ggdist/articles/dotsinterval.html

mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(drives:big_word, l1) %>%
  pivot_longer(
    cols = drives:big_word,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(l1 = as.factor(l1),
         variable = ordered(variable, levels = c("affect", "drives","social","cognitio","big_word","wc"))) %>%
  ggplot(aes(y = variable, x = value, fill = variable)) +
  facet_wrap(~l1) +
  ggdist::stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  ggdist::stat_dotsinterval(side = "bottom", scale = 0.5, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") + ggthemes::theme_tufte() + theme(legend.position="bottom") #+
#labs(title = "Distribtion of linguistic style matching measures",fill = "Matching measure", y = 'Matching measure', x = 'Matching score')

# L1 bar plots - each profile in a facet
mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(drives:big_word, l1) %>%
  pivot_longer(
    cols = drives:big_word,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(l1 = as.factor(l1),
         variable = ordered(variable, levels = c("affect", "drives","social","cognitio","big_word","wc"))) %>%
  group_by(l1, variable) %>%
  summarize(
    m = mean(value),
    sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, fill = variable)) + geom_col() + facet_grid(~l1) + 
  ggthemes::theme_tufte() + theme(
    legend.position="bottom",
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) 


# L1 bar plots - grouped profiles
mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(drives:big_word, l1) %>%
  pivot_longer(
    cols = drives:big_word,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(l1 = as.factor(l1),
         variable = ordered(variable, levels = c("affect", "drives","social","cognitio","big_word","wc"))) %>%
  group_by(l1, variable) %>%
  summarize(
    m = mean(value),
    sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, fill = l1)) + geom_col(position="dodge") + #facet_grid(~l1) + 
  ggthemes::theme_tufte() + theme(
    legend.position="bottom",
    # panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) 

# L1 bar plots - grouped profiles
mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(drives:big_word, l1) %>%
  pivot_longer(
    cols = drives:big_word,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(l1 = as.factor(l1),
         variable = ordered(variable, levels = c("affect", "drives","social","cognitio","big_word","wc"))) %>%
  group_by(l1, variable) %>%
  summarize(
    m = mean(value),
    sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, fill = l1)) + geom_col(position="dodge") + #facet_grid(~l1) + 
  ggthemes::theme_tufte() + theme(
    legend.position="bottom",
    # panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) 

# L1 line
mlLPA_results %>% 
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(drives:big_word, l1) %>%
  pivot_longer(
    cols = drives:big_word,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(l1 = as.factor(l1),
         variable = ordered(variable, levels = c("affect", "drives","social","cognitio","big_word","wc"))) %>%
  group_by(l1, variable) %>%
  summarize(
    m = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, color = l1, group = l1)) + 
  geom_point() + geom_line() + ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set2")


# L1 radar plots... this sucks
radar <- mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(drives:big_word, l1) %>%
  pivot_longer(
    cols = drives:big_word,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(l1 = as.factor(l1),
         variable = ordered(variable, levels = c("affect", "drives","social","cognitio","big_word","wc"))) %>%
  group_by(l1, variable) %>%
  summarize(
    m = mean(value)) %>%
  ungroup() %>%
  pivot_wider(id_cols = l1, names_from = variable, values_from = m) %>% 
  mutate(
    l1 = paste0('l1.',as.character(l1))
  ) %>% remove_rownames %>% column_to_rownames(var="l1") %>%
  rbind(rep(1,6), rep(-1,6),.)

fmsb::radarchart(radar, axistype = 1, pcol = RColorBrewer::brewer.pal(3, 'Set2'), 
                   pfcol = RColorBrewer::brewer.pal(3, 'Set2'), #plwd=4 , plty=1#,
                 glcol="grey", cglty=1, axislabcol="grey", 
                 cglwd=0.8,
                   # caxislabels=seq(-1,-.5,0,.5,1)
  )


library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggtext)

# demo_df_l1 <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'demo_to_l1_df') |>
# #demo_df_l1 <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'demo_to_l1_df') %>%
#   mutate(var_type = case_when(
#                                str_detect(demo_var,'tot_wc') ~ 'tot_wc',
#                                str_detect(demo_var,'patient_sex') ~ 'patient_sex',
#                                str_detect(demo_var,'patient_race') ~ 'patient_race',
#                                str_detect(demo_var,'patient_age') ~ 'patient_age',
#                                str_detect(demo_var,'provider_sex') ~ 'provider_sex',
#                                str_detect(demo_var,'provider_race') ~ 'provider_race',
#                                .default = demo_var)
#   )
#   

# demo_df_l1 %>%
#   mutate(
#     profile = as.factor(profile),
#     sig = if_else(p < .05,TRUE,FALSE)
#   ) %>%
#   # filter(var_type %in% c('tot_wc','patient_age')) %>%
#   #filter(var_type %in% c('provider_sex')) %>%
#   #filter(var_type %in% c('provider_race')) %>%
#    #filter(var_type %in% c('patient_sex')) %>%
#    filter(var_type %in% c('patient_race')) %>%
#   #filter(str_detect(var_type,'study')) %>%
#   ggplot(aes(fill = profile, y = coef, x = demo_var)) +
#   geom_bar(stat = 'identity',position = 'dodge') +
#   # ggplot(aes(x = var_type, y = coef, group = profile, color = profile, shape = sig)) +
#   # geom_line() + 
#   ggthemes::theme_tufte() + labs(title = 'L1 profiles and Demographic measures')


# pt_race_coefs <- demo_df_l1 |> filter(var_type == 'patient_race') |>
#   mutate(demo_var = spatstat.utils::unparen(str_remove(demo_var, pattern = paste0(c('patient_race(',')'), collapse = '|')))) |>
#   select(profile, demo_var, coef) |>
#   pivot_wider(
#     id_cols = demo_var, 
#     names_from = profile, 
#     values_from = coef,
#     # names_glue = 'profile_{profile}_coef'
#     names_prefix = 'coef_'
#   ) |> rename(patient_race = demo_var) |>
#   mutate(
#     patient_race = case_match(patient_race,
#                               'other/unkown' ~ 'other/unknown',
#                               .default = patient_race)
#   )

# gt_pt_race <- df_finalAux |> select(file, m4_l1, patient_race, patient_sex, provider_race, provider_sex) |>
#     drop_na() |>
#     group_by(patient_race, m4_l1) |>
#     summarize(n = n()) |> ungroup() |>
#   pivot_wider(
#     id_cols = patient_race, 
#     names_from = m4_l1, 
#     values_from = n,
#     names_prefix = 'profile_') |>
#   rowwise() |>
#   mutate(total = sum(across(c(profile_1,profile_2,profile_3,profile_4)))) |>
#   mutate(across(starts_with('profile'), ~(.x/total)*100)) |>
#   left_join(pt_race_coefs, by = 'patient_race') |>
#   # relocate(any_of(c('profile_1_coef','profile_2_coef','profile_3_coef','profile_4_coef')), .after = patient_race) |>
#   relocate(any_of(c('coef_1','coef_2','coef_3','coef_4')), .after = patient_race) |>
#   relocate(profile_1, .after = coef_1) |>
#   relocate(profile_2, .after = coef_2) |>
#   relocate(profile_3, .after = coef_3) |>
#   relocate(profile_4, .after = coef_4) |>
#   gt::gt() |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_1,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#F8766D'#"forestgreen"
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_2,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#7CAE00'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_3,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#00BFC4'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_4,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#C77CFF'
#   ) |>
#   gtExtras::gt_plt_point(coef_1, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_2, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_3, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_4, accuracy = .1, width = 25)
#   

# 
# pt_sex_coefs <- demo_df_l1 |> filter(var_type == 'patient_sex') |>
#   mutate(demo_var = spatstat.utils::unparen(str_remove(demo_var, pattern = paste0(c('patient_sex(',')'), collapse = '|')))) |>
#   select(profile, demo_var, coef) |>
#   pivot_wider(
#     id_cols = demo_var, 
#     names_from = profile, 
#     values_from = coef,
#     # names_glue = 'profile_{profile}_coef'
#     names_prefix = 'coef_'
#   ) |> rename(patient_sex = demo_var)

# gt_pt_sex <- df_finalAux |> select(file, m4_l1, patient_race, patient_sex, provider_race, provider_sex) |>
#   drop_na() |>
#   group_by(patient_sex, m4_l1) |>
#   summarize(n = n()) |> ungroup() |>
#   pivot_wider(
#     id_cols = patient_sex, 
#     names_from = m4_l1, 
#     values_from = n,
#     names_prefix = 'profile_') |>
#   rowwise() |>
#   mutate(total = sum(across(c(profile_1,profile_2,profile_3,profile_4)))) |>
#   mutate(across(starts_with('profile'), ~(.x/total)*100)) |>
#   left_join(pt_sex_coefs, by = 'patient_sex') |>
#   # relocate(any_of(c('profile_1_coef','profile_2_coef','profile_3_coef','profile_4_coef')), .after = patient_race) |>
#   relocate(any_of(c('coef_1','coef_2','coef_3','coef_4')), .after = patient_sex) |>
#   relocate(profile_1, .after = coef_1) |>
#   relocate(profile_2, .after = coef_2) |>
#   relocate(profile_3, .after = coef_3) |>
#   relocate(profile_4, .after = coef_4) |>
#   gt::gt() |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_1,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#F8766D'#"forestgreen"
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_2,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#7CAE00'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_3,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#00BFC4'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_4,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#C77CFF'
#   ) |>
#   gtExtras::gt_plt_point(coef_1, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_2, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_3, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_4, accuracy = .1, width = 25)
# 
# 
# p1 <- gt_pt_sex |> bstfun::as_ggplot()
# p2 <- gt_pt_race |> bstfun::as_ggplot()

# provider_race_coefs <- demo_df_l1 |> filter(var_type == 'provider_race') |>
#   mutate(demo_var = spatstat.utils::unparen(str_remove(demo_var, pattern = paste0(c('provider_race(',')'), collapse = '|')))) |>
#   select(profile, demo_var, coef) |>
#   pivot_wider(
#     id_cols = demo_var, 
#     names_from = profile, 
#     values_from = coef,
#     # names_glue = 'profile_{profile}_coef'
#     names_prefix = 'coef_'
#   ) |> rename(provider_race = demo_var) |>
#   mutate(
#     provider_race = case_match(provider_race,
#                               'other/unkown' ~ 'other/unknown',
#                               .default = provider_race)
#   )

# gt_provider_race <- df_finalAux |> select(file, m4_l1, patient_race, patient_sex, provider_race, provider_sex) |>
#   drop_na() |>
#   group_by(provider_race, m4_l1) |>
#   summarize(n = n()) |> ungroup() |>
#   pivot_wider(
#     id_cols = provider_race, 
#     names_from = m4_l1, 
#     values_from = n,
#     names_prefix = 'profile_') |>
#   rowwise() |>
#   mutate(total = sum(across(c(profile_1,profile_2,profile_3,profile_4)))) |>
#   mutate(across(starts_with('profile'), ~(.x/total)*100)) |>
#   left_join(provider_race_coefs, by = 'provider_race') |>
#   # relocate(any_of(c('profile_1_coef','profile_2_coef','profile_3_coef','profile_4_coef')), .after = patient_race) |>
#   relocate(any_of(c('coef_1','coef_2','coef_3','coef_4')), .after = provider_race) |>
#   relocate(profile_1, .after = coef_1) |>
#   relocate(profile_2, .after = coef_2) |>
#   relocate(profile_3, .after = coef_3) |>
#   relocate(profile_4, .after = coef_4) |>
#   gt::gt() |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_1,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#F8766D'#"forestgreen"
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_2,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#7CAE00'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_3,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#00BFC4'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_4,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#C77CFF'
#   ) |>
#   gtExtras::gt_plt_point(coef_1, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_2, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_3, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_4, accuracy = .1, width = 25)

# provider_sex_coefs <- demo_df_l1 |> filter(var_type == 'provider_sex') |>
#   mutate(demo_var = spatstat.utils::unparen(str_remove(demo_var, pattern = paste0(c('provider_sex(',')'), collapse = '|')))) |>
#   select(profile, demo_var, coef) |>
#   pivot_wider(
#     id_cols = demo_var, 
#     names_from = profile, 
#     values_from = coef,
#     # names_glue = 'profile_{profile}_coef'
#     names_prefix = 'coef_'
#   ) |> rename(provider_sex = demo_var)

# gt_provider_sex <- df_finalAux |> select(file, provider_id, m4_l1, patient_race, patient_sex, provider_race, provider_sex) |>
#   drop_na() |>
#   group_by(provider_sex, m4_l1) |>
#   summarize(n = n()) |> ungroup() |>
#   pivot_wider(
#     id_cols = provider_sex, 
#     names_from = m4_l1, 
#     values_from = n,
#     names_prefix = 'profile_') |>
#   mutate(profile_4 = replace_na(profile_4,0)) |>
#   rowwise() |>
#   mutate(total = sum(across(c(profile_1,profile_2,profile_3,profile_4)))) |>
#   mutate(across(starts_with('profile'), ~(.x/total)*100)) |>
#   left_join(provider_sex_coefs, by = 'provider_sex') |>
#   # relocate(any_of(c('profile_1_coef','profile_2_coef','profile_3_coef','profile_4_coef')), .after = patient_race) |>
#   relocate(any_of(c('coef_1','coef_2','coef_3','coef_4')), .after = provider_sex) |>
#   relocate(profile_1, .after = coef_1) |>
#   relocate(profile_2, .after = coef_2) |>
#   relocate(profile_3, .after = coef_3) |>
#   relocate(profile_4, .after = coef_4) |>
#   gt::gt() |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_1,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#F8766D'#"forestgreen"
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_2,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#7CAE00'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_3,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#00BFC4'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = profile_4,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#C77CFF'
#   ) |>
#   gtExtras::gt_plt_point(coef_1, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_2, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_3, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_4, accuracy = .1, width = 25)
# 
# p3 <- gt_provider_sex |> bstfun::as_ggplot()
# p4 <- gt_provider_race |> bstfun::as_ggplot()
# library(patchwork)
# p_pt_prov_sex_race <- p1 / p2 / p3 / p4

################## Forrest plots
#demo_df_l1 <- readxl::read_excel('Model_summaries_04-06-2024.xlsx',sheet = 'demo_to_l1_df') %>%
  #demo_df_l1 <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'demo_to_l1_df') %>%
demo_df_l1 <- readxl::read_excel('Model_sum_last_pass_06-01-2024.xlsx',sheet = 'demo_to_l1_df') |>
  mutate(var_type = case_when(
    str_detect(demo_var,'tot_wc') ~ 'Continuous variables',
    str_detect(demo_var,'patient_sex') ~ 'Patient sex',
    str_detect(demo_var,'patient_race') ~ 'Patient race',
    str_detect(demo_var,'patient_age') ~ 'Continuous variables',
    str_detect(demo_var,'provider_sex') ~ 'Provider sex',
    str_detect(demo_var,'provider_race') ~ 'Provider race',
    .default = demo_var),
    value = case_when(
      str_detect(demo_var, 'female') ~ 'Female',
      str_detect(demo_var, 'male') ~ 'Male',
      str_detect(demo_var, 'tot_wc') ~ 'Total word count',
      str_detect(demo_var, 'age') ~ 'Patient age',
      str_detect(demo_var, 'asian') ~ 'Asian',
      str_detect(demo_var, 'black') ~ 'Black',
      str_detect(demo_var, 'white') ~ 'White',
      str_detect(demo_var, 'other/unkown|other/unknown') ~ 'Other / unkown',
      .default = NA
    ),
    # value = if_else(p < 0.05, glue::glue("**{value}**"),value),
    profile = as.factor(profile),
    lower = coef - se*1.96,
    upper = coef + se*1.96
  )

#define colours for dots and bars
barCOLS = c("#F8766D","#7CAE00","#00BFC4","#C77CFF")
dotCOLS = c("#fcc8c5","#d8ff79","#81fcff","#e9cbff")

p.fp.demo <- ggplot(demo_df_l1, aes(x=value, y=coef, ymin=lower, ymax=upper,col=profile,fill=profile)) + 
  #specify position here
  geom_linerange(linewidth=3,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Demographic and control variables", limits = rev) +
  scale_y_continuous(name="Coefficients", limits = c(-7, 7)) +
  facet_wrap(~var_type, ncol = 1, scales = 'free_y') +
  coord_flip() +
  #ggthemes::theme_tufte()
  theme_minimal() + theme(
    legend.position="bottom")


### example https://stackoverflow.com/questions/58657802/forest-plot-with-subgroups-in-ggplot2

p.demo.bar <- df_finalAux |> select(m4_l1, patient_race, patient_sex) |>
  drop_na() |>
  pivot_longer(cols = starts_with('patient'),
              names_to = 'variable',
              values_to = 'value') |>
  group_by(variable,value,m4_l1) |>
  summarize(n = n()) |>
  ungroup() |>
  mutate(profile = factor(m4_l1, levels = c(1,2,3,4))) |>
  ggplot(aes(x = value, fill = profile, y = n)) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  # labs(y = 'Percent of profiles in demographic category') +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() + theme(
    legend.position = 'bottom',
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 12))
  

p.demo.bar2 <- df_finalAux |> select(m4_l1, provider_race, provider_sex) |>
  drop_na() |>
  pivot_longer(cols = starts_with('provider'),
               names_to = 'variable',
               values_to = 'value') |>
  group_by(variable,value,m4_l1) |>
  summarize(n = n()) |>
  ungroup() |>
  mutate(profile = factor(m4_l1, levels = c(1,2,3,4))) |>
  ggplot(aes(x = value, fill = profile, y = n)) +
  geom_bar(position = 'fill', stat = 'identity') +
  # labs(y = 'Percent of profiles in demographic category') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() + theme(
    legend.position = 'bottom',
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 12))



bar.y.axis.label <- ggplot(data.frame(l = 'Percent of profiles in demographic category', x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")

p.demo.bars <- wrap_elements(bar.y.axis.label + (p.demo.bar / p.demo.bar2) + 
  plot_layout(guides = 'collect', widths = c(1,20)) & theme(legend.position = 'bottom'))
p.fp.demo + p.demo.bars + 
  plot_annotation(
    # title = '**Figure X**. Demographic and control variables by PCP Speech Profile',
    # caption = "**Bolded** varialbes are significant at p < 0.05", 
    tag_levels = 'A',
    theme = theme(
      # plot.caption = element_markdown(),
      plot.title = element_markdown()))

########################################
########################################
###### PRO outcomes
########################################
########################################

# l2_pro <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'L2_to_PRO_df')
# l2_pro_coefs <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'L2_to_PRO_df') |>
#   mutate(pro_var = str_remove_all(pro_var, pattern = fixed('(1)',ignore_case = TRUE))) |>
#   rename(coef = coeficient) |>
#   select(profile, pro_var, coef,p) |>
#   pivot_wider(
#     id_cols = c(pro_var,p), 
#     names_from = profile, 
#     values_from = coef,
#     names_prefix = 'coef_'
#   )
# 
# df_finalAux |> select(m4_l2, listen, und_stnd, respect, spnd_tm) |>
#   drop_na() |>
#   pivot_longer(
#     cols = all_of(c('listen', 'und_stnd', 'respect', 'spnd_tm')),
#     names_to = "pro_var",
#     values_to = "value"
#   ) |>
#   group_by(pro_var,m4_l2) |>
#   summarize(
#     n = n(), 
#     tot_value = sum(value),
#     perc_top_box = (tot_value / n)*100) |> ungroup() |>
#   pivot_wider(
#     id_cols = pro_var, 
#     names_from = m4_l2, 
#     values_from = perc_top_box,
#     names_prefix = 'l2_class_') |>
#   rowwise() |>
#   # mutate(total = sum(across(c(profile_1,profile_2,profile_3,profile_4)))) |>
#   # mutate(across(starts_with('profile'), ~(.x/total)*100)) |>
#   left_join(l2_pro_coefs, by = 'pro_var') |>
#   # relocate(any_of(c('profile_1_coef','profile_2_coef','profile_3_coef','profile_4_coef')), .after = patient_race) |>
#   relocate(any_of(c('coef_1','coef_2','coef_3','coef_4')), .after = pro_var) |>
#   relocate(l2_class_1, .after = coef_1) |>
#   relocate(l2_class_2, .after = coef_2) |>
#   relocate(l2_class_3, .after = coef_3) |>
#   relocate(l2_class_4, .after = coef_4) |>
#   gt::gt() |>
#   gtExtras::gt_plt_bar_pct(
#     column = l2_class_1,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#F8766D'#"forestgreen"
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = l2_class_2,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#7CAE00'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = l2_class_3,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#00BFC4'
#   ) |>
#   gtExtras::gt_plt_bar_pct(
#     column = l2_class_4,
#     scaled = TRUE,
#     labels = TRUE,
#     fill = '#C77CFF'
#   ) |>
#   gtExtras::gt_plt_point(coef_1, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_2, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_3, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_4, accuracy = .1, width = 25)



#################### Forrest plots
#l2_pro_coefs <- readxl::read_excel('Model_summaries_04-06-2024.xlsx',sheet = 'L2_to_PRO_df_v2') |>
  
l2_pro_coefs <- readxl::read_excel('Model_sum_last_pass_06-01-2024.xlsx',sheet = 'L2_to_PRO_df') |>
  mutate(
    pro_var = str_remove_all(pro_var, pattern = fixed('(1)',ignore_case = TRUE)),
    pro_var2 = case_match(pro_var,
                         'und_stnd' ~ 'Understand',
                         'spnd_tm' ~ 'Spend time',
                         'listen' ~ 'Listen',
                         'respect' ~ 'Respect',
                         .default = pro_var),
    #pro_var = factor(pro_var, levels = c('spnd_tm','respect','und_stnd','listen'))
    pro_var2 = if_else(wald_sig < 0.05, glue::glue("**{pro_var2}**"),pro_var2),
    m4_l2 = profile,
    profile = factor(profile, levels = c(1,2,3,4)),
    lower  = coef - se*1.96,
    upper = coef + se*1.96
    ) |>
  select(profile, pro_var, pro_var2, coef,se,p, lower, upper,m4_l2) 

#define colours for dots and bars
barCOLS = c("#F8766D","#7CAE00","#00BFC4","#C77CFF")
dotCOLS = c("#fcc8c5","#d8ff79","#81fcff","#e9cbff")

p.fp.pro <- ggplot(l2_pro_coefs, aes(x=pro_var2, y=coef, ymin=lower, ymax=upper,col=profile,fill=profile)) + 
  #specify position here
  geom_linerange(size=3,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Patient reported outcome variables") +
  scale_y_continuous(name="Coefficients", limits = c(-1, 1)) +
  coord_flip() +
  labs(caption = "**Bolded** variable names are significant at p < 0.05"#,
       # title = '**Figure X.** PCP Speech Profile Class coefficients\nfor each Patient Reported Outcome'
       ) +
  theme_minimal() + 
  theme(
    legend.position="bottom",
    axis.text.y = element_markdown(),
    plot.title = element_markdown(),
    plot.caption = element_markdown(lineheight = 1.2)
    )

### example https://stackoverflow.com/questions/58657802/forest-plot-with-subgroups-in-ggplot2
# p.bars.pro <- df_finalAux |> select(m4_l2, listen, und_stnd, respect, spnd_tm) |>
#   drop_na() |>
#   pivot_longer(
#     cols = all_of(c('listen', 'und_stnd', 'respect', 'spnd_tm')),
#     names_to = "pro_var",
#     values_to = "value"
#   ) |>
#   group_by(pro_var,m4_l2) |>
#   summarize(
#     n = n(), 
#     tot_value = sum(value),
#     perc_top_box = (tot_value / n)*100) |> ungroup() |>
#   left_join(l2_pro_coefs, by = c('pro_var', 'm4_l2')) |>
#   ggplot(aes(x = profile, y = perc_top_box, fill = profile)) +
#   geom_bar(stat = 'identity') + ylim(0,100) +
#   scale_color_manual(values = barCOLS) +
#   facet_wrap(~pro_var2, ncol = 1) +
#   labs(
#     title = "Percentage of patients with top-box\nratings by PCP speech profile class",
#     fill = 'PCP Speech profile class:',
#     x = '',
#     y = 'Percentage of patients reporting a top-box score') +
#   theme_minimal() + theme(
#     legend.position = 'bottom',
#     strip.text.x = element_markdown())
# 
# p.fp.pro + p.bars.pro + 
#   plot_annotation(
#     title = '**Figure X**. Patient Reported Outcomes by PCP Speech Profile Class',
#     caption = "**Bolded** varialbes are significant at p < 0.05", 
#     tag_levels = 'A',
#     theme = theme(
#       plot.caption = element_markdown(),
#       plot.title = element_markdown()))
#   

########################################
########################################
###### RIAS continuous
########################################
########################################

# RIAS_to_L2 <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'RIAS_to_L2')
# # RIAS_to_L2 <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'RIAS_to_L2')
# 
# rias_cont_coefs <- RIAS_to_L2 |>
#   filter(var_type == 'cont') |>
#   select(profile, rias_var, coef,p) |>
#   pivot_wider(
#     id_cols = c(rias_var,p), 
#     names_from = profile, 
#     values_from = coef,
#     names_prefix = 'coef_'
#   )
# 
# 
# p_rias_cont <- df_finalAux |> select(m4_l2, ptcent1, turndens, turndur, verbdom) |>
#   drop_na() |>
#   pivot_longer(
#     cols = all_of(c('ptcent1', 'turndens', 'turndur', 'verbdom')),
#     names_to = "rias_var",
#     values_to = "value"
#   ) |>
#   group_by(rias_var,m4_l2) |>
#   summarize(
#     distribution = list(value)) |> ungroup() |>
#   pivot_wider(
#     id_cols = rias_var, 
#     names_from = m4_l2, 
#     values_from = distribution,
#     names_prefix = 'l2_class_') |>
#   rowwise() |>
#   # mutate(total = sum(across(c(profile_1,profile_2,profile_3,profile_4)))) |>
#   # mutate(across(starts_with('profile'), ~(.x/total)*100)) |>
#   left_join(rias_cont_coefs, by = 'rias_var') |>
#   # relocate(any_of(c('profile_1_coef','profile_2_coef','profile_3_coef','profile_4_coef')), .after = patient_race) |>
#   relocate(any_of(c('coef_1','coef_2','coef_3','coef_4')), .after = rias_var) |>
#   relocate(l2_class_1, .after = coef_1) |>
#   relocate(l2_class_2, .after = coef_2) |>
#   relocate(l2_class_3, .after = coef_3) |>
#   relocate(l2_class_4, .after = coef_4) |>
#   gt::gt() |>
#   gtExtras::gt_plt_dist(l2_class_1, fill_color = '#F8766D', type = 'boxplot') |>
#   gtExtras::gt_plt_dist(l2_class_2, fill_color = '#7CAE00', type = 'boxplot') |>
#   gtExtras::gt_plt_dist(l2_class_3, fill_color = '#00BFC4', type = 'boxplot') |>
#   gtExtras::gt_plt_dist(l2_class_4, fill_color = '#C77CFF', type = 'boxplot') |>
#   gtExtras::gt_plt_point(coef_1, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_2, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_3, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_4, accuracy = .1, width = 25)


################# Forrest plots

#RIAS_to_L2 <- readxl::read_excel('Model_summaries_04-06-2024.xlsx',sheet = 'RIAS_to_L2') |>
  
RIAS_to_L2 <- readxl::read_excel('Model_sum_last_pass_06-01-2024.xlsx',sheet = 'RIAS_to_L2') |>
  mutate(
    profile = factor(profile, levels = c(1,2,3,4)),
    lower = coef - se*1.96,
    upper = coef + se*1.96,
    var_group = case_when(
      str_detect(rias_var,pattern = "verbdom|turndur|turndens") ~ 'Talk distribution',
      # str_detect(rias_var, pattern = "posd|negd|emod") ~ 'Emotion?',
      str_detect(rias_var, pattern = 'infopsyd|infomedd') ~ 'Information giving',
      # str_detect(rias_var, pattern = "chitd|bcd") ~ 'Do these go together?',
      str_detect(rias_var, pattern = "rapportx|chitd|posd|negd|emod") ~ 'Rapport building',
      str_detect(rias_var, pattern = "ptcent1") ~ 'Patient centeredness',
                           TRUE ~ rias_var),
    var_group = factor(var_group, levels = c('Patient centeredness','Rapport building','Talk distribution',
                                             'Information giving')),
    rias_var2 = case_match(rias_var,
                           "rapportx" ~ "Rapport",
                           "ptcent1" ~ "Patient centeredness",
                           "verbdom" ~ "Verbal dominance",
                           "turndur" ~ "Turn duration",
                           "turndens" ~ "Turn density",
                           "chitd" ~ "Chit chat",
                           #"bcd" ~ "Backchanneling",
                           "posd" ~ "Positive talk",
                           "negd" ~ "Negative talk",
                           "emod" ~ "Emotional talk",
                           "infopsyd" ~ "Psychosocial",
                           "infomedd" ~ "Medical",
                           .default = rias_var),
    rias_var2 = if_else(wald_sig < 0.05, glue::glue("**{rias_var2}**"),rias_var2)
  ) #|> filter(rias_var != 'bcd')

#define colours for dots and bars
barCOLS = c("#F8766D","#7CAE00","#00BFC4","#C77CFF")
dotCOLS = c("#fcc8c5","#d8ff79","#81fcff","#e9cbff")

p.fp.rias <- ggplot(RIAS_to_L2, aes(x=rias_var2, y=coef, ymin=lower, ymax=upper,col=profile,fill=profile)) + 
  #specify position here
  geom_linerange(size=3,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="RIAS variables") +
  scale_y_continuous(name="Coefficient", limits = c(-1.4, 1.4)) +
  coord_flip() +
  facet_wrap(~var_group, ncol = 1, scales = 'free_y') +
  #ggthemes::theme_tufte()
  theme_minimal() + theme(
    legend.position="bottom",
    axis.text.y = element_markdown(),
    plot.title = element_markdown()
    ) +
  labs(
    # title = "**Figure X.** PCP Speech Profile Class\nCoeficients for each RIAS variable",
    fill = 'PCP Speech profile class:',
    col = 'PCP Speech profile class:')


p.rias.tot.box <- df_finalAux |> select(m4_l2, ptcent1, turndens, 
                                     turndur, verbdom, emod, posd, negd, 
                                     chitd, rapportx, infomedd, infopsyd, bcd) |>
  drop_na() |>
  mutate(across(.cols = all_of(c('ptcent1', 'turndens', 
                                 'turndur', 'verbdom','emod', 'posd', 
                                 'negd', 'chitd', 'rapportx', 'infomedd', 'infopsyd', 'bcd')), ~ as.numeric(datawizard::standardize(.x,
                                                                      robust = FALSE,
                                                                      two_sd = FALSE,
                                                                      weights = NULL,
                                                                      reference = NULL,
                                                                      center = NULL,
                                                                      scale = NULL,
                                                                      verbose = TRUE)))) |>
  pivot_longer(
    cols = all_of(c('ptcent1', 'turndens', 
                    'turndur', 'verbdom','emod', 'posd', 
                    'negd', 'chitd', 'rapportx', 'infomedd', 'infopsyd', 'bcd')),
    names_to = "rias_var",
    values_to = "value"
  ) |> 
  ##############
  filter(value < 28) |> ############## One big outlier here; makes graphs VERY smooshed if
  ##############
  mutate(profile = factor(m4_l2, levels = c(1,2,3,4))) |>
  ggplot(aes(x = profile, y = value, fill = profile)) +
  geom_boxplot() +
  scale_color_manual(values = barCOLS) +
  facet_wrap(~rias_var) +
  theme_minimal() + theme(legend.position = 'bottom') +
  labs(title = "RIAS variable by PCP Speech Profile Class",
       fill = "PCP Speech Profile Class")

p.fp.rias + p.rias.tot.box + 
  plot_annotation(
    title = '**Figure X**. RIAS variables by PCP Speech Profile Class',
    caption = "**Bolded** varialbes are significant at p < 0.05", 
    tag_levels = 'A',
    theme = theme(
      plot.caption = element_markdown(),
      plot.title = element_markdown()))

########################################
########################################
###### RIAS count
########################################
########################################

# RIAS_to_L2 <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'RIAS_to_L2')
# RIAS_to_L2 <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'RIAS_to_L2')


# rias_count_coefs <- RIAS_to_L2 |>
#   filter(var_type == 'count') |>
#   select(profile, rias_var, coef, p) |>
#   pivot_wider(
#     id_cols = c(rias_var,p), 
#     names_from = profile, 
#     values_from = coef,
#     names_prefix = 'coef_'
#   )
# 
# 
# p_rias_count <- df_finalAux |> select(m4_l2, emod, posd, negd, chitd, rapportx, infomedd, infopsyd, bcd) |>
#   drop_na() |>
#   pivot_longer(
#     cols = all_of(c('emod', 'posd', 'negd', 'chitd', 'rapportx', 'infomedd', 'infopsyd', 'bcd')),
#     names_to = "rias_var",
#     values_to = "value"
#   ) |>
#   group_by(rias_var,m4_l2) |>
#   summarize(
#     distribution = list(value)) |> ungroup() |>
#   pivot_wider(
#     id_cols = rias_var, 
#     names_from = m4_l2, 
#     values_from = distribution,
#     names_prefix = 'l2_class_') |>
#   rowwise() |>
#   # mutate(total = sum(across(c(profile_1,profile_2,profile_3,profile_4)))) |>
#   # mutate(across(starts_with('profile'), ~(.x/total)*100)) |>
#   left_join(rias_count_coefs, by = 'rias_var') |>
#   # relocate(any_of(c('profile_1_coef','profile_2_coef','profile_3_coef','profile_4_coef')), .after = patient_race) |>
#   relocate(any_of(c('coef_1','coef_2','coef_3','coef_4')), .after = rias_var) |>
#   relocate(l2_class_1, .after = coef_1) |>
#   relocate(l2_class_2, .after = coef_2) |>
#   relocate(l2_class_3, .after = coef_3) |>
#   relocate(l2_class_4, .after = coef_4) |>
#   gt::gt() |>
#   gtExtras::gt_plt_dist(l2_class_1, fill_color = '#F8766D', type = 'density') |>
#   gtExtras::gt_plt_dist(l2_class_2, fill_color = '#7CAE00', type = 'density') |>
#   gtExtras::gt_plt_dist(l2_class_3, fill_color = '#00BFC4', type = 'density') |>
#   gtExtras::gt_plt_dist(l2_class_4, fill_color = '#C77CFF', type = 'density') |>
#   gtExtras::gt_plt_point(coef_1, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_2, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_3, accuracy = .1, width = 25) |>
#   gtExtras::gt_plt_point(coef_4, accuracy = .1, width = 25)
# 
# 
# p5 <- p_rias_count |> bstfun::as_ggplot()
# p6 <- p_rias_cont |> bstfun::as_ggplot()
# library(patchwork)
# p_rias_comb <- p5 / p6

########################################
########################################
###### OLD line plots
########################################
########################################

#demo_df_l2 <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'demo_to_l2_df') %>%
demo_df_l2 <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'demo_to_l2_df') %>%
  mutate(var_type = case_when(
    str_detect(demo_var,'tot_wc') ~ 'tot_wc',
    str_detect(demo_var,'patient_sex') ~ 'patient_sex',
    str_detect(demo_var,'patient_race') ~ 'patient_race',
    str_detect(demo_var,'patient_age') ~ 'patient_age',
    str_detect(demo_var,'provider_sex') ~ 'provider_sex',
    str_detect(demo_var,'provider_race') ~ 'provider_race',
    .default = demo_var)
  )

demo_df_l2 %>%
  mutate(
    profile = as.factor(profile),
    sig = if_else(p < .05,TRUE,FALSE)
  ) %>%
  #filter(var_type %in% c('tot_wc','patient_age')) %>%
  #filter(var_type %in% c('provider_sex')) %>%
  filter(var_type %in% c('provider_race')) %>%
  #filter(var_type %in% c('patient_sex')) %>%
  #filter(var_type %in% c('patient_race')) %>%
  #filter(str_detect(var_type,'study')) %>%
  ggplot(aes(fill = profile, y = coef, x = demo_var)) +
  geom_bar(stat = 'identity',position = 'dodge') +
  # ggplot(aes(x = var_type, y = coef, group = profile, color = profile, shape = sig)) +
  # geom_line() + 
  ggthemes::theme_tufte() + labs(title = 'L2 profiles and Demographic measures')


#l2_pro <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'L2_to_PRO_df')
l2_pro <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'L2_to_PRO_df')

l2_pro %>%
  mutate(
    profile = as.factor(profile),
    sig = if_else(p < .05,TRUE,FALSE)
    ) %>%
  ggplot(aes(x = pro_var, y = coeficient, group = profile, color = profile, shape = sig)) +
  geom_line() + ggthemes::theme_tufte() + labs(title = 'L2 profiles and CAPS measures')

#l1_pro <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'L1_to_PRO_df')
l1_pro <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'L1_to_PRO_df')

l1_pro %>%
  mutate(
    profile = as.factor(profile),
    sig = if_else(p < .05,TRUE,FALSE)
  ) %>%
  ggplot(aes(x = pro_var, y = coeficient, group = profile, color = profile, shape = sig)) +
  geom_line() + ggthemes::theme_tufte() + labs(title = 'L1 profiles and CAPS measures')

#RIAS_to_L1 <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'RIAS_to_L1')
RIAS_to_L1 <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'RIAS_to_L1')

RIAS_to_L1 %>%
  filter(var_type == 'count') %>%
  mutate(
    profile = as.factor(profile),
    sig = if_else(p < .05,TRUE,FALSE)
  ) %>%
  arrange(desc(p)) %>%
  ggplot(aes(x = rias_var, y = coef, group = profile, color = profile, shape = sig)) +
  geom_line() + ggthemes::theme_tufte() + labs(title = 'L1 profiles and RIAS COUNT measures')
RIAS_to_L1 %>%
  filter(var_type == 'cont') %>%
  mutate(
    profile = as.factor(profile),
    sig = if_else(p < .05,TRUE,FALSE)
  ) %>%
  ggplot(aes(x = rias_var, y = coef, group = profile, color = profile, shape = sig)) +
  geom_line() + ggthemes::theme_tufte() + labs(title = 'L1 profiles and RIAS CONTINUOUS measures')

#RIAS_to_L2 <- readxl::read_excel('Model_summaries_12-13-2023.xlsx',sheet = 'RIAS_to_L2')
RIAS_to_L2 <- readxl::read_excel('Model_summaries_with_study_12-19-2023.xlsx',sheet = 'RIAS_to_L2')

RIAS_to_L2 %>%
  filter(var_type == 'count') %>%
  mutate(
    profile = as.factor(profile),
    sig = if_else(p < .05,TRUE,FALSE)
  ) %>%
  ggplot(aes(x = rias_var, y = coef, group = profile, color = profile, shape = sig)) +
  geom_line() + ggthemes::theme_tufte() + labs(title = 'L2 PCP Classes and RIAS COUNT measures')

RIAS_to_L2 %>%
  filter(var_type == 'cont') %>%
  mutate(
    profile = as.factor(profile),
    sig = if_else(p < .05,TRUE,FALSE)
  ) %>%
  ggplot(aes(x = rias_var, y = coef, group = profile, color = profile, shape = sig)) +
  geom_line() + ggthemes::theme_tufte() + labs(title = 'L2 PCP Classes and RIAS CONTINUOUS measures')


RIAS_to_L2 %>%
  filter(var_type == 'count') %>%
  mutate(
    profile = as.factor(profile),
    sig = if_else(p < .05,TRUE,FALSE)
  ) %>%
  # ggplot(aes(fill = profile, y = coef, x = rias_var)) +
  # geom_bar(stat = 'identity',position = 'dodge') +
  ggplot(aes(x = rias_var, y = coef, group = profile, color = profile, shape = sig)) +
  ggthemes::theme_tufte() + labs(title = 'L2 PCP Classes and RIAS CONTINUOUS measures')




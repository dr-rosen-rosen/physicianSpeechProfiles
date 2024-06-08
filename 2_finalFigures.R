final_df <- haven::read_sav('NEWprofile_auxVarWithRias_LG_withTotAf_11-09-2023.sav')
names(final_df)
# L! bar charts faceted by profile
l1_by_liwc_plot <- final_df %>%
  dplyr::select(drives:wc, m4_l1) %>%
  drop_na() %>%
  pivot_longer(
    cols = drives:wc,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    L1 = as.factor(m4_l1),
    variable = ordered(tolower(variable), levels = c("affect", "drives","social","cognition","big_word","wc"))
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

# L2 profiles by L1 composition
l2_by_l1_plot <- final_df %>% 
  janitor::clean_names() %>%
  select(m4_l1,m4_l2_rc) %>%
  drop_na() %>%
  mutate(
    l1 = as.factor(m4_l1),
    l2 = as.factor(m4_l2_rc)
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

l1_by_l2_tbl <- final_df %>% 
  janitor::clean_names() %>%
  select(m4_l1,m4_l2_rc) %>%
  drop_na() %>%
  mutate(
    l1 = as.factor(m4_l1),
    l2 = as.factor(m4_l2_rc)
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
library(patchwork)
l1_by_liwc_plot / (l2_by_l1_plot + l1_by_l2_tbl) + plot_annotation(tag_levels = 'A')

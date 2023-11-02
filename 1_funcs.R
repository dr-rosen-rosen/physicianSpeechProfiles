get_combined_data <- function() {
  df_lpa_echo1 <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/ECHO1_Study/ECHO_LSM_MLM_V3.csv') %>%
    mutate(
      provider_id = paste0('ECHO1_',(str_sub(File, 1, 4))),
      study = 'ECHO1') %>%
    janitor::clean_names() %>%
    # creates 'verbal dominance' measure; >1 is more pcp talk; <1 is more pt talk
    group_by(file) %>%
    mutate(pcp_to_pt_wc = wc[speaker == 'D']/wc[speaker == 'P']) %>%
    ungroup() %>%
    
    filter(speaker == 'D') %>%
    select(-c(x,segment,speaker,text))
  table(df_lpa_echo1$provider_id)
  
  df_lpa_echo3 <- read.csv(here('/Users/mrosen44/Documents/Data_Analysis_Local/Patient_MD_Com/LIWC-22 Results - final_echo3_conversational - LIWC Analysis.csv')) %>%
    mutate(
      provider_id = paste0('ECHO3_',(str_sub(file_id, -2))),
      speaker = case_match(speaker, 'doctor' ~ 'D',.default = speaker),
      study = 'ECHO3') %>%
    janitor::clean_names() %>%
    rename('file' = 'file_id') %>%
    # creates 'verbal dominance' measure; >1 is more pcp talk; <1 is more pt talk
    group_by(file) %>%
    mutate(pcp_to_pt_wc = wc[speaker == 'D']/wc[speaker == 'patient']) %>%
    ungroup() %>%
    
    filter(speaker == 'D') %>%
    select(-c(x,speaker,text, column_id,segment)) %>%
    select(-contains('x_')) # not sure where all the extra columns are coming from, but they are empty
  table(df_lpa_echo3$provider_id)
  
  df_lpa_bb <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/blackbox_study/blackbox_conv_liwc.csv') %>%
    mutate(
      provider_id = paste0('BB_',str_sub(File, 1, 3)), # need to update with correct parsing of file_id
      Speaker = case_match(Speaker, 'pcp' ~ 'D',.default = Speaker),
      study = 'BB') %>%
    janitor::clean_names() %>%
    # creates 'verbal dominance' measure; >1 is more pcp talk; <1 is more pt talk
    group_by(file) %>%
    mutate(pcp_to_pt_wc = wc[speaker == 'D']/wc[speaker == 'patient']) %>%
    ungroup() %>%
    
    filter(speaker == 'D') %>%
    select(-c(x,segment,speaker,text))
  
  df_lpa_mar <- read.csv('/Users/mrosen44/Johns Hopkins/Salar Khaleghzadegan - Patient _Provider_Communication_Projects/maripohsa_study/maripohsa_conv_liwc.csv') %>%
    mutate(
      provider_id = paste0('MAR_',str_sub(File, 1, 2)),
      Speaker = case_match(Speaker, 'doctor' ~ 'D',.default = Speaker),
      study = 'MAR'
    ) %>%
    janitor::clean_names() %>%
    # creates 'verbal dominance' measure; >1 is more pcp talk; <1 is more pt talk
    group_by(file) %>%
    mutate(pcp_to_pt_wc = wc[speaker == 'D']/wc[speaker == 'patient']) %>%
    ungroup() %>%
    
    filter(speaker == 'D') %>%
    select(-c(x,segment,speaker,text))
  
  df_lpa <- rbind(df_lpa_bb,df_lpa_echo1,df_lpa_echo3,df_lpa_mar)
  df_lpa$provider_id <- factor(df_lpa$provider_id, ordered = FALSE)
  df_lpa <- df_lpa %>%
    group_by(provider_id) %>%
    mutate(PCP_ID = cur_group_id()) %>% ungroup()  %>%
    mutate(row_num = row_number()) %>%
    relocate(study,provider_id, PCP_ID, row_num)
  return(df_lpa)
}

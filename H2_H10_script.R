

library(tidyverse)

#load anon jatos data 
df<- read_csv('data/anon_jatos_results_p1.csv')
df2<- read_csv('data/anon_jatos_results_p2.csv')

df <- rbind(df, df2)
rm(df2)

#load anon prolific data 
demo_prolific <- read.csv('data/anon_demo_prolific.csv')


# create OCI data ---------------------------------------------------------

#parse oci data from df 
OCI_df <- df %>%
  filter(grepl('oci', Identifier)) %>% 
  select(subj_id, question, answer_oci) %>% 
  mutate(attention_check = question %in% c("OCI-Attention_check_1","OCI-Attention_check_2")) %>%
  arrange(subj_id, question)%>%
  #arrange it in a long format df 
  group_by(subj_id) %>%
  summarise(oci_overall=sum(as.numeric(answer_oci[!attention_check])), #compute oci general score 
            failed_attention_OCI = answer_oci[question=='OCI-Attention_check_1']!=0 | #attention check failure
              answer_oci[question=='OCI-Attention_check_2']!=2,
            OCI_Absent = answer_oci[question=='OCI-Absent']) %>%
  mutate(
    #split to quartiles based on oci score
    OCI_quantile = as.numeric(cut(oci_overall,
                                  breaks = quantile(oci_overall[!failed_attention_OCI],
                                                    probs = seq(0, 1, 0.25),
                                                    na.rm = T,
                                                    type = 9),
                                  right = F, 
                                  include.lowest = T,
                                  include.highest = T))
  )

# create DASS data ---------------------------------------------------------

#create vectors for DASS subscales. 
DASS_anxiety <- c("DASS-4","DASS-7", "DASS-9", "DASS-15", "DASS-19", "DASS-20")
DASS_depression <- c("DASS-3","DASS-5", "DASS-10", "DASS-13", "DASS-16", "DASS-17","DASS-21")

#parse DASS data from df
DASS_df <- df %>%
  filter(grepl('DASS', Identifier)) %>% 
  select(subj_id, question, answer_dass) %>% 
  arrange(subj_id, question) %>%
  #arrange in a new df 
  group_by(subj_id)%>%
  summarise(DASS_anxiety = sum(as.numeric(answer_dass[question %in% DASS_anxiety])),
            DASS_depression = sum(as.numeric(answer_dass[question %in% DASS_depression])))

# unite dass and oci
questionnaires_df <- left_join(OCI_df, DASS_df,by = "subj_id")


# df of explicit difficulty rating---------------------------------------------------

#get data from df 
difficulty_rating <- df %>% 
  filter(trial_type == "html-slider-response") %>% 
  select(subj_id,test_part,response) %>%
  #retrieve variable names. 
  mutate(difficulty_rating = case_when(str_detect(test_part,"PresentOinC25") ~ "OinC_25_TP",
                                       str_detect(test_part,"Absent09") ~ "CinO_9_TA",
                                       str_detect(test_part,"PresentOinC09") ~ "OinC_9_TP",
                                       str_detect(test_part,"Absent25") ~ "CinO_25_TA")) %>% 
  select(-test_part) %>% 
  
  #adding OCI quantile to difficulty df 
  left_join(questionnaires_df %>% 
              group_by(subj_id) %>%
              summarise(OCI_quantile=first(OCI_quantile)) %>% 
              select(subj_id, OCI_quantile), 
            difficulty_rating,
            by = "subj_id")

# visual search df -------------------------------------------------------------

search_df <- df %>% 
  filter(trial_type == "p5vs_yn_small_grid") %>%
  select(trial_type,trial_index,subj_id,sequence,target_position,
         subj_id,set_size,target_present,
         RT,test_part,correct,search_type) %>% 
  # unite search and questionnaire dfs together 
  left_join(questionnaires_df,
            by = "subj_id") %>% 
  #unite search df with demo prolific 
  left_join(.,demo_prolific,
            by = "subj_id")

#recode variables 
search_df$target_present<-search_df$target_present=='TRUE'
search_df$RT <- as.numeric(search_df$RT)
search_df$correct <- as.numeric(as.logical(search_df$correct))
search_df$set_size <- as.factor(search_df$set_size)


# Rejecting participants with more than 15% error trials -----------------------

rejection_df <- search_df %>% 
  group_by(subj_id) %>% 
  summarise(accuracy = mean(correct)) %>% 
  mutate(reject = accuracy<0.85)

rejected_num <- sum(rejection_df$reject=='TRUE')


# flag bad trails
search_df <- search_df %>% rowwise() %>% 
  mutate(under_100 = as.numeric(RT <= 100)) 


#filter out participants for accuracy and attention check 
search_df <- left_join(search_df, 
                       rejection_df %>% select(subj_id, reject), 'subj_id') %>%
  # filter(reject=='FALSE' | failed_attention_OCI == 1)
  filter(reject=='FALSE' & failed_attention_OCI == 0) %>%
  filter(under_100 == 0) #filter our trials 


# Summary dfs  ------------------------------------------------------------

#create a df in which every participant gets a mean score for each search option (8)
search_summary_mean_RT <- 
  search_df %>% 
  filter(correct==1) %>% #only correct trials 
  group_by(subj_id, set_size,search_type, target_present, OCI_quantile) %>% 
  summarise(mean_rt = mean(RT, na.rm = T)) %>%
  ungroup() 

# Extracting slopes ------------------------------------------------------------------

#Reordering set size to get the slopes for the effect of going from 9 to 25. 
search_df$set_size <- relevel(as.factor(search_df$set_size), '9')

#extracting slopes 
search_slopes_df <- search_df %>%
  filter(correct==1) %>% 
  group_by(subj_id,search_type,target_present) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(RT ~ set_size, data =.)),
         tidy = map(model, ~ tidy(.x))) %>%
  unnest(tidy) %>%
  # we are interested in the slope, i.e., the effect of set size.
  filter(term=='set_size25')

search_slopes_df<- left_join(search_slopes_df, questionnaires_df %>% 
                               select(subj_id, OCI_quantile, oci_overall, DASS_depression, DASS_anxiety),
                             by='subj_id') 


H2.df<- search_slopes_df %>% 
  filter(target_present==TRUE & search_type=='OinC' |target_present==FALSE & search_type=='CinO')  %>% 
  mutate(group = case_when(search_type == 'OinC' ~ 'hard_present',
                           search_type == 'CinO' ~ 'easy_absent'));

H2.desc <- H2.df %>% 
  group_by(group) %>% 
  summarise(mean_slope=mean(estimate),
            ms_item = mean_slope/(25-9))


#create a df in the same format as the difficulty df, so include only 4 searches of difficulty estimation:
#hard present and easy absent with both set sizes. 

search_correlation_mean <- search_summary_mean_RT %>% 
  filter(search_type=='CinO' & set_size=='9' & target_present=='FALSE'| #easy absent small set size
           search_type=='CinO' & set_size=='25' & target_present=='FALSE'| #easy absent big set size 
           search_type=='OinC' & set_size=='9' & target_present=='TRUE'| #hard present small set size
           search_type=='OinC' & set_size=='25' & target_present=='TRUE') %>% #hard present big set size 
  mutate(target_present =case_when(target_present=='TRUE' ~ 'TP', #change names 
                                   target_present=='FALSE' ~ 'TA')) %>% 
  unite('difficulty_rating', c(search_type, set_size, target_present), remove = F) #create names like in the difficulty df 

#combine with the difficulty df 
search_correlation_mean<- left_join(search_correlation_mean, difficulty_rating %>% select(-OCI_quantile))

search_correlation_wide <- search_correlation_mean %>%
  select(subj_id, set_size, search_type, target_present, OCI_quantile, mean_rt, response) %>% 
  pivot_wider(
    names_from = c(search_type, target_present, set_size),
    values_from = c(mean_rt, response))

search_correlation_wide <- search_correlation_wide %>% 
  mutate(slope_easy_absent = mean_rt_CinO_TA_25 - mean_rt_CinO_TA_9,
         slope_hard_present = mean_rt_OinC_TP_25- mean_rt_OinC_TP_9) 

search_correlation_wide %>% 
  select(slope_easy_absent, slope_hard_present) %>% 
  summarise(mean_slope_easy_absent = mean(slope_easy_absent),
            mean_slope_hard_present = mean(slope_hard_present))


search_correlation_wide %>% 
  select(slope_easy_absent, slope_hard_present, OCI_quantile) %>% 
  group_by(OCI_quantile) %>% 
  summarise(mean_slope_easy_absent = mean(slope_easy_absent),
            mean_slope_hard_present = mean(slope_hard_present))



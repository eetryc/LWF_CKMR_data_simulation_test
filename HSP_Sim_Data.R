# this first (set up of data and all pairs) bit will be completed if you already did the POP_Sim_Data


library(tidyverse)

# load in data table with full information 
original=read.csv("LWF_Info.csv",h=T)

original 


# select columns and add cohort year column
unique_fish <- LWF_final %>%
  distinct(SampleID, .keep_all = TRUE)
LWF_final <- unique_fish %>%
  mutate(Cohort = SampleYear - FinalAge) %>% 
  select(SampleID,Cohort,SampleYear)




# Select n amount fish from each cohort for a first analysis
test_set <- LWF_final %>%
  group_by(Cohort) %>%
  slice_sample(n = 5) %>%
  ungroup()














# Create an initial list of all potential comparisons regardless of filter rules 
all_pair_comps <- test_set %>%
  rename_with(~ paste0(.x, "_1"), everything()) %>%      
  cross_join(test_set %>% rename_with(~ paste0(.x, "_2"), everything())) %>%
  filter(SampleID_1 != SampleID_2)

#get rid of NA
all_pair_comps <- all_pair_comps %>% 
  drop_na()














# More stringent filter 
HSpairs <- test_set %>%
  rename_with(~ paste0(.x, "_1"), everything()) %>% # sort into col for each indiv
  cross_join(test_set %>% rename_with(~ paste0(.x, "_2"), everything())) %>% # remove self-pairings
  mutate(POP_candidate = ifelse(Cohort_1 < Cohort_2 & (Cohort_1 != Cohort_2),"yes","no")) %>% 
  #get rid of the ones that are the same comp, eg 1 and 2 vs 2 and 1 
  filter(SampleID_1 != SampleID_2) %>% 
  mutate(ID_min = pmin(SampleID_1, SampleID_2),ID_max = pmax(SampleID_1, SampleID_2)) %>%
  filter(ID_min != ID_max) %>%
  distinct(ID_min, ID_max, .keep_all = TRUE) %>%
  select(-ID_min, -ID_max) %>% 
  drop_na() %>% 
  mutate(AgeDif = Cohort_2 - Cohort_1) %>% 
  rename(
    Individual_1 = SampleID_1,
    Individual_2 = SampleID_2
  )


# count numnber of yes or no to get a better idea of how they are changing
HSpairs %>% 
  dplyr::count(POP_candidate)

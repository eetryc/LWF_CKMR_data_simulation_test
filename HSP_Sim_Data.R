# this first (set up of data and all pairs) bit will be completed if you already did the POP_Sim_Data


library(tidyverse)

# load in data table with full information 
original=read.csv("LWF_Info.csv",h=T)

original_clean <- original %>% 
  drop_na(FinalAge)


# select columns and add cohort year column

LWF_final <- original_clean %>%
  mutate(Cohort = SampleYear - FinalAge) %>% 
  select(SampleID,Cohort,SampleYear,SampleDate,Sex) %>% 
  mutate(SampleDate = mdy(SampleDate),
         Month = month(SampleDate))




# Select n amount fish from each cohort for a first analysis
test_set <- LWF_final %>%
  group_by(Cohort) %>%
  slice_sample(n = 5) %>%
  ungroup()

















# More stringent filter 
HSpairs <- test_set %>%
  rename_with(~ paste0(.x, "_1"), everything()) %>% # sort into col for each indiv
  cross_join(test_set %>% rename_with(~ paste0(.x, "_2"), everything())) %>% # remove self-pairings
  mutate(HSP_candidate = ifelse(Cohort_1 < Cohort_2 & (Cohort_1 != Cohort_2),"yes","no")) %>% 
  #get rid of the ones that are the same comp, eg 1 and 2 vs 2 and 1 
  filter(SampleID_1 != SampleID_2) %>% 
  mutate(ID_min = pmin(SampleID_1, SampleID_2),ID_max = pmax(SampleID_1, SampleID_2),PairID = paste(ID_min, ID_max, sep = "_")) %>%
  filter(ID_min != ID_max) %>%
  distinct(ID_min, ID_max, .keep_all = TRUE) %>%
  select(-ID_min, -ID_max) %>% 
  drop_na() %>% 
  mutate(AgeDif = Cohort_2 - Cohort_1) %>% 
  rename(Individual_1 = SampleID_1,Individual_2 = SampleID_2)


# count numnber of yes or no to get a better idea of how they are changing
HSpairs %>% 
  dplyr::count(HSP_candidate)


# join the two tables
kinships <- full_join(POpairs %>% select(PairID,AgeDif,Individual_1,Cohort_1,SampleYear_1,Sex_1,Individual_2,Cohort_2,SampleYear_2,Sex_2,POP_candidate),
                      HSpairs %>% select(PairID,HSP_candidate), by = "PairID")
                    
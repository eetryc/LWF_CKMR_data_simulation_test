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














# Create an initial list of all potential comparisons regardless of filter rules 
all_pair_comps <- test_set %>%
  rename_with(~ paste0(.x, "_1"), everything()) %>%      
  cross_join(test_set %>% rename_with(~ paste0(.x, "_2"), everything())) %>%
  filter(SampleID_1 != SampleID_2) %>% 
  #get rid of the ones that are the same comp, eg 1 and 2 vs 2 and 1 
  filter(SampleID_1 != SampleID_2) %>% 
  mutate(
    ID_min = pmin(SampleID_1, SampleID_2),
    ID_max = pmax(SampleID_1, SampleID_2)
  ) %>%
  filter(ID_min != ID_max) %>%
  distinct(ID_min, ID_max, .keep_all = TRUE) %>%
  select(-ID_min, -ID_max)

















# More stringent filter 
POpairs <- test_set %>%
  rename_with(~ paste0(.x, "_1"), everything()) %>% # add the columns fro each indiv
  cross_join(test_set %>% rename_with(~ paste0(.x, "_2"), everything())) %>% # remove self-pairings
  mutate(conception = ymd(paste0(Cohort_2-1,"-10-01"))) %>% ###### Change as needed to fiddle with conception date
  # mutate to add the POP possibility (y/n) with the year and reproductive age filters
  mutate(POP_candidate = ifelse(Cohort_1 < Cohort_2 & 
                                  (Cohort_2 - Cohort_1 >= 5) & 
                                  (SampleYear_1 >= Cohort_2) & 
                                  (SampleDate_1 >= conception),"yes","no")) %>% 
  #get rid of the ones that are the same comp, eg 1 and 2 vs 2 and 1 
  filter(SampleID_1 != SampleID_2) %>% 
  mutate(ID_min = pmin(SampleID_1, SampleID_2),ID_max = pmax(SampleID_1, SampleID_2),PairID = paste(ID_min, ID_max, sep = "_")) %>%
  filter(ID_min != ID_max) %>%
  distinct(ID_min, ID_max, .keep_all = TRUE) %>%
  select(-ID_min, -ID_max) %>% 
  mutate(AgeDif = Cohort_2 - Cohort_1) %>% 
  rename(Individual_1 = SampleID_1,Individual_2 = SampleID_2) 



# count numnber of yes or no to get a better idea of how they are changing
POpairs %>% 
  dplyr::count(POP_candidate)


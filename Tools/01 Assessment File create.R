library(tidyverse)
library(openxlsx)




# AU to OWRD basin lookup -----------------------------------------------------------------------------------------



load('Data/AU_Basin.Rdata')

# Import rollup file ----------------------------------------------------------------------------------------------


AU_all <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/AU_all_rollup.xlsx")



# Add basin to AU_all ---------------------------------------------------------------------------------------------

AU_all_basin <-AU_all %>%
  left_join(AU_Basin) 


# Reformat table for we display -----------------------------------------------------------------------------------


Parameter_assessments <- AU_all_basin %>%
  select(AU_ID, AU_Name, AU_Description,  OWRD_Basin, Pollu_ID, wqstd_code, Assessment, Pollutant,
         period, DO_Class, stations, AU_parameter_category, Rationale, assessed_2022,year_assessed,  AU_delist, Year_listed, 
         recordID) %>%
  rename(Parameter_category = AU_parameter_category) %>%
  mutate(Rationale = str_replace(Rationale, 'Â°', '° C'))




Parameter_assessments_row_num <- Parameter_assessments %>%
  mutate(row_num = row_number())

Parameter_assessments_duplicates_remove <- Parameter_assessments_row_num %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period, DO_Class, Parameter_category) %>%
  mutate(num = n()) %>%
  filter(num > 1) %>%
  mutate(keep = case_when(year_assessed < max(year_assessed) ~ 0,
                          (!all(is.na(Rationale))) & is.na(Rationale) ~ 0,
                          length(unique(Rationale)) == 1 & row_number() > 1 ~ 0,
                          row_number() > 1 ~ 0,
                          TRUE ~ 1
                          
  )) %>%
  filter(keep == 0)


Parameter_assessments_no_duplicates <- Parameter_assessments_row_num %>%
  filter(!row_num %in% Parameter_assessments_duplicates_remove$row_num)

Parameter_assessments <- Parameter_assessments_no_duplicates


assessed_AUs <- unique(Parameter_assessments$AU_ID)

save(Parameter_assessments, assessed_AUs, file = 'Data/Parameter_assessments.Rdata')



library(tidyverse)
library(openxlsx)




# AU to OWRD basin lookup -----------------------------------------------------------------------------------------



load('Data/AU_Basin.Rdata')

# Import rollup file ----------------------------------------------------------------------------------------------


AU_all <- read.xlsx("//deqHQ1/WQASSESSMENT/2022IRFiles/Draft List/Rollup outputs/Actual Final Final Final/AU_all_rollup.xlsx")



# Add basin to AU_all ---------------------------------------------------------------------------------------------

AU_all_basin <-AU_all %>%
  left_join(AU_Basin) 


# Reformat table for we display -----------------------------------------------------------------------------------


Parameter_assessments <- AU_all_basin %>%
  select(AU_ID, AU_Name, AU_Description,  OWRD_Basin, Pollu_ID, wqstd_code, Assessment, Pollutant,
         period, DO_Class, stations, AU_final_status, Rationale, assessed_2022,year_assessed,  AU_delist, Year_listed, 
         recordID) %>%
  rename(Parameter_category = AU_final_status) %>%
  mutate(Rationale = str_replace(Rationale, 'Â°', '° C'))


assessed_AUs <- unique(Parameter_assessments$AU_ID)

save(Parameter_assessments, assessed_AUs, file = 'Data/Parameter_assessments.Rdata')



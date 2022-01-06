rm(list=ls())


library(tidyverse)
library(openxlsx)


options(scipen = 999999999)


# bacteria --------------------------------------------------------------------------------------------------------



bacteria_coast_contact <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/bacteria coast contact.xlsx",
                                    sheet = 'Coast Bacteria Data_Other') %>%
  arrange(AU_ID, MLocID, SampleStartDate) 


bacteria_fresh_contact <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/bacteria freshwater contact.xlsx",
                                    sheet = 'Fresh Bacteria Data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)


# biocriteria -----------------------------------------------------------------------------------------------------
biocriteria <- read.xlsx('C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/Biocriteria.xlsx',
                         sheet = 'Raw_Scores_ALL')%>%
  arrange(AU_ID, MLocID)


# chl -------------------------------------------------------------------------------------------------------------

chl <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/chl-a.xlsx",
                 sheet = 'Chl-a Raw Data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)


# DO --------------------------------------------------------------------------------------------------------------


DO_cont_spawn <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/DO Spawn.xlsx", 
                           sheet = 'Spawn Cont Data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)

DO_cont_yearround <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/DO Year Round.xlsx",
                               sheet = 'Yr Rnd Cont Data' )%>%
  arrange(AU_ID, MLocID, SampleStartDate)

DO_instant_spawn <-  read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/DO Spawn.xlsx",
                               sheet = 'Spawn Instant Data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)


DO_inst_yearround <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/DO Year Round.xlsx",
                               sheet = 'Yr Rnd Instant Data' )%>%
  arrange(AU_ID, MLocID, SampleStartDate)

# pH --------------------------------------------------------------------------------------------------------------



pH_WS <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/pH.xlsx",
                   sheet = 'pH WS Data')

pH_other <-  read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/pH.xlsx",
                       sheet = 'pH other AU data')

pH <- bind_rows(pH_WS,pH_other )%>%
  arrange(AU_ID, MLocID, Result_Date)


# temp ------------------------------------------------------------------------------------------------------------

temp <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/temperature- data.xlsx")%>%
  arrange(AU_ID, MLocID, SampleStartDate)


# Tox AL ----------------------------------------------------------------------------------------------------------

Tox_AL_Ammonia <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/Tox_AL.xlsx",
                            sheet = 'tox_AL_Ammonia_data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_CU <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/Tox_AL.xlsx",
                       sheet = 'tox_AL_Copper_data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Hardness_Metals <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/Tox_AL.xlsx",
                       sheet = 'tox_AL_hard_data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Others <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/Tox_AL.xlsx",
                           sheet = 'tox_AL_data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Penta <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/Tox_AL.xlsx",
                           sheet = 'tox_AL_penta_data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_aluminum <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/Tox_AL.xlsx",
                             sheet = 'tox_AL_Aluminum_data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)




# Tox HH ----------------------------------------------------------------------------------------------------------

Tox_HH <-  read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/Tox_HH.xlsx", 
                     sheet = 'HH Toxt Data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)


# Turbidity -------------------------------------------------------------------------------------------------------

turbidity_data <-  read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/turbidity.xlsx", 
                     sheet = 'Turb Data')%>%
  arrange(AU_ID, MLocID, SampleStartDate)



# HABS ------------------------------------------------------------------------------------------------------------


Habs_data <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Rollup Assessment/HABs.xlsx")


save(bacteria_coast_contact,bacteria_fresh_contact, chl, DO_cont_spawn, DO_cont_yearround, 
     DO_instant_spawn, DO_inst_yearround, pH, temp,  Tox_AL_Ammonia,Tox_AL_CU,Tox_AL_Hardness_Metals,
     Tox_AL_Others, Tox_AL_Penta, Tox_HH,  biocriteria,turbidity_data,Habs_data, file = "Data/IR_data.Rdata" )



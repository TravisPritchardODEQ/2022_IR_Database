rm(list=ls())


#' This file loads the IR data from 'data/IR_data.Rdata' and creates excel
#' files from it. The resukting excel files should be zipped up and used to
#' create a zip file used in the database data download. This zipped file needs
#' to be names All_data.zip and placed in the data folder.
#' 

library(zip)

load('Data/IR_data.Rdata')



# Temperature -----------------------------------------------------------------------------------------------------


write.xlsx(temp, file = "Temperature.xlsx",
           overwrite = TRUE)




# Bacteria --------------------------------------------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "E coli")
addWorksheet(wb, "Enterococcus")
addWorksheet(wb, "Fecal Coliform")

writeData(wb,"E coli",  bacteria_fresh_contact, rowNames = FALSE)
writeData(wb,"Enterococcus", bacteria_coast_contact, rowNames = FALSE)


saveWorkbook(wb, file = "Bacteria.xlsx", 
             overwrite = TRUE)


# Chl -------------------------------------------------------------------------------------------------------------


write.xlsx(chl, 
           file = "Chlorophyll.xlsx", 
           overwrite = TRUE)


# pH --------------------------------------------------------------------------------------------------------------


write.xlsx( pH, 
            file = "pH.xlsx", 
            overwrite = TRUE)


# DO --------------------------------------------------------------------------------------------------------------



# wb <- createWorkbook()
# addWorksheet(wb, "DO_spawn_continuous")
# addWorksheet(wb, "DO_spawn_instantaneous")
# 
# writeData(wb,"DO_spawn_continuous",  DO_cont_spawn, rowNames = FALSE)
# writeData(wb,"DO_spawn_instantaneous", DO_instant_spawn, rowNames = FALSE)
# 
# saveWorkbook(wb, "DO_Spawning.xlsx", 
#              overwrite = TRUE)




wb <- createWorkbook()
addWorksheet(wb, "DO_yearround_continuous")
addWorksheet(wb, "DO_yearround_instantaneous")
addWorksheet(wb, "DO_spawn_continuous")
addWorksheet(wb, "DO_spawn_instantaneous")

writeData(wb,"DO_yearround_continuous", DO_cont_yearround, rowNames = FALSE)
writeData(wb,"DO_yearround_instantaneous",DO_inst_yearround, rowNames = FALSE)
writeData(wb,"DO_spawn_continuous",  DO_cont_spawn, rowNames = FALSE)
writeData(wb,"DO_spawn_instantaneous", DO_instant_spawn, rowNames = FALSE)

saveWorkbook(wb, file = "DO.xlsx", 
             overwrite = TRUE)



# Tox AL ----------------------------------------------------------------------------------------------------------


wb <- createWorkbook()
addWorksheet(wb, 'Tox_AL_Others')
addWorksheet(wb,'Tox_AL_Ammonia')
addWorksheet(wb,'Tox_AL_CU')
addWorksheet(wb, 'Tox_AL_Hardness_Metals')
addWorksheet(wb, 'Tox_AL_Pentachlorophenol')
addWorksheet(wb, 'Tox_AL_Aluminum')

writeData(wb,  'Tox_AL_Others',Tox_AL_Others, rowNames = FALSE)
writeData(wb, 'Tox_AL_Ammonia',  Tox_AL_Ammonia, rowNames = FALSE)
writeData(wb, 'Tox_AL_CU',  Tox_AL_CU, rowNames = FALSE)
writeData(wb, 'Tox_AL_Hardness_Metals',  Tox_AL_Hardness_Metals, rowNames = FALSE)
writeData(wb, 'Tox_AL_Pentachlorophenol',Tox_AL_Penta, rowNames = FALSE)
writeData(wb, 'Tox_AL_Aluminum',Tox_AL_aluminum, rowNames = FALSE)

saveWorkbook(wb, file = "Aquatic_Life_Toxics.xlsx", 
             overwrite = TRUE)



# Tox HH ----------------------------------------------------------------------------------------------------------




wb <- createWorkbook()
addWorksheet(wb, 'Tox_HH')

writeData(wb, 'Tox_HH',  Tox_HH, rowNames = FALSE)





saveWorkbook(wb, file = "Human_Health_Toxics.xlsx", 
             overwrite = TRUE)



# biocriteria
write.xlsx( biocriteria, 
            file = "Biocriteria.xlsx", 
            overwrite = TRUE)


# Turbidity
write.xlsx( turbidity_data, 
            file = "Turbidity.xlsx", 
            overwrite = TRUE)



# HABs
write.xlsx( Habs_data, 
            file = "HABs.xlsx", 
            overwrite = TRUE)


if (file.exists("data/All_data.zip")) {
  file.remove("data/All_data.zip")
  
}

zip::zip(zipfile = "Data/All_data.zip", files = c("Temperature.xlsx",
                                                  "Bacteria.xlsx",
                                                  "Chlorophyll.xlsx",
                                                  "pH.xlsx",
                                                  "DO.xlsx",
                                                  "Aquatic_Life_Toxics.xlsx",
                                                  "Human_Health_Toxics.xlsx",
                                                  "Biocriteria.xlsx",
                                                  "Turbidity.xlsx",
                                                  "HABs.xlsx") )

file.remove( c("Temperature.xlsx",
               "Bacteria.xlsx",
               "Chlorophyll.xlsx",
               "pH.xlsx",
               "DO.xlsx",
               "Aquatic_Life_Toxics.xlsx",
               "Human_Health_Toxics.xlsx",
               "Biocriteria.xlsx",
               "Turbidity.xlsx",
               "HABs.xlsx"))
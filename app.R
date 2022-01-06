
library(shiny)
library(tidyverse)
library(shinybusy)
library(knitr)
library(kableExtra)
library(shinythemes)
library(shinyWidgets)
library(openxlsx)
library(zip)





# Load in data files ------------------------------------------------------



# Load assessment result data
load("Data/Parameter_assessments.Rdata")
load('Data/IR_data.Rdata')


# populate values in selectize boxes
#get a vector of unique AUs
AU_s = unique(Parameter_assessments$AU_ID)
AU_Names <- sort(unique(Parameter_assessments$AU_Name[Parameter_assessments$AU_Name != "" & Parameter_assessments$AU_Name != "<Null>" ]), na.last = TRUE)

#Get a vector of pollutants
pollutants <- sort(unique(Parameter_assessments$Pollutant))

admin_basins <- sort(unique(Parameter_assessments$OWRD_Basin))
status <-  c("Attains", "Insufficient", "Impaired")


ben_uses <- c(
    "Aesthetic Quality",
    "Fish and Aquatic Life",
    "Fishing",
    "Private Domestic Water Supply",
    "Public Domestic Water Supply",
    "Water Contact Recreation",
    "Boating",
    "Livestock Watering"
)

ben_uses <- c(
    "Aesthetic Quality",
    "Fish and Aquatic Life",
    "Fishing",
    "Private Domestic Water Supply",
    "Public Domestic Water Supply",
    "Water Contact Recreation",
    "Boating",
    "Livestock Watering"
)
# shiny ui section -------------------------------------------------------


#Create the page
ui <- navbarPage("2018/2020 Integrated Report",
                 theme = shinytheme("yeti"),
                 inverse = TRUE,
                 collapsible = TRUE,
                 
                 tabPanel("Assessments",
                          # Application title
                          titlePanel(
                              fluidRow(
                                  column(6, img(src = "logo.png")), 
                                  column(6,  "2018/2020 Integrated Report",style = "font-family: 'Arial'; font-si16pt; vertical-align: 'bottom'")),
                              windowTitle = "2018/2020 Integrated Report"
                          ),
                          
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                              sidebarPanel(
                                  actionButton("go", "Filter",  icon("filter")),
                                  
                                  selectizeInput("AUs",
                                                 "Select Assessment Unit",
                                                 choices = AU_s,
                                                 multiple = TRUE,
                                                 options = list(maxOptions = 7000)),
                                  selectizeInput("Select_AUName",
                                                 "Select AU Name",
                                                 choices = AU_Names,
                                                 multiple = TRUE,
                                                 options = list(maxOptions = 7000)),
                                  selectizeInput("admin_basin_selector",
                                                 "Select Admin Basin",
                                                 choices = admin_basins,
                                                 multiple = TRUE),
                                  selectizeInput("pollutant_selector",
                                                 "Select Pollutant",
                                                 choices = pollutants,
                                                 multiple = TRUE),
                                  selectizeInput("category_selector",
                                                 "Select IR category",
                                                 choices = unique(sort(Parameter_assessments$Parameter_category)),
                                                 multiple = TRUE),
                                  selectizeInput("status_selector",
                                                 "Select Parameter Attainment Status",
                                                 choices =status,
                                                 multiple = TRUE),
                                  
                                  selectizeInput("benuse_selector",
                                                 "Select Beneficial Use",
                                                 choices =ben_uses,
                                                 multiple = TRUE)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              id = "Tabset",
                                              tabPanel("Instructions",
                                                       value = "InstructionTab",
                                                       h2(strong(" Instructions to Review the 2018/2020 Integrated Report Database"), style = "font-family: 'Arial'"),
                                                       p("DEQ recommends using the current version of Google Chrome or Mozilla Firefox for this application.", style = "font-family: 'times'"),
                                                       p("The 2018/2020 Integrated Report Assessment Database contains new assessment information and updates to assessments from 1998, 2002, 2004, 2010, and 2012. (See",
                                                         a("2012 Integrated Report Database", href="https://www.deq.state.or.us/wq/assessment/rpt2012/search.asp", target="_blank"),"). The current assessment categorizations  
                             are described in the “Parameter_category” report field. The “Assessed_in_2018” report field indicates if new data evaluations or assessments were done in 2018/2020, otherwise
                             the status assigned in previous assessments was carried forward from previous reports. Assessment categorized as Category 4 or Category 5 (including all subcategories) are considered impaired.", style = "font-family: 'times'"),
                             p("Click on ", strong("Raw Data Download"), "in the header at the top of this page to access raw data used in 2018/2020 assessments.", style = "font-family: 'times'"),
                             p( 
                                 a("The 2018/2020 Assessment Methodology can be found here.", href="https://www.oregon.gov/deq/FilterDocs/ir2018assessMethod.pdf", target="_blank"), style = "font-family: 'times'"),
                             p("A more complete mapping and dataset, including water quality standards information can be found on the ", 
                               a("Interactive Web Map.", href="https://hdcgcx2.deq.state.or.us/HVR291/?viewer=wqsa#", target="_blank"), style = "font-family: 'times'"),
                             p("The DEQ 2018/2020 IR webpage page can be found", a("here.", href="https://www.oregon.gov/deq/wq/Pages/2018-Integrated-Report.aspx", target="_blank"), style = "font-family: 'times'"),
                             p("Water quality data used in assessments can also be downloaded from ", a("AWQMS.", href="https://www.oregon.gov/deq/wq/Pages/WQdata.aspx", target="_blank"),  style = "font-family: 'times'"),
                             p(strong("Use search criteria on left to filter results. Press the filter button to see assessment results"), style = "font-family: 'times'"),
                             p(strong("Information for each record in the assessment database includes:"), style = "font-family: 'times'"),
                             tags$ul(
                                 tags$li(strong("AU_ID "), " - Assessment Unit ID", style = "font-family: 'times'"), 
                                 tags$li(strong("AU_Name "), " - Assessment Unit Name", style = "font-family: 'times'"), 
                                 tags$li(strong("OWRD_Basin "), " - Oregon Water Resources Department Administrative Basin", style = "font-family: 'times'"), 
                                 tags$li(strong("Assessment "), " - Parameter being assessed. Includes specific standard, if applicable", style = "font-family: 'times'"), 
                                 tags$li(strong("Parameter_category "), " - Current Integrated Report category for that specific assessment", style = "font-family: 'times'"), 
                                 tags$ul(
                                     tags$li(strong("Category 2"), " - Available data and information indicate that some designated uses are supported and the water quality standard is attained", style = "font-family: 'times'"), 
                                     tags$li(strong("Category 3"), " - Insufficient data to determine whether a designated use is supported", style = "font-family: 'times'"), 
                                     tags$ul(
                                         tags$li(strong("Category 3B"), " - This category is used when there is insufficient data to determine use support, but some data indicate  possible impairment", style = "font-family: 'times'"), 
                                         tags$li(strong("Category 3C"), " - This category is used to identify waters whose biocriteria scores differ from reference condition, but are not classified as impaired", style = "font-family: 'times'"), 
                                         tags$li(strong("Category 3D"), " - This category is used when all the available data has criteria values below the test method’s quantification limits", style = "font-family: 'times'")
                                         
                                         
                                     ),
                                     tags$li(strong("Category 4"), " - Data indicate that at least one designated use is not supported, but a TMDL is not needed to address the pollutant", style = "font-family: 'times'"),
                                     tags$ul(
                                         tags$li(strong("Category 4A"), " - Clean-up plans (also called TMDLs) that will result in the waterbody meeting water quality standards and supporting its beneficial uses have been approved", style = "font-family: 'times'"), 
                                         tags$li(strong("Category 4B"), " - Other pollution control requirements are expected to address pollutant of concern and will result in attainment of water quality standards", style = "font-family: 'times'"), 
                                         tags$li(strong("Category 4C"), " - The impairment is caused by pollution, not a pollutant. For example, flow, or lack of flow, are not considered pollutants, but may be affecting the waterbody’s beneficial uses", style = "font-family: 'times'")
                                         
                                         
                                     ),
                                     tags$li(strong("Category 5"), " - Data indicate a designated use is not supported or a water quality standard is not attained and a TMDL is needed. This category constitutes the Section 303(d) list that EPA will approve or disapprove under the Clean Water Act", style = "font-family: 'times'")
                                     
                                 ),
                                 tags$li(strong("Monitoring Locations "), " - Monitoring stations used in 2018/2020 assessment. Data from these monitoring locations can be downloaded from AWQMS, providing the raw data used in assessment.", style = "font-family: 'times'"), 
                                 tags$li(strong("Year_listed "), " - If Assessment Unit is identified as impaired (Category 4 or 5), year it first appeared on the 303(d) List", style = "font-family: 'times'"), 
                                 tags$li(strong("Assessed_in_2018 "), " - Identifies if assessment was conducted in 2018", style = "font-family: 'times'"),
                                 tags$li(strong("Rationale "), " - Rationale for impairment, if any", style = "font-family: 'times'"),
                                 tags$li(strong("Beneficial_uses "), " - Which beneficial uses this assessment applies to", style = "font-family: 'times'")
                             )
                             
                             
                                              ),
                             tabPanel("Assessments",
                                      value = "Datatab",
                                      downloadButton('downloadassessmentData', label = "Download Assessment Results"),
                                      dataTableOutput('table')
                             )
                                  )
                              )
                             
                             
                             
                             
                          )
                          
                          
                 ), # End tab panel 1
                 
                 #Begin tab panel 2
                 tabPanel("Raw Data Download",
                          value = "data",
                          # Application title
                          titlePanel(
                              fluidRow(
                                  column(6, img(src = "logo.png")), 
                                  column(6,  "2018/2020 Integrated Report Data Download",style = "font-family: 'Arial'; font-si16pt; vertical-align: 'bottom'")),
                              windowTitle = "2018/2020 Integrated Report Data"
                          ),
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  # Show a plot of the generated distribution
                                  downloadButton('downloadallData', label = "Download All Assessment Data"),
                                  downloadButton('downloadData', label = "Download Assessment Data by Unit"),
                                  selectizeInput("Data_AUs",
                                                 "Select one or more Assessment Units",
                                                 choices = assessed_AUs,
                                                 multiple = TRUE,
                                                 options = list(maxOptions = 7000))
                                  
                              ),
                              
                              
                              
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              id = "Tabset",
                                              tabPanel("Instructions",
                                                       value = "InstructionTab",
                                                       h2(strong("Download numeric data used in the 2018/2020 Integrated Report"), style = "font-family: 'Arial'"),
                                                       p("DEQ recommends using the current version of Google Chrome or Mozilla Firefox for this application.", style = "font-family: 'times'"),
                                                       p("This application provides the numeric data used in new assessments for the 2018/2020 Integrated Report. Clicking on the", strong('Download All Assessment Data'), "will
                               download all numeric data used in new 2018/2020 assessments. Entering one or more Assessment Units in the search box and pressing",strong('Download All Assessment Data'), 
                               "will download select data. Data will be downloaded bundled into a zip file" , 
                               style = "font-family: 'times'"),
                               p(strong("Due to the size of the file, downloading All Assessment Data may take a few minutes"), style = "font-family: 'times'"),
                               p("A dictionary describing column headers is included in the zip file", style = "font-family: 'times'"),
                               p("A  complete mapping and dataset, including water quality standards information can be found on the ", 
                                 a("Interactive web map.", href="https://hdcgcx2.deq.state.or.us/HVR291/?viewer=wqsa#", target="_blank"), 
                                 "Assessment conculsions can be found on the ", a("online assessment database.", href="https://travispritchard.shinyapps.io/2018-2020_IR_Database/", target="_blank"), style = "font-family: 'times'"),
                               p( 
                                   a("The 2018/2020 Assessment Methodology can be found here.", href="https://www.oregon.gov/deq/FilterDocs/ir2018assessMethod.pdf", target="_blank"), style = "font-family: 'times'"),
                               p(
                                   a("The DEQ 2018/2020 IR webpage page can be found here.", href="https://www.oregon.gov/deq/wq/Pages/2018-Integrated-Report.aspx", target="_blank"), style = "font-family: 'times'")
                                              ))
                               
                               
                               
                              )
                              
                          ))
                 
                 
                 ,add_busy_spinner(spin = "fading-circle")
                 # ,
                 # 
                 # 
                 # 
                 # # bootstrapPage('',
                 # # 
                 # #               tags$style(type = 'text/css', ".navbar { background-color: #71bcb4;}",
                 # #                        ".navbar-default .navbar-nav > .active > a",
                 # #                        ".navbar-default .navbar-nav > .active > a:focus",
                 # #                        ".navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}",
                 # #                        ".navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}",
                 # #                        ".navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}",
                 # #                        ".navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}",
                 # #                        ".navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;"
                 # #                          
                 # #               )
                 # 
                 # tags$style(type = 'text/css', 
                 #            HTML('.navbar { background-color: #71bcb4;}
                 #                           .navbar-default .navbar-brand{color: black;}
                 #                           .tab-panel{ background-color: red; color: black}
                 #                           .navbar-default .navbar-nav > .active > a, 
                 #                            .navbar-default .navbar-nav > .active > a:focus, 
                 #                            .navbar-default .navbar-nav > .active > a:hover {
                 #                                 color: black;
                 #                                 background-color: #00907e;
                 #                             }')
                 #            
                 #            
                 # )
                 
)



# shiny server section ----------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    
    
    
    
    # Table_data --------------------------------------------------------------
    
    
    # Reactive table data
    # This is where all the filtering is going to happen.
    # Modify to include any filtering needed for webdiplay
    
    table_Data <- eventReactive(input$go,{
        
        t <- Parameter_assessments
        
        if (!is.null(input$AUs)){
            t <- t %>%
                filter(AU_ID %in% input$AUs)
        }
        
        if (!is.null(input$Select_AUName)){
            t <- t %>%
                filter(AU_Name %in% input$Select_AUName)
        }
        
        
        if(!is.null(input$category_selector)){
            
            t <- t %>%
                filter(Parameter_category %in% input$category_selector)
        }
        
        if(!is.null(input$pollutant_selector)){
            
            t <- t %>%
                filter(Pollutant %in% input$pollutant_selector)
        }
        
        if(!is.null(input$admin_basin_selector)){
            t <- t %>%
                filter(OWRD_Basin %in% input$admin_basin_selector)
            
        }
        
        if(!is.null(input$status_selector)){
            t <- t %>%
                mutate(param_status = case_when(grepl('5', Parameter_category) | grepl('4', Parameter_category) ~ "Impaired",
                                                grepl('3', Parameter_category) ~ "Insufficient",
                                                grepl('2',Parameter_category) ~ 'Attains',
                                                TRUE ~ 'Unassessed')) %>%
                filter(param_status %in% input$status_selector) %>%
                select(-param_status)
            
        }
        
        if(!is.null(input$benuse_selector)){
            
            BU_pattern = paste(input$benuse_selector, collapse="|")
            
            t <- t %>%
                filter(grepl(BU_pattern, Beneficial_uses))
            
            
        }
        t <- t %>%
            select(-Pollutant)
        
        t
        
    })
    
    
    
    # I have no idea what this does
    AU_Names_reactive <- eventReactive(input$AUs,{
        
        BU_filtered <- filter(Parameter_assessments, AU_ID %in% input$AUs )
        
        unique(BU_filtered$AU_Name)
        
        
        
    }, ignoreNULL = TRUE)
    
    
    
    # Render the table --------------------------------------------------------
    
    
    #render the table from the reactive table_Data 
    # Reactive function
    
    output$table <- renderDataTable(
        
        table_Data(),
        options = list(paging = FALSE)
        
        
    )
    
    
    # Download button actions -------------------------------------------------
    
    
    #Handle the download output.
    output$downloadassessmentData <- downloadHandler(
        filename = function() { 
            paste("Oregon 2020 Integrated Report Filtered Download", ".csv", sep="")
        },
        content = function(file) {
            write.csv(table_Data(), file, row.names = FALSE,  na = "")
        })
    
    
    
    # When filter button is hit. move focus to data tab -----------------------
    
    observeEvent(input$go, {
        updateTabsetPanel(session, "Tabset",
                          selected = 'Datatab'
        )
    })
    
    
    
    # Data download server companants -----------------------------------------
    
    
    filtered_data <- reactive({
        
        filtered_bacteria_coast_contact <- bacteria_coast_contact %>%
            filter(AU_ID %in% input$Data_AUs)
        
        
        filtered_bacteria_fresh_contact <- bacteria_fresh_contact  %>%
            filter(AU_ID %in% input$Data_AUs)
        
     
        
        filtered_chl <- chl  %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_DO_cont_spawn <- DO_cont_spawn %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_DO_cont_yearround <- DO_cont_yearround  %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_DO_instant_spawn <- DO_instant_spawn %>%
            filter(AU_ID %in% input$Data_AUs) 
        
        filtered_DO_inst_yearround <- DO_inst_yearround %>%
            filter(AU_ID %in% input$Data_AUs) 
        
     
        
        filtered_pH <- pH %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_temp <- temp   %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_Tox_AL_Ammonia <-Tox_AL_Ammonia %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_Tox_AL_CU <-Tox_AL_CU %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_Tox_AL_Hardness_Metals <-Tox_AL_Hardness_Metals %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_Tox_AL_Others <- Tox_AL_Others %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_Tox_AL_Penta <- Tox_AL_Penta %>%
            filter(AU_ID %in% input$Data_AUs)
        

        filtered_Tox_HH <- Tox_HH %>%
            filter(AU_ID %in% input$Data_AUs)
        
       
        filtered_biocriteria <- biocriteria %>%
            filter(AU_ID %in% input$Data_AUs)
        
        filtered_turbidity <- turbidity_data %>%
            filter(AU_ID %in% input$Data_AUs)
        
  
        filtered_HABs <- Habs_data %>%
            filter(AU_ID %in% input$Data_AUs)
        
        
        return(list(filtered_bacteria_coast_contact = filtered_bacteria_coast_contact,
                    filtered_bacteria_fresh_contact = filtered_bacteria_fresh_contact,
                    filtered_bacteria_Shell_harvest = filtered_bacteria_Shell_harvest, 
                    filtered_chl = filtered_chl, 
                    filtered_DO_cont_spawn = filtered_DO_cont_spawn,
                    filtered_DO_cont_yearround = filtered_DO_cont_yearround,
                    filtered_DO_instant_spawn = filtered_DO_instant_spawn,
                    filtered_DO_inst_yearround = filtered_DO_inst_yearround,
                   
                    filtered_pH = filtered_pH,
                    filtered_temp = filtered_temp,
                    filtered_Tox_AL_Ammonia = filtered_Tox_AL_Ammonia,
                    filtered_Tox_AL_CU = filtered_Tox_AL_CU,
                    filtered_Tox_AL_Hardness_Metals = filtered_Tox_AL_Hardness_Metals,
                    filtered_Tox_AL_Others = filtered_Tox_AL_Others,
                    filtered_Tox_AL_Penta =filtered_Tox_AL_Penta,
                    filtered_Tox_HH = filtered_Tox_HH,
                   
                    filtered_biocriteria = filtered_biocriteria,
                    filtered_turbidity = filtered_turbidity,
                  
                    filtered_HABs = filtered_HABs))
        
    })
    
    
    
    
    
    output$downloadData <- downloadHandler(
        filename = '2018_2020_IR_select_data_download.zip',
        content = function(fname) {
            original_wd <- getwd()
            tmpdir <- tempdir()
            setwd(tempdir())
            print(tempdir())
            
            fs <- c("Temperature.xlsx", "Bacteria.xlsx", "Chlorophyll.xlsx",
                    "DO.xlsx", "pH.xlsx",
                    "Aquatic_Life_Toxics.xlsx", "Human_Health_Toxics.xlsx",
                    "Biocriteria.xlsx", "Turbidity.xlsx","Aquatic_Weeds.xlsx",
                    'HABs.xlsx'
            )
            
            #temperature
            write.xlsx(filtered_data()$filtered_temp, file = "Temperature.xlsx",
                       overwrite = TRUE)
            
            wb <- createWorkbook()
            
            # bacteria
            addWorksheet(wb, "E coli")
            addWorksheet(wb, "Enterococcus")
            addWorksheet(wb, "Fecal Coliform")
            
            writeData(wb,"E coli",  filtered_data()$filtered_bacteria_fresh_contact, rowNames = FALSE)
            writeData(wb,"Enterococcus", filtered_data()$filtered_bacteria_coast_contact, rowNames = FALSE)
            writeData(wb,"Fecal Coliform", filtered_data()$filtered_bacteria_Shell_harvest, rowNames = FALSE)
            
            saveWorkbook(wb, file = "Bacteria.xlsx", 
                         overwrite = TRUE)
            
            #chl
            write.xlsx(filtered_data()$filtered_chl, 
                       file = "Chlorophyll.xlsx", 
                       overwrite = TRUE)
            
            # pH
            write.xlsx( filtered_data()$filtered_pH, 
                        file = "pH.xlsx", 
                        overwrite = TRUE)
            
            #DO
            
            # wb <- createWorkbook()
            # addWorksheet(wb, "DO_spawn_continuous")
            # addWorksheet(wb, "DO_spawn_instantaneous")
            # 
            # writeData(wb,"DO_spawn_continuous",  filtered_data()$filtered_DO_cont_spawn, rowNames = FALSE)
            # writeData(wb,"DO_spawn_instantaneous", filtered_data()$filtered_DO_instant_spawn, rowNames = FALSE)
            # 
            # saveWorkbook(wb, "DO_Spawning.xlsx", 
            #              overwrite = TRUE)
            
            
            
            
            wb <- createWorkbook()
            addWorksheet(wb, "DO_yearround_continuous")
            addWorksheet(wb, "DO_yearround_instantaneous")
            addWorksheet(wb, "DO_spawn_continuous")
            addWorksheet(wb, "DO_spawn_instantaneous")
            
            writeData(wb,"DO_yearround_continuous",  filtered_data()$filtered_DO_cont_yearround, rowNames = FALSE)
            writeData(wb,"DO_yearround_instantaneous",filtered_data()$filtered_DO_inst_yearround, rowNames = FALSE)
            writeData(wb,"DO_spawn_continuous",  filtered_data()$filtered_DO_cont_spawn, rowNames = FALSE)
            writeData(wb,"DO_spawn_instantaneous", filtered_data()$filtered_DO_instant_spawn, rowNames = FALSE)
            
            saveWorkbook(wb, file = "DO.xlsx", 
                         overwrite = TRUE)
            
            
            
            wb <- createWorkbook()
            addWorksheet(wb, 'Tox_AL_Others')
            addWorksheet(wb,'Tox_AL_Ammonia')
            addWorksheet(wb,'Tox_AL_CU')
            addWorksheet(wb, 'Tox_AL_Hardness_Metals')
            addWorksheet(wb, 'Tox_AL_Pentachlorophenol')
            
            writeData(wb,  'Tox_AL_Others', filtered_data()$filtered_Tox_AL_Others, rowNames = FALSE)
            writeData(wb, 'Tox_AL_Ammonia',  filtered_data()$filtered_Tox_AL_Ammonia, rowNames = FALSE)
            writeData(wb, 'Tox_AL_CU',  filtered_data()$filtered_Tox_AL_CU, rowNames = FALSE)
            writeData(wb, 'Tox_AL_Hardness_Metals',  filtered_data()$filtered_Tox_AL_Hardness_Metals, rowNames = FALSE)
            writeData(wb, 'Tox_AL_Pentachlorophenol',  filtered_data()$filtered_Tox_AL_Penta, rowNames = FALSE)
            
            saveWorkbook(wb, file = "Aquatic_Life_Toxics.xlsx", 
                         overwrite = TRUE)
            
            
            
            
            wb <- createWorkbook()
            addWorksheet(wb, 'Tox_HH')
            addWorksheet(wb,'Tox_HH_Hg_Tissue')
            writeData(wb, 'Tox_HH',  filtered_data()$filtered_Tox_HH, rowNames = FALSE)
            writeData(wb, 'Tox_HH_Hg_Tissue',  filtered_data()$filtered_Tox_HH_Hg_tissue, rowNames = FALSE)
            
            
            saveWorkbook(wb, file = "Human_Health_Toxics.xlsx", 
                         overwrite = TRUE)
            
            # file.copy(paste0(original_wd, "/data/IR_Data_Dictionary.xlsx"), "IR_Data_Dictionary.xlsx")
            
            #biocriteria
            write.xlsx(filtered_data()$filtered_biocriteria, 
                       file = "Biocriteria.xlsx", 
                       overwrite = TRUE)
            
            #turbidity
            write.xlsx(filtered_data()$filtered_turbidity, 
                       file = "Turbidity.xlsx", 
                       overwrite = TRUE)
            
            #weeds
          
            
            #HABs
            write.xlsx(filtered_data()$filtered_HABs, 
                       file = "HABs.xlsx", 
                       overwrite = TRUE)
            
            print (fs)
            
            zip(zipfile=fname, files=fs)
            
            setwd(original_wd)
        },
        contentType = "application/zip"
    )
    
    
    # output$downloadallData <- downloadHandler(
    #     filename = 'data_download.zip',
    #     content = function(fname) {
    #         tmpdir <- tempdir()
    #         setwd(tempdir())
    #         print(tempdir())
    # 
    #         fs <- c("temp.xlsx", "Bacteria.xlsx", "Chlorophyll.xlsx",
    #                 "DO_Spawning.xlsx", "DO_Yearround.xlsx", "pH.xlsx",
    #                 "Aquatic_Life_Toxics.xlsx", "Human_Health_Toxics.xlsx"
    #         )
    # 
    #         #temperature
    #         write.xlsx(filtered_data()$filtered_temp, file = "temp.xlsx",
    #                    overwrite = TRUE)
    # 
    #         wb <- createWorkbook()
    # 
    #         # bacteria
    #         addWorksheet(wb, "E coli")
    #         addWorksheet(wb, "Enterococcus")
    #         addWorksheet(wb, "Fecal Coliform")
    # 
    #         writeData(wb,"E coli",  bacteria_fresh_contact, rowNames = FALSE)
    #         writeData(wb,"Enterococcus", bacteria_coast_contact, rowNames = FALSE)
    #         writeData(wb,"Fecal Coliform", bacteria_Shell_harvest, rowNames = FALSE)
    # 
    #         saveWorkbook(wb, file = "Bacteria.xlsx",
    #                      overwrite = TRUE)
    # 
    #         #chl
    #         write.xlsx(chl,
    #                    file = "Chlorophyll.xlsx",
    #                    overwrite = TRUE)
    # 
    #         # pH
    #         write.xlsx( pH,
    #                     file = "pH.xlsx",
    #                     overwrite = TRUE)
    # 
    #         #DO
    # 
    #         wb <- createWorkbook()
    #         addWorksheet(wb, "DO_spawn_continuous")
    #         addWorksheet(wb, "DO_spawn_instantaneous")
    # 
    #         writeData(wb,"DO_spawn_continuous",  DO_cont_spawn, rowNames = FALSE)
    #         writeData(wb,"DO_spawn_instantaneous", DO_instant_spawn, rowNames = FALSE)
    # 
    #         saveWorkbook(wb, "DO_Spawning.xlsx",
    #                      overwrite = TRUE)
    # 
    # 
    # 
    # 
    #         wb <- createWorkbook()
    #         addWorksheet(wb, "DO_yearround_continuous")
    #         addWorksheet(wb, "DO_yearround_instantaneous")
    # 
    #         writeData(wb,"DO_yearround_continuous",  DO_cont_yearround, rowNames = FALSE)
    #         writeData(wb,"DO_yearround_instantaneous",DO_inst_yearround, rowNames = FALSE)
    # 
    #         saveWorkbook(wb, file = "DO_Yearround.xlsx",
    #                      overwrite = TRUE)
    # 
    # 
    # 
    #         wb <- createWorkbook()
    #         addWorksheet(wb, 'Tox_AL_Others')
    #         addWorksheet(wb,'Tox_AL_Ammonia')
    #         addWorksheet(wb,'Tox_AL_CU')
    #         addWorksheet(wb, 'Tox_AL_Hardness_Metals')
    #         addWorksheet(wb, 'Tox_AL_Pentachlorophenol')
    # 
    #         writeData(wb,  'Tox_AL_Others', Tox_AL_Others, rowNames = FALSE)
    #         writeData(wb, 'Tox_AL_Ammonia',  Tox_AL_Ammonia, rowNames = FALSE)
    #         writeData(wb, 'Tox_AL_CU',  Tox_AL_CU, rowNames = FALSE)
    #         writeData(wb, 'Tox_AL_Hardness_Metals',  Tox_AL_Hardness_Metals, rowNames = FALSE)
    #         writeData(wb, 'Tox_AL_Pentachlorophenol',  Tox_AL_Penta, rowNames = FALSE)
    # 
    #         saveWorkbook(wb, file = "Aquatic_Life_Toxics.xlsx",
    #                      overwrite = TRUE)
    # 
    # 
    # 
    # 
    #         wb <- createWorkbook()
    #         addWorksheet(wb, 'Tox_HH')
    #         addWorksheet(wb,'Tox_HH_Hg_Tissue')
    #         writeData(wb, 'Tox_HH',  Tox_HH, rowNames = FALSE)
    #         writeData(wb, 'Tox_HH_Hg_Tissue',  Tox_HH_Hg_tissue, rowNames = FALSE)
    # 
    # 
    #         saveWorkbook(wb, file = "Human_Health_Toxics.xlsx",
    #                      overwrite = TRUE)
    # 
    #         print (fs)
    # 
    #         zip(zipfile=fname, files=fs)
    #     },
    #     contentType = "application/zip"
    # )
    
    
    output$downloadallData <-  downloadHandler(
        filename <- function() {
            paste("2018_2020_IR_all_data_download", "zip", sep=".")
        },
        
        content <- function(file) {
            file.copy("data/All_data.zip", file)
        },
        contentType = "application/zip"
    )
    
    
    
}



# Run the application  ----------------------------------------------------


shinyApp(ui = ui, server = server)



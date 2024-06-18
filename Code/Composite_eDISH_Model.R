####################################################################################
####################################################################################
# A shiny app to prepare data and generate Composite eDISH plot and eDISH
# migration table.
#
# Author: Bereket Tesfaldet
# Date  : March 27, 2024
#
# The following two datasets in ADaM CDISC standard are required to run the application:
#   adsl - A subject level dataset (~ADSL) with at least the following two variables: 
#          USUBJID and ARM.
#   adlb - A laboratory dataset (~ADLB) with at least the following two variables:  
#          USUBJID, PARAMCD, AVISTN, AVAL, ANRHI, and BASE. AVISTN = 0 is assumed 
#          to be the baseline visit while all visits with AVISTN > 0 are assumed to 
#          be treatment visits.
#############################################

library(shiny)
library(shinyBS)
library(shinythemes, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(datawizard) #Read different data types
library(rio) #Read jmp data types
library(gt) #For making tables
library(janitor) #For making tables
library(patchwork)

## packageVersion('patchwork')

# By default, Shiny limits file uploads to 5MB per file.
# Increase it to 900MB
options(shiny.maxRequestSize=900*1024^2) 


myLabels <- c("Normal & NN", "Cholestasis", "Temple's Corollary", "Hy's Law")
myQLabels <- c("Subjects in Normal & NN", "Subjects in Cholestasis", 
               "Subjects in Temple's Corollary", "Subjects in Hy's Law")
myColors <- c("Normal & NN" = "darkgreen", "Cholestasis" = "magenta", 
              "Temple's Corollary" = "blue", "Hy's Law" = "red")
myShapes <- c("Normal & NN" = 3, "Cholestasis" = 2, "Temple's Corollary" = 2, "Hy's Law" = 4)

####################################################################################


# Define functions for data uploading and preparations
ADSLDATA <- function(ADSL){
  # Load ADSL
  ADSL <- ADSL()
  
  # Change variable names to upper case
  names(ADSL) <- toupper(names(ADSL))
  
  # Collect variables needed and check if they all exist
  myADSLV1 <- names(ADSL %>% select(any_of(c("STUDYID", "USUBJID", "SEX", "RACE"))))
  myADSLV2 <- names(ADSL %>% select(contains("ARM") | starts_with("TRT")))
  myADSLV3 <- names(ADSL %>% select(ends_with("FL"))) 
  myADSLV4 <- names(ADSL %>% select(ends_with("GR1")))
  
  myADSLVars <- c(myADSLV1, myADSLV2, myADSLV3, myADSLV4)
  
  if(!("USUBJID" %in%  myADSLV1) | length(myADSLV2) == 0){
    stop("ADSL dataset doesn't have required varialbes: USUBJID and a treatment arm.")
    
  } else {print("ADSL is good to go.")}
  
  # Create ADSL dataset with required variables
  myADSL <- ADSL %>% select(all_of(myADSLVars))
  
  myADSL
}

ADLBDATA <- function(ADLB){
  # Load ADLB
  ADLB <- ADLB()
  
  # Change variable names to upper case
  names(ADLB) <- toupper(names(ADLB))
  
  myADLBV1 <- names(ADLB %>% 
                      select(any_of(c("STUDYID", "USUBJID", "PARAM", "PARAMCD", 
                                      "AVAL", "ANRHI", "BASE", "AVISIT", "AVISITN", "ADY"))))
  myADLBV2 <- names(ADLB %>% select(ends_with("FL")))
  
  myADLBVars <- c(myADLBV1, myADLBV2)
  
  if(!all(c("USUBJID", "PARAMCD", "AVAL", "ANRHI", "BASE", "AVISITN") %in%  myADLBV1)){
    stop("ADLB dataset doesn't have required varialbes: USUBJID, PARAMCD, AVAL, ANRHI, BASE and AVISITN.")
    
  } else {print("ADLB is good to go.")}
  
  # Create ADLB with required variables
  myADLB <- ADLB %>% select(all_of(myADLBVars))
  
  # Subset ADLB for the required labs
  myADLB <- myADLB %>% subset(PARAMCD %in% c("ALT", "BILI") & AVISITN >= 0)
  
  # Normalize lab values
  myADLB <- myADLB %>% mutate(BASExULN = BASE/ANRHI, AVALxBLN = AVAL/BASE, AVALxULN = AVAL/ANRHI)
  
  myADLB
}

ADLBDRILL <- function(ADLB){
  # Load ADLB
  ADLB <- ADLB()
  
  # Change variable names to upper case
  names(ADLB) <- toupper(names(ADLB))
  
  myADLBV1 <- names(ADLB %>% 
                      select(any_of(c("STUDYID", "USUBJID", "PARAM", "PARAMCD", 
                                      "AVAL", "ANRHI", "BASE", "AVISIT", "AVISITN", "ADY"))))
  myADLBV2 <- names(ADLB %>% select(ends_with("FL")))
  
  myADLBVars <- c(myADLBV1, myADLBV2)
  
  if(!all(c("USUBJID", "PARAMCD", "AVAL", "ANRHI", "BASE", "AVISITN") %in%  myADLBV1)){
    stop("ADLB dataset doesn't have required varialbes: USUBJID, PARAMCD, AVAL, ANRHI, BASE and AVISITN.")
    
  } else {print("ADLB is good to go.")}
  
  # Create ADLB with required variables
  myADLB <- ADLB %>% select(all_of(myADLBVars))
  
  # Subset ADLB for the required labs
  myADLB <- myADLB %>% subset(PARAMCD %in% c("ALT", "BILI", "AST", "ALP"))
  
  # Normalize lab values
  myADLB <- myADLB %>% mutate(BASExULN = BASE/ANRHI, AVALxBLN = AVAL/BASE, AVALxULN = AVAL/ANRHI)
  
  myADLB
}

eDISHDATAMaking <- function(ADSL, ADLB){
  # Load ADSL and ADLB
  myADSL <- ADSLDATA(ADSL)
  myADLB <- ADLBDATA(ADLB)
  
  # Identify peak on-treatment labs
  # Assume AVISITN = 0 is baseline and remaining visits are on-treatment
  myADLBPeak <- myADLB %>% subset(AVISITN > 0) %>% 
    group_by(USUBJID, PARAMCD) %>% 
    summarise(BASExULN=max(BASExULN), PAVALxBLN=max(AVALxBLN), PAVALxULN=max(AVALxULN))
  
  # Transpose lab data into a wide format
  myADLBWd <- myADLBPeak %>% pivot_wider(names_from = PARAMCD, values_from = c(BASExULN, PAVALxBLN, PAVALxULN))
  
  # Rename using a named vector and `all_of()`
  names <- c(BALTxULN = "BASExULN_ALT", BBILIxULN = "BASExULN_BILI", PALTxBLN = "PAVALxBLN_ALT",
             PBILIxBLN = "PAVALxBLN_BILI", PALTxULN = "PAVALxULN_ALT", PBILIxULN = "PAVALxULN_BILI")
  
  # Remove rows with missing values
  myADLBWd <- myADLBWd %>% rename(any_of(names)) %>%
    filter(if_any(BALTxULN:PBILIxULN, ~ !is.na(.)))
  
  # Identify the baseline and on-treatment Hy's Law quadrants
  myADLBgr <- myADLBWd %>% 
    mutate(BALTStatus = cut(BALTxULN, breaks=c(0, 3.0, Inf), labels=c("left", "right"))) %>% 
    mutate(BBILIStatus = cut(BBILIxULN, breaks=c(0, 2.0, Inf), labels=c("below", "above"))) %>%
    mutate(PALTStatus = cut(PALTxULN, breaks=c(0, 3.0, Inf), labels=c("left", "right"))) %>% 
    mutate(PBILIStatus = cut(PBILIxULN, breaks=c(0, 2.0, Inf), labels=c("below", "above"))) %>% 
    mutate(BLNHYsLAWStatus = case_when(BALTStatus == "left" & BBILIStatus == "below" ~ "Normal & NN",
                                       BALTStatus == "left" & BBILIStatus == "above" ~ "Cholestasis",
                                       BALTStatus == "right" & BBILIStatus == "above" ~ "Hy's Law",
                                       TRUE ~ "Temple's Corollary")) %>%
    mutate(HYsLAWStatus = case_when(PALTStatus == "left" & PBILIStatus == "below" ~ "Subjects in Normal & NN",
                                    PALTStatus == "left" & PBILIStatus == "above" ~ "Subjects in Cholestasis",
                                    PALTStatus == "right" & PBILIStatus == "above" ~ "Subjects in Hy's Law",
                                    TRUE ~ "Subjects in Temple's Corollary"))  %>%
    mutate(PHYsLAWStatus = case_when(PALTStatus == "left" & PBILIStatus == "below" ~ "Normal & NN",
                                    PALTStatus == "left" & PBILIStatus == "above" ~ "Cholestasis",
                                    PALTStatus == "right" & PBILIStatus == "above" ~ "Hy's Law",
                                    TRUE ~ "Temple's Corollary")) 
  
  myADLBgr <- myADLBgr %>%select(!c(BALTStatus, BBILIStatus, PALTStatus, PBILIStatus))
  
  # Determines the order of their appearance
  myADLBgr$BLNHYsLAWStatus <- factor(myADLBgr$BLNHYsLAWStatus, levels = myLabels)
  myADLBgr$PHYsLAWStatus <- factor(myADLBgr$PHYsLAWStatus, levels = myLabels)
  
  # Check if the two datasets share common subjects
  if(length(intersect(myADSL$USUBJID, myADLBgr$USUBJID)) == 0){
    stop("ADSL and ADLB datasets provided don't share common subjects.")
    
  } else {print("ADSL and ADLB are good to go.")}
  
  # Create composite eDISH plot dataset
  eDISHData <- myADLBgr %>% left_join(myADSL, join_by(USUBJID))
  
  # Determines the order of their appearance
  eDISHData$BLNHYsLAWStatus <- factor(eDISHData$BLNHYsLAWStatus, levels = myLabels)
  eDISHData$PHYsLAWStatus <- factor(eDISHData$PHYsLAWStatus, levels = myLabels)
  
  #Add hovering text for Composite Plot
  eDISHData$myText = paste("\nPeak xBLN (", sprintf(eDISHData$PALTxBLN, fmt='%.2f'), ", ", 
                           sprintf(eDISHData$PBILIxBLN, fmt='%.2f'), ")",
                           "\nPeak xULN (", sprintf(eDISHData$PALTxULN, fmt='%.2f'), ", ", 
                           sprintf(eDISHData$PBILIxULN, fmt='%.2f'), ")")
  #Add USUBJID as row name to be used to identify points in drill down.
  #row.names(eDISHData) <- eDISHData$USUBJID
  eDISHData
}  

DRILLDATAMaking <- function(eDISH, ADLB){
  # Load ADSL and ADLB
  myADSL <- ADSLDATA(ADSL)
  myADLB <- ADLBDATA(ADLB)
  
  # Identify peak on-treatment labs
  # Assume AVISITN = 0 is baseline and remaining visits are on-treatment
  myADLBPeak <- myADLB %>% subset(AVISITN > 0) %>% 
    group_by(USUBJID, PARAMCD) %>% 
    summarise(BASExULN=max(BASExULN), PAVALxBLN=max(AVALxBLN), PAVALxULN=max(AVALxULN))
  
  # Transpose lab data into a wide format
  myADLBWd <- myADLBPeak %>% pivot_wider(names_from = PARAMCD, values_from = c(BASExULN, PAVALxBLN, PAVALxULN))
  
  # Rename using a named vector and `all_of()`
  names <- c(BALTxULN = "BASExULN_ALT", BBILIxULN = "BASExULN_BILI", PALTxBLN = "PAVALxBLN_ALT",
             PBILIxBLN = "PAVALxBLN_BILI", PALTxULN = "PAVALxULN_ALT", PBILIxULN = "PAVALxULN_BILI")
  
  # Remove rows with missing values
  myADLBWd <- myADLBWd %>% rename(any_of(names)) %>%
    filter(if_any(BALTxULN:PBILIxULN, ~ !is.na(.)))
  
  # Identify the baseline and on-treatment Hy's Law quadrants
  myADLBgr <- myADLBWd %>% 
    mutate(BALTStatus = cut(BALTxULN, breaks=c(0, 3.0, Inf), labels=c("left", "right"))) %>% 
    mutate(BBILIStatus = cut(BBILIxULN, breaks=c(0, 2.0, Inf), labels=c("below", "above"))) %>%
    mutate(PALTStatus = cut(PALTxULN, breaks=c(0, 3.0, Inf), labels=c("left", "right"))) %>% 
    mutate(PBILIStatus = cut(PBILIxULN, breaks=c(0, 2.0, Inf), labels=c("below", "above"))) %>% 
    mutate(BLNHYsLAWStatus = case_when(BALTStatus == "left" & BBILIStatus == "below" ~ "Normal & NN",
                                       BALTStatus == "left" & BBILIStatus == "above" ~ "Cholestasis",
                                       BALTStatus == "right" & BBILIStatus == "above" ~ "Hy's Law",
                                       TRUE ~ "Temple's Corollary")) %>%
    mutate(HYsLAWStatus = case_when(PALTStatus == "left" & PBILIStatus == "below" ~ "Subjects in Normal & NN",
                                    PALTStatus == "left" & PBILIStatus == "above" ~ "Subjects in Cholestasis",
                                    PALTStatus == "right" & PBILIStatus == "above" ~ "Subjects in Hy's Law",
                                    TRUE ~ "Subjects in Temple's Corollary"))  %>%
    mutate(PHYsLAWStatus = case_when(PALTStatus == "left" & PBILIStatus == "below" ~ "Normal & NN",
                                     PALTStatus == "left" & PBILIStatus == "above" ~ "Cholestasis",
                                     PALTStatus == "right" & PBILIStatus == "above" ~ "Hy's Law",
                                     TRUE ~ "Temple's Corollary")) 
  
  myADLBgr <- myADLBgr %>% select(!c(BALTStatus, BBILIStatus, PALTStatus, PBILIStatus))
  
  # Determines the order of their appearance
  myADLBgr$BLNHYsLAWStatus <- factor(myADLBgr$BLNHYsLAWStatus, levels = myLabels)
  myADLBgr$PHYsLAWStatus <- factor(myADLBgr$PHYsLAWStatus, levels = myLabels)
  
  # Check if the two datasets share common subjects
  if(length(intersect(myADSL$USUBJID, myADLBgr$USUBJID)) == 0){
    stop("ADSL and ADLB datasets provided don't share common subjects.")
    
  } else {print("ADSL and ADLB are good to go.")}
  
  # Create composite eDISH plot dataset
  eDISHData <- myADLBgr %>% left_join(myADSL, join_by(USUBJID))
  
  # Determines the order of their appearance
  eDISHData$BLNHYsLAWStatus <- factor(eDISHData$BLNHYsLAWStatus, levels = myLabels)
  eDISHData$PHYsLAWStatus <- factor(eDISHData$PHYsLAWStatus, levels = myLabels)
  
  #Add hovering text for Composite Plot
  eDISHData$myText = paste("Peak xBLN (", sprintf(eDISHData$PALTxBLN, fmt='%.2f'), ", ", 
                           sprintf(eDISHData$PBILIxBLN, fmt='%.2f'), ")",
                           "\nPeak xULN (", sprintf(eDISHData$PALTxULN, fmt='%.2f'), ", ", 
                           sprintf(eDISHData$PBILIxULN, fmt='%.2f'), ")")
  #Add USUBJID as row name to be used to identify points in drill down.
  row.names(eDISHData) <- eDISHData$USUBJID
  eDISHData
}  

####################################################################################
# Define functions to prepare data and generate eDISH migration table

migrationTableMaking <- function(mydata){
# Create a eDISH migration table using gt 
  
  mydata <- mydata() %>% filter(BLNHYsLAWStatus != "None") %>% 
              droplevels()
  
  mydata$BLNHYsLAWStatus <- factor(mydata$BLNHYsLAWStatus, levels = myLabels)
  mydata$PHYsLAWStatus <- factor(mydata$PHYsLAWStatus, levels = myLabels)
    
  edishTable <- mydata %>% 
              tabyl(BLNHYsLAWStatus, PHYsLAWStatus) %>% 
              adorn_totals(c("row", "col"))
  
  names(edishTable)[names(edishTable) == "BLNHYsLAWStatus"] <- "Pretreatment Quadrants"

  migrationTable <- edishTable %>% gt(rowname_col = "PHYsLAWStatus") %>% 
    tab_spanner(label = "On-treatment Quadrants (n)", columns = 2:5) 
  # tab_stubhead(label = "Pretreatment Quadrants") %>%

  migrationTable %>%  
    tab_style(
      style = list(cell_fill("royalblue"), cell_text(color = "white", weight = "bold")),
      locations = cells_column_spanners()) %>% 
    tab_style(
      style = list(cell_fill("royalblue"), cell_text(color = "white", weight = "bold")),
      locations = cells_column_labels(columns = 1:5)) %>% 
    tab_style(
      style = list(cell_fill("navy"), cell_text(color = "white", weight = "bold")),
      locations = cells_column_labels(columns = 6)) %>%
    tab_style(
      style = list(cell_fill("royalblue"), cell_text(color = "white", weight = "bold")),
      locations = cells_stubhead()) %>%
    tab_style(
      style = list(cell_fill("royalblue"), cell_text(color = "white", weight = "bold")),
      locations = cells_stub()) %>%
    tab_style(
      style = list(cell_fill("royalblue"), cell_text(color = "white", weight = "bold")),
      locations = cells_row_groups()) %>%
    tab_style(
      style = list(cell_fill("royalblue"), cell_text(color = "white", weight = "bold")),
      locations = cells_body(columns = 1, rows = 1:4)) %>%
    tab_style(
      style = list(cell_text(align ="left")),
      locations = cells_body(columns = 1, rows = 1:5)) %>%
    tab_style(
      style = list(cell_text(align ="center")),
      locations = cells_body(columns = 2:6, rows = 1:5)) %>%
    tab_style(
      style = list(cell_fill("navy"), cell_text(color = "white", weight = "bold")),
      locations = cells_body(columns = 6, rows = 1:5)) %>%
    tab_style(
      style = list(cell_fill("navy"), cell_text(color = "white", weight = "bold")),
      locations = cells_body(columns = 1:6, rows = 5))%>%
    tab_style(
      style = list(cell_fill("gray", alpha=1.0), cell_text(color = "white", weight = "bold")),
      locations = cells_body(rows=1:4, columns=2:5))%>%
    tab_style(
      style = list(cell_fill("red3", alpha=0.5), cell_text(color = "white", weight = "bold")),
      locations = cells_body(columns = 3:5, rows = 1))%>%
    tab_style(
      style = list(cell_fill("red3", alpha=0.5), cell_text(color = "white", weight = "bold")),
      locations = cells_body(columns = 5, rows = 2:3))%>%
    tab_style(
      style = list(cell_fill("yellow3", alpha=0.5), cell_text(color = "white", weight = "bold")),
      locations = cells_body(rows = 3, columns = 3))%>%
    tab_style(
      style = list(cell_fill("yellow3", alpha=0.5), cell_text(color = "white", weight = "bold")),
      locations = cells_body(rows = 2, columns = 4))%>%
    tab_style(
      style = list(cell_fill("green3", alpha=0.5), cell_text(color = "white", weight = "bold")),
      locations = cells_body(rows = 2:4, columns = 2))%>%
    tab_style(
      style = list(cell_fill("green3", alpha=0.5), cell_text(color = "white", weight = "bold")),
      locations = cells_body(rows = 4, columns = 3:4)) %>%
    tab_footnote(
      footnote = md("*NN – near normal.*"),
      locations = NULL) %>%
    tab_footnote(
      footnote = md("*Color codes: red - migration of concern; 
                    yellow - migration of potential concern; 
                    green - migration of no concern; 
                    gray - no migration.*"),
      locations = NULL) 
}

####################################################################################
# Define functions to prepare data and generate eDISH migration table

ui <- fluidPage(
  titlePanel("Composite eDISH Plot"),
  shiny::navbarPage(
    theme = shinytheme("flatly"), 
    collapsible = TRUE,
    br(),
    tabPanel("Datasets",
       sidebarLayout(
         sidebarPanel(
           fileInput('adsl', 'Choose ADSL File',
                     accept=c('.sas7bdat'))
         ),
         mainPanel(
           tableOutput('contents1')
         )),
       
       sidebarLayout(
         sidebarPanel(
           fileInput('adlb', 'Choose ADLB File',
                     accept=c('.sas7bdat'))
         ),
         mainPanel(
           tableOutput('contents2')
         )),
       
       sidebarLayout(
         sidebarPanel(
           uiOutput('treatmentArm')
         ),
         mainPanel(
           tableOutput('contents3')
         ))
    ),
    
    tabPanel("Composite Plot",
       sidebarLayout(
         sidebarPanel(
           uiOutput('myPlotArm') # inserts a placeholder in your ui. 
                             # This leaves a “hole” that your server code can later fill in.
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("Composite Plot", plotlyOutput("edishGraph", width = "100%", height = "600"))
           ))
       ),
       
       #Panel for drill down line plots
       sidebarLayout(
         sidebarPanel(
           bsCollapse(id = "DrillDownI", multiple = FALSE, open = NULL,
                      bsCollapsePanel("Subject's information", style = "info", gt_output("subInfo"))
            )),
         
         mainPanel(
           bsCollapse(id = "DrillDownP", multiple = FALSE, open = NULL,
                      bsCollapsePanel("Subject's liver tests overtime", style = "info", plotOutput("linePlot"))
           ))
       )
    ),
    
    tabPanel("Migration Table",
       sidebarLayout(
         sidebarPanel(
           uiOutput('myTableArm') 
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("eDISH Migration Table", gt_output("eDISHTable"))
           ))
       ))
   )
)

####################################################################################
# Define functions to prepare data and generate eDISH migration table
# Define server to generate the plot and table

server <- function(input, output, session) {
  readADSL <- reactive({
    inFile1 <- input$adsl
    if (is.null(inFile1)) return(NULL)
    adslData <- data_read(inFile1$datapath)
    adslData
  })
  
  readADLB <- reactive({
    inFile2 <- input$adlb
    if (is.null(inFile2)) return(NULL)
    adlbData <- data_read(inFile2$datapath)
    adlbData
  })
  
  # Call the function that generates 
  # the plot dataset based on the user selections
  edishData <- reactive({
    eDISHDATAMaking(readADSL, readADLB)
  })
  
  # Identify Plot dimensions
  plotArea <- reactive({
    data.frame(rBMin = min(0.5, min(edishData()$PALTxBLN, edishData()$PBILIxBLN)),
               rBMax = max(0.5, max(edishData()$PALTxBLN, edishData()$PBILIxBLN)) + 1,
               rUMin = min(0.5, min(edishData()$PALTxULN, edishData()$PBILIxULN)),
               rUMax = max(0.5, max(edishData()$PALTxULN, edishData()$PBILIxULN)) + 1)
  })
  
  # Identify columns that represent treatment arms in the dataset excluding numeric ones
  output$treatmentArm <- renderUI({
    armVals <- names(edishData() %>% ungroup() %>% select(!where(is.numeric)) 
                                %>% select(contains("ARM") | starts_with("TRT")))
    
    selectInput("targetArm", "Select Treatment Variable:", sort(armVals))
  })
  
  nQuadrants <- reactive({
    QD <- edishData() %>%
      group_by(input$targetArm) %>%
      summarise(nBQs = n_distinct(BLNHYsLAWStatus, na.rm=TRUE),
                nPQs = n_distinct(PHYsLAWStatus, na.rm=TRUE)) 
    
    min(QD$nBQs, QD$nPQs)
  })
  
  # Identify unique treatment arms for the plot in the dataset
  output$myPlotArm <- renderUI({
    selectInput("plotArm", "Treatment Arm:", 
                c('All arms', sort(unique(edishData() %>% pull(input$targetArm)))))
  })
  
  # Identify unique treatment arms for table in the dataset
  output$myTableArm <- renderUI({
    selectInput("tableArm", "Treatment Arm:", 
                c('All arms', sort(unique(edishData() %>% pull(input$targetArm)))))
  })
  
  panelData <- reactive({
    fCell <- c(1, 1, 1, 1)
    panels <- data.frame(PALTxBLN=fCell, PBILIxBLN=fCell, 
                        BLNHYsLAWStatus = factor(myLabels, levels=myLabels),
                        HYsLAWStatus = factor(myQLabels, levels=myQLabels))
    panels
  })
  
  # Filter dataset for plot using user selected treatment arm.
  edishPlotData <- reactive({
    # use the key aesthetic/argument to help uniquely identify selected observations
    
    mydata <- edishData()
    
    if(input$plotArm != "All arms") {
      
      mydata <- mydata %>% filter(!!rlang::sym(input$targetArm) == input$plotArm)
    }
   
    #Add USUBJID as row name to be used to identify points in drill down.
    row.names(mydata) <- mydata$USUBJID
    mydata
  })
  
  # Filter dataset for table using user selected treatment arm.
  edishTableData <- reactive({
    if(input$tableArm == "All arms") {
      edishData()
    } 
    else {
      edishData() %>% filter(!!rlang::sym(input$targetArm) == input$tableArm)
    }
  })
  
  # Generate eDISH migration table
  edishTable <- reactive({
    migrationTableMaking(edishTableData) 
  })
  
  # Display eDISH migration table
  output$eDISHTable <- render_gt({
    edishTable()
  })
  
  # Generate and display the composite eDISH plot
  output$edishGraph <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    myKey <- row.names(edishPlotData())
    
    #print(edishPlotData())
   
    # Based of the number of baseline and on-treatment quadrants, select the base plot
    if(nQuadrants()==4){
      p0 <- ggplot(data = edishPlotData(), aes(PALTxBLN, PBILIxBLN, group=BLNHYsLAWStatus, key = myKey, text = myText)) + 
        geom_point(aes(color=BLNHYsLAWStatus, shape=BLNHYsLAWStatus), show.legend=T) + 
        facet_wrap(~HYsLAWStatus, nrow = 2, dir="h", drop=FALSE) + 
        scale_y_log10(limits=c(plotArea()$rBMin, plotArea()$rBMax)) + scale_x_log10(limits=c(plotArea()$rBMin, plotArea()$rBMax)) +  
        labs(x="Peak on-treatment ALT/BLN (Log scale)", y="Peak on-treatment BILI/BLN (Log scale)") +
        theme_bw()
    } else {
      p0 <- ggplot(data = panelData(), aes(PALTxBLN, PBILIxBLN, group=BLNHYsLAWStatus)) +
        geom_point(aes(color=BLNHYsLAWStatus, shape=BLNHYsLAWStatus), alpha=0, show.legend=T) +
        facet_wrap(~HYsLAWStatus, nrow = 2, dir="h", drop=FALSE) + 
        scale_y_log10(limits=c(plotArea()$rBMin, plotArea()$rBMax)) + scale_x_log10(limits=c(plotArea()$rBMin, plotArea()$rBMax)) + 
        labs(x="Peak on-treatment ALT/BLN (Log scale)", y="Peak on-treatment BILI/BLN (Log scale)") +
        theme_bw() +  
        geom_point(data = edishPlotData(), 
                   aes(PALTxBLN, PBILIxBLN, group=BLNHYsLAWStatus, color=BLNHYsLAWStatus, shape=BLNHYsLAWStatus, key = myKey, text = myText), show.legend=T) +
        guides(color = guide_legend(override.aes = list(alpha = 1)))
    }
      
    # Add legend (with items independent of data) and format plot area
    p1 <- p0 +
      scale_shape_manual(values=myShapes, limits = myLabels) + 
      scale_color_manual(values=myColors, limits = myLabels) +
      theme(strip.background=element_rect(fill="white", colour="darkgray", linewidth=rel(2)), 
            panel.grid = element_blank(), panel.spacing = unit(0, "lines"),
            strip.switch.pad.grid = unit(0, "cm"))
    
    # Add line markers and labels
    p20 <- p1 +
      geom_vline(xintercept = 1, color='darkgray', linetype=2) +
      geom_hline(yintercept = 1, color='darkgray', linetype=2) +
      annotate(geom="text", label='BLN', x=c(plotArea()$rMin, 1.175), y=c(1.175, plotArea()$rMin), size=3, color="black") 
    
    if(plotArea()$rBMax > 5.0) {
      p2 <- p20 + geom_vline(xintercept=c(3, 5), color='orange', linetype=3) +
        geom_hline(yintercept=c(3, 5), color='orange', linetype=3) +
        annotate(geom="text", label="x3", x=c(plotArea()$rBMin, 3.175), y=c(3.175, plotArea()$rBMin), size=3, color="black") +
        annotate(geom="text", label="x5", x=c(plotArea()$rBMin, 5.175), y=c(5.175, plotArea()$rBMin), size=3, color="black")
    } else if(plotArea()$rBMax > 3.0) {
      p2 <- p20 + geom_vline(xintercept=3, color='orange', linetype=3) + 
        geom_hline(yintercept=3, color='orange', linetype=3) +
        annotate(geom="text", label="x3", x=c(plotArea()$rMin, 3.175), y=c(3.175, plotArea()$rMin), size=3, color="black") 
    } else {p2 <- p20}
    
    
    # Format legend items and position 
    p3 <- p2 +
      guides(group = guide_colourbar(title.position="top", title.hjust = 0.5),
             shape = guide_legend(title.position="top", title.hjust = 0.5)) +
      theme(legend.position="bottom", legend.box="horizontal") 
    
    p3 %>% ggplotly(source = "eDISH", tooltip = "text") %>%  
      layout(legend = list(orientation = "h", xref='container', side="top", xanchor ="left", yref='container',
                           title = list(text = "Pretreatment Elevations:"),
                           bordercolor='darkgray', groupclick = "togglegroup")) %>% 
      event_register("plotly_click")
  })
  
  # Drill down subject line plots
  DrillData <- reactive({
    edish <- edishData() %>% 
        select(!(c("BALTxULN", "BBILIxULN", "PALTxBLN", "PBILIxBLN", "PALTxULN", "PBILIxULN", 
                   "HYsLAWStatus", "myText"))) #"BLNHYsLAWStatus", "PHYsLAWStatus", 
    adlb <- ADLBDRILL(readADLB)
    
    # Create drill down data for line plot
    edish %>% left_join(adlb, join_by(USUBJID))
  })
  
  # Identify Plot dimensions
  plotYLimit <- reactive({
    data.frame(rBMin = min(0.5, min(DrillData()$AVALxBLN, na.rm = T)),
               rBMax = max(DrillData()$AVALxBLN, na.rm = T) + 0.5,
               rUMin = min(0.5, min(DrillData()$AVALxULN, na.rm = T)),
               rUMax = max(DrillData()$AVALxULN, na.rm = T) + 0.5,
               rxMin = min(DrillData()$ADY, na.rm = T))
  })
  
  output$subInfo <- render_gt({
    drillSubj <- event_data("plotly_click", source = "eDISH")
    req(drillSubj)
    
    myData <- DrillData() %>% filter(USUBJID == drillSubj$key & AVISITN == 0 & PARAMCD == "ALT") %>% 
              select(any_of(c("USUBJID", input$targetArm, "SEX", "ADY", "BLNHYsLAWStatus", "PHYsLAWStatus")))
   
    myData$ADY <- as.character(myData$ADY)
   
    # Transpose lab data into a wide format
    myDataLN <- myData %>% pivot_longer(cols=c("USUBJID", input$targetArm, "SEX", "ADY", "BLNHYsLAWStatus", "PHYsLAWStatus"), 
                                        names_to="Subject",
                                        values_to="attributes")
    myDataLN$Subject <- c('ID', 'Arm', 'Sex', 'Baseline visit', 'Baseline Q', 'Peak Q')
    
    myDataLN %>% gt() %>% sub_missing() %>% 
      tab_options(column_labels.hidden = TRUE)
  })
  
  drillSubjData <- reactive({
    drillSubj <- event_data("plotly_click", source = "eDISH")
    req(drillSubj)
    DrillData() %>% filter(USUBJID == drillSubj$key)
  })
  
  BLNADY <- reactive({
    drillSubjData() %>% filter(AVISITN == 0 & PARAMCD == "ALT") %>% ungroup() %>% select(any_of(c("ADY")))
  })
  
  output$linePlot <- renderPlot({
    #print(drillSubjData() %>% select(c('USUBJID', 'AVISIT', 'ADY', 'BASE', 'AVAL', 'AVALxBLN', 'AVALxULN')))
    #print(BLNADY())
    
    q1 <- ggplot(data = drillSubjData()) + 
        geom_line(aes(ADY, AVALxBLN, color=PARAMCD, group=PARAMCD), show.legend=F) + 
        geom_point(aes(ADY, AVALxBLN, color=PARAMCD, group=PARAMCD), show.legend=F) +
        geom_hline(yintercept = 1, color='darkgray', linetype=2) +
        geom_vline(xintercept = BLNADY()$ADY, color='darkgray', linetype=2) +
        annotate(geom="text", label='BLN', x=plotYLimit()$rxMin, y=1.175, size=3, color="black") +
        labs(x="", y="Test values /BLN (Log scale)") +
        scale_y_log10(limits=c(plotYLimit()$rBMin, plotYLimit()$rBMax)) + 
        theme_bw()
    
    q20 <- ggplot(data = drillSubjData()) + 
      geom_line(aes(ADY, AVALxULN, color=PARAMCD, group=PARAMCD), show.legend=T) + 
      geom_point(aes(ADY, AVALxULN, color=PARAMCD, group=PARAMCD), show.legend=F) +
      geom_vline(xintercept = BLNADY()$ADY, color='darkgray', linetype=3) +
      labs(x="Analysis study day", y="Test values /ULN (Log scale)") +
      scale_y_log10(limits=c(plotYLimit()$rUMin, plotYLimit()$rUMax)) +
      guides(color=guide_legend(title="Liver tests:")) + 
      theme_bw() +
      theme(legend.position="bottom", legend.box="horizontal") 
    
    
    if(plotYLimit()$rUMax > 5.0) {
      q2 <- q20 + geom_hline(yintercept=c(2, 3, 5), color='orange', linetype=3) + 
        annotate(geom="text", label="x2", x=plotYLimit()$rxMin, y=2.175, size=3, color="black") +
        annotate(geom="text", label="x3", x=plotYLimit()$rxMin, y=3.175, size=3, color="black") +
        annotate(geom="text", label="x5", x=plotYLimit()$rxMin, y=5.175, size=3, color="black")
    } else if(plotYLimit()$rUMax > 3.0) {
      q2 <- q20 + geom_hline(yintercept=c(2, 3), color='orange', linetype=3) + 
            annotate(geom="text", label="x2", x=plotYLimit()$rxMin, y=2.175, size=3, color="black") +
            annotate(geom="text", label="x3", x=plotYLimit()$rxMin, y=3.175, size=3, color="black") 
    } else if(plotYLimit()$rUMax > 2.0) {
      q2 <- q20 + geom_vline(xintercept=2, color='orange', linetype=3) + 
        geom_hline(yintercept=2, color='orange', linetype=3) +
        annotate(geom="text", label="x2", x=plotYLimit()$rxMin, y=2.175, size=3, color="black") 
    } else {q2 <- q20}

    q1 / q2
    
  })
  
}


#########################################################
# Run the app ----
shinyApp(ui = ui, server = server) 

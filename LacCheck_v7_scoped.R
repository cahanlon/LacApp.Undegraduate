################################################
#PUBLISHED AS LacOperon_v3
################################################



# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(DT)
library(tidyr)
library(dplyr)
library(combinat)
library(DescTools)
library(bslib)
library(shinyjs)

##############################################


ui <-  
  fluidPage(
    useShinyjs(),
    bootstrapPage(
      theme = bs_theme(version = 5),
      actionButton("check","Check my answer"),
      actionButton("refresh","Give me another"),
      tabsetPanel(
        id = "panels",
        tabPanel("Tutorial", 
                 h3("Gene Regulation"),
                 p("Gene expression (the process of a segment of DNA being transcribed and translated) is tightly controlled. 
                   After all, it is disadvantageous for cells to waste energy making a protein when it is not needed. The process of regulating the expression of a gene is called ", strong("gene regulation."), 
                   ("While gene regulation can be  exquisitely complicated, here we will use a simple model to illustrate to concept.")),
                 p(
                   h3("The Lac Operon"),
                   "The", em("lac"), "operon consists of five components:",
                      div(strong("LacI:"), "a gene that encodes the", em("lac"), "inhibitor protein.", style="margin-left:40px"),
                      div(strong("Promoter:"), "the promoter upstream of LacZ and LacY.", style="margin-left:40px"),
                      div(strong("LacO:"), "stands for", em("'lac"), "operator' and is the DNA sequence that the", em("lac"), "inhibitor binds", style="margin-left:40px"),
                      div(strong("LacZ:"), "gene encoding beta-galactosidase.", style="margin-left:40px"),
                      div(strong("LacY:"), "gene encoding permease.", style="margin-left:40px"),
                   br("LacZ and LacY encode genes that enable lactose to enter the cell (permease) and for it to be metabolized by the cell (beta-galactosidase). 
                      When there is no lactose in the environment, the cell keeps transcription of LacZ and Y off to conserve energy. 
                      This is accomplished through LacI protein binding to LacO, thus blocking the ability of RNA polymerase to bind the promoter and initiate transcription. 
                      When lactose is present, lactose binds to the", em("lac"), "inhibitor (LacI) protein, which causes LacI to 'fall off' the operator; this enables RNA polymerase to bind to the promoter and transcribe the LacZ and LacY genes. 
                      Therefore, in a normal (or wild type) scenario, when there is no lactose present, LacZ and LacY are OFF; when lactose is present, LacZ and LacY are ON.")),
                 p(
                   h3("Wild type and Mutant Alleles"),
                   "There are three possible alleles for each component of the", em("lac"), "operon:",
                   div(strong("+:"), "the wild type (or typical) version of the component", style="margin-left:40px"),
                   div(strong("-:"), "a null (non-existent) or non-functional mutant version of the component", style="margin-left:40px"),
                   div(strong("CA:"), "a mutant component that is constitutively (or always) active. ", em("NOTE: In this tutorial, only LacO will be constitutively active."), style="margin-left:40px")
                   ),
                 p(
                   h3("Genotypes"),
                   "There are two genotypes for you to work with:",
                   div(strong("Haploid:"), "there is one copy of each lac operon component.", style="margin-left:40px"),
                   div(strong("Merodiploid:"), "there are two copies of each lac operon component. This allows you to investigate the activity
                       of", em("cis"), "and", em("trans"), "factors.", style="margin-left:40px")
                 ),
                 p(
                   h3("Using this tutorial"),
                   p("Watch a video for how to use this tutorial", tags$a(href="https://www.youtube.com/watch?v=atkgPjCIYQk", "here!", target="_blank")),
                   
                   p("Select the 'Haploid' or 'Merodiploid' tab. You will be presented with a genotype and four columns with radio buttons. 
                   The columns correspond to LacY without lactose (LacY/-lac), LacY with lactose (LacY/+lac), LacZ without lactose (LacZ/-lac), and LacZ with lactose (LacZ/+lac).
                   Use the radio buttons to predict whether each gene will be transcribed (ON) or not (OFF). Click 'Check my answer' to see how you did. A tracker will also
                   appear to count how many times in a row you can correctly predict the output of the lac operon. To get a new genotype, click the 'Give me another' button.")
                 )),
        tabPanel("The LacOperon: Haploids",
                 div(class = "container-fluid",
                     h5("Draw the components of the Lac operon and use the genotype below to predict the effects on B-Gal and Permease with and without lactose.
                When you have a solution, click 'Check my answer' to see if you are correct."),
                     h5(textOutput("A")),
                     h5(textOutput("ID")),
                     div(class="row",
                         div(class= "col-sm-4",
                             DT::dataTableOutput("new.geno")),
                         div(class= "col-sm-2",
                             radioButtons("Zminus", "LacZ/-lac", choices = c("ON", "OFF")), textOutput("value1")),
                         div(class= "col-sm-2",
                             radioButtons("Zplus", "LacZ/+lac", choices = c("ON", "OFF")),  textOutput("value2")),
                         div(class= "col-sm-2",
                             radioButtons("Yminus", "LacY/-lac", choices = c("ON", "OFF")), textOutput("value3")),
                         div(class= "col-sm-2",
                             radioButtons("Yplus", "LacY/+lac", choices = c("ON", "OFF")), textOutput("value4")),
                         div(class= "col-sm-12",
                             htmlOutput("Explain.Haplo"))
                     )
                 )
        ),
        tabPanel("The LacOperon: Merodiploids",
                 div(class = "container-fluid",
                     h5("Draw the components of the Lac operon and use the genotype below to predict the effects on B-Gal and Permease with and without lactose.
                When you have a solution, click 'Check my answer' to see if you are correct."),
                     h5(textOutput("B")),
                     h5(textOutput("ID2")),
                     div(class="row",
                         div(class= "col-sm-4",
                             DT::dataTableOutput("new.geno.MD")),
                         div(class= "col-sm-2",
                             radioButtons("Zminus_MD", "LacZ/-lac", choices = c("ON", "OFF")), textOutput("value1_MD")),
                         div(class= "col-sm-2",
                             radioButtons("Zplus_MD", "LacZ/+lac", choices = c("ON", "OFF")),  textOutput("value2_MD")),
                         div(class= "col-sm-2",
                             radioButtons("Yminus_MD", "LacY/-lac", choices = c("ON", "OFF")), textOutput("value3_MD")),
                         div(class= "col-sm-2",
                             radioButtons("Yplus_MD", "LacY/+lac", choices = c("ON", "OFF")), textOutput("value4_MD")),
                         div(class= "col-sm-12",
                             htmlOutput("Explain.Haplo.MD"))
                     )
                 )
        ),
        tabPanel("Bug Report", "Did you find a mistake? Email", a(href="lacON.lacOFF@gmail.com","lacON.lacOFF@gmail.com"), "with the genotype and phenotype information.")
      # tabPanel("Tracking", DT::dataTableOutput("tracks"), DT::dataTableOutput("tracks2"))
    
      )))



server <- function(input,output,session) 
  



  
{

#Setting up all possible haploid permutations for the lac operon genes as WT, mutant, or constitutively active for HAPLOID
  f <- (c("+","-","CA"))
  x <- f
  n <- 5
  m <- 5
  lac.permutations<-as.data.frame(CombSet(x, m, repl=TRUE, ord=TRUE))
  names(lac.permutations)<- c("LacI","Promoter","LacO","LacZ","LacY")
 
  #Limiting permutations to only LacO being constitutively active
  lac.permutations <- subset(lac.permutations, LacI != "CA" & Promoter != "CA" & LacZ != "CA" & 
                               LacY != "CA")
 
#Logic of the lac operon
  lac.permutations <- lac.permutations %>%
    mutate(`Bgal/-lac` = case_when(LacZ == "-" ~ "OFF",
                                   Promoter == "-" ~ "OFF",
                                   LacI == "-" ~ "ON",
                                   LacI == "CA" & LacO == "+" ~ "OFF",
                                   LacO == "+" ~ "OFF",
                                   LacO == "+" ~ "OFF",
                                   LacO == "-" | LacO == "CA" ~ "ON",
                                   Promoter == "CA" ~ "ON",
                                   Promoter == "+" ~ "OFF",
                                   LacZ == "CA" ~ "ON",
                                   TRUE ~ "NA")) %>%
    mutate(`Bgal/+lac` = case_when(LacZ == "-" ~ "OFF",
                                   Promoter == "-" ~ "OFF",
                                   LacI == "-" ~ "ON",
                                   LacI == "CA" & LacO == "+" ~ "OFF",
                                   LacO == "+" ~ "ON",
                                   LacO == "-" | LacO == "CA" ~ "ON",
                                   Promoter == "CA" ~ "ON",
                                   Promoter == "+" ~ "ON",
                                   TRUE ~ "NA")) %>%
    mutate(`Permease/-lac` = case_when(LacY == "-" ~ "OFF",
                                       Promoter == "-" ~ "OFF",
                                       LacI == "-" ~ "ON",
                                       LacI == "CA" & LacO == "+" ~ "OFF",
                                       LacO == "+" ~ "OFF",
                                       LacO == "-" | LacO == "CA" ~ "ON",
                                       Promoter == "CA" ~ "ON",
                                       Promoter == "+" ~ "OFF",
                                       LacY == "CA" ~ "ON",
                                       TRUE ~ "NA")) %>%
    mutate(`Permease/+lac` = case_when(LacY == "-" ~ "OFF",
                                       Promoter == "-" ~ "OFF",
                                       LacI == "-" ~ "ON",
                                       LacI == "CA" & LacO == "+" ~ "OFF",
                                       LacO == "+" ~ "ON",
                                       LacO == "-" | LacO == "CA" ~ "ON",
                                       Promoter == "CA" ~ "ON",
                                       Promoter == "+" ~ "ON",
                                       TRUE ~ "NA"))
  

  
#Creating genotype -- haploid
  input.data <- lac.permutations[1,]
  Element1 = c("I","P","O","Z","Y")
  list <- as.data.frame(paste(Element1,input.data[1,c(1:5)], sep = "", collapse = ' '))
  list <- as.data.frame(gsub("OCA", "O(CA)", list))
  names(list)<- "Genotype"
  
  RI <- reactiveValues(data = input.data)
  RV <- reactiveValues(data = list)
  
#Creating "unique" ID for each user
  
  InputID <- function(n = 10000) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }
  
  RID <- reactiveValues(data = InputID(1))
  
#############################################
#############################################
  
  
  
#Setting up all possible haploid permutations for the lac operon genes as WT, mutant, or constitutively active for DIPKLOID
  g <- (c("+","-","CA"))
  d <- g
  p <- 10
  q <- 10
  lac.permutations_MD<-as.data.frame(CombSet(d, q, repl=TRUE, ord=TRUE))
  names(lac.permutations_MD)<- c("LacI_1","Promoter_1","LacO_1","LacZ_1","LacY_1", "LacI_2","Promoter_2","LacO_2","LacZ_2","LacY_2")
  
  
  #Limiting permutations to only LacO being constitutively active
  lac.permutations_MD <- subset(lac.permutations_MD , LacI_1 != "CA" & Promoter_1 != "CA" & LacZ_1 != "CA" & 
                                  LacY_1 != "CA" & LacI_2 != "CA" & Promoter_2 != "CA" &
                                  LacZ_2 != "CA" & LacY_2 != "CA")
  
  #Logic of lac operon -- DIPLOID
  lac.permutations_MD  <- lac.permutations_MD %>%
    mutate(`Bgal/-lac` = case_when((Promoter_1  == "-" & Promoter_2 == "-") ~ "OFF",
                                   (LacZ_1 == "-" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_1 == "-" & LacZ_1 == "+" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_2 == "-" & LacZ_2 == "+" & LacZ_1 == "-") ~ "OFF",
                                   ((LacO_1 == "-" & LacO_2 == "-") | (LacO_1 == "CA" & LacO_2 == "CA") | (LacO_1 == "-" & LacO_2 == "CA") | (LacO_1 == "CA"& LacO_2 == "-") & (Promoter_1  == "+") & (LacZ_1 == "+")) ~ "ON",
                                   ((LacO_1 == "-" & LacO_2 == "-") | (LacO_1 == "CA" & LacO_2 == "CA") | (LacO_1 == "-" & LacO_2 == "CA") | (LacO_1 == "CA"& LacO_2 == "-") & (Promoter_2  == "+") & (LacZ_2 == "+")) ~ "ON",
                                   ((LacI_1 == "-" & LacI_2 == "-") & (Promoter_1  == "+") & LacO_1 == "-" & (LacZ_1 == "+")) ~ "ON",
                                   ((LacI_1 == "-" & LacI_2 == "-") & (Promoter_2  == "+") & LacO_2 == "-" & (LacZ_2 == "+")) ~ "ON",
                                   ((Promoter_1 == "+") & (LacO_1  == "-" |LacO_1  == "CA") & (LacZ_1  == "+")) ~ "ON",
                                   ((Promoter_2 == "+") & (LacO_2  == "-" |LacO_2  == "CA") & (LacZ_2  == "+")) ~ "ON",
                                   ((LacI_1 == "-" & LacI_2== "-") & (Promoter_1 == "+") & LacZ_1 == "+") ~ "ON",
                                   ((LacI_1 == "-" & LacI_2== "-") & (Promoter_2 == "+") & LacZ_2 == "+") ~ "ON",
                                   TRUE ~ "OFF")) %>%
    mutate(`Bgal/+lac` = case_when((LacZ_1  == "-" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_1  == "-" & Promoter_2 == "-") ~ "OFF",
                                   (Promoter_1  == "-" & Promoter_2 == "+" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_2  == "-" & Promoter_1 == "+" & LacZ_1 == "-") ~ "OFF",
                                   (Promoter_1 == "-" & LacZ_1 == "+" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_2 == "-" & LacZ_2 == "+" & LacZ_1 == "-") ~ "OFF",
                                   TRUE ~ "ON")) %>%
    mutate(`Permease/-lac` = case_when((Promoter_1  == "-" & Promoter_2 == "-") ~ "OFF",
                                       (LacY_1 == "-" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_1 == "-" & LacY_1 == "+" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_2 == "-" & LacY_2 == "+" & LacY_1 == "-") ~ "OFF",
                                       ((LacO_1 == "-" & LacO_2 == "-") | (LacO_1 == "CA" & LacO_2 == "CA") | (LacO_1 == "-" & LacO_2 == "CA") | (LacO_1 == "CA"& LacO_2 == "-") & (Promoter_1  == "+") & (LacY_1 == "+")) ~ "ON",
                                       ((LacO_1 == "-" & LacO_2 == "-") | (LacO_1 == "CA" & LacO_2 == "CA") | (LacO_1 == "-" & LacO_2 == "CA") | (LacO_1 == "CA"& LacO_2 == "-") & (Promoter_2  == "+") & (LacY_2 == "+")) ~ "ON",
                                       ((LacI_1 == "-" & LacI_2 == "-") & (Promoter_1  == "+") & LacO_1 == "-" & (LacY_1 == "+")) ~ "ON",
                                       ((LacI_1 == "-" & LacI_2 == "-") & (Promoter_2  == "+") & LacO_2 == "-" & (LacY_2 == "+")) ~ "ON",
                                       ((Promoter_1 == "+") & (LacO_1  == "-" |LacO_1  == "CA") & (LacY_1  == "+")) ~ "ON",
                                       ((Promoter_2 == "+") & (LacO_2  == "-" |LacO_2  == "CA") & (LacY_2  == "+")) ~ "ON",
                                       ((LacI_1 == "-" & LacI_2== "-") & (Promoter_1 == "+") & LacY_1 == "+") ~ "ON",
                                       ((LacI_1 == "-" & LacI_2== "-") & (Promoter_2 == "+") & LacY_2 == "+") ~ "ON",
                                       TRUE ~ "OFF")) %>%
    mutate(`Permease/+lac` = case_when((LacY_1  == "-" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_1  == "-" & Promoter_2 == "-") ~ "OFF",
                                       (Promoter_1 == "-" & LacY_1 == "+" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_2 == "-" & LacY_2 == "+" & LacY_1 == "-") ~ "OFF",
                                       (Promoter_1  == "-" & Promoter_2 == "+" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_2  == "-" & Promoter_1 == "+" & LacY_1 == "-") ~ "OFF",
                                       TRUE ~ "ON"))
 
#Creating genotype -- diploid
  input.data_MD <- sample_n(lac.permutations_MD, 1)
  Element1 = c("I","P","O","Z","Y")
  Element2 = c("I","P","O","Z","Y")
  list1 <- as.data.frame(paste(Element1,input.data_MD[1,c(1:5)], sep = "", collapse = ' '))
  list1 <- gsub("OCA", "O(CA)", list1)
  names(list1)<- "Genotype"
  list2 <- as.data.frame(paste(Element2,input.data_MD[1,c(6:10)], sep = "", collapse = ' '))
  list2 <- gsub("OCA", "O(CA)", list2)
  names(list2)<- "Genotype"
  list_MD <- rbind(list1, list2)
  rownames(list_MD) <- c("1", "2")
  names(list_MD)<- "Genotype"
  
  RI_MD<- reactiveValues(data = input.data_MD)
  RV_MD <- reactiveValues(data = list_MD)
  
  #####
  
  
  
  
  
#Output: Creation of new genotype  
  output$new.geno = DT::renderDataTable(RV$data, options=list(dom='t'))
  output$new.geno.MD = DT::renderDataTable(RV_MD$data, options=list(dom='t'))

#ID call  
  #If we want ID to be different between haploid and diploid
  #output$ID = renderText(paste(c("Unique ID = ", InputID(1))))
  #output$ID2 = renderText(paste(c("Unique ID = ", InputID(1))))
  
  #If we want ID to be the same between haploid and diploid
  output$ID <- renderText({paste("Unique ID: ", RID$data)})
  output$ID2 <- renderText({paste("Unique ID: ", RID$data)})
  
  
#Disabling buttons while moving between tabs
  observeEvent(input$check, {
    if(input$panels == "The LacOperon: Haploids"){
      shinyjs::disable("check")}}
    )
  observeEvent(input$refresh, {
    if(input$panels == "The LacOperon: Haploids"){
      shinyjs::enable("check")}}
  )

   observeEvent(input$check, {
    if(input$panels == "The LacOperon: Merodiploids"){
     shinyjs::disable("check")}}
    )
   
  observeEvent(input$refresh, {
    if(input$panels == "The LacOperon: Merodiploids"){
    shinyjs::enable("check")}}
  )

   observeEvent(input$panels, {
     if(input$panels == "The LacOperon: Merodiploids" && is.na(report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),2])) {
        shinyjs::enable("check")}
       else if(input$panels == "The LacOperon: Haploids" && is.na(report.dataRV$data[(nrow(report.dataRV$data)),2])) {
         shinyjs::enable("check")}
     else shinyjs::disable("check")
     }
    )



#making dataframe to house HAPLOID attempts  
  report.data <- data.frame(matrix(ncol=3, nrow=0))
  names(report.data) <- c("Attempt", "correct", "consec")
    report.data[1,1] = 1
  report.dataRV = reactiveValues(data = report.data)
  
 output$tracks = DT::renderDataTable(report.dataRV$data)
 
#making dataframe to house MERODIPLOID attempts  
 report.data2 <- data.frame(matrix(ncol=3, nrow=0))
 names(report.data2) <- c("Attempt", "correct", "consec")
 report.data2[1,1] = 1
 report.dataRV_MD = reactiveValues(data = report.data2)
 
 output$tracks2 = DT::renderDataTable(report.dataRV_MD$data)
 

#making inputs reactive after click, not all the time   
  c1 <- reactive(input$Zminus)
  c2 <- reactive(input$Zplus)
  c3 <- reactive(input$Yminus)
  c4 <- reactive(input$Yplus)
  
  c1_MD <- reactive(input$Zminus_MD)
  c2_MD <- reactive(input$Zplus_MD)
  c3_MD <- reactive(input$Yminus_MD)
  c4_MD <- reactive(input$Yplus_MD)
  
  

    
#REFRESH: Button to make new genotype
  #Haploid
  observeEvent(input$refresh, {
    if(input$panels == "The LacOperon: Haploids"){
      
    RI$data <- sample_n(lac.permutations, 1)
    RV$data[1,1] <- paste(c("I","P","O","Z","Y"), RI$data[1,c(1:5)], sep = "", collapse = ' ')
    RV$data[1,1] <- gsub("OCA", "O(CA)", RV$data[1,1])
    rownames(RV$data) <- "1"
    
    #making is so nothing is below input areas and that a reset changes the input to ON
    output$value1 = renderText("")
    output$value2 = renderText("")
    output$value3 = renderText("")
    output$value4 = renderText("")
    output$Explain.Haplo = renderText("")
    
    updateSelectInput(session, "Zminus", selected = "ON")
    updateSelectInput(session, "Zplus", selected = "ON")
    updateSelectInput(session, "Yminus", selected = "ON")
    updateSelectInput(session, "Yplus", selected = "ON")
    
    }

    #Diploid 
    if(input$panels == "The LacOperon: Merodiploids"){
      
    RI_MD$data <- sample_n(lac.permutations_MD, 1)
    RV_MD$data[1,1] <- paste(c("I","P","O","Z","Y"), RI_MD$data[1,c(1:5)], sep = "", collapse = ' ')
    RV_MD$data[1,1] <- gsub("OCA", "O(CA)", RV_MD$data[1,1])
    RV_MD$data[2,1] <- paste(c("I","P","O","Z","Y"), RI_MD$data[1,c(6:10)], sep = "", collapse = ' ')
    RV_MD$data[2,1] <- gsub("OCA", "O(CA)", RV_MD$data[2,1])
    rownames(RV_MD$data) <- c("1", "2")
    names(RV_MD$data)<- "Genotype"
    
    #making is so nothing is below input areas and that a reset changes the input to ON
    output$value1_MD = renderText("")
    output$value2_MD = renderText("")
    output$value3_MD = renderText("")
    output$value4_MD = renderText("")
    output$Explain.Haplo.MD = renderText("")
    updateSelectInput(session, "Zminus_MD", selected = "ON")
    updateSelectInput(session, "Zplus_MD", selected = "ON")
    updateSelectInput(session, "Yminus_MD", selected = "ON")
    updateSelectInput(session, "Yplus_MD", selected = "ON")
    }
  })

#Making a dataframe to keep track of correct answers
  #haploid
observeEvent(input$refresh, {
  if(input$panels == "The LacOperon: Haploids"){
   new_row = data.frame(matrix(ncol=3, nrow=1))
    names(new_row) <- c("Attempt", "correct", "consec")
    new_row[1,1] = nrow(report.dataRV$data)+1
    new_row[1,3] = report.dataRV$data[(nrow(report.dataRV$data)),3]
    report.dataRV$data=rbind(report.dataRV$data, new_row)}
    }
  )


#Making a dataframe to keep track of correct answers
  #diploid
observeEvent(input$refresh, {
  if(input$panels == "The LacOperon: Merodiploids")
  {new_row2 = data.frame(matrix(ncol=3, nrow=1))
  names(new_row2) <- c("Attempt", "correct", "consec")
  new_row2[1,1] = nrow(report.dataRV_MD$data)+1
  new_row2[1,3] = report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3]
  report.dataRV_MD$data=rbind(report.dataRV_MD$data, new_row2)}}
) 




#Button to check answer, give feedback, and track number of correct answers (haploid)
observeEvent(input$check, {
  
  d1 <- c1()
  d2 <- c2()
  d3 <- c3()
  d4 <- c4()
  
  if(input$panels == "The LacOperon: Haploids")
  {output$value1 <- renderText(
    {if(d1 == RI$data[1,6]) "Correct"
      else "Try again"})
  output$value2 <- renderText(
    {if(d2 == RI$data[1,7])  "Correct"
      else "Try again"})
  output$value3 <- renderText(
    {if(d3 == RI$data[1,8]) "Correct"
      else "Try again"})
  output$value4 <- renderText(
    {if(d4 == RI$data[1,9])  "Correct"
      else "Try again" })
  }
  else (NULL)


#Giving feedback: Haploid    
  if(input$panels == "The LacOperon: Haploids")
      {if ((input$Zminus == RI$data[1,6]) & (input$Zplus == RI$data[1,7]) & (input$Yminus == RI$data[1,8]) & (input$Yplus == RI$data[1,9])) output$Explain.Haplo <- renderText("")
        else 
          output$Explain.Haplo <- renderUI({
          str1 <- paste("<b> EXPLANATION </b>")
          str2 <- paste("<b> LacZ/-lac: </b>", 
                      (if (RI$data[1,2] == "-") "Because the Promoter is '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                        if (RI$data[1,4] == "-") "Because LacZ is '-', transcription will be OFF because the gene is nonfunctional."     else           #lacZ
                        if (RI$data[1,1] == "-") "Because LacI is '-', transcription will be ON because the inhibitor is not functional." else           #LacI 
                        if (RI$data[1,1] == "CA" & RI$data[1,3] == "+") "Because LacI is 'CA', transcription will be OFF because the inhibitor will always be bound." else      #LacI and LacO
                        if (RI$data[1,3] == "+") "Because the operator is functional, the inhibitor will be bound when no lactose is present, thus keeping transcription OFF." else        #LacO
                        if (RI$data[1,3] == "-" | RI$data[1,3] == "CA") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                        if (RI$data[1,2] == "+") "Because the Promoter is '+' but RNA polymerase is blocked due to the the inhibitor protein, transcription will be OFF." else        #promoter
                        if (RI$data[1,4] == "CA") "Because LacZ is 'CA', so transcription will be ON."  else             #lacZ 
                        ("")
                        )
                      )
          str3 <- paste("<b> LacZ/+lac: </b>", 
                        (if  (RI$data[1,2] == "-") "Because the Promoter is '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                          if (RI$data[1,4] == "-") "Because LacZ is '-', transcription will be OFF because the gene is nonfunctional."     else           #lacZ
                          if (RI$data[1,1] == "-") "Because LacI is '-', transcription will be ON because the inhibitor is not functional." else           #LacI 
                          if (RI$data[1,1] == "CA" & RI$data[1,3] == "+") "Because LacI is 'CA', transcription will be OFF because the inhibitor will always be bound." else      #LacI and LacO
                          if (RI$data[1,3] == "+") "Because the operator is functional, meaning the inhibitor will not bind when lactose is present, thus transcription is ON" else        #LacO
                          if (RI$data[1,3] == "-" | RI$data[1,3] == "CA") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                          if (RI$data[1,2] == "CA") "Because the Promoter is always bound by RNA polymerase, thus keeping transcription will be ON." else        #promoter
                          if (RI$data[1,2] == "+") "Because the Promoter is functional, thus  transcription is ON."  else             #lacZ 
                          ("")
                        )
                      )
          str4 <- paste("<b> LacY/-lac: </b>", "Because",
                        (if (RI$data[1,2] == "-") "Because the Promoter is '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                          if (RI$data[1,5] == "-") "Because LacY is '-', transcription will be OFF because the gene is nonfunctional."     else           #lacZ
                            if (RI$data[1,1] == "-") "Because LacI is '-', transcription will be ON because the inhibitor is not functional." else           #LacI 
                              if (RI$data[1,1] == "CA" & RI$data[1,3] == "+") "Because LacI is 'CA', transcription will be OFF because the inhibitor will always be bound." else      #LacI and LacO
                                if (RI$data[1,3] == "+") "Because the operator is functional, the inhibitor will be bound when no lactose is present, thus keeping transcription OFF." else        #LacO
                                  if (RI$data[1,3] == "-" | RI$data[1,3] == "CA") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                    if (RI$data[1,2] == "+") "Because the Promoter is '+' but RNA polymerase is blocked due to the the inhibitor protein, transcription will be OFF." else        #promoter
                                      if (RI$data[1,5] == "CA") "Because LacZ is 'CA', so transcription will be ON."  else             #lacZ 
                                        ("")
                        )
          )

          str5 <- paste("<b> LacY/+lac: </b>", "Because",
                        (if  (RI$data[1,2] == "-") "Because the Promoter is '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                          if (RI$data[1,5] == "-") "Because LacZ is '-', transcription will be OFF because the gene is nonfunctional."     else           #lacZ
                            if (RI$data[1,1] == "-") "Because LacI is '-', transcription will be ON because the inhibitor is not functional." else           #LacI 
                              if (RI$data[1,1] == "CA" & RI$data[1,3] == "+") "Because LacI is 'CA', transcription will be OFF because the inhibitor will always be bound." else      #LacI and LacO
                                if (RI$data[1,3] == "+") "Because the operator is functional, meaning the inhibitor will not bind when lactose is present, thus transcription is ON" else        #LacO
                                  if (RI$data[1,3] == "-" | RI$data[1,3] == "CA") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                    if (RI$data[1,2] == "CA") "Because the Promoter is always bound by RNA polymerase, thus keeping transcription will be ON." else        #promoter
                                      if (RI$data[1,2] == "+") "Because the Promoter is functional, thus  transcription is ON."  else             #lacZ 
                                        ("")
                        )
          )
        HTML(paste("<hr>", str1, "<br/>", str2, "<br/><br/>",str3, "<br/><br/>",str4, "<br/><br/>",str5))
        #HTML(paste("<hr>", str1, str2, str3, str4, str5,  sep = '<br/><br/>'))
        
      })
    }
  else ("")  
  
  
  
#Adding in new row to dataframe when check is performed 
  if(input$panels == "The LacOperon: Haploids")
  {new_row = data.frame(matrix(ncol=3, nrow=1))
  names(new_row) <- c("Attempt", "correct", "consec")
  new_row[1,1] = nrow(report.dataRV$data)+1
  new_row[1,3] = report.dataRV$data[(nrow(report.dataRV$data)),3]
  report.dataRV$data=rbind(report.dataRV$data, new_row)
  }
  else (NULL)
})
  
#Counting consecutive answers correct (haploid)
observeEvent(input$check, {
  if(input$panels == "The LacOperon: Haploids")
  {
  if((input$Zminus == RI$data[1,6]) & (input$Zplus == RI$data[1,7]) & (input$Yminus == RI$data[1,8]) & (input$Yplus == RI$data[1,9]))
    report.dataRV$data[(nrow(report.dataRV$data)),2] = "yes"
   else report.dataRV$data[(nrow(report.dataRV$data)),2] = "no"
   
   if(report.dataRV$data[(nrow(report.dataRV$data)),2] == "yes")
     report.dataRV$data[(nrow(report.dataRV$data)),3] = 1
   else report.dataRV$data[(nrow(report.dataRV$data)),3] = 0
   
   if(nrow(report.dataRV$data)>2 & report.dataRV$data[(nrow(report.dataRV$data)),2] == "yes")
     report.dataRV$data[(nrow(report.dataRV$data)),3] = report.dataRV$data[(nrow(report.dataRV$data))-1,3] + report.dataRV$data[(nrow(report.dataRV$data)),3]
   else if(report.dataRV$data[(nrow(report.dataRV$data)),2] == "yes")
     report.dataRV$data[(nrow(report.dataRV$data)),3] = 1
   else report.dataRV$data[(nrow(report.dataRV$data)),3] = 0
   
   output$A = renderText({
     paste("Number in a row correct = ",(report.dataRV$data[(nrow(report.dataRV$data)),3]), sep = "")})
  }
})


 
#Button to check answer, give feedback, and track number of correct answers (diploid)
observeEvent(input$check, {  
    
    d1_MD <- c1_MD()
    d2_MD <- c2_MD()
    d3_MD <- c3_MD()
    d4_MD <- c4_MD()
    
    if(input$panels == "The LacOperon: Merodiploids")
    {output$value1_MD <- renderText(
      {if(d1_MD == RI_MD$data[1,11]) "Correct"
        else "Try again"})
    output$value2_MD <- renderText(
      {if(d2_MD == RI_MD$data[1,12]) "Correct"
        else "Try again"})
    output$value3_MD <- renderText(
      {if(d3_MD == RI_MD$data[1,13]) "Correct"
        else "Try again"})
    output$value4_MD <- renderText(
      {if(d4_MD == RI_MD$data[1,14]) "Correct"
        else "Try again"})
    }
    else (NULL)
    
    
#Giving feedback: Diploid    
    
    if(input$panels == "The LacOperon: Merodiploids")
    {if ((input$Zminus_MD == RI_MD$data[1,11]) && (input$Zplus_MD == RI_MD$data[1,12]) && (input$Yminus_MD == RI_MD$data[1,13]) & (input$Yplus_MD == RI_MD$data[1,14])) 
      output$Explain.Haplo.MD <- renderText("")
    
    output$Explain.Haplo.MD <- renderUI({
      str1 <- paste("<b> EXPLANATION </b>")
      str2 <- paste("<b> LacZ/-lac: </b>", 
                    (if (RI_MD$data[1,2] == "-" & RI_MD$data[1,7] == "-") "Because the Promoters are '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                      if (RI_MD$data[1,4] == "-" & RI_MD$data[1,9] == "-") "Because both copies of LacZ are '-', transcription will be OFF because the genes are nonfunctional."     else           #lacZ
                        if (RI_MD$data[1,2] == "-" & RI_MD$data[1,4] == "+" & RI_MD$data[1,9] == "-") "Because the Promotor for wild-type copy of LacZ is nonfunctional and the other copy of LacZ is nonfunctional, transcription will be OFF." else           #Promoter
                          if (RI_MD$data[1,4] == "-" & RI_MD$data[1,7] == "-" & RI_MD$data[1,9] == "+") "Because the Promotor for wild-type copy of LacZ is nonfunctional and the other copy of LacZ is nonfunctional, transcription will be OFF." else           #Promoter
                            if (((RI_MD$data[1,3] == "-" & RI_MD$data[1,8] == "-") | (RI_MD$data[1,3] == "CA" & RI_MD$data[1,8] == "CA") | (RI_MD$data[1,3] == "-" & RI_MD$data[1,8] == "CA") | (RI_MD$data[1,3] == "CA"& RI_MD$data[1,8] == "-")) & RI_MD$data[1,2]  == "+" & RI_MD$data[1,4] == "+") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                              if (((RI_MD$data[1,3] == "-" & RI_MD$data[1,8] == "-") | (RI_MD$data[1,3] == "CA" & RI_MD$data[1,8] == "CA") | (RI_MD$data[1,3] == "-" & RI_MD$data[1,8] == "CA") | (RI_MD$data[1,3] == "CA"& RI_MD$data[1,8] == "-")) & RI_MD$data[1,7]  == "+" & RI_MD$data[1,9] == "+") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                if (RI_MD$data[1,1] == "-" & RI_MD$data[1,2] == "+" & RI_MD$data[1,3]  == "-" & RI_MD$data[1,4] == "+" & RI_MD$data[1,6] == "-") "Because the Inhibitor is not functional, transcription will remain ON" else        #LacI
                                  if (RI_MD$data[1,1] == "-" & RI_MD$data[1,6] == "-" & RI_MD$data[1,7]  == "+" & RI_MD$data[1,8] == "-" & RI_MD$data[1,9] == "+") "Because the Inhibitor is not functional, transcription will remain ON" else        #LacI
                                    if (RI_MD$data[1,2] == "+" & (RI_MD$data[1,3] == "-" | RI_MD$data[1,3]  == "CA") & RI_MD$data[1,4]  == "+") "Because the operator for the first copy of LacZ is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                      if (RI_MD$data[1,7] == "+" & (RI_MD$data[1,8] == "-" | RI_MD$data[1,8]  == "CA") & RI_MD$data[1,9]  == "+") "Because the operator for the second copy of LacZ is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                        if (RI_MD$data[1,1] == "-" & RI_MD$data[1,2] == "+" & RI_MD$data[1,4] == "+" & RI_MD$data[1,6] == "-") "Because the Inhibitor protein is nonfunctional and the first copy of LacZ is functional, transcription remains ON" else        #LacI
                                          if (RI_MD$data[1,1] == "-" & RI_MD$data[1,6] == "-" & RI_MD$data[1,7] == "+" & RI_MD$data[1,9] == "+") "Because the Inhibitor protein is nonfunctional and the second copy of LacZ is functional, transcription remains ON" else        #LacI
                                            ("")
                    )
      )
      
      str3 <- paste("<b> LacZ/+lac: </b>", 
                    (if (RI_MD$data[1,2] == "-" & RI_MD$data[1,7] == "-") "Because the Promoters are '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                      if (RI_MD$data[1,4] == "-" & RI_MD$data[1,9] == "-") "Because both copies of LacZ are '-', transcription will be OFF because the genes are nonfunctional."     else           #lacZ
                        if (RI_MD$data[1,2] == "-" & RI_MD$data[1,4] == "+" & RI_MD$data[1,9] == "-") "Because the Promotor for wild-type copy of LacZ is nonfunctional and the other copy of LacZ is nonfuncational, transcription will be OFF." else           #Promoter
                          if (RI_MD$data[1,4] == "-" & RI_MD$data[1,7] == "-" & RI_MD$data[1,9] == "+") "Because the Promotor for wild-type copy of LacZ is nonfunctional and the other copy of LacZ is nonfunctional, transcription will be OFF." else           #Promoter  
                            if (RI_MD$data[1,2] == "-" & RI_MD$data[1,7] == "+" & RI_MD$data[1,9] == "-") "Because the LacZ under the control of the wild-type promoter is nonfunctional, transcription will be OFF." else           #Promoter
                              if (RI_MD$data[1,2] == "+" & RI_MD$data[1,4] == "-" & RI_MD$data[1,7] == "-") "Because the LacZ under the control of the wild-type promoter is nonfunctional, transcription will be OFF." else           #Promoter  
                                ("")
                    )
      )
      
      str4 <- paste("<b> LacY/-lac: </b>", 
                    (if (RI_MD$data[1,2] == "-" & RI_MD$data[1,7] == "-") "Because the Promoters are '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                      if (RI_MD$data[1,5] == "-" & RI_MD$data[1,10] == "-") "Because both copies of LacY are '-', transcription will be OFF because the genes are nonfunctional."     else           #lacY
                        if (RI_MD$data[1,2] == "-" & RI_MD$data[1,5] == "+" & RI_MD$data[1,10] == "-") "Because the Promotor for wild-type copy of LacY is nonfunctional and the other copy of LacY is nonfunctional, transcription will be OFF." else           #Promoter
                          if (RI_MD$data[1,5] == "-" & RI_MD$data[1,7] == "-" & RI_MD$data[1,10] == "+") "Because the Promotor for wild-type copy of LacY is nonfunctional and the other copy of LacY is nonfunctional, transcription will be OFF." else           #Promoter
                            if (((RI_MD$data[1,3] == "-" & RI_MD$data[1,8] == "-") | (RI_MD$data[1,3] == "CA" & RI_MD$data[1,8] == "CA") | (RI_MD$data[1,3] == "-" & RI_MD$data[1,8] == "CA") | (RI_MD$data[1,3] == "CA"& RI_MD$data[1,8] == "-")) & RI_MD$data[1,2]  == "+" & RI_MD$data[1,5] == "+") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                              if (((RI_MD$data[1,3] == "-" & RI_MD$data[1,8] == "-") | (RI_MD$data[1,3] == "CA" & RI_MD$data[1,8] == "CA") | (RI_MD$data[1,3] == "-" & RI_MD$data[1,8] == "CA") | (RI_MD$data[1,3] == "CA"& RI_MD$data[1,8] == "-")) & RI_MD$data[1,7]  == "+" & RI_MD$data[1,10] == "+") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                if (RI_MD$data[1,1] == "-" & RI_MD$data[1,2] == "+" & RI_MD$data[1,3]  == "-" & RI_MD$data[1,5] == "+" & RI_MD$data[1,6] == "-") "Because the Inhibitor is not functional, transcription will remain ON" else        #LacI
                                  if (RI_MD$data[1,1] == "-" & RI_MD$data[1,6] == "-" & RI_MD$data[1,7]  == "+" & RI_MD$data[1,8] == "-" & RI_MD$data[1,10] == "+") "Because the Inhibitor is not functional, transcription will remain ON" else        #LacI
                                    if (RI_MD$data[1,2] == "+" & (RI_MD$data[1,3] == "-" | RI_MD$data[1,3]  == "CA") & RI_MD$data[1,5]  == "+") "Because the operator for the first copy of LacY is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                      if (RI_MD$data[1,7] == "+" & (RI_MD$data[1,8] == "-" | RI_MD$data[1,8]  == "CA") & RI_MD$data[1,10]  == "+") "Because the operator for the second copy of LacY is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                        if (RI_MD$data[1,1] == "-" & RI_MD$data[1,2] == "+" & RI_MD$data[1,5] == "-" & RI_MD$data[1,6] == "+") "Because the Inhibitor protein is nonfunctional and the first copy of LacY is functional, transcription remains ON" else        #LacI
                                          if (RI_MD$data[1,1] == "-" & RI_MD$data[1,6] == "-" & RI_MD$data[1,7] == "+" & RI_MD$data[1,10] == "+") "Because the Inhibitor protein is nonfunctional and the second copy of LacY is functional, transcription remains ON" else        #LacI
                                            ("")
                    )
      )
      
      str5 <- paste("<b> LacY/+lac: </b>", 
                    (if (RI_MD$data[1,2] == "-" & RI_MD$data[1,7] == "-") "Because the Promoters are '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                      if (RI_MD$data[1,5] == "-" & RI_MD$data[1,10] == "-") "Because both copies of LacY are '-', transcription will be OFF because the genes are nonfunctional."     else           #lacY
                        if (RI_MD$data[1,2] == "-" & RI_MD$data[1,5] == "+" & RI_MD$data[1,10] == "-") "Because the Promotor for wild-type copy of LacY is nonfunctional and the other copy of LacY is nonfunctional, transcription will be OFF." else           #Promoter
                          if (RI_MD$data[1,5] == "-" & RI_MD$data[1,7] == "-" & RI_MD$data[1,10] == "+") "Because the Promotor for wild-type copy of LacY is nonfunctional and the other copy of LacY is nonfunctional, transcription will be OFF." else           #Promoter  
                            if (RI_MD$data[1,2] == "-" & RI_MD$data[1,7] == "+" & RI_MD$data[1,10] == "-") "Because the LacY under the control of the wild-type promoter is nonfunctional, transcription will be OFF." else           #Promoter
                              if (RI_MD$data[1,2] == "+" & RI_MD$data[1,5] == "-" & RI_MD$data[1,7] == "-") "Because the LacY under the control of the wild-type promoter is nonfunctional, transcription will be OFF." else           #Promoter  
                                ("")
                    )
      )
      
      HTML(paste("<hr>", str1, "<br/>", str2, "<br/><br/>",str3, "<br/><br/>",str4, "<br/><br/>",str5))
      #HTML(paste("<hr>", str1, str2, str3, str4, str5,  sep = '<br/><br/>'))
      
    })
    }
    else ("")  
    
    
#Adding in new row to dataframe when check is performed 
    if(input$panels == "The LacOperon: Merodiploids")
    {new_row2 = data.frame(matrix(ncol=3, nrow=1))
    names(new_row2) <- c("Attempt", "correct", "consec")
    new_row2[1,1] = nrow(report.dataRV_MD$data)+1
    new_row2[1,3] = report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3]
    report.dataRV_MD$data=rbind(report.dataRV_MD$data, new_row2)
    }
    })


#Counting consecutive answers correct (diploid)
observeEvent(input$check, {
     if(input$panels == "The LacOperon: Merodiploids"){
      if((input$Zminus_MD == RI_MD$data[1,11]) & (input$Zplus_MD == RI_MD$data[1,12]) & (input$Yminus_MD == RI_MD$data[1,13]) & (input$Yplus_MD == RI_MD$data[1,14]))
        report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),2] = "yes"
      else report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),2] = "no"
      
      if(report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),2] == "yes")
        report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3] = 1
      else report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3] = 0
      
      if(nrow(report.dataRV_MD$data)>2 & report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),2] == "yes")
        report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3] = report.dataRV_MD$data[(nrow(report.dataRV_MD$data))-1,3] + report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3]
      else if(report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),2] == "yes")
        report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3] = 1
      else report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3] = 0
      
      output$B = renderText({
        paste("Number in a row correct = ",(report.dataRV_MD$data[(nrow(report.dataRV_MD$data)),3]), sep = "")})
     }
    }
    )      

  

  
}



shinyApp(ui, server)





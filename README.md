# LacApp.Undegraduate

---
title: "LacApp.Undergraduate"
author: "Caitlin Hanlon"
date: "1/17/2023"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Designing an app for the lac operon
### Initial Observations and Hypotheses
While teaching an introductory genetics course in the the fall of 2019, my co-instructor and I noticed students struggling with gene regulation. While this may not come as a surprise to seasoned instructors of genetics, we did find it striking that students who seemed comfortable with the topics covered up to that point (Mendelian genetics, probability, etc) were challenged by the concept of gene regulation. 

We hypothesized that gene regulation may be the first concept in genetics that students were not exposed to previously, either in an introductory biology course or in AP Biology. Similarly, gene regulation may be one of the first concepts that requires higher level Bloom's taxonomy skills (e.g. application rather than memorization). Our third hypothesis was that the modality that students use to practice the concept of gene regulation (i.e. worksheets) may not provide enough practice or feedback for students to master the concept. 

### Initial Designs
Our first attempt to address the third hypothesis was a decidely low tech approach. During a review session, a student asked me to demonstrate how to solve a lac operon gene regulation problem. After demonstrating the process by verbalizing the logic and reasoning for each step, I assured the student that even instructors have to "think through" the logic of these types of problems. To demonstrate this, I labeled red solo cups with the gene names for the lac operon and filled each cup with strips of paper labeled "+" (wild type), "-" (mutant), or "CA" (constitutively active). The instructor then invited students to "make" a genotype by choosing strips of paper and then demonstrated to students how to apply the logic of the lac operon to each genotype. This approach was simplistic, but demonstrated how the lac operon could be gamified.

The second attempt used Microsoft Excel to generate a random genotype. This approach was used in the fall of 2020 when the course was being taught primarily online. This approach allowed students to repeatedly practice the concept of gene regulation asynchronously. However, the interface was clunky and it was not possible to give feedback.

In the Fall of 2021, I decided to create an app for the lac operon that would (1) randomly generate genotypes and (2) provide immediate feedback. I chose to create this app in R and Shiny because this is the only coding language that I know. The information below will present key parts of the code and reasoning for the approach.


# Lac.App Undergrad

## Creating a user interface to display genotypes and input responses

### Possible genotypes
The first step in creating the app was to generate a table of all of the possible variants for each site in the lac operon. For our setup, we chose five sites: *LacI, Promoter, LocO, LacZ, and LacY* and three variant types: "+" (wild type), "-" (mutant), or "CA" (constitutively active).

```{r eval = FALSE}
  f <- (c("+","-","CA"))
  x <- f
  n <- 5
  m <- 5
  lac.permutations<-as.data.frame(CombSet(x, m, repl=TRUE, ord=TRUE))
  names(lac.permutations)<- c("LacI","Promoter","LacO","LacZ","LacY")
```

For the purposes of our course, we chose to limit the constitutively active option to only LacO:
```{r eval = FALSE}
  lac.permutations <- subset(lac.permutations, LacI != "CA" & Promoter != "CA" & LacZ != "CA" & 
                               LacY != "CA")
```

### Genotype display
We wanted single rows from the genotype table to be displayed to students. The chunk of code below selects a line from the table and displays it in a convention that students are familiar with. This is held as a reactive value so that random rows can be selected each timestudent pushes a refresh button.

```{r eval = FALSE}
  input.data <- lac.permutations[1,]
  Element1 = c("I","P","O","Z","Y")
  list <- as.data.frame(paste(Element1,input.data[1,c(1:5)], sep = "", collapse = ' '))
  list <- as.data.frame(gsub("OCA", "O(CA)", list))
  names(list)<- "Genotype"
  
  RI <- reactiveValues(data = input.data)
  RV <- reactiveValues(data = list)
```

The genotype is then rendered in Shiny via datatable:
```{r eval = FALSE}
  output$new.geno = DT::renderDataTable(RV$data, options=list(dom='t'))
```

New genotypes were generated from the reactive values with a refresh button:
```{r eval = FALSE}
  observeEvent(input$refresh, {
    if(input$panels == "The LacOperon: Haploids"){
      
    RI$data <- sample_n(lac.permutations, 1)
    RV$data[1,1] <- paste(c("I","P","O","Z","Y"), RI$data[1,c(1:5)], sep = "", collapse = ' ')
    RV$data[1,1] <- gsub("OCA", "O(CA)", RV$data[1,1])
    rownames(RV$data) <- "1"
    }
```


### Interface
In the app interface, students are presented with a genotype (new.geno) and are asked to predict the transcriptional activity of the *lacZ* and *lacY* genes in the presence or absence of lactose (+lac or -lac). To input their answer, users select "ON" or "OFF" with a radio button for *lacZ* and *lacY* for each environmental condition. The textOutput and htmlOutput are areas where feedback will appear (discussed in the Feedback section).

```{r eval = FALSE}
div(class="row",
    div(class= "col-sm-4",DT::dataTableOutput("new.geno")),
      div(class= "col-sm-2", radioButtons("Zminus", "LacZ/-lac", choices = c("ON", "OFF")), textOutput("value1")),
      div(class= "col-sm-2", radioButtons("Zplus", "LacZ/+lac", choices = c("ON", "OFF")),  textOutput("value2")),
      div(class= "col-sm-2", radioButtons("Yminus", "LacY/-lac", choices = c("ON", "OFF")), textOutput("value3")),
      div(class= "col-sm-2", radioButtons("Yplus", "LacY/+lac", choices = c("ON", "OFF")), textOutput("value4")),
         div(class= "col-sm-12",  htmlOutput("Explain.Haplo"))
                     )
```
User interface: ![](/Users/caitlind.hanlon/Desktop/QU related docs/LacOperon/LacApp_UI.png)

### Unique ID
Users are given a unique ID when using LacApp. This appears above the genotype. In future iterations, this function may not be necessary, but in its current form, this ID is sufficient to dissuade academic integrity violations if students are asked to complete a certain number of questions independently.
```{r eval = FALSE}
  InputID <- function(n = 10000) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))}
  
   RID <- reactiveValues(data = InputID(1))
   
   output$ID <- renderText({paste("Unique ID: ", RID$data)})
```






## Assessing the input

### The logic of the operon
One way that students demonstrate understanding of gene regulation in our course is by predicting the transcriptional activity of the *lacZ* and *lacY* genes in the presence or absence of lactose (+lac or -lac). These predictions vary depending on the genotype of the operon. Therefore, four columns corresponding to the *lacZ* and *lacY* genes in the presence or absence of lactose (+lac or -lac) were added to the genotype table. The logic of gene regulation was then used to determine the transcriptional activity (ON or OFF) of each gene. For example, if the Promoter is mutated, then transcription of *lacZ* and *lacY* in the presence or absence of lactose is OFF since RNA polymerase is unable to bind to the promoter. An example is shown below for Beta-gal (*lacZ*) in the absence of lactose (-lac):

```{r eval = FALSE}
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
```

This same approach was completed for all possible variants and conditions for both the haploid and merodiploid genotypes.


### Comparing the input to the answer
The input from the student is then compared to the genotype table (RI$data) by clicking a "check" button. Each input was compared to the appropriate "answer" cell within the genotype table. Based on this comparison, feedback was given: if the input matched the answer, "Correct" appeared; if the input did not match the answer "Try again" appeared.

```{r eval = FALSE}
observeEvent(input$check, {
  c1 <- reactive(input$Zminus)
  c2 <- reactive(input$Zplus)
  c3 <- reactive(input$Yminus)
  c4 <- reactive(input$Yplus)
  
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
```

Example: ![](/Users/caitlind.hanlon/Desktop/QU related docs/LacOperon/LacApp_UIfeedback1.png)

### Further feedback and explanations
After intial testing of LacApp, users indicated that an explanation of the answer would be helpful for their learning. Therefore, further explanations are provided for incorrect answers. These simple explanations are designed to mimic an instructor talking through the logic of gene regulation. This level of feedback is still its beta format. The code for one explanation is shown below:

```{r eval = FALSE}
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
                if (RI$data[1,4] == "CA") "Because LacZ is 'CA', so transcription will be ON." #lacZ  
                else   ("")
                        )
                      )
```

Example: ![](/Users/caitlind.hanlon/Desktop/QU related docs/LacOperon/LacApp_UIfeedback2.png)



## Tracking progress
To promote mastery of the material, we wanted to design a system that would count the number of consecutive correct answers and display this to students. The first step was creating an assessment table to house the attempts. The assessment table tracks the number of attempts, if the attempt was correct, and the number of consecutive correct answers. This is contained within a reactive value so that it is updated automatically.
```{r eval = FALSE}
report.data <- data.frame(matrix(ncol=3, nrow=0))
  names(report.data) <- c("Attempt", "correct", "consec")
    report.data[1,1] = 1
    report.dataRV = reactiveValues(data = report.data)
  
 output$tracks = DT::renderDataTable(report.dataRV$data)
```

Each time the user clicks the "refresh" button, a new line is added to the assessment table.
```{r eval = FALSE}
observeEvent(input$refresh, {
  if(input$panels == "The LacOperon: Haploids"){
   new_row = data.frame(matrix(ncol=3, nrow=1))
    names(new_row) <- c("Attempt", "correct", "consec")
    new_row[1,1] = nrow(report.dataRV$data)+1
    new_row[1,3] = report.dataRV$data[(nrow(report.dataRV$data)),3]
    report.dataRV$data=rbind(report.dataRV$data, new_row)}
    }
  )
```


When the "check" button was clicked, the input is evaluated against the genotype table. If the user answers correctly, a "yes" indicator is placed in the assessment table "correct" column (report.data column 2). This is then transformed into a "1" in the adjacent "consec" column (report.data column 3). The number of rows with consecutive "1" values can then be counted and displayed to the user (output$A).
```{r eval = FALSE}
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
```

Example: ![](/Users/caitlind.hanlon/Desktop/QU related docs/LacOperon/LacApp_UIcorrect.png)



## Future Directions

### Link the user information and attempts to a server database 
Currently, to evaluate LacApp use, we ask students to provide their User ID and screen shots of their activity to our LMS (Blackboard). A more streamlined solution would be to have the ID and activity automatically logged into a server database. 

### Recruit users at different institutions
Quinnipiac University is a primarily undergraduate university. Approximately 60 students take our Genetics (BIO282) course each fall. To better assess LacApp, we would like to increase the number of users. If you are interested in using LacApp at your institution, please complete the following [form](https://forms.gle/vgtZy5RGG797whGc8).

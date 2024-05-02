
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)



REED_TIDY<- read_csv("Data/TidyText/REED_TIDY.csv")

data <-read_csv("Data/Datasets/Most-Recent-Cohorts-Institution.csv") %>%
  mutate(
    TUITIONFEE_IN = as.numeric(as.character(TUITIONFEE_IN)),
    TUITIONFEE_OUT = as.numeric(as.character(TUITIONFEE_OUT)),
    INSTNM = as.character(INSTNM)  
  )


average_tuition <- data %>%
  summarise(
    Average_Tuition_In = mean(TUITIONFEE_IN, na.rm = TRUE),
    Average_Tuition_Out = mean(TUITIONFEE_OUT, na.rm = TRUE)
  )


ui <- dashboardPage(
  dashboardHeader(title = "Rosa 1.0"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tuition Info", tabName = "tuition", icon = icon("dollar-sign")),
      menuItem("College Score", tabName = "score", icon = icon("building-columns"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tuition",
              fluidRow(
                
                box(
                  title = "Select Institution",
                  selectInput(
                    inputId = "institution",
                    label = "Choose an Institution:",
                    choices = unique(data$INSTNM),  
                    selected = unique(data$INSTNM)[1]  
                  )
                )
              ),
              fluidRow(
                
                box(
                  title = "Tuition Details",
                  verbatimTextOutput("tuitionDetails")
                ),
                
                box(
                  title = "Tuition Comparison",
                  plotOutput("tuitionComparisonPlot")
                )
              )
      ),
      
      tabItem(tabName = "score",
              
              tags$style(HTML(".irs-value,.irs-single,.irs-max,.irs-min {opacity: 0;}.text-container {
    display: flex;
    justify-content: space-between;
    margin-top: 10px;
  }
  .irs-bar {background: #52BE80}
  ")),
              
              fluidRow(
                box(
                  title = "How much do the following matter to you",collapsible = TRUE,collapsed=TRUE, width=6,
                  sliderInput(
                    inputId = "scaleCom",
                    label = "Community",
                    min=0.01,
                    max=2,
                    value=1,
                    ticks=TRUE,
                    
                    
                  ),
                  
                  
                  
                  div(class = "text-container",
                      p("Not Very"),
                      
                      p("Very")
                  ),
                  sliderInput(
                    inputId = "scaleCor",
                    label = "Courses",
                    min=0.01,
                    max=2,
                    value=1,
                    ticks=FALSE,
                    
                    
                  ),
                  
                  
                  
                  div(class = "text-container",
                      p("Not Very"),
                      
                      p("Very")
                  ),
                  sliderInput(
                    inputId = "scaleD",
                    label = "Diversity",
                    min=0.01,
                    max=2,
                    value=1,
                    ticks=FALSE,
                    
                    
                  ),
                  
                  
                  
                  div(class = "text-container",
                      p("Not Very"),
                      
                      p("Very")
                  ),
                  sliderInput(
                    inputId = "scaleFin",
                    label = "Financial",
                    min=0.01,
                    max=2,
                    value=1,
                    ticks=FALSE,
                    
                    
                  ),
                  
                  
                  
                  div(class = "text-container",
                      p("Not Very"),
                      
                      p("Very")
                  ),
                  sliderInput(
                    inputId = "scaleO",
                    label = "Outcomes",
                    min=0.01,
                    max=2,
                    value=1,
                    ticks=FALSE,
                    
                    
                  ),
                  
                  
                  
                  div(class = "text-container",
                      p("Not Very"),
                      
                      p("Very")
                  ),
                  sliderInput(
                    inputId = "scaleP",
                    label = "Professors",
                    min=0.01,
                    max=2,
                    value=1,
                    ticks=FALSE,
                    
                    
                  ),
                  
                  
                  
                  div(class = "text-container",
                      p("Not Very"),
                      
                      p("Very")
                  ),
                  sliderInput(
                    inputId = "scaleFd",
                    label = "Food",
                    min=0.01,
                    max=2,
                    value=1,
                    ticks=FALSE,
                    
                    
                  )
                  
                ),
                box(
                  title = "Your College Score",
                  p(strong("\nComposite Score:")),
                  htmlOutput("totalscore"),
                  p("\nCommunity Score:"),
                  htmlOutput("comscore"),
                  p("Courses Score:"),
                  htmlOutput("corscore"),
                  p("Diversity Score:\n"),
                  htmlOutput("divscore"),
                  p("Financial Score:\n"),
                  htmlOutput("finscore"),
                  p("Outcome Score:\n"),
                  htmlOutput("outscore"),
                  p("Professor Score:\n"),
                  htmlOutput("profscore")
                  ,
                  p("Food Score:\n"),
                  htmlOutput("fdscore")
                ))
      )
    )
  ))







server <- function(input, output) {
  
  output$tuitionDetails <- renderText({
    selected_institution <- input$institution
    
    
    tuition_info <- data %>%
      filter(INSTNM == selected_institution) %>%
      select(INSTNM, TUITIONFEE_IN, TUITIONFEE_OUT) %>%
      slice(1) 
    paste(
      "Institution: ", tuition_info$INSTNM, "\n",
      "In-State Tuition: $", tuition_info$TUITIONFEE_IN, "\n",
      "Out-of-State Tuition: $", tuition_info$TUITIONFEE_OUT
    )
  })
  
  
  output$tuitionComparisonPlot <- renderPlot({
    selected_institution <- input$institution
    
    
    tuition_info <- data %>%
      filter(INSTNM == selected_institution) %>%
      select(TUITIONFEE_IN, TUITIONFEE_OUT) %>%
      slice(1)
    
    
    tuition_df <- data.frame(
      TuitionType = c("Selected Institution - In-State", "Selected Institution - Out-of-State", 
                      "Average - In-State", "Average - Out-of-State"),
      TuitionValue = c(tuition_info$TUITIONFEE_IN, tuition_info$TUITIONFEE_OUT,
                       average_tuition$Average_Tuition_In, average_tuition$Average_Tuition_Out)
    )
    
    ggplot(tuition_df, aes(x = TuitionType, y = TuitionValue, fill = TuitionType)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      labs(
        title = paste("Tuition Comparison for", selected_institution),
        x = "Tuition Type",
        y = "Tuition Fee ($)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    
  })
  
  
  
  scoreGroup<- REED_TIDY%>%
    mutate(SentimentSC = ifelse(Sentiment=="Negative", -SentimentSC, SentimentSC))%>%
    group_by(Label)%>%
    summarize(score = mean(SentimentSC))
  
  format_value <- function(value) {
    if (value < 0 && value>=-100) {
      value= -value
      paste("<span style='color: red;'>", value, "</span>", sep = "")
    } else {
      
      if(value<(-100)){
        value=-value
        paste("<span style='color:#641E16 ;'>", value, "</span>", sep = "")
      }
      else{
        if(value>0 && value<=100){
          paste("<span style='color: green;'>", value, "</span>", sep = "")
        }
        else{
          if(value==0){
             paste("<span style='color: gray;'>", value, "</span>", sep = "")
          }
          else{
          paste("<span style='color: #bf9b30;'>", value, "</span>", sep = "")}
        }
      }
      
      
    }
    
  } 
  
  format_value <- function(value) {
    if (value > 0 && value<50) {
      paste("<span style='color: red;'>", value, "</span>", sep = "")
    } else {
      
      if(value > 50 && value<100){
        paste("<span style='color:green ;'>", value, "</span>", sep = "")
      }
      else{
        if(value==100){
          paste("<span style='color: #bf9b30;'>", value, "</span>", sep = "")
        }
        else{
          if(value==50){
            paste("<span style='color:gray;'>", value, "</span>", sep = "")
          }
          else{
          paste("<span style='color:#641E16;'>", value, "</span>", sep = "")}
        }
      }
      
      
    }
    
  } 
  
  indvScoreFix <- function(x){
    return(round(x*5+50,digits=0))
  }
  
  
  totscr<- reactive({
    weightedAverage<- function(scores){
      
      weights <- c(input$scaleCom, input$scaleCor, input$scaleD, input$scaleFin,input$scaleFd, input$scaleO, input$scaleP)  
      
      
      average <- (round(sum((scores)*weights)*1.5, digits=0))+50
      
      
      
      
      
      
      
      if(average>100){
        average=100
        return(average)
      }
      else{
        if(average<0){
          average=0
          return(average)
        }
        else{
          return(average)
        }
      }
     
    }
    
    scoreComp<-weightedAverage(scoreGroup$score)
    
    
    
    
    comp<-format_value(scoreComp)
    comp
  })
  
  divscr<- reactive({
    
   
    
    div<-format_value(indvScoreFix(scoreGroup$score[3]))
    div
  })
  
  finscr<- reactive({
    
   
    
    
    fin<-format_value(indvScoreFix(scoreGroup$score[4]))
    fin
  })
  
  fdscr<- reactive({
    
   
    
    
    fd<-format_value(indvScoreFix(scoreGroup$score[5]))
    fd
  })
  
  corscr<- reactive({
    
    
    
    cor<-format_value(indvScoreFix(scoreGroup$score[2]))
    cor
  })
  
  comscr<- reactive({
    
   
    com<-format_value(indvScoreFix(scoreGroup$score[1]))
    com
    
  })
  
  outscr<- reactive({
    
   
    
    out<-format_value(indvScoreFix(scoreGroup$score[6]))
    out
    
  })
  
  profscr<- reactive({
    
   
    
    prf<- format_value(indvScoreFix(scoreGroup$score[7]))
    prf
    
  })
  
  
  
  output$totalscore <- renderText({
    totscr()
  })
  output$divscore <- renderText({
    divscr()
  })
  output$finscore <- renderText({
    finscr()
  })
  output$fdscore <- renderText({
    fdscr()
  })
  output$corscore <- renderText({
    corscr()
  })
  output$comscore <- renderText({
    comscr()
  })
  output$outscore <- renderText({
    outscr()
  })
  output$profscore <- renderText({
    profscr()
  })
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)



library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(bslib)

linebreaks <- function(n){HTML(strrep(br(), n))}


REED_TIDY<- read_csv("Data/TidyText/REED_TIDY.csv")
UCF_TIDY<- read_csv("Data/TidyText/UCF_TIDY.csv")
OHIO_TIDY<- read_csv("Data/TidyText/OHIO_TIDY.csv")

data <-read_csv("Data/Most-Recent-Cohorts-Institution.csv") %>%
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
      menuItem("Tuition Tool", tabName = "tuition", icon = icon("dollar-sign")),
      menuItem("College Score", tabName = "score", icon = icon("building-columns")),
      menuItem("Project Write-Up", tabName = "writeUp", icon = icon("pen-to-square"))
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
      
      #################COLLEGE SCORE TAB
      tabItem(tabName = "score",
              
              tags$style(HTML(".irs-value,.irs-single,.irs-max,.irs-min {opacity: 0;}.text-container {
    display: flex;
    justify-content: space-between;
    margin-top: 10px;
  }
  .irs-bar {background: #52BE80}
  ")),
              
              box(
                title = "Select Institution",
                selectInput(
                  inputId = "college",
                  label = "Choose an Institution:",
                  choices = c("Reed College","University of Central Florida","Ohio State University"),  
                  selected = "Reed College"  
                )
              ),
              ###############PREFERENCE SLIDERS
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
                ######################## SCORE PANEL
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
      ),
      #################COLLEGE SCORE TAB
      tabItem(tabName = "writeUp",
              
              
              box(
                width=12,
                titlePanel(tags$h1(style = "font-size: 3em; font-weight: bold;", "Rosa 1.0")),
                linebreaks(1),
                p(HTML("The college application experience in the U.S., as we know it today—long nights on the Common App, browsing countless online rankings, scouring college subreddits, etc.—emerged only fifty years ago. Before 1975, 
                       college applicants physically submitted separate paper applications, which usually incurred multiple fees. Each school also mandated different requirements, which effectively set a soft cap on the number of schools 
                       each applicant could feasibly apply to. In 1975, The Common Application was founded as a coalition of admissions offices from 15 schools in the northeast, aimed at centralizing the college application process. This led 
                       to the eventual digitization of college applications, and the Common App grew to become the nation’s leading college application portal. A process formerly bound by written application letters and 100-word <a href='https://www.ivyadmissionsgroup.com/blog/2017/10/23/jfks-harvard-essay'>'essays'</a>, is now inextricably defined by the internet. In turn, applying to college in the U.S. has become more accessible for more groups of people than ever before.")),
                linebreaks(1),
                p("Although applications are on the rise, college admissions in the U.S. are becoming increasingly difficult to navigate. Students are left to embark on their college searches with varying levels of guidance 
                    and support. Despite the abundance of college search tools available, many rely on aging metrics and flimsy 
                    statistics (such as standardized testing scores and college 'prestige') to aid an applicant's decision-making 
                    process. However, internal data (for Reed) tells us that two of the most impactful factors in an applicant's cycle 
                    are speaking to current students of an institution and physically visiting campus, both of which comprise what we deem to 
                    be a college's student sentiment score. Getting 'the vibe' of a college can be challenging, as most students aren't able to 
                    visit campuses in person before making a decision, and even if they could, access to honest and representative student 
                    perspectives is often limited."),
                linebreaks(1),
                p(style="font-weight:bold;",  "Rosa is a college search multi-tool which aims to prepare applicants with honest, student-based college insights."),
                linebreaks(1),
                p("Through the 'Tuition Tracker', users can select a specific college and compare its individual in-state and out-of-state tuition fees to the national averages. 
                By clearly highlighting how the institution's tuition rates measure up against the national average for both in-state and out-of-state students, the tool provides 
                prospective students and their families with valuable insights into the relative affordability of different colleges. This comparison helps them better understand 
                the financial implications of attending their desired school, empowering them to make more informed decisions in their college search process."),
                
                linebreaks(1),
                p("Additionally, the 'College Score' tool enables applicants to discover not just the (ITALIC)best schools in the country, but their (BOLD ITALIC)best schools. A college is scored on seven main areas: The professors (how good is the in-class experience?), community (how are students interacting with each other?), courses (how are the course offerings?), diversity (how is diversity felt on campus?), 
                  financials (How is the cost of attendance? How are the financial outcomes?), outcomes (How are the student prospects?), or food (How's the food?). Each of these 
                  scores is based on how students online feel (negatively or positively) about these topics. Each score is between 0-100. For instance, if 
                    College XYZ has a food score of 25, then students of College XYZ, on average, have a fairly negative opinion of their dining options. On the other hand,
                    if most students at College XYZ love the courses offered, then College XYZ could receive a professor score of 75+. Each area score is then averaged and weighted 
                  by how much you value each, producing a personal composite score tailored to your preferences and the opinions of students at an institution."),
                linebreaks(2),
                p(style="font-style:italic;", "In this report, we will be covering"),
                
                
                tags$ul(
                  tags$li("The tools we used to aggregate our data."),
                  tags$li("Our methods and approach."),
                  tags$li("Future iterations & potential developments")
                ),
                style = "line-height:2;"
              ), 
              fluidRow(
                box(
                  title = strong("Our Data"),
                  
                  p("The College Scorecard, provided by the U.S. Department of Education, is a comprehensive dataset that offers detailed information about 
                    post-secondary institutions across the country. It is designed to help prospective students and their families make informed decisions by providing 
                    data on various metrics such as tuition costs, student demographics, financial aid, graduation rates, and post-graduation earnings. By encompassing 
                    a broad range of institutions, from community colleges and vocational schools to major research universities and graduate programs, it allows users 
                    to compare educational opportunities and outcomes across different types of schools. This dataset specifically includes information about students 
                    who have received federal financial aid, offering crucial insights into the financial aspects of higher education for diverse student populations. 
                    The dataset contains 6,543 observations and 3,232 variables"),
                  
                  linebreaks(1),
                  
                  p("The 'College Score' tool uses custom-made 
                datasets for Reed College, The Ohio State University, and The University of Central Florida (as a sample) that contain student reviews (parsable text) 
                  scored by sentiment (negative or positive, ([0,5]/[-5,0]) and classified by one of the following categories: Community, Courses, Diversity, 
                  Financial, Outcomes, Professors, or Food. How we aggregated this data, how we conducted the text analysis, and our general approach/considerations 
                  can all be found in later sections."),
                  
                  
                  style = "
                  background-color: #fff;
                  border: 1px solid #ddd;
                  border-radius: 4px;
                  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
                  padding: 1rem;
                  line-height:2;
                "
                ),
                box(
                  title = strong("Web Scraping with Rcrawler"),
                  
                  p("Rcrawler is a small package for web crawling and scraping in R, which offers a primary function, 'Rcrawler()', exemplified by the code below:"),
                  linebreaks(1),
                  tags$code(
                    
                    
                    " Rcrawler(Website = 'https://www.unigo.com/colleges/reed-college')"
                    
                  ),
                  linebreaks(2),
                  p("This function automates the process of traveling to a website, and once at this 'root' page, selectively traverses further pages. This act is called 'crawling', and this function
                    also provides us the ability to scrape text from pages being crawled. For our purposes, we customized our bot by the additional parameters available for RCrawler(), which had it
                    target specific pages on Unigo.com that contained the most insightful reviews for each school (Reed College, The University of Central Florida, and The Ohio State University):"),
                  linebreaks(1),
                  tags$code(
                    
                    
                    "  crawlUrlfilter = 
                                     '/describe-the-students-at-your-school',
                                     '/heres-your-chance-say-anything-about-your-college'',
                                     '/what-is-your-overall-opinion-of-this-school'),
                  "
                    
                  ),
                  linebreaks(2),
                  p("For each school, we obtain a dataset containing every review ever written about it on Unigo (from select categories of reviews, as above)."),
                  linebreaks(1),
                  
                  
                  style = "
                  background-color: #fff;
                  border: 1px solid #ddd;
                  border-radius: 4px;
                  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
                  padding: 1rem;
                "
                )
                
              ), fluidRow(
                box(
                  title = strong("Text Analysis via HuggingFace"),
                  
                  p("HuggingFace is an open-source platform popular for its abundance of free-to-use and highly accessible machine learning models. HuggingFace is particularly well-known for its massive library of transformers, which allow developers to more seamlessly incorporate ML models (particularly for Natural Language Processing) into their builds. We used the 'text' package for R to connect to the HuggingFace library, involving NLP models to process and analyze our review data.
                    The following code segment calls the textZeroShot() function to parse through each review and categorize each review. Given a preset list of text classifications (professors, food, etc.), the model assigns a probability to each classification based on how likely a review pertains to it, and once parsed, we chose the classification with the highest probability for each review to be that review's category. By adding this extra facet to our data, we are able to analyze reviews 
                    by how they may pertain to the interests of potential applicants most:"),
                  linebreaks(1),
                  tags$code(
                    
                    
                    "  textZeroShot(df$Reviews, 
                             c('professors','community','food',
                             'courses','financial','outcomes','diversity'), 
                             model='facebook/bart-large-mnli')
                  "
                    
                  ),
                  linebreaks(2),
                  p("Furthermore, we used textClassify() to conduct our sentiment analysis. The function simply uses a sentiment analysis model to assign a mood (positive or negative) with an associated score to each review. We chose a model which was trained on internet reviews specifically (Yelp) to conduct this classification; our assumption being that 
                    the type of text between what the model was trained to accurately assess and the text we are using here (business reviews vs. college reviews) is similar enough to be optimal:"),
                  linebreaks(1),
                  tags$code(
                    
                    
                    "  textClassify(df$Reviews, model='textattack/bert-base-uncased-yelp-polarity',
                             return_incorrect_results = TRUE)
                  "
                    
                  ),
                  linebreaks(2),
                  
                  p("Our intention with incorporating NLP models into the College Score tool was to offer applicants college insights sourced directly and purely from student feedback. 
                    NLPs helped us target the different facets of the college experience that could best inform an applicant's decision to attend a school and helped quantify the sentiment-heavy reviews we had aggregated."),
                  linebreaks(1),
                  
                  style = "
                  background-color: #fff;
                  border: 1px solid #ddd;
                  border-radius: 4px;
                  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
                  padding: 1rem;
                "
                ),
                box(
                  title = strong("Next Steps"),
                  
                  p("In future Rosa iterations, we hope to expand on both of our tools and potentially add more to focus on the areas of the college application process that applicants may need more guidance for—such as navigating campus life without actually being on campus or understanding how a particular department's program may hurt or bolster their post-undergrad plans."),
                  linebreaks(2),
                  p("We see opportunities to expand the tuition tool's functionality to provide even more comprehensive support to prospective students and their families. First, 
                    incorporating additional variables like average student loan debt, graduation rates, and post-graduation earnings would offer a more holistic perspective 
                    on affordability and outcomes. Moreover, adding interactive filters for different demographic groups could tailor the results, making the tool more 
                    personalized. I also envision integrating data on scholarships and financial aid opportunities specific to each college to help users identify potential 
                    cost-saving options. With these improvements, the tool will better empower students to navigate the complexities of college financing and make well-informed decisions."),
                  linebreaks(2),
                  p("For the College Score Tool, we had hoped to aggregate a greater number of reviews with more robust metadata associated with each review, such as time of submission and the reviewer's affiliation with the school. Unigo.com was a sufficient source of reviews,
                    but Niche.com, a general-review site, would have been an optimal source. Niche.com has an overabundance of neatly formatted college reviews, along with the desired metadata; however, when we attempted to crawl/scrape the reviews for Reed College, our bot was immediately flagged as
                    malicious, although we had respected the site's TOS. This prevented us from pulling data from Niche within the project timeframe; however, we may seek more advanced methods, such as utilizing IP rotation or alternate crawlers like Selenium (via Python) to eventually obtain these reviews. If possible,
                    this would greatly improve the accuracy of this tool (implementing temporal awareness, for instance)."),
                  
                  style = "
                  background-color: #fff;
                  border: 1px solid #ddd;
                  border-radius: 4px;
                  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
                  padding: 1rem;
                "
                )))
      
    )
  )
)







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
  
  
  collegechoice<-reactive({
    if(input$college=="Reed College"){
      collegedf<-REED_TIDY
    }
    if(input$college=="University of Central Florida"){
      collegedf<-UCF_TIDY
    }
    if(input$college=="Ohio State University"){
      collegedf<-OHIO_TIDY
    }
    
    
    
    
    
    scoreGroup<- collegedf%>%
      mutate(SentimentSC = ifelse(Sentiment=="Negative", -SentimentSC, SentimentSC))%>%
      group_by(Label)%>%
      summarize(score = mean(SentimentSC))
    
    return(scoreGroup)})
  
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
    
    scoreComp<-weightedAverage(collegechoice()$score)
    
    
    
    
    comp<-format_value(scoreComp)
    comp
  })
  
  divscr<- reactive({
    
    
    
    div<-format_value(indvScoreFix(collegechoice()$score[3]))
    div
  })
  
  finscr<- reactive({
    
    
    
    
    fin<-format_value(indvScoreFix(collegechoice()$score[4]))
    fin
  })
  
  fdscr<- reactive({
    
    
    
    
    fd<-format_value(indvScoreFix(collegechoice()$score[5]))
    fd
  })
  
  corscr<- reactive({
    
    
    
    cor<-format_value(indvScoreFix(collegechoice()$score[2]))
    cor
  })
  
  comscr<- reactive({
    
    
    com<-format_value(indvScoreFix(collegechoice()$score[1]))
    com
    
  })
  
  outscr<- reactive({
    
    
    
    out<-format_value(indvScoreFix(collegechoice()$score[6]))
    out
    
  })
  
  profscr<- reactive({
    
    
    
    prf<- format_value(indvScoreFix(collegechoice()$score[7]))
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


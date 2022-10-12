#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}

if (!require("readr")) {
  install.packages("readr")
  library(readr)
}

if (!require("stringr")) {
  install.packages("v")
  library(stringr)
}

if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}

if (!require("scales")) {
  install.packages("scales")
  library(scales)
}

if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}





############################### Data Processing #######################

# Import data from updated job nyc
nyc_job <- read.csv("../data/NYC_Jobs_updated.csv") #Sangmin's data
job_data_updated <- nyc_job # make copy

### Position and Salary Data processing ###

nyc_job["median_salary"] <- (nyc_job$Salary.Range.To + nyc_job$Salary.Range.From )/2

nyc_job <- nyc_job[, which(colnames(nyc_job) %in% c("Posting.Date", "Job.Category", "Career.Level", "median_salary"))]
nyc_job <- na.omit(nyc_job)
nyc_job$Posting.Date <- as.character(nyc_job$Posting.Date)
nyc_job$Posting.Date <- as.Date(nyc_job$Posting.Date, "%m/%d/%Y")

nyc_job2 <- nyc_job # make copy

###### Degree data processing ######
job_data_updated <- job_data_updated %>%
  mutate(degree = case_when(
    str_detect(job_data_updated$Minimum.Qual.Requirements, fixed("master")) ~ "master",
    str_detect(job_data_updated$Minimum.Qual.Requirements, fixed("baccalaureate")) ~ "baccalaureate",
    str_detect(job_data_updated$Minimum.Qual.Requirements, fixed("high school")) ~ "high school",
    TRUE ~ "no degree requirement"
  ))

degree_requirement<-job_data_updated %>% select(c("Posting.Date", "degree", "X..Of.Positions")) %>% group_by(Posting.Date, degree) %>% summarise(count = sum(X..Of.Positions))
# change Posting.Date to Date format
degree_requirement$Posting.Date <- as.Date(degree_requirement$Posting.Date, format = "%m/%d/%Y")


df_baccalaureate<-degree_requirement[degree_requirement$degree =="baccalaureate",]
df_master<-degree_requirement[degree_requirement$degree =="master",]
df_high_school<-degree_requirement[degree_requirement$degree =="high school",]



#===============================================Shiny UI=========================================================
ui <- navbarPage(
  "Job Market NYC",
  #################### tab 1 ####################
  tabPanel(
    "Introduction",
    tags$img(
      src = "https://origin-www.nycgo.com/images/venues/1097/wall-street-photo-tagger-yancey-iv-nyc-and-company-02-2.jpg#/",
      width = "100%",
      style = "opacity: 0.90"
    ),
    fluidRow(
      absolutePanel(
        style = "background-color: white",
        top = "40%",
        left = "20%",
        right = "20%",
        height = 120,
        tags$p(
          style = "padding: 5%; background-color: white; font-family: alegreya; font-size: 120%",
          "Covid-19 has had a significant impact on job market, especially in New York City. This App helps companies and managers to understand the hiring trends in NYC. Prior to the declaration of COVID-19 as a pandemic, the NYC job market, as indicated by the number of job postings indicated a strong hiring trend. During COVID-19, companies faced a plethora of new risks highlighted by fewer job postings in mid-2020. With time, the firms adjusted to the new environment with Work From Home (WFH) being a part of the solution. This helped the employers increase hiring activity as seen by the increased number of job postings.
          The App further underscores trends in minimum degree requirement of the job, and compensation change across various job types over time."
        )
      )
    )
  ),
  #################### tab 2 ####################
  tabPanel(
    "Salary Change in NYC Jobs",
    sidebarLayout(
      sidebarPanel(
        
        # Select job category to plot
        selectInput(inputId = "Job.Category", label = strong("Job type"),
                    choices = unique(nyc_job$Job.Category),
                    selected = "Social Services"),
        
        # Select level of job to plot
        selectInput(inputId = "Career.Level", label = strong("Job Level"),
                    choices = unique(nyc_job$Career.Level),
                    selected = "Experienced"),
        
      ),
      
      # Output: Description, scatterplot
      mainPanel(
        plotOutput(outputId = "scatterplot1", height = "500px"),
      )
    )
  ),
  #################### tab 3 ####################
  tabPanel(
    "Position Change in NYC Jobs",
    sidebarLayout(
      sidebarPanel(
        
        # Select job category to plot
        selectInput(inputId = "Job.Category2", label = strong("Job type"),
                    choices = unique(nyc_job2$Job.Category),
                    selected = "Social Services"),
        
        # Select level of job to plot
        selectInput(inputId = "Career.Level2", label = strong("Job Level"),
                    choices = unique(nyc_job2$Career.Level),
                    selected = "Experienced"),
        
        
      ),
      # Output: Description, scatterplot
      mainPanel(
        plotOutput(outputId = "scatterplot2", height = "500px"),
      )
    )
  ),
  #################### tab 4 ####################
  tabPanel(
    "Minimum Degree Requirement Change in NYC Jobs",
    fluidPage(
      plotlyOutput("degree_requirement")
    )
  ),
  #################### tab 5 ####################
  tabPanel(
    "References",
    tags$h2(
      "Data and Image Sources"
    ),
    tags$a(
      href = "https://data.cityofnewyork.us/City-Government/NYC-Jobs/kpav-sd4t",
      "NYC Jobs", 
    ),br(),
    tags$a(
      href = "https://www.nycgo.com/attractions/wall-street/#",
      "Wall Street Image", 
    ),
    tags$h2(
      "Contributors"
    ),
    tags$p(
      "Sangmin Lee"
    ),
    tags$p(
      "Shubham Laddha"
    ),
    tags$p(
      "Tianyu Yao"
    ),
    tags$p(
      "Xiaoyuan Ge"
    ),
    tags$h2(
      "GitHub Repository"
    ),
    tags$a(
      href = "https://github.com/TZstatsADS/fall2022-project2-group1",
      "Project2 Group1"
    )
  ) # end of navbar
) # end of ui



#===============================================Shiny SERVER=====================================================
# LOAD AND PREPARE DATA ####################################

shinyServer <- function(input, output, session) {
  
  #################### Salary ####################
  
  # Subset data
  selected_job <- reactive({
    nyc_job %>%
      filter(
        Job.Category == input$Job.Category, 
        Career.Level == input$Career.Level
      )
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot1 <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_job()$Posting.Date, y = selected_job()$median_salary,
         xlab = "Date", ylab = "Median Salary ($)", col = color, fg = color, col.lab = color, col.axis = color)
    
  })
    
  
  
  ################### Position ###################
  
  # Subset data
  selected_job2 <- reactive({
    nyc_job2 %>%
      filter(
        Job.Category == input$Job.Category2, 
        Career.Level == input$Career.Level2
      )
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot2 <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x=selected_job2()$Posting.Date, y=selected_job2()$X..Of.Positions,
         xlab = "Change of number of positions", ylab = "Date", col = color, fg = color, col.lab = color, col.axis = color)
    
  })
  
  
  #################### Degree ####################
  df_J_I<-data.frame(cbind(format(as.Date(df_baccalaureate$Posting.Date),"%Y/%m/%d"),df_baccalaureate$count, df_master$count,df_high_school$count))
  colnames(df_J_I) <- c("Date","bachelor","master","high_school")
  fig_J_I <- plot_ly(df_J_I, type = "scatter" , mode = "lines")
  fig_J_I<- fig_J_I %>% add_lines(x=~Date, y=~bachelor, name="bachelor",
                                  line=list(color="red")) 
  fig_J_I<- fig_J_I%>% add_lines(x=~Date, y=~master, name="master",
                                 line=list(color="blue")) 
  fig_J_I<- fig_J_I%>% add_lines(x=~Date, y=~high_school, name="hish school",
                                 line=list(color="green")) 
  
  update_trace <- list(list(active = -1,type= 'buttons',
                            buttons = list(list(label = "bachelor",
                                                method = "update",
                                                args = list(
                                                  list(visible = c(FALSE, TRUE, FALSE)),
                                                  list(title = "degree requirement: bachelor",
                                                       annotations = 
                                                         list(c(),df_J_I$bachelor, c())))),
                                           list(label = "master",method = "update",
                                                args = list(
                                                  list(visible = c(TRUE, FALSE, FALSE)),
                                                  list(title = "degree requirement: master",
                                                       annotations = 
                                                         list(df_J_I$master, c(), c())))),
                                           list(label = "high school",method = "update",
                                                args = list(
                                                  list(visible = c(FALSE, FALSE, TRUE)),
                                                  list(title = "degree requirement: high school",
                                                       annotations = 
                                                         list(c(), c(), df_J_I$high_school)))),
                                           list(label = "All",method = "update",
                                                args = list(list(visible = c(TRUE, TRUE, TRUE)),
                                                            list(title = "minimum degree requirement",
                                                                 annotations = 
                                                                   list(df_J_I$bachelor,
                                                                        df_J_I$master,
                                                                        df_J_I$high_school)))))))
  fig_J_I_update <- fig_J_I %>% layout(title = "Job Degree", showlegend=FALSE,
                                       xaxis=list(title="Date"),
                                       yaxis=list(title="number of jobs"),
                                       updatemenus=update_trace)
  
  output$degree_requirement <- renderPlotly({
    fig_J_I_update%>% 
      layout(xaxis = list(rangeslider = list(visible = T)))
  })
  
  
  
}


shiny::shinyApp(ui, shinyServer)

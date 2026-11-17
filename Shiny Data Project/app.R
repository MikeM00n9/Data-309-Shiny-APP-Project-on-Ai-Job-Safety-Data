library(shiny)
library(shinythemes)
library(dplyr)
library(fmsb)   # for radar charts
library(DT)
library(ggplot2)
library(forcats)
#df is the data table
# ---- Load Data ----
aidata <- read.csv("C:\\Users\\mikey\\OneDrive\\Desktop\\data 309\\DATA SETS\\AI_Impact_on_Jobs_2030.csv")

# Numeric columns for radar plot
skill_cols <- grep("Skill_", names(aidata), value = TRUE)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("AI Job Automation Risk Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("risk_filter", "Filter by Risk Level:",
                  choices = c("All", "High", "Medium", "Low"),
                  selected = "All"),
      hr(),
      h4("Instructions:"),
      p("1. Select a risk category to filter the table."),
      p("2. Click a job in the table to see its risk drivers.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Job Table",
                 h3("Job Dataset"),
                 DT::dataTableOutput("jobtable")
        ),
        tabPanel("Risk Drivers",
                 h3(textOutput("job_title")),
                 tableOutput("job_info"),
                 plotOutput("radarPlot", height = "400px")
        ),
        tabPanel("Bar Graph",
                 h3("Job Count by Risk Category"),
                 actionButton('button', 'Resample'),
                 plotOutput("barPlot", height = "400px")
        ),
        tabPanel("Scatter Plot",
                   h3("Scatter Plot with Regression Line"),
                   
                   # user chooses variables
                   selectInput("scatter_x", "X-axis Variable:",
                               choices = names(aidata), selected = "Years_Experience"),
                   selectInput("scatter_y", "Y-axis Variable:",
                               choices = names(aidata), selected = "Automation_Probability_2030"),
                   
                   plotOutput("scatterPlot", height = "400px")
        )
        
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  
  
  # filtering data
  filteredData <- reactive({
    if (input$risk_filter == "All") return(aidata)
    aidata |> filter(Risk_Category == input$risk_filter)
  })
  
  #A Datatable with multi selected
  output$jobtable <- DT::renderDataTable({
    DT::datatable(filteredData(), selection = "multiple")
  })
  
  # selected jobs table
  selectedJob <- reactive({
    req(input$jobtable_rows_selected)
    filteredData()[input$jobtable_rows_selected, ]
  })
  
  #job info table
  output$job_info <- renderTable({
    req(selectedJob())
    selectedJob() |>
      select(Job_Title, Automation_Probability_2030,
             AI_Exposure_Index, Tech_Growth_Factor,
             Average_Salary, Years_Experience)
  })
  
  # Job title text
  output$job_title <- renderText({
    req(selectedJob())
    if (nrow(selectedJob()) == 1) {
      paste("Risk Drivers for:", selectedJob()$Job_Title)
    } else {
      paste(nrow(selectedJob()), "jobs selected — averaged risk drivers",selectedJob()$Risk_Category)
    }
  })
  
  
  #A Radar Plot that can do a single or multiple
  output$radarPlot <- renderPlot({
    req(selectedJob())
    
    jobs <- selectedJob()[, skill_cols, drop = FALSE]
    
    # converting numeric
    jobs <- as.data.frame(lapply(jobs, as.numeric))
    
    # average if mutli jobs are selected
    job_skills <- if (nrow(jobs) > 1) {
      colMeans(jobs, na.rm = TRUE)
    } else {
      as.numeric(jobs[1, ])
    }
    
    
    max_row <- rep(1, length(skill_cols))#max row
    min_row <- rep(0, length(skill_cols))#min row
    
    radarData <- rbind(max_row, min_row, job_skills)
    radarData <- as.data.frame(radarData)
    colnames(radarData) <- skill_cols
    
    
    rownames(radarData) <- c("Max", "Min", "Selected")
    
    #radar plot
    radarchart(radarData,
               axistype = 1,
               pcol = "red",
               pfcol = rgb(1,0,0,0.3),
               plwd = 2,
               cglcol = "black",
               cglty = 1,
               vlcex = 0.7)
  })
  
  
  my_plot <- eventReactive(input$button,{
    
    data <- filteredData() |> slice_sample(n=50)|> 
      group_by(Job_Title) |>
      summarise(
        Years_Experience = mean(Years_Experience, na.rm = TRUE),
        .groups = "drop"
      )
    
    data$Job_Title <- factor(
      data$Job_Title,
      levels = data$Job_Title[order(data$Years_Experience, decreasing = TRUE)]
    )
    
    Bar_graph_title = paste(input$risk_filter)
    
    # refactor Job_Title to be orderd by Years_Experience
    p <-  ggplot(data, aes(y= reorder(Job_Title, -Years_Experience), 
                           x=Years_Experience, 
                           fill = Job_Title)) +
      geom_bar(stat='identity')+
      labs(title = Bar_graph_title )+
      scale_x_continuous(
        
        limits =c( 0,40),
        breaks = seq(0,40,by = 1)
      )
      #scale_y_discrete(limits = reorder(Job_Title, -Years_Experience))
    
    return(p)
    
  })
  
  output$barPlot <- renderPlot({
    
   
    my_plot()  
    
    
    
  })
  output$scatterPlot <- renderPlot({
    data <- filteredData()
    
    req(input$scatter_x, input$scatter_y)
    
    # Ensure chosen columns are numeric
    
    
    ggplot(data, aes(x = .data[[input$scatter_x]], 
                     y = .data[[input$scatter_y]])) +
      geom_point(color = "cyan", size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "yellow") +
      theme_minimal(base_size = 14) +
      labs(
        x = input$scatter_x,
        y = input$scatter_y,
        title = paste("Scatter Plot of", input$scatter_y, "vs", input$scatter_x),
        subtitle = "Includes a linear regression line"
      )
  })
  
  
}



shinyApp(ui, server)



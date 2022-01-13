rm(list = ls())
cat("\014")

## Need to keep data frame name same i.e. ProjectEvaluations ##
## Need to make sure result columns are always last 3 columns ##

## loading packages
{
  library(shiny)
  library(shinydashboard)
  library(htmltools)
  library(rvest)
  library(XML)
  library(measurements)
  library(ggplot2)
  library(ggrepel)
  library(plotly)
  library(tableHTML)
  library(shinycssloaders)
  library(shinyWidgets)
}

#reading csv
{
ProjectEvaluations <- read.csv("ProjectEvaluations.csv")
summary(ProjectEvaluations)
}

#ui
{
ui <- dashboardPage(
  dashboardHeader(title = span("", 
                               span("PEST - \nProject Evaluation Study & Test", 
                                    style = "color: white; font-size: 40px;font-weight: bold")), titleWidth = 1500),
  dashboardSidebar(
    
    collapsed = TRUE
    
  ),
  
  dashboardBody(
    
    fluidPage(
      
      tags$head(
        tags$style(
          HTML('
        body, label, input, button, select { 
          font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size: 15px;
          color: black;
        }')
        )),
      
      sidebarLayout(
        
        sidebarPanel(
          
          #ensures input panel updates as the data updates
          lapply(4:ncol(ProjectEvaluations)-3, function(i) {
            
            selectInput(intToUtf8(96+i),colnames(ProjectEvaluations[i]),
                        choices = unique(ProjectEvaluations[[i]]),
                        selected = NULL)
            
          }),
          
          #ensures updated output is shown only when action button is clicked
          # and not when any one input is changed by the user
          actionButton("Button", "Update")
          
        ),
        
        mainPanel(
          
          fluidRow(valueBoxOutput("info_box1", "100%")),
          
          fluidRow(valueBoxOutput("info_box2", width = "100%")),
          
          fluidRow(withSpinner(plotlyOutput("distribution")), width = "100%", height = "100%")
        )
        
      )
      
    )
    
  )
)
}

#server 
{
server <- function(input, output) {
  
  #plots default input option when app starts for the 1st time
  calculatedInput <- reactiveVal(c(TRUE, rep(FALSE, nrow(ProjectEvaluations)-1)))
  
  #ensures output only shows when action button pressed (for 2nd time and onwards)
  observeEvent(input$Button, {
    calculatedInput(Reduce(`&`,lapply(4:ncol(ProjectEvaluations)-3,
                                    function(i) {ProjectEvaluations[[colnames(ProjectEvaluations[i])]] == input[[intToUtf8(96+i)]]})))
  })
    
    # plotting distribution of scores
    output$distribution <- renderPlotly({
      
      ggplot(ProjectEvaluations, aes(x=Scaled.Score)) +
        geom_histogram(binwidth = 5, colour="black", fill="lightblue") +
        annotate("text", x= ProjectEvaluations$Scaled.Score[calculatedInput()] + 6 , y=1350,
                 label=paste("Your project"), size=3) +
        geom_vline(aes(xintercept = Scaled.Score[calculatedInput()]),
                   color="black", linetype="dashed", size=1,)+
        ggtitle(label = "Distribution of Scores (0 = Precarious, 100 = Excellent)")+
        labs(
          x= "Score",
          y ="Count"
        )+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              title = element_text(face = "bold", color = "black"),
              axis.title = element_text(face = "bold", color = "black"),
              text=element_text(size = 14),
              plot.title = element_text(hjust = 0.5))
      
    })
  
  
  # shows score output in value box
  output$info_box1 <- renderValueBox({

      valueBox(value = paste0("Score in %: ",
                              round(ProjectEvaluations$Scaled.Score[calculatedInput()],
                                    2),
                              collapse = ", "),
               color = if(round(ProjectEvaluations$Scaled.Score[calculatedInput()],
                                2) > 20) "light-blue" else "red",
               subtitle = NULL)

  })
  
  # shows assessment output in value box
  output$info_box2 <- renderValueBox({

      valueBox(value = paste0("Assessment: ",
                              ProjectEvaluations$Prediction[calculatedInput()],
                              collapse = ", "),
               color = if(ProjectEvaluations$Prediction[calculatedInput()] == "precarious")
                 "red" else "light-blue",
               subtitle = NULL)
    
  })
  
  
} 
}

shinyApp(ui, server)

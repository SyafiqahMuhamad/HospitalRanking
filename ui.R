library(shiny)
library(shinydashboard)

outcome_data=read.csv(file = "D:/New folder/SYAFIQAH/SEM 4 WIE170036/WIE2003- DATA SCIENCE/Shiny App/hospital/outcome-of-care-measures.csv",stringsAsFactors = F)

outcome_data=outcome_data[outcome_data[,6]!='Not Available',]
outcome_data=outcome_data[outcome_data[,7]!='Not Available',]
outcome_data=outcome_data[outcome_data[,8]!='Not Available',]



dashboardPage(
  dashboardHeader(title="Hospital Ranking"),
  
  dashboardSidebar(width = 300,
                   selectInput("city", 
                               label = em("Select City",style="text-align:center;color:#FFA319;font-size:150%"),
                               unique(outcome_data$City),selected = 'DOTHAN'), 
                   
                   selectInput("outcome", 
                               label = em("Select Disease",style="text-align:center;color:#FFA319;font-size:150%"),
                               choices = c("Heart Attack","Pneumonia","Hear Failure"),
                               selected = "Heart Attack"),
                   
                   selectInput('columns',em('Choose Hospital',style="text-align:center;color:#FFA319;font-size:150%"),"",
                               selectize = FALSE,selected = '')
                   
                   
                   
                   
  ),
  
  
  dashboardBody(   
    
    fluidRow(
      column(width = 12,
             
             plotOutput("myplot")),
      
      
      column(width = 7,
             
             h5(strong("Best Hospitals in the City",style="text-align:centre;color:darkblue;font-size:150%")),
             
             div(tableOutput("table1"), style = "font-size:150%;background-color:white",collapsible = TRUE)))
    
  )
)







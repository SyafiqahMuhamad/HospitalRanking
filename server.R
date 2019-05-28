library(shiny)
library(ggplot2)
library(dplyr)    
library(tidyr)

outcome_data=read.csv(file = "D:/New folder/SYAFIQAH/SEM 4 WIE170036/WIE2003- DATA SCIENCE/Shiny App/hospital/outcome-of-care-measures.csv",stringsAsFactors = F)


names(outcome_data)=c('Hospital',"Address","City","State","ZIP","Heart Attack","Heart Failure","Pneumonia")

outcome_data=outcome_data[outcome_data[,6]!='Not Available',]
outcome_data=outcome_data[outcome_data[,7]!='Not Available',]
outcome_data=outcome_data[outcome_data[,8]!='Not Available',]


outcome_data1 <- outcome_data%>% gather(Type, Deaths, -Hospital,-Address,-City,-State,-ZIP)
outcome_data1$Type=as.factor(outcome_data1$Type)
outcome_data1$Deaths=as.numeric(outcome_data1$Deaths)




shinyServer(function(input, output, session) {
  
  
  city<-reactive({
    input$city
  })
  
  
  
  hospital<-reactive({
    input$columns
  })
  
  
  outvar=reactive({
    mm=outcome_data$Hospital[outcome_data$City==city()]
    unique(mm)
  })
  
  
  observe({
    
    updateSelectInput(session,"columns",
                      choices=outvar())
  })
  
  
  deaths<- reactive({
    
    
    m=filter(outcome_data1, City==city(),Type==outcome(),Hospital==hospital())
    
    as.numeric(select(m,Deaths))
    
    
  })
  
  outcome<-reactive({
    input$outcome
  })
  
  
  
  best_in_this_city<-reactive({
    
    m=filter(outcome_data1, City==city(),Type==outcome())
    m=arrange(m,Deaths,Hospital)
    m=select(m,Hospital, City)
    m$Rank=rownames(m)
    m=select(m,Rank,Hospital,City)
    
    if(nrow(m)>9){
      m[1:10,]
    } else {m}
    
  })
  
  
  its_rank<-reactive({
    
    m=filter(outcome_data1, City==city(),Type==outcome())
    m=arrange(m,Deaths,Hospital)
    m=select(m,Hospital)
    m$Rank=rownames(m)
    m=select(m,Rank,Hospital)
    as.numeric(m$Rank[m$Hospital==hospital()])
    
    
  })
  
  
  
  output$table1 <- renderTable(best_in_this_city(),include.rownames=FALSE)
  
  color=c('#75a3a3','#999966','#79a6d2','#c68c53')
  
  
  
  
  output$myplot<-renderPlot({
    
    if(outcome()=="Heart Attack"){
      
      hist(as.numeric(outcome_data[, 6]),xlab="Deaths from heart attack",
           main ="Death rates from heart attack\n
           and performance of selected hospital",cex.lab=1,cex.axis=1,
           col=sample(color,1,replace=T),border='white',cex.main=1.2)
      abline(v=deaths(),col="red",lwd=2)
      
      text(1.25*mean(as.numeric(outcome_data[, 6]),na.rm=TRUE), 600, hospital(), col = "#660066",
           cex = 1)
      text(mean(as.numeric(outcome_data[, 7]),na.rm=TRUE), 500, paste0("Rank in ",city(),": ", its_rank()), col = "blue",cex=1)
      
    }
    
    else if(outcome()=="Heart Failure"){
      
      hist(as.numeric(outcome_data[, 7]),xlab="Deaths from heart failure",
           main ="Death rates from heart failure\n and performance of selected hospital",cex.lab=1,cex.axis=1,
           col=sample(color,1,replace=T),border='white',cex.main=1.2)
      abline(v=deaths(),col="red",lwd=2)
      text(1.25*mean(as.numeric(outcome_data[, 7]),na.rm=TRUE), 600, hospital(), col = "#660066",cex=1)
      text(1.3*mean(as.numeric(outcome_data[, 7]),na.rm=TRUE), 500, paste0("Rank in ",city(),": ", its_rank()), col = "blue",cex=1)
      
    }
    
    
    else if(outcome()=="Pneumonia"){
      
      hist(as.numeric(outcome_data[, 8]),xlab="Deaths from pneumonia",
           main ="Death rates from pneumonia\n and performance of selected hospital",cex.lab=1,cex.axis=1,
           col=sample(color,1,replace=T),border='white',cex.main=1.2)
      abline(v=deaths(),col="red",lwd=2)
      text(1.25*mean(as.numeric(outcome_data[, 8]),na.rm=TRUE), 500, hospital(), col = "#660066",cex=1)
      
      text(1.3*mean(as.numeric(outcome_data[, 7]),na.rm=TRUE), 400, paste0("Rank in ",city(),": ", its_rank()), col = "blue",cex=1)
      
    }
  })
  
  
  
  
})





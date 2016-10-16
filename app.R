library(shiny)
library(ggplot2)

dat <- read.csv("http://www.math.usu.edu/cfairbourn/Stat2300/RStudioFiles/data/preg.csv",header=TRUE)
age <- dat$age
mu <- mean(age)
stdev <- sd(age)
reps <- 100
ui <- fluidPage(
  titlePanel("Reactive Experiment"),
  sidebarPanel(
    numericInput("nsize","Sample Size",value=100,min=1),
    numericInput("conf","Confidence Level (enter a percentage value for the confidence level between 1 and 99)",value=95,min=1,max=99),
    hr()
  ),
  mainPanel(
    tableOutput("ConfPlot"),
    plotOutput("PracticeHist"),
    plotOutput("SampMeanHist")
  )
)

server <-function(input, output) {
  Q <- NULL
  result <- reactive({
    res<<-array(0,dim=c(reps,3))
    for(i in 1:reps) {
      y<-sample(age,input$nsize)
      res[i,1]<-mean(y)
      res[i,2]<-sd(y)
      res[i,3]<-sd(y)/sqrt(input$nsize)
    }
    res
  } )
  Q <- reactive({
    abs(qnorm((100-input$conf)/200))
  })
  output$ConfPlot<-renderTable({
    head(result())
  })
  output$PracticeHist <- renderPlot({
    plot(mu + c(-5,5),c(1,1),type="n",xlab="Age",
         ylab="Intervals",ylim=c(1,100))
    abline(v=mu)
    res<-result()
    for(i in 1:reps){
      interval<-c(res[i,1]-Q()*res[i,3],res[i,1]+Q()*res[i,3])
      color<-ifelse(interval[1]<=mu & interval[2]>=mu,1,2)
      lines(interval, c(i,i), col=color)
    } 
  })
  output$SampMeanHist <- renderPlot({
    res <- result()
    Q <- Q()
   
    intmin<-mean(age)-Q*sd(age)/sqrt(input$nsize)
    intmax<-mean(age)+Q*sd(age)/sqrt(input$nsize)
    hist(res[,1],prob=TRUE,main=paste("Histogram of",reps," sample averages"),
         xlab="Sample average",ylab="Proportion per sample average",
         xlim=c(22,32),ylim=c(0,1), breaks=15)
    abline(v=intmin,col="red",lwd=2)
    abline(v=intmax,col="red",lwd=2) 
    
  })
  
}

shinyApp(ui = ui, server = server)
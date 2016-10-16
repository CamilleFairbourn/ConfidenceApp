library(shiny)
library(ggplot2)
plaintheme <- theme_bw() + 
   theme(plot.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank() ) +
   theme(panel.border = element_blank()) +
   theme(axis.line.x = element_line(color="black", size = 1),
         axis.line.y = element_line(color="black", size = 1))

axistheme <- theme(plot.title = element_text(color = "black", face = "bold", size=28)) +
   theme(axis.title = element_text(color = "black", size = 20)) +
   theme(axis.text.x = element_text(size = 16)) +
   theme(axis.text.y = element_text(size = 16))
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
    plotOutput("ConfPlot"),
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
  output$ConfPlot <- renderPlot({
    plot(mu + c(-5,5), c(1,1), type = "n", xlab = "Age",
         ylab = "Intervals", ylim = c(1,100), 
         cex.axis = 2, cex.lab = 2, cex.main = 2)
    abline(v = mu)
    res<-result()
    for(i in 1:reps){
      interval<-c(res[i,1]-Q()*res[i,3],res[i,1]+Q()*res[i,3])
      color<-ifelse(interval[1]<=mu & interval[2]>=mu,1,2)
      lines(interval, c(i,i), col=color)
    } 
  })
  output$SampMeanHist <- renderPlot({
    res <- result()
    #Q <- Q()
    dfres <- as.data.frame(res)
    names(dfres) <- c("mean", "sd", "se")
    intmin<-mean(age)-Q()*sd(age)/sqrt(input$nsize)
    intmax<-mean(age)+Q()*sd(age)/sqrt(input$nsize)
    
    ggplot(dfres, aes(mean)) +
       geom_dotplot(binwidth = .15, method = "histodot") +
       geom_vline(xintercept = intmin, col = "red") +
       geom_vline(xintercept = intmax, col = "red") +
       scale_x_continuous("sample means", limits = c(24,30)) +
       plaintheme +
       axistheme
    #hist(res[,1],prob=TRUE,main=paste("Histogram of",reps," sample averages"),
    #     xlab="Sample average",ylab="Proportion per sample average",
    #     xlim=c(22,32),ylim=c(0,1), breaks=15)
    #abline(v=intmin,col="red",lwd=2)
    #abline(v=intmax,col="red",lwd=2) 
    
  })
  
}

shinyApp(ui = ui, server = server)
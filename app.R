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

#Simulated Confidence Intervals

dat<-read.csv("http://www.math.usu.edu/cfairbourn/Stat2300/RStudioFiles/data/preg.csv",header=TRUE)
age<-dat$age
mu<-mean(age)
reps<-100
res <- NULL
Q <- NULL
# This is to keep the samples the same for each student, but let them differ
# between students
#random_seed <- as.numeric(Sys.time())
ui <- fluidPage(
    titlePanel("Simulated Confidence Intervals for the mean age of pregnant women"),
    sidebarLayout(
      sidebarPanel(
        numericInput("N","Sample Size",value=100,min=1),
        numericInput("conflev","Confidence Level (enter a value (Danny suggests: in percent) between 1 and 99.99)",value=95,min=1,max=100),
        hr(),
        tags$div(class="header", checked=NA,
                 tags$p("This application uses recent NHANES data about pregnant women 
                        in the United States. The average age of these women was 27.03 years."),
                 tags$p("The application will draw 100 samples of the size you specify
                        and visually represent the confidence interval based on each
                        sample. If the confidence interval does not include the true
                        mean of 27.03, the interval will be colored red."),
                 tags$p("Try different values for both the sample size and the confidence
                        level. Notice how the width of the intervals changes for different
                        sample sizes and confidence levels. Does a larger sample size give
                        narrower or wider intervals? Does a larger confidence level give
                        narrower or wider intervals?"),
                 tags$p("Beneath the confidence interval plot, the application
                        will construct a histogram of the sample averages, along with the
                        normal curve based on the expected value and standard error. Red 
                        vertical lines will indicate which sample means fell outside of
                        the indicated confidence level. ")
                 
                 
                 ),
        helpText(" ")
                 ),
      mainPanel(
        plotOutput("ConfPlot"),
        plotOutput("SampMeanHist")
      )
                 )
    )
  
# Define a server for the Shiny app
server <-function(input, output) {
  random_seed <- as.numeric(Sys.time())  
  output$ConfPlot<-renderPlot({
    N<-input$N
    Q<<-abs(qnorm((100-input$conflev)/200))
    res<<-array(0,dim=c(reps,3)) 
    set.seed(random_seed)
    for(i in 1:reps) {y<-sample(age,N)
    res[i,1]<-mean(y)
    res[i,2]<-sd(y)
    res[i,3]<-sd(y)/sqrt(N)} 
    plot(mu + c(-5,5),c(1,1),type="n",xlab="Age",
         ylab="Intervals",ylim=c(1,100))
    abline(v=mean(age))
    
    for(i in 1:reps){
      interval<-c(res[i,1]-Q*res[i,3],res[i,1]+Q*res[i,3])
      color<-ifelse(interval[1]<=mean(age) & 
                      interval[2]>=mean(age),1,2)
      lines(interval, c(i,i),col=color)
    } 
    
    
  })
  output$SampMeanHist <- renderPlot({
    
    # Render a histogram of 100 sample means
    res<<-array(0,dim=c(reps,3)) 
    set.seed(random_seed)
    for(i in 1:reps) {y<-sample(age,N)
    res[i,1]<-mean(y)
    res[i,2]<-sd(y)
    res[i,3]<-sd(y)/sqrt(N)}
    Q<<-abs(qnorm((100-input$conflev)/200))
    hist(res[,1],prob=TRUE,main=paste("Histogram of",reps," sample averages"),
         xlab="Sample average",ylab="Proportion per sample average",
         xlim=c(22,32),ylim=c(0,1), breaks=15)
    points(seq(min(age), max(age), length.out=500),
           dnorm(seq(min(age), max(age), length.out=500),
                 mean(age), sd(age)/sqrt(input$N)), type="l", col="darkblue", lwd=2)
    intmin<-mean(age)-Q*sd(age/sqrt(input$N))
    intmax<-mean(age)+Q*sd(age/sqrt(input$N))
    abline(v=intmin,col="red",lwd=2)
    abline(v=intmax,col="red",lwd=2) 
    
  })
}

shinyApp(ui = ui, server = server)


library(shiny)
library(ggplot2)
library(dplyr)

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
  titlePanel("Simulated Confidence Intervals for the Mean Age of Pregnant Women"),
  sidebarPanel(
    numericInput("nsize", "Sample Size", value = 100, min = 1, max = 150),
    numericInput("conf", "Confidence Level (enter a percentage value for the confidence level between 1 and 99)",value=95,min=1,max=99),
    hr(),
    tags$div(class = "header", checked = NA,
             tags$p("This application uses recent NHANES data about pregnant women 
                    in the United States. The average age of these women was 27.03 years."),
             tags$p("The application will draw 100 samples of the size you specify
                    and visually represent the confidence interval based on each
                    sample. If the confidence interval does not include the true
                    mean of 27.03, the interval will be colored red."),
             tags$p("Try different values for both the sample size and the confidence
                    level. Notice how the width of the intervals changes for different
                    sample sizes and confidence levels. Does a larger sample size give
                    narrower or wider intervals?"),
             tags$p("Beneath the confidence interval plot, the application
                    will construct a dotplot of the sample averages. Red 
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

server <-function(input, output) {
  Q <- NULL
  lims <- seq(22,32, 2)
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
    par(mar=c(5,6,4,2)+0.1)
    plot(mu + c(-5,5), c(1,1), type = "n", xlab = "Age",
         ylab = "Intervals", ylim = c(1,100), 
         cex.axis = 2, cex.lab = 2, cex.main = 2)
    abline(v = mu)
    res<-result()
    for(i in 1:reps){
      interval<-c(res[i,1]-Q()*stdev/sqrt(input$nsize),res[i,1]+Q()*stdev/sqrt(input$nsize))
      color<-ifelse(interval[1]<=mu & interval[2]>=mu,1,2)
      lines(interval, c(i,i), col=color)
    } 
  })
  output$SampMeanHist <- renderPlot({
    
    res <- result()
    #Q <- Q()
    dfres <- as.data.frame(res)
    names(dfres) <- c("mean", "sd", "se")
    intmin<-mean(age)-Q()*stdev/sqrt(input$nsize)
    intmax<-mean(age)+Q()*stdev/sqrt(input$nsize)
    dfres <- mutate(dfres, cover = (mean > intmin & mean < intmax))
    
    ggplot(dfres, aes(mean)) +
       geom_dotplot(aes(fill = cover), binwidth = .15, method = "histodot") +
       geom_vline(xintercept = intmin, col = "red") +
       geom_vline(xintercept = intmax, col = "red") +
       scale_x_continuous("Sample Means", limits = c(22.25,32.25), breaks = seq(22,32,2)) +
       scale_y_continuous(" ") +
       guides(fill=FALSE) +
       plaintheme +
       axistheme
    
  })
  
}

shinyApp(ui = ui, server = server)
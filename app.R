#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(usl)

dat <- NULL

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  headerPanel("Universal Scalability Law"),
  
     sidebarLayout(
      sidebarPanel(

         sliderInput("conn",
                     "N (X-Axis)",
                     min = 1,
                     max = 1000,
                     value = 30),
         fileInput("file1", "Choose CSV File",
                   accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")),
         # uiOutput("choose_columns"),
         
         # fluidRow(
         #   column(6,radioButtons("xaxisGrp","X-Axis:", c("1"="1","2"="2"))),
         #   column(6,radioButtons("yaxisGrp","Y-axis:", c("1"="1","2"="2")))
         # ),
         radioButtons('sep', 'Separator',
                      c(Comma=',', Semicolon=';',Tab='\t'), ','),
         radioButtons('quote', 'Quote',
                      c(None='','Double Quote'='"','Single Quote'="'"),''),
        h3("Steps to apply USL"),
        strong("1. Choose Parameter"),
        p("• Concurrency N – MySQL(thread_running) or CPU cores "),
        p("• Load – Throughput - C(N), Utilization - U(N)"),
        strong("2. Collect Data"),
        br(),
        strong("3. Consolidate Data"),
        p("• The metric data is saved in the CSV file"),
        p("• Field name must be  'conn' as Concurrency or CPU cores , 'tput' as Throughput or Load "),
        strong("4. Upload Data"),
        br(),
        strong("5. Analyze Model"),
        p("• Find Max Concurrency - Nmax"),
        p("• Predict Max Load - Xmax, Umax")
        
      ),
 
      # Show a plot of the generated distribution
      mainPanel(
        h3("USL Description"),
        p("The universal scalability law (USL) is an analytic model used to quantify application scaling."),

        h3("USL Formula"),
        uiOutput("usl"),
        p(strong("The three terms in the denominator of eqn. (1) are associated repectively with the three Cs:")),
        p(em("1.the level of Concurrency or ideal parallelism: basically, linear scaling.")),
        p(em("2.the level of Contention (with strength α) due to waiting or ",strong("queueing"), "for shared resources.")),
        p(em("3. the level of Coherency (with strength β) due to the delay for data to become consistent (or coherent) by virtue of ",strong("point-to-point exchange."))),
        
        h3("Trends"),
        tabsetPanel(
          tabPanel("Plot",plotOutput("plot")),
          tabPanel("Data", tableOutput('contents'))
        ),
        h3("Conclusion"),
        verbatimTextOutput("conclusion"),
        
        h3("References"),
        a("USL Scalability", href="http://www.perfdynamics.com/Manifesto/USLscalability.html")
        
      )
      
   ),
  h5("Site made by", a("Hong Bin",href="mailto:hongbin@actionsky.com"), align = "center")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$usl <- renderUI({
    withMathJax(
      helpText("Relative capacity C(N) $$C(N) = \\frac{N} {1+\\alpha(N-1)+ \\beta N(N-1)}$$"),
      helpText("Maximum scalability Nmax $$N_{max} = \\sqrt{(1-\\alpha) / \\beta}$$"),
      helpText("Throughput Xmax at load Nmax $$X_{max} = X(1) * C(N_{max})$$")
    )
  })
  # dsnames <- c()
  data_set <- reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath,sep=input$sep, quote=input$quote)
  })
  
  output$contents <- renderTable({
    data_set()
  })
  # observe({
  #   dsnames <- names(data_set())
  #   cb_options <- list()
  #   cb_options[ dsnames] <- dsnames
  #   updateRadioButtons(session, "xaxisGrp",
  #                      label = "X-Axis",
  #                      choices = cb_options,
  #                      selected = "")
  #   updateRadioButtons(session, "yaxisGrp",
  #                            label = "Y-Axis",
  #                            choices = cb_options,
  #                            selected = "")
  # })
  # output$choose_dataset <- renderUI({
  #   selectInput("dataset", "Data set", as.list(data_sets))
  # })
  
  # output$choose_columns <- renderUI({
  #   
  #   if(is.null(input$dataset))
  #     return()
  #   colnames <- names(contents)
  #   checkboxGroupInput("columns", "Choose columns", 
  #                      choices  = colnames,
  #                      selected = colnames)
  # }) 
  
   output$plot <- renderPlot({
     dd <- data_set()
     if(is.null(dd))
       return(NULL)
 
     usl.model <- usl(tput ~ conn, dd)
     
     s <- scalability(usl.model)

      plot(s,from=1,to=input$conn, xlab="Concurrency", col="green", ylab="Throughput", main= "USL C(N)", lty="dashed")
      grid()
      points(dd$conn,dd$tput)

      plot(usl.model,bounds=TRUE,add=TRUE)

   })

   output$conclusion <- renderPrint({
     dd <- data_set()
     if(is.null(dd))
       return(NULL)
     usl.model <- usl(tput ~ conn, dd)
     s1 <- paste("α =", round(coef(usl.model)['sigma'],4))
     s2 <- paste("β =",round(coef(usl.model)['kappa'],4))
     nmax <- as.integer(peak.scalability(usl.model)+0.5)
     s3 <- paste("R-Squared = ", round(summary(usl.model)$adj.r.squared,2))
     s4 <- paste("The model forecasts a maximum Concurrency at", nmax)
     s5 <- paste("The system's X(1) was estimated at", round(usl.model$scale.factor,2))
     xmax <- nmax * usl.model$scale.factor
     s6 <- paste("The model forecasts a maximum throughput of", xmax, "at N =", nmax)
     cat(paste(s1, s2,s3,s4,s5,s6, sep="\n"))

   })

   
}

# Run the application 
shinyApp(ui = ui, server = server)


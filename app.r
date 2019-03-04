library(shiny)
library(networkD3)
library(openxlsx)
library(colourpicker)
library(devtools)
library(readr)
DB<-read.xlsx("DB.xlsx",startRow = 2,sheet = 1,colNames = TRUE)

ui <- fluidPage( 
  helpText(h6("Powerd by")),
  tags$a(href='https://www.linkedin.com/in/frady-ali-ab8700132/',tags$h6("FRADY Ali")),
  tags$a(href='http://data-expert.net/',tags$img(src='alidata.png',height=50,width=150)),
  tabsetPanel(
    tabPanel("Data",  fileInput("myData", "Upload your data "),
             helpText(h6("Default max. file size is 5MB")),
             uiOutput("tb")),
    tabPanel("Display graph", flowLayout(
      
      flowLayout( verticalLayout(sliderInput(inputId ='x',label = "Font size",min = 8,max = 24,value = 11,step = 1),
                                 sliderInput(inputId ='y',label = "Graph size",min = 12,max = 20,value = 20,step = 2)
      ),verticalLayout(textOutput("codec"),
                       colourInput("col", "Select colour", "purple"))
      ),
      verticalLayout(textInput("domaine","Group names "),
                     textInput("couleur","Group colors","'blue','#1FF22A','pink','#EFFC00','red'"),
                     helpText("* Same order of group names as",'"1600D9","red"#F7F705"')
      ),
      uiOutput("sankey",position="right"))),
    tabPanel("Summary",  uiOutput("s")))
  
  
)
server <- function(input, output) {
  
  #read links data 
  data <- reactive({
    file1 <- input$myData
    if (is.null(file1)) {
      return(NULL)
    }
    read.xlsx(file1$datapath,startRow = 2,sheet = 1,colNames = TRUE,cols =1:6)
    
  })
  #about data
  output$filedf <- renderTable({
    if (is.null(data())) {
      return ()
    }
    
    input$myData
  })
  output$s <- renderUI({
    if (is.null(data()))
      h1("Check your file!", align='center'
      )
    else
      tabsetPanel(
        tabPanel("Source", tableOutput("from")),
        tabPanel("Target", tableOutput("to")),
        tabPanel("Value", tableOutput("weight"))
        
      )
  }) 
  #summary data 
  output$from <- renderTable({
    if (is.null(data())) {
      return ()
    }
    x <- reactive({
      file1 <- input$myData
      if (is.null(file1)) {
        return(NULL)
      }
      read.xlsx(file1$datapath,startRow = 2,sheet = 1,colNames = TRUE,cols =2)
      
    })
    
    
    summary(x())
  })
  output$to <- renderTable({
    if (is.null(data())) {
      return ()
    }
    x <- reactive({
      file1 <- input$myData
      if (is.null(file1)) {
        return(NULL)
      }
      read.xlsx(file1$datapath,startRow = 2,sheet = 1,colNames = TRUE,cols =4)
      
    })
    
    
    summary(x())
  })
  output$weight <- renderTable({
    if (is.null(data())) {
      return ()
    }
    x <- reactive({
      file1 <- input$myData
      if (is.null(file1)) {
        return(NULL)
      }
      read.xlsx(file1$datapath,startRow = 2,sheet = 1,colNames = TRUE,cols =5)
      
    })
    
    
    summary(x())
  })
  #display data table 
  output$table <- renderTable({
    if (is.null(data())) {
      return ()
    }
    
    data()
  })
  #read nodes data
  label <- reactive({
    file1 <- input$myData
    if (is.null(file1)) {
      return(NULL)
    }
    read.xlsx(file1$datapath,startRow = 2,sheet = 1,colNames = TRUE,cols = 7:8)
  })
  output$splot <- renderSankeyNetwork({
    
    colorJS <- paste('d3.scaleOrdinal().domain([',input$domaine,'])','.range([',input$couleur,'])')
    
    sankeyNetwork(
      Links = data(),
      Nodes = label(),
      Source = 'i',
      Target = 'j',
      Value = 'value',
      NodeID = "name",
      fontSize = input$x,
      nodeWidth =0.6*input$x,
      NodeGroup = "ngroup", LinkGroup = "lgroup"
      ,colourScale = colorJS
    )
  })
  #render demanded outputs
  output$tb <- renderUI({
    if (is.null(data()))
      h3("Watch me - Tutorial",br(),tags$video(src='Sankey.mp4',type="video/mp4",width="720px",height="450px",controls="controls"),align="center")
    else
      tabsetPanel(
        tabPanel("About file", tableOutput("filedf")),
        tabPanel("Data",tableOutput("table"))
        
      )
  })
  output$codec<-renderText({paste("Code:",input$col)})
  output$sankey <- renderUI({
    if (is.null(data()))
      h1("Check your file!", align='center'
      )
    else
      sankeyNetworkOutput("splot",width = 46*input$y,height = 23*input$y)
  })
  
}

shinyApp(ui = ui, server = server)
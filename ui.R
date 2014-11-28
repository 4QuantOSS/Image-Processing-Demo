source("imgbasics.R")

var.names<-c("Radius","AISO","POS_X","POS_Y","POS_Z","PROJ_PCA1","PROJ_PCA2","PROJ_PCA3","VOLUME")
shinyUI(pageWithSidebar(
  
  headerPanel("MNT Image Processing Demo"),
  
  sidebarPanel(
    
    selectInput('image_name', 'Image Name', ls(img.names)),
    checkboxInput('show_all', 'Show All'),
    conditionalPanel(
      condition = "input.show_all == false",
      checkboxInput('use_histogram', 'As Histogram'),
      checkboxInput('weigh_volume', 'Weigh by Volume')
    )
    

  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Overview",includeMarkdown("overview.md")),
      tabPanel("Filtering", 
               selectInput('filter_name', 'Filter Function', ls(filter.funs),selected="None"),
               sliderInput('filter_size', 'Neighborhood Size', min=1, max=10,value=5,animate=T),
               sliderInput('filter_sigma', 'Sigma Value', min=0, max=10,value=3),
               plotOutput("filterPlot"),
               plotOutput("fhistPlot")
               ),
      tabPanel("Gamma / Contrast Function",  
               selectInput('gamma_name', 'Gamma / Contrast Function', ls(gamma.funs),selected="Linear"),
               plotOutput("gthreshPlot"),
               plotOutput("gammaPlot"), 
               plotOutput("ghistPlot")
               ),
      tabPanel("Threshold", 
               sliderInput('threshold', 'Threshold Value', min=1, max=10,value=5,animate=T),
               plotOutput("threshPlot") ,
               plotOutput("thistPlot"),
               dataTableOutput("samplesummary")
               ),
      tabPanel("Morphology",
               selectInput('morph_name', 'Morphology Function', ls(morph.funs),selected="None"),
               sliderInput('morph_size', 'Neighborhood Size', min=1, max=10,value=3,animate=T),
               plotOutput("morphPlot")
               )
    )
  )
))
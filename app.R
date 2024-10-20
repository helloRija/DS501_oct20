library(shiny)
library(rsconnect)
library(shinythemes)
library(ggvis)
library(cluster)
library(cluster.datasets)  


OpenDef_Data_naRm <- read.csv(text = getURL("https://github.com/helloRija/RijaDSF01/blob/main/OpenDef_Data_naRm.csv")) 

####################################
#   UI   #
####################################
ui <- fluidPage(theme = shinytheme("yeti"),
      navbarPage("Rija Rochefort",
    
########Navigation Bar 1              
             tabPanel("Overview",
             sidebarPanel(
               h4("This Project is created by Rija Rochefort for
                       DS501 – Introduction to Data Science
                       for Professor Narahara Chari Dingari, PhD
                      https://canvas.wpi.edu/courses/63229/assignments/368331
                      Fall 2024")
               
             ),
             mainPanel(
                 
                 h1("Welcome to My App"),
                 
                 h2("Motivations"),
                 p("My main motivations for this was because I had to. I didn't want to lose $5000, countless hours and have my life fall apart.
                    I was surprised to find that I really enjoyed trying to make sense of this data, however the teaching aspect of it seems to be lacking. 
                    This seems like relatively complex coding without fully understanding how to code at this leve and the data Analysis. 
                    Therefore I felt the best way to do this project was to replicate something I had code for"),
                 
                 h2("Data collected"),
                 p("In Week 7 of this class there was a lab on k-means clustering. I looked at the data and felt
                    it would be best to replicate this analysis. I found some really interesting data from Worldbank.org"
                 ), 
                 
                 
                 h2("Analysis"),
                 p("I opted to do a Clustering analysis. I took a subset of data from WorldBank.org. 
                Health Nutritition and Population Statistics. I initially selected all countries. I chose 2019. The data bank
                didn't have the most recent years. Because of Covid in 2020, I felt the data might not but fully reflective of my chosen categories. 
                The cluster analysis focused on people practicing open defecation and deaths related to nutrition and communicable diseases. 
                I loaded a sample data set. I removed any countries that that had na in either of the categories. Open defecation or Deaths.
                If this were a real study I'd have to tighten the data, to include sanitation and or water access. 
                (Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total) )
Remove the labels
Select number of clusters
Create Clusters
Assign Labels
Plot Clusters
Summary"),   
                 
                 h1("Findings"),
                 
                 h4("Header 4")
               
             
    )###Closes SidebarPanel1
    ),
    
########Navigation Bar 2     
    tabPanel("Rija's k-Means App",
             sidebarPanel(
               fileInput("file", "File input:"),
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Default actionButton:"),
               actionButton("action", "Search"),
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Tab 1",
                          h4("Table"),
                          tableOutput("table"),
                          h4("whatis this"),
                          verbatimTextOutput("txtout"),
                          h1("Header 1"),
                          h2("Header 2"),
                          h3("Header 3"),
                          h4("Header 4"),
                 )#closesTabs
               )#closesTabset
               )#closesMainPanel
             
    ),###Closes SidebarPanel2
    
    
########Navigation Bar 3     
 tabPanel("Rija's k-Means App",  
        fluidPage( 
          sidebarLayout(            
            sidebarPanel(              
              selectInput("cluster", label = "Cluster:", 
                         choices = birth.death$Cluster,
                          selected = "All"),
             actionButton("update_plot", "Show Plot")
            ),
            mainPanel(
              plotOutput("plot")
           )
         ))
          
          
)###Closes SidebarPanel3
###############################

) )### Closes Page ### Close UI

####################################
#   Server   #
####################################

server <- function(input, output, session) {
  
  ####  Reactive expression for the plot
  plot_data <- reactive({
    if (input$update_plot) {

      ###data(birth.death.rates.1966)
      
     # OpenDef_Data_naRm <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv") 
      data(OpenDef_Data_naRm)
      print(OpenDef_Data_naRm)
      ###birth.death = birth.death.rates.1966
      birth.death = OpenDef_Data_naRm
      head(birth.death)

      ### Remove the labels
      bd = birth.death[,-1]
      head(bd)

      ### Select number of clusters
      ###Total within cluster sum of squares (center=1)
      wss = kmeans(bd, centers=1)$tot.withinss

      ####Find out SSE for clusters 2 to 15
      for (i in 2:15)
        wss[i] = kmeans(bd, centers=i)$tot.withinss
    }
    ###Plot SSE vs. Number of Clusters
###library(ggvis)
    sse = data.frame(c(1:15), c(wss))
    names(sse)[1] = 'Clusters'
    names(sse)[2] = 'SSE'
    sse %>%
    ggvis(~Clusters, ~SSE) %>%
    layer_points(fill := 'blue') %>%
    layer_lines() %>%
    set_options(height = 300, width = 400)

    ### Create Clusters
    clusters = kmeans(bd, 4)
    clusters

    ### Assign Labels
    birth.death$Cluster = clusters$cluster
    head(birth.death)

    ######Notsureif this is right for assigning valuesRijaRochefort####
    birth.death$Cluster<- factor(birth.death$Cluster, levels = c("1", "2", "3", "4"))

    ### Plot Clusters
    #### customize clusplot
    clusPlotNew <- clusplot(bd, clusters$cluster,
                          color=T, shade=F,
                          labels=0,lines=0, # display cluster labels
                          main="k-Means Cluster Analysis",
                          xlab = "Pop Practicing Open Defecation",
                          ylab = "Deaths by Conditions")
  })


  #### Render the plot
  output$plot <- renderPlot({
    plot_data()
  }) 
  
  
}

####################################
# Create the shiny app             #
####################################
# Run the app
shinyApp(ui = ui, server = server)


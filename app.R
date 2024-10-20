#### Use 'tables' in tabs to help organize the data and add pictures. 
###ThisIsIn_kMeansCluster_RijaShinyOct15###

library(rsconnect)

# ui.R
library(shiny)
ui = navbarPage("Rija's k-Means App",
             tabPanel("Home",
                      fluidPage(
                        h1("Welcome to My App"),
                        p("This Project is created by Rija Rochefort for
                       DS501 – Introduction to Data Science
                       for Professor Narahara Chari Dingari, PhD
                      https://canvas.wpi.edu/courses/63229/assignments/368331
                      Fall 2024"),
                        
                        
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
                        
                        h2("Findings")
                         
                        
                        
                      ) ##Close fluid page
            ),###closes name of panel
             
             ###SecondTab
             tabPanel("Summary", 
                      
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("cluster", label = "Cluster:", 
                                        choices = birth.death$cluster,
                                        selected = "All"),
                            actionButton("update_plot", "Show Plot")
                          ),
                          mainPanel(
                            plotOutput("plot")
                          ))
                      ) ##Close fluid page
             ),###closes name of panel
             
             
             ###ThirdTab
             tabPanel("Summary", 
                      
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
                          ))
                      ) ##Close fluid page
             ))###closes name of panel
             
             ###Plots
             tabPanel("Plots", 
                      
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("cluster", label = "Cluster:", 
                                        choices = birth.death$cluster,
                                        selected = "All"),
                            actionButton("update_plot", "Show Plot")
                          ),
                          mainPanel(
                            plotOutput("plot")
                          ))
                      ) ##Close fluid page
             )###closes name of panel
             
             
### Close Sections
  



#########$# server.R
server = shinyServer(function(input, output) {
  
  
  ####  Reactive expression for the plot
  plot_data <- reactive({
    if (input$update_plot) {
      # Update data (e.g., filter or transform)
      
      ### Load a sample data set
      library(cluster)
      library(cluster.datasets)
      
      ###data(birth.death.rates.1966)
      data(OpenDef_Data_naRm)
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
    library(ggvis)
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
    
    ######Notsureif this is right for assigning values####
    birth.death$Cluster<- factor(birth.death$Cluster, levels = c("1", "2", "3", "4"))
    
    ### Plot Clusters
    #### customize clusplot
    plot_data <- clusplot(bd, clusters$cluster, 
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
  
})

####################################
# Create the shiny app             #
####################################
# Run the app
shinyApp(ui = ui, server = server)
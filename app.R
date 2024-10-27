library(shiny)
library(rsconnect)
library(ggvis)
library(cluster)
library(cluster.datasets)  
library(RCurl)

##testSingle##Rrochefort_kMeans_DS501

OpenDef_Data_naRm <- read.csv("OpenDef_Data_naRm.csv")

###data(birth.death.rates.1966)
##data(OpenDef_Data_naRm)
###birth.death = birth.death.rates.1966
birth.death = OpenDef_Data_naRm
head(birth.death)

### Load a sample data set

### Remove the labels
bd = birth.death[,-1]
head(bd)

### Select number of clusters
###Total within cluster sum of squares (center=1)
wss = kmeans(bd, centers=1)$tot.withinss

####Find out SSE for clusters 2 to 15
for (i in 2:15){
  wss[i] = kmeans(bd, centers=i)$tot.withinss}

###Plot SSE vs. Number of Clusters
sse = data.frame(c(1:15), c(wss))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'

# sse %>%
#   ggvis(~Clusters, ~SSE) %>%
#   layer_points(fill := 'blue') %>% 
#   layer_lines() %>%
#   set_options(height = 300, width = 400)

### Create Clusters
clusters = kmeans(bd, 4)
clusters

### Assign Labels
birth.death$Cluster = clusters$cluster
head(birth.death)

######Notsureif this is right for assigning values####
birth.death$Cluster<- factor(birth.death$Cluster, levels = c("1", "2", "3", "4"))


# ui.R

ui = navbarPage("Rija's k-Means App",
                tabPanel("Home",
                         fluidPage(
                           h1("Cluster Analysis"),
                           h4("People Practicing Open Defecation & Death"),
                           p("This Project is created by Rija Rochefort for
                       DS501 – Introduction to Data Science
                       for Professor Narahara Chari Dingari, PhD
                      https://canvas.wpi.edu/courses/63229/assignments/368331
                      Fall 2024"),
                           
                           
                           h2("Analysis"),
                           p("I opted to do a Clustering analysis. I took a subset of data from WorldBank.org. 
                Health Nutritition and Population Statistics. I initially selected all countries. And 2019 as the initial year. The data bank
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
                           
                           h2("Motivations"),
                           p("My main motivations for this was because I had to. I didn't want to lose $5000, countless hours and have my life fall apart.
                    I was surprised to find that I really enjoyed trying to make sense of this data, however the teaching aspect of it seems to be lacking. 
                    This seems like relatively complex coding without fully understanding how to code at this leve and the data Analysis. 
                    Therefore I felt the best way to do this project was to replicate something I had code for."),
                           
                           h2("Data collected"),
                           p("In Week 7 of this class there was a lab on k-means clustering. I looked at the data and felt
                    it would be best to replicate this analysis. I found some really interesting data from Worldbank.org"
                           ), p("https://github.com/helloRija/DS501_oct20"),
                           
                           
                           h2("Findings"),
                           p("This analysis will shows all 4 of the clusters in one image in one tab. And the subsequent tab,
                           allows the user to filter each of the clusters. It's obvious that low death counts seem to be consistent with 
                           not practicing open defecation. There are outliers and it would be helpful to run studies of my 
                           own with my specific requirments for data acceptance. Nevertheless it is interesting to look at this information 
                           plotted."),
                           
                           br(),
                           p("Source: https://databank.worldbank.org/source/health-nutrition-and-population-statistics#"),
                           p("Dingari, Narahar Chari Phd., 2024. Course Material. Worcester Polytechnic Institute.
                             DS501 - Introduction to Data Science. Week 07")
                           
                           
                         ) ##Close fluid page
                ),###closes name of panel
                
                
                # ###MoreDataTab###
                # tabPanel("More",
                #          fluidPage(
                #            h1("New Inf"),
                #            p("text"),
                #            
                #            h4("Observations"),
                #            tableOutput("plot1"),
                #            
                #            h4("Observations"),
                #            tableOutput("tableClus1b")
                #            
                #          ) ##Close fluid page
                # ),###closes name of panel
                
                
                
                
                ###AboutClustering###
                tabPanel("Clustering",
                         fluidPage(
                           h1("A bit about Clustering"),
                           p("Clustering is an unsupervised algorithm. We use our data analysis to 
                             recognize and group our data into a pattern. It's considered simple and basic. 
                             Perfect for my level of data understanding."),
                           
                           h4("What do you need"),
                           p("Clustering involves specifing the number of clusters ahead of time. Termed
                             'k'. Then you need your data of course. A key component of clustering will
                             be your centroids. These are chosen randomly by your data analysis tool. Finally
                             in any real study, you will try iterations to understand the data"),
                           
                           h4("How does the algorithm work"),
                           p("First your data tool select K centroids from your dataset.Then it looks at each individual
                           data point and assigns it to the closes centroid. Then it re-calculates a new centroid. 
                           How? it takes the mean of all the data points in that cluster. That's alot of math. 
                           Then it just keeps assigning and re-calculating until something called convergence."),
                           
                           h4("What's Convergence"),
                           p("This is when your algorithm stops iterating because the means-centroids aren't
                           changing anymore. So if your algorithm keeps iterating and the centroids stop changing, 
                           Convergence has been achieved. So the algorithm stops. Easy. It calculates the 
                           within-cluster sum of squares (WCSS) between iterations to ascertain convergence. 
                           
                           There are two types of convergence in K-Means:
                           Local: The algorithm converges to a local minimum, it finds a cluster and centroid values that minimize  WCSS, but may not be  global optimum.

                           Global : The algorithm converges to the global optimum, which is often hard to get to. Local convergence is often enough."),
                           
                           br(),
                           p("Source: https://www.youtube.com/watch?v=jZs5rX8Kl3o"),
                           p("Source: Brave: Clustering. Oct 2024"),
                           p("Dingari, Narahar Chari Phd., 2024. Course Material. Worcester Polytechnic Institute.
                             DS501 - Introduction to Data Science. Week 07")
                           
                         ) ##Close fluid page
                ),###closes name of panel
                
                
                ###SecondTab
                tabPanel("Overview", 
                         
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               ##selectInput("cluster2", label = "Cluster:", 
                               ##  choices = birth.death$Cluster,
                               ##  selected = "All")
                               # Include clarifying text ----
                               helpText("This Plot shows all 4 of the clusters in one image. Its very 
                               obvious here to see that low death counts seem to be consistent with 
                               not practicing open defecation. Its' and interesting study. 
                               I can see that there are outliers and it would be helpful to 
                               run studies of my own with my specific requirments for data acceptance. 
                                        ."),
                               actionButton("update_plot2", "Show Plot")
                             ),
                             mainPanel(
                               plotOutput("plot2")
                             ))
                         ) ##Close fluid page
                ),###closes name of panel
                
                
                ###ThirdTab
                tabPanel("Summary", 
                         
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("cluster3", label = "Cluster:", 
                                           choices = birth.death$Cluster,
                                           selected = "All"),
                               actionButton("update_plot3", "Show Plot")
                             ),
                             mainPanel(
                               plotOutput("plot3")
                             ))
                         ) ##Close fluid page
                ))###closes name of panel

###Plots
tabPanel("Plots", 
         
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
)###closes name of panel


### Close Sections




#########$# server.R
server = shinyServer(function(input, output) {
  
  
  ####  Reactive expression for the plot
  plot_data2 <- reactive({
    if (input$update_plot2) {
      
      ### Plot Clusters
      #### customize clusplot
      plot_data <- clusplot(bd, clusters$cluster, 
                            color=T, shade=F,
                            labels=0,lines=0, # display cluster labels
                            main="k-Means Cluster Analysis", 
                            xlab = "Pop Practicing Open Defecation",
                            ylab = "Deaths by Conditions")
    }
  })
  
  plot_data3 <- reactive({
    req(input$update_plot3)
    ### Plot Clusters
    myClusters <-which(clusters$cluster==input$update_plot3)
    
    #### customize clusplot
    plot_data <- clusplot(bd[myClusters,], clusters$cluster[myClusters], 
                          color=T, shade=F,
                          labels=0,lines=0, # display cluster labels
                          main="k-Means Cluster Analysis", 
                          xlab = "Pop Practicing Open Defecation",
                          ylab = "Deaths by Conditions")
  })
  
  
  #### Render the plot
  output$plot2 <- renderPlot({
    plot_data2()
  })
  
  output$plot3 <- renderPlot({
    plot_data3()
  })
  
  # ####RenderTables
  # output$plot1 <- renderTable({
  #  plot_data1
  # })
  
})

####################################
# Create the shiny app             #
####################################
# Run the app
shinyApp(ui = ui, server = server)

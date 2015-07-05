#Richard Shanahan  
#https://github.com/rjshanahan  
#rjshanahan@gmail.com
#4 July 2015

# shiny app for interactive k-Means clustering of continuous Boeing 737 Survivability by seat attributes

# shiny app deployed @ shinyapps.io. Run the app via the following URL
# https://rjshanahan.shinyapps.io/publish01

# this R script is the working code. 
# Note: the raw data is sourced here via GitHub. For the interactive app the CSV is deployed on shinyapps


## load packages

library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(devtools)
library(cluster)
devtools::install_github('rstudio/shinyapps')
library(shinyapps)


############################################################
## build data
############################################################
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%y"))

# SOURCE RAW CSV FILE FROM GITHUB
git_path <- 'https://raw.github.com/rjshanahan/INFS5098_Boeing737_SurvivabilitybySeat/master/'
raw_csv <- "Boeing737_RAW.csv"
missing_types <- c("NA", "")

# source function from github gist to download data from github raw
#https://gist.github.com/rjshanahan/d26099cf1792a388ec55
source_gist("d26099cf1792a388ec55")   

# build Boeing 737 dataframe from site
boeing737_raw <- source_GitHubData_Boeing737(url = paste(git_path, raw_csv, sep=""))
boeing737 <- boeing737_raw

# assign id field for visualisations
boeing737$id <- 1:nrow(boeing737)

###### 3.1 seat window, middle or aisle?
boeing737$seatlocation <- ifelse(grepl('[AF]', boeing737$PASSENGER_SEAT_ID) == T,
                                 "window",
                                 ifelse(grepl('[BE]', boeing737$PASSENGER_SEAT_ID) == T,
                                        "middle",
                                        "aisle"))
###### 3.2 seat column
boeing737 <- mutate(boeing737, column = gsub('\\s|[0-9]', "", PASSENGER_SEAT_ID))

###### 3.3 seat row
boeing737 <- mutate(boeing737, row = gsub('\\s|[A-F]', "", PASSENGER_SEAT_ID))

###### 3.4 seat region - front, mid, aft
boeing737 <- mutate(boeing737, seatregion = substr(PASSENGER_SEAT_REGION, 1, 3))

###### 3.5 immediate neighbouring seat survivability
# attribute to determine if passenger seated immediately next to passenger was a fatality

boeing737 <- mutate(boeing737, neighbour = ifelse(lag(PASSENGER_STATUS) == "fatality" &
                                                    lead(PASSENGER_STATUS) == "fatality",
                                                  "both",
                                                  ifelse(lag(PASSENGER_STATUS) == "fatality" &
                                                           lead(PASSENGER_STATUS) != "fatality",
                                                         "one",
                                                         ifelse(lag(PASSENGER_STATUS) != "fatality" &
                                                                  lead(PASSENGER_STATUS) == "fatality",
                                                                "one",
                                                                "none"))))

#replace NAs at start and end of dataframe
boeing737$neighbour[c(1,868)] = "none"

###### 3.6 binary fatality attribute
boeing737 <- mutate(boeing737, fatality = ifelse(PASSENGER_STATUS == 'fatality', 1, 0))

#subset dataset for desired attributes
boeing737_select <- boeing737 %>% 
  select(id, 
         PASSENGER_STATUS,
         PASSENGER_SEAT_ID,
         PASSENGER_SEAT_REGION,
         PASSENGER_CLASS,
         PASSENGER_EXIT,
         SEAT_EXIT_AVAILABLE,
         SEAT_EXIT_PROXIMITY,
         SEATING_TYPE,
         #PASSENGER_RELATION,
         FATALITY_CAUSE,
         INJURY_CAUSE,
         FUSELAGE_RUPTURE,
         FIRE_PRESENCE,
         CREW_PROXIMITY_OPERATIONAL_COUNT,
         CREW_OPERATIONAL_PER_REGION,
         MODEL,
         #LOCATION,
         SEAT_MODEL,
         SEAT_WIDTH,
         SEAT_PITCH,
         SEAT_PER_CLASS,
         seatlocation,
         column,
         row,
         seatregion,
         neighbour,
         fatality) 

# Subset for continuous variables
boeing737_select_cont <- 
  boeing737 %>% 
  select(
    PASSENGER_SEAT_REGION,
    id,
    SEAT_EXIT_AVAILABLE,
    SEAT_EXIT_PROXIMITY,
    FUSELAGE_RUPTURE,
    FIRE_PRESENCE,
    CREW_PROXIMITY_OPERATIONAL_COUNT,
    CREW_OPERATIONAL_PER_REGION,
    SEAT_WIDTH,
    SEAT_PITCH,
    fatality) 

# scale continuous variables for boxplots
boeing737_select.s.b <- scale(boeing737_select_cont[,2:11])
boeing737_select.s.b <- as.data.frame(boeing737_select.s.b[1:868, 1:10])
# scale continuous variables for clustering
boeing737_select.s.c <- scale(boeing737_select_cont[,3:11])
boeing737_select.s.c <- as.data.frame(boeing737_select.s.c[1:868, 1:9])

# reshape dataset for boxplot representation - standardised
boeing737_select.s.m <- melt(boeing737_select.s.b,
                             id.var="id")


#GitHub URL
rjs <- as.character(tags$html(
  tags$body(
    a("my GitHub repository", href="https://github.com/rjshanahan/INFS5098_Boeing737_SurvivabilitybySeat", target="_blank"))))


############################################################
## shiny user interface function
############################################################

ui <- fluidPage(
  titlePanel('k-Means Clustering of Boeing 737 Survivability by Seat'),
  sidebarPanel(
    sliderInput(inputId="k","How many clusters do you want to find?",value=1,min=2,max=15,step=1),
    helpText("Choose a value of k to find the number of natural groupings or 'clusters'",
             "in the Boeing 737 Survivability by Seat dataset",
             "Refer to: ",rjs),
    actionButton("cluststart","Start clustering now!"),
    width=12), # end sidebarPanel
  mainPanel(
    plotOutput("clusplot"),
    width=12
  )  # end mainPanel
)


#################################################################
## shiny server function
#################################################################

server <- function(input, output) {
  
  rv <- reactiveValues(k=c(2))
  
  observeEvent(input$cluststart,
               {
                 rv$k <- input$k})               
  
  ## call the function to make coordinate plot
  #output$clusplot <- renderPlot({plot_clus_coord(kmeans(boeing737_select.s.c, rv$k), boeing737_select.s.c[,1:8])})} 

#clus plot
#set theme for 'minimal' appearance
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top")

output$clusplot <- renderPlot({
  all_cluster_results = round(as.data.frame(rbind(kmeans(boeing737_select.s.c, rv$k)$centers, 
                                                  colMeans(boeing737_select.s.c[,1:8]))), 2)
  maxs <- apply(all_cluster_results, 2, max)
  mins <- apply(all_cluster_results, 2, min)
  cl_scaled = as.data.frame(scale(all_cluster_results, center = mins, 
                                  scale = maxs - mins))
  cl_scaled$cluster = c(paste("C", 1:max(kmeans(boeing737_select.s.c, rv$k)$cluster), 
                              sep = "_"), "All")
  melted_data = melt(cl_scaled, id.vars = "cluster")
  coor_plot = ggplot(melted_data, aes(x = variable, y = value, 
                                      group = cluster, color = cluster), environment = environment()) + 
    geom_path(alpha = 0.9) + 
    geom_point() + 
    xlab("Boeing 737 Passenger Variables") + 
    ylab("Scaled average") + 
    ggtitle("Boeing 737 Survivability by Seat Cluster Plot") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
          plot.title = element_text(size = 14, face = "bold"))
  plot(coor_plot)
})}


#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)
  

#######################################################
## shinyapps.io deployment instructions
#######################################################
# ensure ui.R, server.R and raw data are deployed in the same folder (with nothing else)
# set working directory to this folder for deployApp() function


# shinyapps::deployApp()
# 
# runApp()
# deployApp()


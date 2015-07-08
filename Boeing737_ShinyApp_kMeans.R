#Richard Shanahan  
#https://github.com/rjshanahan  
#rjshanahan@gmail.com
#4 July 2015

# shiny app for interactive k-Means clustering of continuous Boeing 737 Survivability by seat attributes

# shiny app deployed @ shinyapps.io. Run the app via the following URLs:
# test:     https://rjshanahan.shinyapps.io/publish01
# active:   https://rjshanahan.shinyapps.io/Boeing737_Survivability_by_Seat_Clustering

# this R script is the WORKING CODE ONLY. Actual ui.R and server.R files are hosted on shinyapps.io
# Note: the raw data below is sourced here via GitHub. For the interactive app the CSV is deployed on shinyapps


## load packages

library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(devtools)
library(cluster)
devtools::install_github('rstudio/shinyapps')
library(shinyapps)
library(data.table)


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


###### 3.7 recode seat location
recode_region <- c("FWD-PRT"=1, "FWD-SBD"=2, "MID-PRT"=3, "MID-SBD"=4, "AFT-PRT"=5, "AFT-SBD"=6)

boeing737$passenger_seat_region <- recode_region[boeing737$PASSENGER_SEAT_REGION]

###### 3.8 recode seat exit type
recode_exit <- c("passenger_door_front_prt"=1, "service_door_front_sbd"=2, "wing_emergency_prt"=3,"wing_emergency_sbd"=4,"passenger_door_aft_prt"=5, "service_door_aft_sbd"=6,"passenger_door_aft_sbd"=7 )

boeing737$passenger_exit_type <- recode_exit[boeing737$PASSENGER_EXIT]

###### 3.9 recode passenger class
recode_class <- c("business_class"=1, "economy"=2 )

boeing737$passenger_class <- recode_class[boeing737$PASSENGER_CLASS]

###### 3.10 recode passenger neighbour status
recode_neighbour <- c("none"=1, "one"=2 , "both"=3)

boeing737$neigbour_fatality <- recode_neighbour[boeing737$neighbour]




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
         fatality,
         passenger_seat_region,
         passenger_exit_type,
         passenger_class,
         neigbour_fatality) 

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
    fatality,
    passenger_seat_region,
    passenger_exit_type,
    passenger_class,
    neigbour_fatality) 

# scale continuous variables for boxplots
boeing737_select.s.b <- scale(boeing737_select_cont[,2:15])
boeing737_select.s.b <- as.data.frame(boeing737_select.s.b[1:868, 1:14])
# scale continuous variables for clustering
boeing737_select.s.c <- scale(boeing737_select_cont[,3:15])
boeing737_select.s.c <- as.data.frame(boeing737_select.s.c[1:868, 1:13])

# reshape dataset for boxplot representation - standardised
boeing737_select.s.m <- melt(boeing737_select.s.b,
                             id.var="id")

write.csv(boeing737_select.s.c, file = "Boeing737_Clustering.csv", row.names = FALSE)


c("SEAT_EXIT_AVAILABLE: is a passenger's closest exit available" = "SEAT_EXIT_AVAILABLE",
  "SEAT_EXIT_PROXIMITY: distance passengers must travel to reach closest exit" = "SEAT_EXIT_PROXIMITY",
  "FUSELAGE_RUPTURE: was the fuselage in the passenger's vicinity ruptured during the accident" = "FUSELAGE_RUPTURE",
  "FIRE_PRESENCE: was there fire in the passenger's vicinity" = "FIRE_PRESENCE",
  "CREW_PROXIMITY_OPERATIONAL_COUNT: distance from passenger to a functioning crewmember" = "CREW_PROXIMITY_OPERATIONAL_COUNT",
  "CREW_OPERATIONAL_PER_REGION: count of functioning crew in passenger's vicinity" = "CREW_OPERATIONAL_PER_REGION",
  "SEAT_WIDTH: width of passenger's seat" = "SEAT_WIDTH",
  "SEAT_PITCH: width of passenger's seat" = "SEAT_PITCH",
  "fatality: was the passenger a fatality or survivor (includes injuries)" = "fatality",
  "passenger_seat_region: portion of the cabin the passenger was seated in, starting from front port to aft starboard" = "passenger_seat_region",
  "passenger_exit_type: type of exit closest to passenger" = "passenger_exit_type",
  "passenger_class: business/first class or economy" = "passenger_class",
  "neigbour_fatality: were the passenger's neighbours survivors or fatalities" = "neigbour_fatality")


#GitHub URL
rjs <- as.character(tags$html(
  tags$body(
    a("my GitHub repository", href="https://github.com/rjshanahan/INFS5098_Boeing737_SurvivabilitybySeat", target="_blank"))))

selections <- c("SEAT_EXIT_AVAILABLE",             
                "SEAT_EXIT_PROXIMITY",             
                "FUSELAGE_RUPTURE",                
                "FIRE_PRESENCE",                   
                "CREW_PROXIMITY_OPERATIONAL_COUNT",
                "CREW_OPERATIONAL_PER_REGION",     
                "SEAT_WIDTH",                      
                "SEAT_PITCH",                      
                "fatality",                        
                "passenger_seat_region",           
                "passenger_exit_type",             
                "passenger_class",                 
                "neigbour_fatality")


############################################################
## shiny user interface function
############################################################

ui <- fluidPage(
  titlePanel('k-Means Clustering of Boeing 737 Survivability by Seat'),
  sidebarPanel(
    sliderInput(inputId="k","How many clusters do you want to find?",value=4,min=2,max=15,step=1),
    helpText("Choose a value of k to find the number of natural groupings or 'clusters' in the Boeing 737 Survivability by Seat dataset."),
    helpText("For background information on this dataset and related code please refer to ",rjs),
    actionButton("cluststart","Start clustering now!"),
    width=12), # end sidebarPanel
  mainPanel(
    plotOutput("clusplot"),
    width=12
  ),  # end mainPanel
  fluidRow(column(3,
                  div(checkboxGroupInput(inputId="centre_select", "Select at least two attributes to see their average value per cluster:",
                                     selections,
                                     selected=c("fatality", "passenger_seat_region")            
                                     ),
                      style = "font-size:75%")),
           column(9,
                  div(dataTableOutput("centre_choice"),
                      style = "font-size:75%"))),
  

  helpText("Descriptions of each of the attributes follows:"),
  helpText("SEAT_EXIT_AVAILABLE: is a passenger's closest exit available"),
  helpText("SEAT_EXIT_PROXIMITY: distance passengers must travel to reach closest exit"),  
  helpText("FUSELAGE_RUPTURE: was the fuselage in the passenger's vicinity ruptured during the accident"),
  helpText("FIRE_PRESENCE: was there fire in the passenger's vicinity"),
  helpText("CREW_PROXIMITY_OPERATIONAL_COUNT: distance from passenger to a functioning crewmember"),
  helpText("CREW_OPERATIONAL_PER_REGION: count of functioning crew in passenger's vicinity"),
  helpText("SEAT_WIDTH: width of passenger's seat"),
  helpText("SEAT_PITCH: width of passenger's seat"),
  helpText("fatality: was the passenger a fatality or survivor (includes injuries)"),
  helpText("passenger_seat_region: portion of the cabin the passenger was seated in, starting from front port to aft starboard"),
  helpText("passenger_exit_type: type of exit closest to passenger"),
  helpText("passenger_class: business/first class or economy"),
  helpText("neigbour_fatality: were the passenger's neighbours survivors or fatalities")
  #fluidRow(column(12, dataTableOutput("table_centres")))
  )


#################################################################
## shiny server function
#################################################################

server <- function(input, output) {
  
  rv <- reactiveValues(
              k=c(4))
  
  observeEvent(input$cluststart,
               {rv$k <- input$k})       
  
#clus plot
#set theme for 'minimal' appearance
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top")

output$clusplot <- renderPlot({
  all_cluster_results = round(as.data.frame(rbind(kmeans(boeing737_select.s.c, rv$k)$centers, 
                                                  colMeans(boeing737_select.s.c[,1:13]))), 2)
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
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5, face = 'italic'), 
          plot.title = element_text(size = 20, face = "bold"))
  plot(coor_plot)
})


output$centre_choice <- renderDataTable(cbind(CLUSTER=sapply(seq(1, rv$k, 1), function(x) paste("cluster", x)),
                                              data.frame(round(kmeans(boeing737_select.s.c, rv$k)$centers,2)))[,input$centre_select])

#output$table_centres <- renderDataTable(round(kmeans(boeing737_select.s.c, rv$k)$centers,2))
}


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


#Richard Shanahan  
#https://github.com/rjshanahan  
#rjshanahan@gmail.com
#18 June 2015

##portions of code adapted from the following text:
#"Practical Data Science with R" by Nina Zumel and John Mount, Manning 2014.
#https://github.com/WinVector/zmPDSwR

###### INFS 5098: PART 2_Boeing 737 Survivability by Seat ###### 
# load required packages
library(dplyr)
library(ggplot2)
library(reshape2)
library(devtools)
library(cluster)
library(HSAUR)
library(party)
library(rpart)
library(ROCR)
library(randomForest)
library(e1071)
library(aod)

# source custom code for Boxplot from GitHub Gist
source_gist("e47c35277a36dca7189a")

#clus plot
install_github("pablo14/clusplus")
library(clusplus)

###### 1. IMPORT & INTERROGATE DATA ######
# set date class
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

# interrogate data frame
dim(boeing737)
head(boeing737)
str(boeing737)

# basic summary statistics
summary(boeing737)


###### 2. INITIAL AUDIT + PREPROCESS ######

# check if there are any missing values
colSums(is.na(boeing737)) 

# assign id field for visualisations
boeing737$id <- 1:nrow(boeing737)

###### 3. FEATURE ENGINEERING + SELECTION #####    

###### 3.1 seat window, middle or aisle?
boeing737$seatlocation <- ifelse(grepl('[AF]', boeing737$PASSENGER_SEAT_ID) == T,
                                 "window",
                                 ifelse(grepl('[BE]', boeing737$PASSENGER_SEAT_ID) == T,
                                        "middle",
                                        "aisle"))
#validate
boeing737[1:20,] %>% select(PASSENGER_SEAT_ID, seatlocation)

###### 3.2 seat column
boeing737 <- mutate(boeing737, column = gsub('\\s|[0-9]', "", PASSENGER_SEAT_ID))

#validate
boeing737[1:20,] %>% select(PASSENGER_SEAT_ID, column)

###### 3.3 seat row
boeing737 <- mutate(boeing737, row = gsub('\\s|[A-F]', "", PASSENGER_SEAT_ID))

#validate
boeing737[1:20,] %>% select(PASSENGER_SEAT_ID, row)

###### 3.4 seat region - front, mid, aft
boeing737 <- mutate(boeing737, seatregion = substr(PASSENGER_SEAT_REGION, 1, 3))

#validate
boeing737[1:20,] %>% select(PASSENGER_SEAT_ID, seatregion)

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
which(is.na(boeing737$neighbour))
boeing737$neighbour[c(1,868)] = "none"

#validate
boeing737[1:30,] %>% select(PASSENGER_SEAT_ID, PASSENGER_STATUS, neighbour)

table(boeing737$PASSENGER_STATUS, boeing737$neighbour)

###### 3.6 binary fatality attribute
boeing737 <- mutate(boeing737, fatality = ifelse(PASSENGER_STATUS == 'fatality', 1, 0))

#validate
boeing737[1:20,] %>% select(PASSENGER_SEAT_ID, PASSENGER_STATUS, fatality)

table(boeing737$PASSENGER_STATUS, boeing737$fatality, boeing737$PASSENGER_SEAT_REGION)

#high level stats on survivability by region
region_table <- table(boeing737$PASSENGER_STATUS, boeing737$PASSENGER_SEAT_REGION)
round(prop.table(region_table, 1), 2)

#by class
class_table <- table(boeing737$PASSENGER_STATUS, boeing737$PASSENGER_CLASS)
round(prop.table(class_table, 1), 2)

#by row
row_table <- table(boeing737$PASSENGER_STATUS, boeing737$row)
round(prop.table(row_table, 1), 2)

#by column
col_table <- table(boeing737$PASSENGER_STATUS, boeing737$column)
round(prop.table(col_table, 1), 2)

#by seatlocation
loc_table <- table(boeing737$PASSENGER_STATUS, boeing737$seatlocation)
round(prop.table(loc_table, 1), 2)

#by seatregion
reg_table <- table(boeing737$PASSENGER_STATUS, boeing737$seatregion)
round(prop.table(reg_table, 1), 2)

#by neighbour
neighbour_table <- table(boeing737$PASSENGER_STATUS, boeing737$neighbour)
round(prop.table(neighbour_table, 1), 2)

region_table
neighbour_table
class_table
row_table
col_table
loc_table
reg_table

###### 4. FINAL AUDIT & SUBSET

colSums(is.na(boeing737)) 

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
#filter(PASSENGER_STATUS == 'fatality')

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

# identify classes of variables
discrete <- colnames(boeing737_select)[sapply(boeing737_select[,colnames(boeing737_select)], class) 
                                       %in% c('factor', 'character')]
  
continuous <- colnames(boeing737_select)[sapply(boeing737_select[,colnames(boeing737_select)], class) 
                                         %in% c('numeric', 'integer')]

discrete
continuous

#reclassify attributes for modeling
boeing737_select$PASSENGER_SEAT_REGION <- as.factor(boeing737_select$PASSENGER_SEAT_REGION)
boeing737_select$SEATING_TYPE <- as.factor(boeing737_select$SEATING_TYPE)
boeing737_select$PASSENGER_EXIT <- as.factor(boeing737_select$PASSENGER_EXIT)
boeing737_select$neighbour <- as.factor(boeing737_select$neighbour)
boeing737_select$seatlocation <- as.factor(boeing737_select$seatlocation)
boeing737_select$column <- as.factor(boeing737_select$column)
boeing737_select$FATALITY_CAUSE <- as.factor(boeing737_select$FATALITY_CAUSE)
boeing737_select$INJURY_CAUSE <- as.factor(boeing737_select$INJURY_CAUSE)
boeing737_select$PASSENGER_CLASS <- as.factor(boeing737_select$PASSENGER_CLASS)
boeing737_select$row <- as.factor(boeing737_select$row)
boeing737_select$seatregion <- as.factor(boeing737_select$seatregion)
boeing737_select$MODEL <- as.factor(boeing737_select$MODEL)
boeing737_select$PASSENGER_STATUS <- as.factor(boeing737_select$PASSENGER_STATUS)


###### 4. VISUALISATIONS ######

###### 4.1 Boxplots (new - facet wrap) ######

#rework function downloaded from GitHub Gist
source_GitHubGist_boxplot <- function (reshapedf, var, val, faces, title_main, title_x, title_y)
{
  require(ggplot2)
  
  #set theme for 'minimal' appearance
  theme = theme_set(theme_minimal())
  theme = theme_update(legend.position="top")
  
  ggplot(data = reshapedf, 
         aes(x=var, y=val)) + 
    geom_boxplot(mapping=aes(fill=var, color=var), outlier.colour="indianred1") + 
    xlab(title_x) + 
    ylab(title_y) + 
    ggtitle(title_main) +
    stat_summary(geom = "crossbar", width=0.65, fatten=1, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
    facet_wrap(~faces) }

source_GitHubGist_boxplot(select(boeing737, var=PASSENGER_STATUS, val=SEAT_EXIT_PROXIMITY, faces=PASSENGER_SEAT_REGION), 
                          var,
                          val,
                          faces,
                          "Boeing 737 Distributions: Passenger Survivability by Seat Region & Exit Proximity", 
                          "Passenger Status", 
                          "Exit Proximity_Number of Rows/Seats")

source_GitHubGist_boxplot(select(boeing737, var=PASSENGER_STATUS, val=CREW_PROXIMITY_OPERATIONAL_COUNT, faces=PASSENGER_SEAT_REGION), 
                          var,
                          val,
                          faces,
                          "Boeing 737 Distributions: Passenger Survivability by Seat Region & Operational Crew Proximity", 
                          "Passenger Status", 
                          "Operational Crew Proximity per Region")

source_GitHubGist_boxplot(select(boeing737, var=PASSENGER_STATUS, val=CREW_OPERATIONAL_PER_REGION, faces=PASSENGER_SEAT_REGION), 
                          var,
                          val,
                          faces,
                          "Boeing 737 Distributions: Passenger Survivability by Seat Region & Operational Crew", 
                          "Passenger Status", 
                          "Operational Crew per Region")


source_GitHubGist_boxplot(select(boeing737, var=PASSENGER_STATUS, val=SEAT_PITCH, faces=PASSENGER_SEAT_REGION), 
                          var,
                          val,
                          faces,
                          "Boeing 737 Distributions: Passenger Survivability by Seat Region & Seat Pitch", 
                          "Passenger Status", 
                          "Seat Pitch")


source_GitHubGist_boxplot(select(boeing737_select, var=PASSENGER_STATUS, val=FIRE_PRESENCE, faces=PASSENGER_SEAT_REGION), 
                          var,
                          val,
                          faces,
                          "Boeing 737 Distributions: Passenger Survivability by Seat Region & the Presence of Fire", 
                          "Passenger Status", 
                          "Fire Presence (1=yes, 0=no")


source_GitHubGist_boxplot(select(boeing737_select, var=PASSENGER_STATUS, val=FUSELAGE_RUPTURE, faces=PASSENGER_SEAT_REGION), 
                          var,
                          val,
                          faces,
                          "Boeing 737 Distributions: Passenger Survivability by Seat Region & Fuselage Rupture", 
                          "Passenger Status", 
                          "Fuselage Rupture (1=yes, 0=no")


###### 4.2 Bar Charts ######

# order dataframe for bar chart purposes
boeing737 <- boeing737[order(boeing737$PASSENGER_STATUS, boeing737$PASSENGER_CLASS),]

ggplot(data = boeing737, 
       aes(x=PASSENGER_STATUS, y=FUSELAGE_RUPTURE, fill=PASSENGER_CLASS)) + 
  geom_bar(stat="identity") +
  #geom_boxplot(mapping=aes(fill=PASSENGER_STATUS), outlier.colour="indianred1") + 
  #guides(fill=FALSE) +
  #xlab(title_x) + 
  #ylab(title_y) + 
  ggtitle("Boeing 737: Passenger Survivability with Fuselage Rupture by Class") 


ggplot(data = boeing737, 
       aes(x=PASSENGER_STATUS, y=FIRE_PRESENCE, fill=PASSENGER_CLASS)) + 
  geom_bar(stat="identity") +
  ggtitle("Boeing 737: Passenger Survivability with Fire Presence by Class") 

###### 4.3 Panel Histograms ######
ggplot(data = boeing737, 
       aes(x=PASSENGER_STATUS, 
           #fill=PASSENGER_CLASS
           fill=factor(FIRE_PRESENCE))) +
  geom_bar(stat="bin") +
  facet_wrap(~PASSENGER_SEAT_REGION) +
  ggtitle("Boeing 737: Passenger Survivability by Seat Region & Fire Presence")

ggplot(data = boeing737, 
       aes(x=PASSENGER_STATUS, 
           #fill=PASSENGER_CLASS
           fill=factor(FUSELAGE_RUPTURE))) +
  geom_bar(stat="bin") +
  facet_wrap(~PASSENGER_SEAT_REGION) +
  ggtitle("Boeing 737: Passenger Survivability by Seat Region & Fuselage Rupture")

ggplot(data = boeing737, 
       aes(x=PASSENGER_STATUS, 
           fill=factor(PASSENGER_CLASS))) +
  geom_bar(stat="bin") +
  facet_wrap(~PASSENGER_SEAT_REGION) +
  ggtitle("Boeing 737: Passenger Survivability by Seat Region & Exit Availability")

ggplot(data = boeing737, 
       aes(x=PASSENGER_STATUS, 
           fill=neighbour)) +
  geom_bar(stat="bin") +
  facet_wrap(~PASSENGER_SEAT_REGION) +
  ggtitle("Boeing 737: Passenger Survivability by Seat Region & Neighbour Status")

###### 4.4 Scatterplots ######
# scatterplot
  ggplot(data = filter(boeing737, PASSENGER_STATUS == 'fatality'), 
  #ggplot(data = boeing737, 
         aes(x=SEAT_EXIT_PROXIMITY,
             y=CREW_PROXIMITY_OPERATIONAL_COUNT)) +
             #fill=PASSENGER_CLASS)) +
  geom_point(aes(colour = factor(FUSELAGE_RUPTURE),
                 shape = factor(FIRE_PRESENCE)), 
             size = 5) +
  ggtitle("Boeing 737: Fatality Scatterplot of Exit & Crew Proximity by Fuselage Rupture & Fire Presence") +
  facet_wrap(~PASSENGER_SEAT_REGION) 

# correlation scatterplot
pairs(boeing737_select.s.b[2:10],
      main="Scatterplot of Boeing 737 Continuous Attributes",
      pch = 10,
      col="goldenrod")

#correlation matrix for continuous attributes
boeing737_select.s.b.corr <- round(cor(boeing737_select.s.b, 
                            use = "complete.obs",
                            y=NULL,
                            method = "pearson"), 2)

###### 5 UNSUPERVISED METHODS ######

###### 5.1 Principal Component Analysis

boeing737.pca <- prcomp(boeing737_select.s.b)

#PCA details
summary(boeing737.pca)
boeing737.pca$rotation
boeing737.pca$center
boeing737.pca$sdev

#check sum of variance
sum((boeing737.pca$sdev)^2)

#assess eigenvalues
round((boeing737.pca$sdev)^2, 2)

#the first 5 eigenvalues account for ~80% of variance

#generate screeplot to show how many components to retain
lty.o <- par("lty") 
par(lty = 2) 
screeplot(boeing737.pca, type="lines",pch=19,col='red3')

#assess variable loadings against the first four factors
round(boeing737.pca$rotation[,1:5], 3)

#check scores of variables loaded against factors
#round(boeing737.pca$x[,1:5], 3)

###### 5.2 Clustering

boeing737_select.s.clus <- kmeans(boeing737_select.s.c, 5)

## call the function to make coordinate plot
plot_clus_coord(boeing737_select.s.clus, boeing737_select.s.c[,1:10]) 

# inspect cluster info
str(boeing737_select.s.clus)
boeing737_select.s.clus$centers
boeing737_select.s.clus$size

# silhouette plot - using HSAUR
dissim <- daisy(boeing737_select.s.c) 
dE2   <- dissim^2
sil   <- silhouette(boeing737_select.s.clus$cluster, dE2)
plot(sil, col = boeing737_select.s.clus$cluster)
dev.off()

# flattened cluster representation
op <- par(new=TRUE, cex = 0.7)
clusplot(boeing737_select.s.c, 
         boeing737_select.s.clus$cluster, 
         col.p=c('seagreen3','yellow','brown'),
         col.clus='seagreen3',
         #col.txt='blue', 
         labels=3,
         #color=TRUE, 
         #shade=TRUE, 
         #labels=0, 
         lines=0)
par(op)
dev.off()



###### 6. MODELLING ######
##### 6.1 Split dataframe into TRAIN and TEST and CALIBRATE#####

#NOTE: comment this code once run initially - if rerun it will recreate slightly different sized and 
#randomised samples - ie, differing results for predictive models

#assign random value to each ob based on uniform distribution - for reproducibility this is commented
boeing737_select$sample <- runif(nrow(boeing737_select))

boeing_train <- filter(boeing737_select, sample > 0.3)
boeing_test <- filter(boeing737_select, sample <= 0.3)

dim(boeing_train)
dim(boeing_test)
####
# ALTERNATIVE METHOD 
# # #create TEST, TRAIN and CALIBRATION dataframes
# boeing737_select$groupid <- paste(boeing737_select$PASSENGER_SEAT_REGION,".",boeing737_select$LOCATION, sep="")
# # 
# gid <- unique(boeing737_select$groupid)
# gid_region <- data.frame(groupid = gid, sample_group = runif(length(gid)))
# boeing737_select <- merge(boeing737_select, gid_region, by="groupid")
# # 
# # #create TEST and TRAIN data frame based on grouping sample
# boeing_train <- filter(boeing737_select, sample_group > 0.3)
# boeing_test <- filter(boeing737_select, sample_group <= 0.3)
# ##boeing_test <- filter(boeing737_select, sample_group <= 0.3 & sample_group > 0.1)
# ##boeing_calib <- filter(boeing737_select, sample_group <= 0.1)
# 
# #drop dependent variable from test dataframe
# ##boeing_test <- select(boeing_test, -fatality)
# 
# # #check dimensions
# dim(boeing_train)
# dim(boeing_test)
# #dim(boeing_calib)


#define all factor levels
factor_levels <- sapply(boeing737_select, 
                        function(x) if(class(x) == "factor"){
                          levels(x) } else {
                            "not a factor"
                          })

#apply levels to TRAIN dataframe after splitting
apply(boeing_train,
      2,
      function(x) if(class(x) == "factor")
                     {levels(x) = factor_levels$x})

#apply levels to TEST dataframe after splitting
apply(boeing_test,
      2,
      function(x) if(class(x) == "factor")
                    {levels(x) = factor_levels$x})

#apply levels to CALIBRATION dataframe after splitting
# apply(boeing_calib,
#       2,
#       function(x) if(class(x) == "factor")
#       {levels(x) = factor_levels$x})

# set themes for diagnostic visualisations
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top")


############ 6.2 LOGISTIC REGRESSION model and generate output ###############

# create variable vectors for inclusion in model
var_exist <- c(
  #'PASSENGER_SEAT_ID',        
  'PASSENGER_SEAT_REGION',           
  'PASSENGER_CLASS',           
  'PASSENGER_EXIT',
  'SEAT_EXIT_AVAILABLE',
  'SEATING_TYPE',         
  'SEAT_EXIT_PROXIMITY',          
  #'FATALITY_CAUSE',
  #'INJURY_CAUSE',
  #'MODEL',
  'FUSELAGE_RUPTURE',
  'FIRE_PRESENCE',
  'CREW_PROXIMITY_OPERATIONAL_COUNT',
  'CREW_OPERATIONAL_PER_REGION',
  'SEAT_WIDTH',
  'SEAT_PITCH')

var_new <- c(
  'seatlocation',   
  #'column',
  #'row',       
  #'seatregion',          
  'neighbour')  

var_independent <- c(var_new, var_exist)

var_dependent <- 'fatality'

# define logistic regression formula
logit_formula <- paste(var_dependent, paste(var_independent, collapse='+'), sep = '~')

# run model against training dataset to determine coefficients
model1 <- glm(logit_formula, 
             data=boeing_train, 
             family=binomial(link='logit'))

# view coefficients
as.integer(coefficients(model1))

# assess significance of coefficients
summary(model1)

#chi-square statistic
sum(residuals(model1, type = "pearson")^2)

#confidence intervals
confint(model1)

#wald statistic
wald.test(Sigma = vcov(model1), b = coef(model1), Terms = 3:25)

# run prediction using model
boeing_train$pred <- predict(model1,
                              newdata=boeing_train,
                              type="response")

# compare predictions against actual in TRAIN
ggplot(boeing_train, 
       aes(x=pred, 
           color=fatality, 
           linetype=factor(fatality))) +
  geom_density() +
  ggtitle("Boeing 737 Training Dataset_LOGIT Predictions") 


# generate predictions for TEST dataset
boeing_test$pred <- predict(model1,
                             newdata=boeing_test,
                             type="response")

# convert probabilities to 1 for survived or 0 for perished
Prediction <- ifelse(boeing_test$pred < 0.5,
                     0,
                     1)

submit1 <- data.frame(SeatID = boeing_test$PASSENGER_SEAT_ID, 
                     Fatality = boeing_test$PASSENGER_STATUS,
                     Prediction = as.vector(Prediction))

write.csv(submit1, file = "boeing737_logit.csv", row.names = FALSE)

#ANOVA for seatlocation
seatsurv <- aov(fatality ~ seatlocation, data=boeing_train)

summary(seatsurv)
seatsurv$coefficients
print(model.tables(seatsurv,"means"),digits=3) 

############ 6.3 DECISION TREE with 'rpart' package ###############

fit <- rpart(fatality ~ SEAT_EXIT_AVAILABLE + SEAT_EXIT_PROXIMITY + FUSELAGE_RUPTURE + FIRE_PRESENCE + CREW_PROXIMITY_OPERATIONAL_COUNT + CREW_OPERATIONAL_PER_REGION + SEAT_WIDTH + SEAT_PITCH,
                    data=boeing_train)

# model execution
Prediction <- predict(fit, boeing_test)                     

# generate predictions for TEST dataset
boeing_test$pred <- predict(fit, boeing_test) 

# convert probabilities to 1 for survived or 0 for perished
Prediction <- ifelse(boeing_test$pred < 0.5,
                     0,
                     1)

# create dataframe and submission file for Kaggle
submit0 <- data.frame(SeatID = boeing_test$PASSENGER_SEAT_ID, 
                      Fatality = boeing_test$PASSENGER_STATUS,
                      Prediction = as.vector(Prediction))

write.csv(submit0, file = "boeing737_rpart.csv", row.names = FALSE)

# #AUC
# pos <- 1
# print(calcAUC(predict(fit, boeing_train, type='vector'),
#               boeing_test[, 'fatality']))
# 
# performance(prediction(fit, boeing_test), "tpr","fpr")

############ 6.4 ENSEMBLE DECISION TREE with 'randomForest' package ###############

#maintain this for reproducibility
set.seed(499)

#train model
fit <- randomForest(fatality ~ PASSENGER_SEAT_REGION +  SEAT_EXIT_PROXIMITY + FIRE_PRESENCE + CREW_PROXIMITY_OPERATIONAL_COUNT + neighbour +  CREW_OPERATIONAL_PER_REGION +  FUSELAGE_RUPTURE,
                    data=boeing_train, 
                    importance=TRUE, 
                    ntree=3000,
                    type='classification')

# # full formula
# fatality ~ PASSENGER_SEAT_REGION + PASSENGER_EXIT + SEAT_EXIT_AVAILABLE + SEATING_TYPE + SEAT_EXIT_PROXIMITY + FUSELAGE_RUPTURE + FIRE_PRESENCE + CREW_PROXIMITY_OPERATIONAL_COUNT + CREW_OPERATIONAL_PER_REGION + SEAT_WIDTH + SEAT_PITCH + neighbour + seatlocation + column + row  + PASSENGER_CLASS,

#plot results to determine variable importance - default from package
varImpPlot(fit)

#code to generate prettier variable importance plot
varImpdf <- data.frame(var = factor(row.names(importance(fit))),
                       imp = importance(fit)[,2],
                       row.names=NULL)

#ACCURACY: how worse the model performs without each variable, so a high decrease in accuracy would be expected for very predictive variables
#GINI: measures how pure the nodes are at the end of the tree. Again it tests to see the result if each variable is taken out and a high score means the variable was important.

ggplot(data = varImpdf,
       aes(x=reorder(varImpdf$var, -varImpdf$imp),
           #x=var,
           y=imp,
           fill=var)) +
  #scale_x_discrete(limits = varorder) +
  xlab("Boeing 737 Attribute - note: lowercase vars are produced through feature selection/engineering") + 
  ylab("Variable Importance") + 
  geom_bar(stat="identity") +
  ggtitle("Boeing 737: Variable Importance Ratings using 'randomForest'")

#specify levels for 'row' as differs between test and train
levels(boeing_test$row) <- unique(boeing737_select$row)

# model execution
Prediction <- predict(fit, boeing_test)                     

# generate predictions for TEST dataset
boeing_test$pred <- predict(fit, boeing_test) 

# convert probabilities to 1 for survived or 0 for perished
Prediction <- ifelse(boeing_test$pred < 0.5,
                     0,
                     1)

# create dataframe and submission file for Kaggle
submit2 <- data.frame(SeatID = boeing_test$PASSENGER_SEAT_ID, 
                     Fatality = boeing_test$PASSENGER_STATUS,
                     Prediction = as.vector(Prediction))

write.csv(submit2, file = "boeing737_randomforest.csv", row.names = FALSE)

## produce variable importance plot for ALL variables
fit <- randomForest(fatality ~ PASSENGER_SEAT_REGION + PASSENGER_EXIT + SEAT_EXIT_AVAILABLE + SEATING_TYPE + SEAT_EXIT_PROXIMITY + FUSELAGE_RUPTURE + FIRE_PRESENCE + CREW_PROXIMITY_OPERATIONAL_COUNT + CREW_OPERATIONAL_PER_REGION + SEAT_WIDTH + SEAT_PITCH + neighbour + seatlocation + column + row  + PASSENGER_CLASS,
                    data=boeing_train, 
                    importance=TRUE, 
                    ntree=1000,
                    type='classification')

#plot results to determine variable importance - default from package
varImpPlot(fit)

#code to generate prettier variable importance plot
varImpdf <- data.frame(var = factor(row.names(importance(fit))),
                       imp = importance(fit)[,2],
                       row.names=NULL)

ggplot(data = varImpdf,
       aes(x=reorder(varImpdf$var, -varImpdf$imp),
           #x=var,
           y=imp,
           fill=var)) +
  #scale_x_discrete(limits = varorder) +
  xlab("Boeing 737 Attribute - note: lowercase vars are produced through feature selection/engineering") + 
  ylab("Variable Importance") + 
  geom_bar(stat="identity") +
  ggtitle("Boeing 737: Variable Importance Ratings using 'randomForest'")

############ 6.5 CONDITIONAL INFERENCE DECISION TREE with 'cTree' package ###############


fit <- ctree(fatality ~ SEAT_EXIT_AVAILABLE + SEAT_EXIT_PROXIMITY + FUSELAGE_RUPTURE + FIRE_PRESENCE + CREW_PROXIMITY_OPERATIONAL_COUNT + CREW_OPERATIONAL_PER_REGION + SEAT_WIDTH + SEAT_PITCH + row, 
             data=boeing_train,
             controls=cforest_unbiased(ntree=4000, mtry=2))

plot(fit)

# run model over test data
#boeing_test <- select(boeing_test, -pred)
boeing_test$pred <- predict(fit, boeing_test, OOB=T, type="response")     #for ctree

# convert probabilities to 1 for survived or 0 for perished
Prediction <- ifelse(boeing_test$pred < 0.5,
                     0,
                     1)

# create dataframe and submission file
submit3 <- data.frame(SeatID = boeing_test$PASSENGER_SEAT_ID, 
                     Fatality = boeing_test$PASSENGER_STATUS,
                     Prediction = as.vector(Prediction))

write.csv(submit3, file = "boeing737_ctree.csv", row.names = FALSE)
# 

############ 6.6 NAIVE BAYES with 'e1071' package ###############

fit <- naiveBayes(fatality ~ PASSENGER_SEAT_REGION + PASSENGER_EXIT  + SEATING_TYPE  + FUSELAGE_RUPTURE + FIRE_PRESENCE + CREW_PROXIMITY_OPERATIONAL_COUNT + CREW_OPERATIONAL_PER_REGION + SEAT_WIDTH + SEAT_PITCH + neighbour + row  + PASSENGER_CLASS,
                  data=boeing_train,
                  laplace = 0)

# model execution
# generate predictions for TEST dataset
boeing_test$pred <- predict(fit, boeing_test, type='raw')[,2]

# convert probabilities to 1 for survived or 0 for perished
Prediction <- ifelse(boeing_test$pred < 0.5,
                     0,
                     1)

# create dataframe and submission file
submit4 <- data.frame(SeatID = boeing_test$PASSENGER_SEAT_ID, 
                      Fatality = boeing_test$PASSENGER_STATUS,
                      Prediction = as.vector(Prediction))

write.csv(submit4, file = "boeing737_naivebayes.csv", row.names = FALSE)


#check assumption of indepdence between attributes - chi square test
apply(boeing737_select,
       2,
       function(x) {chisq.test(table(boeing737_select$fatality, x))})
       
############ 6.7 NEURAL NETWORK with 'neuralnet' package ###############

library(neuralnet)
#note: "select" function from the coupled MASS package masks "select" from dplyr

## build the neural network (NN)
boeing_net <- neuralnet(fatality ~ SEAT_EXIT_AVAILABLE  + FUSELAGE_RUPTURE + FIRE_PRESENCE + SEAT_EXIT_PROXIMITY + CREW_PROXIMITY_OPERATIONAL_COUNT + CREW_OPERATIONAL_PER_REGION + SEAT_WIDTH, 
                       boeing_train, 
                       hidden = 4, 
                       lifesign = "minimal", 
                       linear.output = FALSE, 
                       threshold = 0.1)


#visualise
plot(boeing_net, rep = "best")

#run model on test
temp_test <- subset(boeing_test, select = c('SEAT_EXIT_AVAILABLE','FUSELAGE_RUPTURE','FIRE_PRESENCE','SEAT_EXIT_PROXIMITY','CREW_PROXIMITY_OPERATIONAL_COUNT','CREW_OPERATIONAL_PER_REGION','SEAT_WIDTH'))

#compute results
boeingnet.results <- compute(boeing_net, temp_test)

results <- data.frame(actual = boeing_test$fatality, prediction = boeingnet.results$net.result)
results$prediction <- round(results$prediction)

       
# create dataframe and submission file
submit5 <- data.frame(SeatID = boeing_test$PASSENGER_SEAT_ID, 
                      Fatality = boeing_test$PASSENGER_STATUS,
                      Prediction = as.vector(results$prediction))

write.csv(submit5, file = "boeing737_neuralnetwork.csv", row.names = FALSE)


#NOTE: unload these namespaces and packages if you want to reun scripy
#the issue is the 'select' function from MASS masks the dplyr version which is used 
#for creating the subset dataframes and visuals
unloadNamespace("package:neuralnet")
unloadNamespace("package:ggplot2")
unloadNamespace("package:MASS")
detach("package:neuralnet", unload=TRUE)
detach("package:clusplus", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
detach("package:MASS", unload=TRUE)


############ 6.8 COMPARE PREDICTIONS ###############

tbl_logit <- addmargins(table(submit1$Fatality,submit1$Prediction), FUN = list(Total = sum), quiet = TRUE)
tbl_rpart <- addmargins(table(submit0$Fatality,submit0$Prediction), FUN = list(Total = sum), quiet = TRUE)
tbl_rforest <- addmargins(table(submit2$Fatality,submit2$Prediction), FUN = list(Total = sum), quiet = TRUE)
tbl_ctree <- addmargins(table(submit3$Fatality,submit3$Prediction), FUN = list(Total = sum), quiet = TRUE)
tbl_naivebayes <- addmargins(table(submit4$Fatality,submit4$Prediction), FUN = list(Total = sum), quiet = TRUE)
tbl_neural <- addmargins(table(submit5$Fatality,submit5$Prediction), FUN = list(Total = sum), quiet = TRUE)

tbl_logit
tbl_rpart
tbl_rforest
tbl_ctree
tbl_naivebayes
tbl_neural

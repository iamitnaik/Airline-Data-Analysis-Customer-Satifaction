#Final Code


#Getting Data
dataset <- read.csv("SatisfactionSurvey.csv")

#Cleaning Data
cleanedDataset <- dataset
View(cleanedDataset)
which(is.na(dataset$Departure.Delay.in.Minutes))

#The Case of replacing with Highest number
cleanedDataset$Departure.Delay.in.Minutes <- ifelse(is.na(cleanedDataset$Departure.Delay.in.Minutes) & cleanedDataset$Flight.cancelled == "Yes", 9999, cleanedDataset$Departure.Delay.in.Minutes)
cleanedDataset$Arrival.Delay.in.Minutes <- ifelse(is.na(cleanedDataset$Arrival.Delay.in.Minutes) & cleanedDataset$Flight.cancelled == "Yes", 9999, cleanedDataset$Arrival.Delay.in.Minutes)
cleanedDataset$Flight.time.in.minutes <- ifelse(is.na(cleanedDataset$Flight.time.in.minutes) & cleanedDataset$Flight.cancelled == "Yes", 9999, cleanedDataset$Flight.time.in.minutes)
cleanedDataset <- na.omit(cleanedDataset)
summary(cleanedDataset)
str(dataset)
str(cleanedDataset)
which(is.na(cleanedDataset$Departure.Delay.in.Minutes))

cleanedDataset$Satisfaction <- as.numeric(as.character(cleanedDataset$Satisfaction))

# After we use this function, they change to NA data. We need delete these 3 rows.
cleanedDataset<- na.omit(cleanedDataset)
str(cleanedDataset$Satisfaction)

################################### NPS ###############################
################# Using NPS to find which Airlines to use #############

install.packages("NPS")
library("NPS")
vector1 <- as.numeric(as.character( cleanedDataset$Satisfaction))
summary(vector1)
ss <-npc(vector1, breaks = list(1:2.5, 3, 3.5:5)) #Based on our customer satisfaction
ss # We get 3 levels: Detractor Passive Promoter
sum(as.numeric(as.character(ss)))#calculate the total number of rows 
table(ss)
#Detractor   Passive  Promoter 
#    26533     36888     53608
#Now find the Airline to chose from a Detractor and a Promoter
PromoterAirlines <- cleanedDataset[which(ss=="Promoter"),] #creating a dataset with highest customer satisfaction
View(PromoterAirlines)
DetractorAirlines <- cleanedDataset[which(ss=="Detractor"),]#creating a dataset with highest customer satisfaction
summary(PromoterAirlines$Airline.Name)
# Cheapseats Airlines Inc.           Cool&Young Airlines Inc.          EnjoyFlying Air Services 
#                   10528                                581                               3577 
#     FlyFast Airways Inc.                   FlyHere Airways              FlyToSun Airlines Inc.  
#                    6262                               1051                               1489 
# GoingNorth Airlines Inc.   Northwest Business Airlines Inc.             OnlyJets Airlines Inc.  
#                     586                               5710                               2142 
#     Oursin Airlines Inc.           Paul Smith Airlines Inc.                Sigma Airlines Inc.  
#                    4551                               5187                               7118 
#   Southeast Airlines Co.                  West Airways Inc.  
#                    4039                                787 
#Thus we select Cheapseats Airline
summary(DetractorAirlines$Airline.Name)
# Cheapseats Airlines Inc.          Cool&Young Airlines Inc.           EnjoyFlying Air Services 
#                     5602                               222                               1884 
#     FlyFast Airways Inc.                   FlyHere Airways              FlyToSun Airlines Inc.  
#                     3327                               512                                608 
# GoingNorth Airlines Inc.   Northwest Business Airlines Inc.             OnlyJets Airlines Inc.  
#                     396                               2731                               1183 
#     Oursin Airlines Inc.           Paul Smith Airlines Inc.                Sigma Airlines Inc.  
#                     2241                              2369                               3334 
#   Southeast Airlines Co.                  West Airways Inc.  
#                     1852                               272 
# Thus we select Cheapseats Airlines


################Using wordcount to find the airline which has low satisfation########
install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)
createWordCounts<- function(vFtext)
{
  words.vec <- VectorSource(vFtext) #create a Corpus, a "Bag of Words"
  words.corpus <- Corpus(words.vec)
  words.corpus
  words.corpus <- tm_map(words.corpus,content_transformer(tolower))
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, removeNumbers)
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
  words.corpus <- tm_map(words.corpus, removeWords, c("airlines","inc"))
  tdm<- TermDocumentMatrix(words.corpus)
  tdm
  m<- as.matrix(tdm)# create a matrix
  wordCounts <- rowSums(m)
  wordCounts<- sort(wordCounts, decreasing=TRUE)
  return(wordCounts) 
}
wordCounts<- createWordCounts(cleanedDataset$Airline.Name)
View(wordCounts)

genWordCloud <- function(wordCounts)
{
  cloudFrame <- data.frame( word= names(wordCounts), frequency = wordCounts)
  wordcloud(names(wordCounts),wordCounts, min.freq = 2, max.words=30, rot.per=0.35,
            colors= brewer.pal(8,"Dark2"))
  
}
genWordCloud(wordCounts)
happyCust <- cleanedDataset[cleanedDataset$Satisfaction>3,]
View(happyCust)
unhappyCust <-cleanedDataset[cleanedDataset$Satisfaction<=3,]
View(unhappyCust)
wordCounts1<- createWordCounts(happyCust$Airline.Name) 
wordCounts1
genWordCloud(wordCounts1)
wordCounts2<- createWordCounts(unhappyCust$Airline.Name) 
wordCounts2
genWordCloud(wordCounts2)

############################# Creating Buckets For Complete Dataset #####################
#Sattisfaction Grouping: High >3 , Average = 3, Low < 3
createBucketsSurvey <- function(vec) {
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 3] <- "High"
  vBuckets[vec <= 2] <- "Low"
  return(vBuckets)
}
#arbitary selection of quantile to be 40% and 60%
createBuckets <- function(vec) {
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

Bucketing <- function(a){
  Satisfaction <- createBucketsSurvey(a$Satisfaction)
  head(Satisfaction) 
  Airline.Status <- a$Airline.Status
  Age <- createBuckets(a$Age)
  head(Age)
  Gender <- a$Gender
  Price.Sensitivity <- createBucketsSurvey(a$Price.Sensitivity)
  head(Price.Sensitivity)
  Year.of.First.Flight <- createBuckets(a$Year.of.First.Flight)
  head(Year.of.First.Flight)
  No.of.Flights.p.a <- createBuckets(a$No.of.Flights.p.a.)
  X..of.Flight.with.other.Airlines <- createBuckets(a$X..of.Flight.with.other.Airlines)
  Type.of.Travel <- a$Type.of.Travel
  No..of.other.Loyalty.Cards <- createBuckets(a$No..of.other.Loyalty.Cards)
  Shopping.Amount.at.Airport <- createBuckets(a$Shopping.Amount.at.Airport)
  Eating.and.Drinking.at.Airport <- createBuckets(a$Eating.and.Drinking.at.Airport)
  Class <- a$Class
  Day.of.Month <- createBuckets(a$Day.of.Month)
  # Flight.date, don't know how to examine
  Flight.date <- a$Flight.date
  Airline.Code <- a$Airline.Code
  head(Airline.Code)
  Airline.Name <- a$Airline.Name
  # Orgin.City, Origin.State, Destination.City, Destination.State don't know
  Origin.City <- a$Orgin.City
  Origin.State <- a$Origin.State
  Destination.State <- a$Destination.State
  Destination.City <- a$Destination.City
  
  Scheduled.Departure.Hour <- createBuckets(a$Scheduled.Departure.Hour)
  head(Scheduled.Departure.Hour)
  Departure.Delay.in.Minutes <- createBuckets(a$Departure.Delay.in.Minutes)
  head(Departure.Delay.in.Minutes)
  Arrival.Delay.in.Minutes <- createBuckets(a$Arrival.Delay.in.Minutes)
  head(Arrival.Delay.in.Minutes)
  Flight.cancelled <- a$Flight.cancelled
  Flight.time.in.minutes <- createBuckets(a$Flight.time.in.minutes)
  head(Flight.time.in.minutes)
  Flight.Distance <- createBuckets(a$Flight.Distance)
  head(Flight.Distance)
  Arrival.Delay.greater.5.Mins <- a$Arrival.Delay.greater.5.Mins
  
  df <- data.frame(Satisfaction, Airline.Status, Age, Gender, Price.Sensitivity, Year.of.First.Flight,
                   No.of.Flights.p.a, X..of.Flight.with.other.Airlines, Type.of.Travel, No..of.other.Loyalty.Cards,
                   Shopping.Amount.at.Airport, Eating.and.Drinking.at.Airport, Class, Flight.date, Day.of.Month,
                   Airline.Code, Airline.Name, Scheduled.Departure.Hour, Departure.Delay.in.Minutes, Arrival.Delay.in.Minutes,
                   Flight.cancelled, Flight.time.in.minutes, Flight.Distance, Arrival.Delay.greater.5.Mins,Origin.City,Origin.State,Destination.City,Destination.State)
  #View(df)
  return(df)
}



####################### Creating Subset Dataset ###################
install.packages("gdata")
library(gdata)


#Removing whitespace
cleanedDataset$Airline.Name <- trim(cleanedDataset$Airline.Name)
CheapseatsAirlineDF <- cleanedDataset[which(cleanedDataset$Airline.Name=="Cheapseats Airlines Inc."),]
View(CheapseatsAirlineDF)
CheapseatsAirlineDF1<-CheapseatsAirlineDF
dfAir1<-CheapseatsAirlineDF1
str(CheapseatsAirlineDF)
summary(CheapseatsAirlineDF)

####CheapseatsAirlineDF

#Verifying the number of rows to be 25,985
table(cleanedDataset$Airline.Name)

CheapseatsAirlineDF <- Bucketing(CheapseatsAirlineDF)
View(CheapseatsAirlineDF)


FullDataset <- Bucketing(cleanedDataset)
View(FullDataset)

CheapSeatsdataset <- Bucketing(CheapseatsAirlineDF)
View(CheapseatsAirlineDF)


################################## Linear Model For complete Data Set #############################
#Find significant Columns
linearModel1 <- lm(formula = as.numeric(Satisfaction) ~ . , data = FullDataset)
linearModel2 <- lm(formula = as.numeric(Satisfaction) ~ Airline.Status + Age + Gender + Price.Sensitivity + Year.of.First.Flight + No.of.Flights.p.a + Type.of.Travel + Shopping.Amount.at.Airport +Eating.and.Drinking.at.Airport + Class + Scheduled.Departure.Hour + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Flight.cancelled + Flight.time.in.minutes + Flight.Distance + Arrival.Delay.greater.5.Mins, data = FullDataset)
summary(linearModel1)
linearModel1
summary(linearModel2)
linearModel2

################################## Linear Model For Cheapseat Airline Data Set #############################
#Find significant Columns
#Removing 2 factor data
cheapseats<- CheapseatsAirlineDF[-16:-17]

linearModel3 <- lm(formula = as.numeric(Satisfaction) ~ ., data = cheapseats)
linearModelS <- lm(formula = as.numeric(Satisfaction) ~ Airline.Status + Age + Gender + Price.Sensitivity + Year.of.First.Flight + No.of.Flights.p.a + Type.of.Travel + Shopping.Amount.at.Airport +Eating.and.Drinking.at.Airport + Class + Scheduled.Departure.Hour + Departure.Delay.in.Minutes + Arrival.Delay.in.Minutes + Flight.cancelled + Flight.time.in.minutes + Flight.Distance + Arrival.Delay.greater.5.Mins, data = CheapseatsAirlineDF)
summary(linearModel3)
linearModel3
summary(linearModelS)
linearModelS

############################ Observation from Linear Modelling ############################

## Following columns play a Significant role in improving Customer Satisfaction for Cheapseat Airlines. 
#CheapseatsAirlineDF$Age CheapseatsAirlineDF$Gender , CheapseatsAirlineDF$Price.Sensitivity, CheapseatsAirlineDF$Flight.cancelled, CheapseatsAirlineDF$Departure.Delay.in.Minutes
#1.
l1 <- lm(formula = as.numeric(Satisfaction) ~ Age, data = CheapseatsAirlineDF)
summary(l1)
#p-value: < 2.2e-16 which is less that 0.5
#Thus we reject Null Hypothesis
#Conclusion: Age affects the Customer Satisfaction for the CheapseatsAirline


lmcs1 <- lm(formula= as.numeric(Satisfaction)~ Age, data = dfAir1)
summary(lmcs1)
lmcs1
lmcs2 <- lm(formula= as.numeric(Satisfaction)~ Gender, data = dfAir1)
summary(lmcs2)
lmcs2
lmcs3 <- lm(formula= as.numeric(Satisfaction)~ Price.Sensitivity, data = dfAir1)
summary(lmcs3)
lmcs3
lmcs4 <- lm(formula= as.numeric(Satisfaction)~Eating.and.Drinking.at.Airport , data = dfAir1)
summary(lmcs4)
lmcs4 # not significant at all
lmcs6 <- lm(formula= as.numeric(Satisfaction)~Arrival.Delay.greater.5.Mins , data = dfAir1)
summary(lmcs6)
lmcs6
lmcs7 <- lm(formula= as.numeric(Satisfaction)~Airline.Status , data = dfAir1)
summary(lmcs7)
lmcs7
lmcs8 <- lm(formula= as.numeric(Satisfaction)~ Class , data = dfAir1)
summary(lmcs8)
lmcs8
lmcs9 <- lm(formula= as.numeric(Satisfaction)~ Type.of.Travel , data = dfAir1)
summary(lmcs9)
lmcs9

summary(dfAir1)

############################## KSVM MODEL################################

# any variable starting with dfAir*
library(kernlab)

createBucketsSurveya <- function(vec) {
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec >= 3] <- "Happy"
  vBuckets[vec < 3] <- "unHappy"
  return(vBuckets)
}
dfAir1<- CheapseatsAirlineDF1

dfAir1$SatHuH <- createBucketsSurveya(as.numeric(dfAir1$Satisfaction))

#before bucketing all the fields, add a column SatHuH which indicates the buckets 
# acc to satisfaction taking <3 as Low and >=3 as high

dfAirF <- subset(dfAir1, SatHuH == "Happy" | SatHuH == "unHappy")
dfAirF
table(dfAirF$SatHuH)

dim(dfAirF)
randIndex <- sample(1:dim(dfAirF)[1])
head(randIndex)

cutPoint2_3 <- floor(2 * dim(dfAirF)[1]/3)
cutPoint2_3

trainData <- dfAirF[randIndex[1:cutPoint2_3],]
testData <- dfAirF[randIndex[(cutPoint2_3 + 1):dim(dfAir1)[1]],]

dim(trainData)
dim(testData)

modelKs<-ksvm(SatHuH ~.,data=trainData, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model =TRUE)
modelKs
# Support Vector Machine object of class "ksvm" 
# 
# SV type: C-svc  (classification) 
# parameter : cost C = 5 
# 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  0.0304469805112404 
# 
# Number of Support Vectors : 960 
# 
# Objective Function Value : -220.7832 
# Training error : 0 
# Cross validation error : 0.00052 
# Probability model included. 

modelKs1<-ksvm(SatHuH ~ Airline.Status+Age+Gender+Price.Sensitivity+Arrival.Delay.greater.5.Mins+Class+Flight.cancelled,data=trainData, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model =TRUE)
#age,gender, price sens,arrivaldelay>5,class,typeoftr,flight canceleld
summary(modelKs1)
modelKs1
# Support Vector Machine object of class "ksvm" 
# 
# SV type: C-svc  (classification) 
# parameter : cost C = 5 
# 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  0.283666711460145 
# 
# Number of Support Vectors : 7056 
# 
# Objective Function Value : -31257.64 
# Training error : 0.1737 
# Cross validation error : 0.17751 
# Probability model included. 


svmPred<-predict(modelKs,testData,type="votes")
head(svmPred)
svmPred1<-predict(modelKs1,testData,type="votes")
compTable<- data.frame(testData$SatHuH,svmPred[2,])
#Confusion Matrix
cm1<-table(compTable)
# 
# svmPred.2...
# testData.SatHuH    0    1
# Happy   6800    0
# unHappy    1 1861

compTable1<- data.frame(testData$SatHuH,svmPred1[2,])
#Confusion Matrix
cm2<-table(compTable1)
# testData.SatHuH    0    1
# Happy   6685  115
# unHappy 1399  463


#Confusion Matrix
ES1<- cm1[1,2]+cm1[2,1]
ES2<- cm2[1,2]+cm2[2,1]

er1<- ES1/sum(cm1)*100
# 0
er2<- ES2/sum(cm2)*100
# 17.55946

################################# Validating Our Selected Columns using Association Model ###################
library(arules)
library(arulesViz)

ruleDF1 <- data.frame(CheapseatsAirlineDF$Satisfaction, CheapseatsAirlineDF$Airline.Status, CheapseatsAirlineDF$Age, CheapseatsAirlineDF$Gender, CheapseatsAirlineDF$Price.Sensitivity, CheapseatsAirlineDF$Year.of.First.Flight, 
                      CheapseatsAirlineDF$No.of.Flights.p.a, CheapseatsAirlineDF$X..of.Flight.with.other.Airlines, CheapseatsAirlineDF$Type.of.Travel, CheapseatsAirlineDF$No..of.other.Loyalty.Cards,
                      CheapseatsAirlineDF$Shopping.Amount.at.Airport, CheapseatsAirlineDF$Eating.and.Drinking.at.Airport, CheapseatsAirlineDF$Class, CheapseatsAirlineDF$Day.of.Month,
                      CheapseatsAirlineDF$Scheduled.Departure.Hour, CheapseatsAirlineDF$Departure.Delay.in.Minutes, CheapseatsAirlineDF$Arrival.Delay.in.Minutes,
                      CheapseatsAirlineDF$Flight.cancelled, CheapseatsAirlineDF$Flight.time.in.minutes, CheapseatsAirlineDF$Flight.Distance, CheapseatsAirlineDF$Arrival.Delay.greater.5.Mins)
ruleX <- as(ruleDF1, "transactions")
ruleX

ruleset <- apriori(ruleX, parameter = list(support=0.30,confidence=0.30,maxtime=10, maxlen=30),appearance = list(default="lhs", rhs=("CheapseatsAirlineDF.Satisfaction=High")))
ruleset <- sort(ruleset, decreasing = TRUE, by="lift")
inspect(ruleset)
summary(CheapseatsAirlineDF)


ruleDF2 <- data.frame(CheapseatsAirlineDF$Satisfaction, CheapseatsAirlineDF$Airline.Status ,CheapseatsAirlineDF$Age, CheapseatsAirlineDF$Gender, CheapseatsAirlineDF$Price.Sensitivity,
                      CheapseatsAirlineDF$Shopping.Amount.at.Airport, CheapseatsAirlineDF$Eating.and.Drinking.at.Airport, CheapseatsAirlineDF$Class, CheapseatsAirlineDF$Day.of.Month,
                      CheapseatsAirlineDF$Flight.cancelled, CheapseatsAirlineDF$Arrival.Delay.greater.5.Mins)
ruleX <- as(ruleDF2, "transactions")
ruleX

ruleset <- apriori(ruleX, parameter = list(support=0.30,confidence=0.30,maxtime=10, maxlen=30),appearance = list(default="lhs", rhs=("CheapseatsAirlineDF.Satisfaction=High")))
ruleset <- sort(ruleset, decreasing = TRUE, by="lift")
inspect(ruleset)
summary(CheapseatsAirlineDF)

#OBSERVATIONS
#For the customers **whose flight delayed and who are low class/status** tend to give **low satisfaction**
#For the customers **whose price sensitivity are low and Male** tend to give **high satisfaction**









#####ggplot2 for age########







############################ PROPOSALS ####################################
## 1
## We propose to reduce the delay in arrival of flights at major cities like Los Angeles, San Jose, Seattle, San Diego, Phoenix, Flint, Norfolk, Rochester, West Palm Beach/Palm Beach, New York, NY.  
# Proposal is made on an analysis of the airline with highest and lowest average arrival delay time and the major cities being affected in those were then segregated.


##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify 

library(ggplot2)
# g <- ggplot(c) + aes(x= reorder(c$Orgin.City,c$Airline.Code), y=c$count ) + geom_col(aes(fill = c$Airline.Code))
# g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# g <- g + ggtitle("Average Arrival Delay Count greater than 5 Minutes across cities")
# g




#### 2
#Using Arules prove that Airline.Status=Blue,Class=Eco,Price.Sensitivity=Low,Flight.cancelled=No together gives a negative impact i.e. gives a lower Satisfaction 
ruleDF3 <- data.frame(CheapseatsAirlineDF$Satisfaction, CheapseatsAirlineDF$Airline.Status ,CheapseatsAirlineDF$Age, CheapseatsAirlineDF$Gender, CheapseatsAirlineDF$Price.Sensitivity,
                      CheapseatsAirlineDF$Shopping.Amount.at.Airport, CheapseatsAirlineDF$Eating.and.Drinking.at.Airport, CheapseatsAirlineDF$Class, CheapseatsAirlineDF$Day.of.Month,
                      CheapseatsAirlineDF$Flight.cancelled, CheapseatsAirlineDF$Arrival.Delay.greater.5.Mins)
ruleX <- as(ruleDF3, "transactions")
ruleX

ruleset <- apriori(ruleX, parameter = list(support=0.15,confidence=0.20,maxtime=10, maxlen=30),appearance = list(default="lhs", rhs=("CheapseatsAirlineDF.Satisfaction=Low")))
ruleset <- sort(ruleset, decreasing = TRUE, by="lift")
inspect(ruleset)
summary(CheapseatsAirlineDF)

##### age vs satisfaction ###########










################################  GGMAP
#ggmaps code - Avg Delay in Minutes - Average Satisfaction - Displayed on Map

#for ggmaps

# View(dfAir1)
summary(dfAir1)


#Removing 9999 Values which were introduced for removing Na's
sqldf(' select count("Arrival.Delay.in.Minutes") from dfAir1 where "Arrival.Delay.in.Minutes" = 9999')
dfAirif<- dfAir1[dfAir1$Arrival.Delay.in.Minutes!=9999,]
dfAir1<- dfAirif

#extracting data form the dataset for destination and average arrival in delay
library(sqldf)
statesDelay<- sqldf('select "Destination.State" as "stateName", avg("Arrival.Delay.in.Minutes") as "adih",avg("Satisfaction") as "AverageSatisfaction"  from dfAir1 group by "Destination.State"')

#taking and merging default system data with our dataset
area <- state.area
latlong<- state.center
stateName<- state.name
mergeDf<- data.frame(stateName,latlong,area)

fds<- merge(mergeDf,statesDelay, by='stateName')

#using lower case for stateName
fds$stateName<-tolower(fds$stateName)

us <- map_data("state")


#ggmaps
m.s1<-ggplot(fds , aes(map_id=stateName))
m.s1<- m.s1 + geom_map(map = us,aes(fill=fds$AverageSatisfaction),color="white")
m.s1<- m.s1 + expand_limits(x= fds$x,y=fds$y)
m.s1<- m.s1 + geom_point(data=fds, aes(x=fds$x,y=fds$y,size=fds$adih), color ="green")+ scale_size(name="Avg Delay in Minutes")
m.s1<- m.s1 + geom_text( data=fds, hjust=0.5, vjust=-0.5, aes(x=x, y=y, label=toupper(stateName)), colour="gold", size=2.5 )
m.s1<- m.s1 + coord_map() + ggtitle("Map of USA (Average Delay in Minutes for Cheapseats)")+ xlab("Longitude") + ylab("Latitude")
m.s1


#####BAR CHART#######
counts <- table( cleanedDataset$Price.Sensitivity, cleanedDataset$Age)

counts <- table(cleanedDataset$Year.of.First.Flight)
barplot(counts, main="First Flight distribution ", 
        xlab="Year", col=c("RED"))





##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify ##### Rectify 

### SEVITHA PLEASE RECTIFY THIS CODE

####GGRIDGES###########GGRIDGES###########GGRIDGES###########GGRIDGES###########GGRIDGES###########GGRIDGES###########GGRIDGES#######
install.packages("ggridges")
library(ggridges)
ggplot(ss1) + geom_density_ridges(aes( x = ss1$Satisfaction, y = ss1$Type.of.Travel, fill = ss1$Type.of.Travel), scale = 3) +
  scale_fill_brewer("Status", palette = "Set1")

################Corrolation Plot#########################Corrolation Plot#########################Corrolation Plot###################
install.packages("corrplot")
install.packages("corrgram")
library(corrplot)
library(corrgram)
Cheapseats<- dfAir1
names(Cheapseats)
Cheapseats<- Cheapseats[,c(1,3,5:8,10:12,22:24,26,27)]
names(Cheapseats)<-c("SAT","AGE","Sens", "YrsFli","NOFLi","FLiother","Loyalty","Shop","EatandDrink","Schedhour","depdel","arrdelay","flimin","dist")
Cheapseats<- na.omit(Cheapseats)
corr_data <- cor(Cheapseats)
corplot1 <- corrplot.mixed (corr_data,lower.col = "red",number.cex = 0.7)









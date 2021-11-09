# Installing Packages
install.packages('here')
install.packages('skimr')
install.packages('janitor')
install.packages('ggplot2') #To do visualization
install.packages('dplyr')
install.packages('tidyverse')
install.packages('readr')
install.packages('lubridate')
install.packages('stringr') 
install.packages('tidyr') 
install.packages('rmarkdown') 
install.packages('vroom')  #To open zip file
install.packages("Hmisc")
install.packages("pastecs")
install.packages("forcats")
install.packages("purr")
install.packages("psych")
install.packages("explore")
install.packages("funModeling")
install.packages("naniar")
install.packages("visdat")
install.packages("modeest")
install.packages("fastDummies")
install.packages("labelled")
install.packages("DataExplorer")
install.packages("dataMaid")
install.packages("scales")
install.packages("ggforce")
install.packages("plyr")

#Loading data/packages
getwd()
setwd('/Users/jerometsangyein/JEROME/Google Analytics Certificate/Cases study /case 1 bicycle/Case study 1 google R project')
library(vroom)
library(skimr)
library(dplyr)
library(tidyverse)
library(explore)
library(Hmisc)
library(psych)
library(tidyr)
library(readr)
library(lubridate)
library(pastecs)
library(purr)
library(ggplot2)
library(here)
library(janitor)
library(visdat)
library(funModeling)
library(naniar)
library(modeest)
library(fastDummies)
library(labelled)
library(DataExplorer)
library(dataMaid)
library(scales)
library(ggforce)
library(plyr)


# Exploring different data set to adjust strategy
str(vroom(file = '202105-divvy-tripdata.zip'))
read_csv('202104-divvy-tripdata.zip')
View(vroom(file = '202004-divvy-tripdata.zip'))
View(vroom(file = 'Divvy_Trips_2020_Q1.zip'))
str(vroom(file = 'Divvy_Trips_2019_Q2.zip'))


# Combining data 2020/2021
q1_20 <- vroom(file = 'Divvy_Trips_2020_Q1.zip')
april_20 <- vroom(file = '202004-divvy-tripdata.zip')
may_20 <- vroom(file = '202005-divvy-tripdata.zip')
june_20 <- vroom(file = '202006-divvy-tripdata.zip')
july_20 <- vroom(file = '202007-divvy-tripdata.zip')
august_20 <- vroom(file = '202008-divvy-tripdata.zip')
september_20 <- vroom(file = '202009-divvy-tripdata.zip')
october_20 <- vroom(file = '202010-divvy-tripdata.zip')
november_20 <- vroom(file = '202011-divvy-tripdata.zip')
december_20 <- vroom(file = '202012-divvy-tripdata.zip')
january_21 <- vroom(file = '202101-divvy-tripdata.zip')
february_21 <- vroom(file = '202102-divvy-tripdata.zip')
march_21 <- vroom(file = '202103-divvy-tripdata.zip')
april_21 <- vroom(file = '202104-divvy-tripdata.zip')
may_21 <- vroom(file = '202105-divvy-tripdata.zip')
june_21 <- vroom(file = '202106-divvy-tripdata.zip')
july_21 <- vroom(file = '202107-divvy-tripdata.zip')
august_21 <- vroom(file = '202108-divvy-tripdata.zip')
september_21 <- vroom(file = '202109-divvy-tripdata.zip')


#Created gathered dataset of 2020/2021 
df_20_21 <- rbind(
  q1_20, april_20, may_20, june_20, july_20, august_20, september_20, october_20, november_20, december_20, 
  january_21, february_21, march_21, april_21, may_21, june_21, july_21, august_21, september_21
  )


# Full Dataset preview
dim(df_20_21)
colnames(df_20_21)
head(df_20_21)
str(df_20_21)
spec(df_20_21)
glimpse(df_20_21)
explore_all(df_20_21) #a bit long to get output
look_for(df_20_21)
create_report(df_20_21) #a bit long to get output
makeCodebook(df_20_21) #a bit long to get output

# Display tables to see where we need to clean, and metrics we need to create
View(df_20_21)
describe(df_20_21) # missing values on stations
skim_without_charts(df_20_21) # allow to see missing value/ no empty space
## 56% member vs 43% casual
## 41% docked bike, 34% classic bike, 24% electric bike


# Statistics
sapply(df_20_21, mean , na.rm=TRUE) # Calculation of a metric excluding NA 
summary(df_20_21)
stat.desc(df_20_21)
df_status(df_20_21) # retrieve quantity data to be clean
gg_miss_var(df_20_21) # visualize missing values
naniar::gg_miss_upset(df_20_21) #visualize missing value and relation with features
which(is.na(df_20_21),arr.ind=TRUE) #output all line with NA + the position row/col


# Find & clean NA/empty space/doublon
clean_names(df_20_21) # check that columns have correct names

## Create new metrics

### Calculating duration of each rides
ride_length <- time_length(interval(start = df_20_21$started_at, end = df_20_21$ended_at)) #rides duration in seconds
df_20_21 <- mutate(df_20_21, ride_length_second = ride_length)  #adding new column to df
df_20_21 <- mutate(df_20_21, ride_length = seconds_to_period(ride_length)) # converting second to period

### Day of the week
day_of_week <- weekdays(df_20_21$started_at) #conversion datetime to days of week
month_of_year <- format(df_20_21$started_at, format = "%B") # extracting month in string
year_of_data <- format(df_20_21$started_at, format = "%Y") # extracting year
  
df_20_21 <- mutate(df_20_21, day_of_week = day_of_week, year_of_data = year_of_data, month_of_year = month_of_year) #adding to df
View(df_20_21)

## Check NA/missing values
df_20_21_2 <- df_20_21[is.na(df_20_21$end_station_id),] # Show data with NA column in specific column
View(df_20_21_2)
#### df_20_21_2 <- subset(df_20_21, !is.na(df_20_21$end_station_id)) #Delete NA in specific column

## Add column to new dataset if is doublon on ride_id
doublon_ride_id <- data.frame(df_20_21$ride_id) #select only ride_id
duplicated_data <- duplicated(doublon_ride_id) #find only doublon, ouput is true/false list
df_20_21_with_dbl <- cbind(is_doublon = duplicated_data, df_20_21) #Add column to a new df

select(df_20_21_with_dbl, is_doublon, ride_id) %>%
  filter(is_doublon == 'TRUE') #ouput list of doublon with ride_id
df_20_21[df_20_21$ride_id == '70F458C5AAE4C49F', ] #verifying ride_id doublon in original df

df_20_21_without_dbl <- (distinct(df_20_21_with_dbl, ride_id, .keep_all=TRUE)) #keep only unique rows, and create new df

df_20_21_without_dbl[df_20_21_without_dbl$ride_id == '70F458C5AAE4C49F', ]#veryfying any doublon left 
View(df_20_21_without_dbl )

## Extreme value/outliers

#check xtrm values
df1 <- df_20_21_without_dbl[df_20_21_without_dbl$ride_length_second > 86400, ] #6K rows above 24hours ride length
df2 <- df_20_21_without_dbl[df_20_21_without_dbl$ride_length_second < 60 , ] #127K rows below 60 second ride
View(arrange(df1, -ride_length_second)) #visualise extrem values, sort by ride length

df_20_21_without_dbl_no_xtrm <- df_20_21_without_dbl[between(df_20_21_without_dbl$ride_length_second, 60, 86400) , ] #Keeping only "normal values"
View(df_20_21_without_dbl_no_xtrm)


# Analyze dataset

stat.desc(df_20_21_without_dbl_no_xtrm)
mean(df_20_21_without_dbl_no_xtrm$ride_length_second) #calculation of ride length mean
median(df_20_21_without_dbl_no_xtrm$ride_length_second) #calculation of ride length median
max(df_20_21_without_dbl_no_xtrm$ride_length_second)
min(df_20_21_without_dbl_no_xtrm$ride_length_second)

mfv(df_20_21_without_dbl_no_xtrm$day_of_week, na_rm = TRUE) #calculating mode/most frequent value of day
mfv(df_20_21_without_dbl_no_xtrm$start_station_name, na_rm = TRUE) #calculating mode/most frequent value of starting station
mfv(df_20_21_without_dbl_no_xtrm$end_station_name, na_rm = TRUE) #calculating mode/most frequent value of ending station
tabyl(df_20_21_without_dbl_no_xtrm, start_station_name ) # %>%    #Counting each unique values of columns (possible to have in %)
# adorn_percentages("col") %>%
# adorn_pct_formatting(digits= 1) %>%
tabyl(df_20_21_without_dbl_no_xtrm, member_casual, day_of_week) #counting higher frequency of observations per days per type of customer
tabyl(df_20_21_without_dbl_no_xtrm, member_casual, month_of_year) #per month
View(tabyl(df_20_21_without_dbl_no_xtrm, member_casual, start_station_name))
tabyl(df_20_21_without_dbl_no_xtrm, member_casual, rideable_type)

df_20_21_without_dbl_no_xtrm %>% 
  group_by(rideable_type) %>%
  summarise(count=n()) #counting how many time each type of bikes appears

aggregate(df_20_21_without_dbl_no_xtrm[, "ride_length_second"], list(df_20_21_without_dbl_no_xtrm$day_of_week), mean) #Calcul of the length duration mean per day's week

df_only_casual <- df_20_21_without_dbl_no_xtrm %>% #filter ds only with casual
  filter(df_20_21_without_dbl_no_xtrm$member_casual =='casual')
aggregate(df_only_casual[, "ride_length_second"], list(df_only_casual$day_of_week), mean) #Calcul of the length duration mean per day's week for casual
sd(df_only_casual$ride_length_second)


df_only_member <- df_20_21_without_dbl_no_xtrm %>% #filter ds only with member
  filter(df_20_21_without_dbl_no_xtrm$member_casual =='member')
aggregate(df_only_member[, "ride_length_second"], list(df_only_member$day_of_week), mean) #Calcul of the length duration mean per day's week for members
sd(df_only_member$ride_length_second)

aggregate(df_20_21_without_dbl_no_xtrm[, "ride_length_second"], list(df_20_21_without_dbl_no_xtrm$rideable_type), mean) #Calcul of the length duration mean per bike type
aggregate(df_20_21_without_dbl_no_xtrm[, "ride_length_second"], list(df_20_21_without_dbl_no_xtrm$member_casual), mean) #Calcul of the length duration mean per customer type
aggregate(df_20_21_without_dbl_no_xtrm[, "ride_length_second"], list(df_20_21_without_dbl_no_xtrm$month_of_year), mean) #Calcul of the length duration mean per month

aggregate(df_20_21_without_dbl_no_xtrm$ride_length_second ~ df_20_21_without_dbl_no_xtrm$member_casual, FUN = median) #Calcul of the median lenght duration per customer type
aggregate(df_20_21_without_dbl_no_xtrm$ride_length_second ~ df_20_21_without_dbl_no_xtrm$member_casual + df_20_21_without_dbl_no_xtrm$day_of_week, FUN = mean) #Mean per day per type of customer


#Mean calculation per day for each group


moy_grp <- ddply(df_20_21_without_dbl_no_xtrm, "member_casual", summarise, grp.mean = mean(ride_length_second)) # average calcul of member vs casual
med_grp <- ddply(df_20_21_without_dbl_no_xtrm, "member_casual", summarise, grp.median = median(ride_length_second)) # median calcul

ddply(df_only_casual,"day_of_week",summarise, mean_ride_length = mean (ride_length_second))
ddply(df_only_member,"day_of_week",summarise, mean_ride_length = mean (ride_length_second))


mean_to_plot <- ddply(df_20_21_without_dbl_no_xtrm,c("day_of_week","member_casual") ,summarise, mean_ride_length = mean (ride_length_second))



#PLOT
#creation of a final tab with important data
df_20_21_without_dbl_no_xtrm_final <- df_20_21_without_dbl_no_xtrm %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>%
  dplyr::summarise(rides_nbr = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)

#Sample creation for faster custom graphs test
df <- sample_n(df_20_21_without_dbl_no_xtrm, 70000)       


customer_per_day <- df_20_21_without_dbl_no_xtrm_final %>%
  ggplot(aes(x = day_of_week, y = rides_nbr, fill = member_casual)) +
  geom_col(position = "dodge")
print(customer_per_day)


ride_per_day <-
  ggplot(df_20_21_without_dbl_no_xtrm) + 
  geom_boxplot(mapping=aes(x= day_of_week,  y= ride_length, fill = member_casual)) +
  scale_y_continuous(breaks = breaks_width(300), limits = c(0, 3600)) +
  scale_x_discrete(limits = c("Dimanche", "Lundi", "Mardi", "Mercredi", "Jeudi", 
                              "Vendredi", "Samedi" )) 
print(ride_per_day)


bike_per_day <- 
  ggplot(df_20_21_without_dbl_no_xtrm) + 
  geom_bar(aes(x= day_of_week, fill= day_of_week)) + 
  facet_grid(rideable_type ~ member_casual)
print(bike_per_day)


average_ride <- 
  ggplot(mean_to_plot, aes(x = day_of_week, y=mean_ride_length, fill = member_casual)) +
  geom_bar(stat="identity", position = "dodge") +
  #facet_zoom(ylim = c(28, 30)) + 
  coord_flip()
print(average_ride)

ride_month <- 
  ggplot(df_20_21_without_dbl_no_xtrm,aes(x = month_of_year, fill = member_casual)) +
  geom_bar(aes(x = month_of_year, stat="identity", position = "dodge"))
print(ride_month)


hist_ride_length <- 
  ggplot(df_20_21_without_dbl_no_xtrm, aes(x = ride_length_second, color = member_casual)) + 
  geom_histogram(binwidth=30, aes(y= ..density..), colour="black",fill="black", alpha=0.5, position="dodge") +
  geom_vline(data=moy_grp, aes(xintercept=grp.mean, color=member_casual), linetype="dashed") +
  geom_vline(data=med_grp, aes(xintercept=grp.median , color=member_casual), linetype="solid") +
  theme(legend.position="top") +
  geom_density(alpha=.4) +
  scale_color_manual(values=c("#F73416", "#2352FF", "#69FFFF"))+
  scale_x_continuous(limits = c(30, 2700), breaks = breaks_width(300),) 
print(hist_ride_length)







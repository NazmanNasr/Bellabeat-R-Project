setwd("C:/Users/ASUS X441S/Desktop/Bellabeat_project")

library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(skimr)
library(ggpubr)
library(here)
library(janitor)
library(ggrepel)

activity <- read.csv('dailyActivity_merged.csv')
sleep <- read.csv('sleepDay_merged.csv')
steps <- read.csv('hourlySteps_merged.csv')

#We pick daily activity, daily sleep and hourly steps

#Preview data
head(activity)
str(activity)

head(sleep)
str(sleep)

head(steps)
str(steps)

#Data Cleaning and formatting

#1.Verify number of unique users
n_unique(activity$Id) #33
n_unique(sleep$Id)    #24
n_unique(steps$Id)    #33

n_distinct(activity$Id)
n_distinct(sleep$Id)
n_distinct(steps$Id)

#2. Look for any duplicate or missing value

sum(duplicated(activity)) #0
sum(duplicated(sleep))    #3 duplicates
sum(duplicated(steps))    #0

sum(is.na(activity))      #All datasets have no null value
sum(is.na(sleep))
sum(is.na(steps))

#3. Remove duplicate
#Using distinct() and pipeline
activity <- activity %>% 
  distinct() %>% 
  drop_na()

sleep <- sleep %>% 
  distinct() %>% 
  drop_na()

steps <- steps %>% 
  distinct() %>% 
  drop_na()

#Check for duplicate
sum(duplicated(sleep)) #0 duplicate

#4. Clean data and rename column
#all column to lower letter, easy to merge
clean_names(activity)
activity <- rename_with(activity, tolower)

clean_names(sleep)
sleep <- rename_with(sleep, tolower)

clean_names(steps)
steps <- rename_with(steps, tolower)

#5. format date time in each data
activity <- activity %>%
  rename(date = activitydate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

sleep <- sleep %>%
  rename(date = sleepday) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

steps<- steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

#6. Merging Dataset
activity_sleep <- merge(activity, sleep, c('id','date'))
head(activity_sleep)

#Analyze and Share

#1.Using the merge data for activity and sleep, we calculate the mean for.. 
#daily steps, calories burn and sleep
daily_average <- activity_sleep %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(totalsteps), 
             mean_daily_calories = mean(calories), 
             mean_daily_sleep = mean(totalminutesasleep))

head(daily_average)

#2. Classify our users by daily average steps
user_type <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "lightly active", 
    mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "fairly active", 
    mean_daily_steps >= 10000 ~ "very active"
  ))

head(user_type)

#3. percentage by user type
user_type_percent <- user_type %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

user_type_percent$user_type <- factor(user_type_percent$user_type , 
                                      levels = c("very active", "fairly active", "lightly active", "sedentary"))


head(user_type_percent)

#4. plot pie chart for daily step by category
#New plot (Use this)
ggplot(data = user_type_percent, aes(x = "",y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() + 
  
  geom_text(aes(label = labels),position = position_stack(vjust = 0.5)) +
  labs(title="User Type Distribution") +
  scale_fill_brewer(palette="Spectral")

#5. Plot for steps and minutes per weekday
#Add new column where the date changed into days using weekdays()
weekday_steps_sleep <- activity_sleep %>%
  mutate(weekday = weekdays(date))

#Sort the days accordingly by weekdays sequence
weekday_steps_sleep$weekday <-ordered(weekday_steps_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                            "Friday", "Saturday", "Sunday"))
#Group by weekday and find average for every days
weekday_steps_sleep <- weekday_steps_sleep %>%
  group_by(weekday) %>%
  summarise (daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))

head(weekday_steps_sleep)

#Plot bar chart
ggarrange(
  ggplot(data = weekday_steps_sleep, aes(x = weekday, y = daily_steps)) +
    geom_col(fill = 'darkblue') +
    geom_hline(yintercept = 7500) +
    labs(title = "Daily steps per weekday", x= "", y = "")  ,
  
  ggplot(data = weekday_steps_sleep, aes(x = weekday, y =  daily_sleep)) +
    geom_col(fill = 'lightblue') +
    labs(title = "Minutes asleep per weekday", x= "", y = "") +
    geom_hline(yintercept = 480) 
  
)

#Users walk daily the recommended amount of steps of 7500 besides Sunday's.
#Users don't sleep the recommended amount of minutes/ hours - 8 hours.

#6. Hourly steps throughout the day
str(steps)
colnames(steps)

#separate POSIXct datetime in column date_time by date and time
steps <- steps %>% 
  separate(date_time, into = c('date', 'time'), sep = " ") %>% 
  mutate( date = ymd(date))

hourly_steps <- steps %>% 
  group_by(time) %>% 
  summarise(average_steps = mean(steptotal))

#PLot chart to represent hourly steps throughout the day
ggplot(data = hourly_steps, aes(x = time, y = average_steps, fill = average_steps)) +
  geom_col() +
  scale_fill_gradient(low = 'green', high = 'red')+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Hourly Steps Throughout the Day')

#7. Find correlation between variables
# We want to find any new discovery between variables that we could use to make recommendation
#Daily steps and daily sleep
#Daily steps and calories
ggarrange(
  ggplot(activity_sleep, aes(x=totalsteps, y=totalminutesasleep))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Daily steps vs Minutes asleep", x = "Daily steps", y= "Minutes asleep") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14))
  ,
  
  ggplot(activity_sleep, aes(x=totalsteps, y=calories))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Daily steps vs Calories", x = "Daily steps", y= "Calories") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14))
)

#There is little to no correlation between daily steps and minutes asleep. 
#However, there is a positive  correlation between daily steps and calories burned. There higher the daily steps, more calories are burned.

#8. Use of smart device
#Now that we have seen some trends in activity, sleep and calories burned, 
#we want to see how often do the users in our sample use their device. 

daily_use <- activity_sleep %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))

head(daily_use)

daily_use_percent <- daily_use %>%
  group_by(usage) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(usage) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

daily_use_percent$usage <- factor(daily_use_percent$usage, levels = c("high use", "moderate use", "low use"))

head(daily_use_percent)

#20/2/2023
#Now that we have our new table we can create our plot:
daily_use_percent %>%
  ggplot(aes(x="",y=total_percent, fill=usage)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme_void() +
  
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#006633","#00e673","#80ffbf"),
                    labels = c("High use - 21 to 31 days",
                               "Moderate use - 11 to 20 days",
                               "Low use - 1 to 10 days"))+
  labs(title="Daily use of smart device")


#9. Time used smart device
daily_use_merged <- merge(activity, daily_use, by= c("id"))
head(daily_use_merged)

#Categorize
#All day - device was worn all day.
#More than half day - device was worn more than half of the day.
#Less than half day - device was worn less than half of the day.
#1 day = 24 hr = 1440 min

minutes_worn <- daily_use_merged %>% 
  mutate (total_minutes_worn = veryactiveminutes+fairlyactiveminutes+lightlyactiveminutes+sedentaryminutes)%>%
  mutate (percent_minutes_worn = (total_minutes_worn/1440)*100) %>%
  mutate (worn = case_when(
    percent_minutes_worn == 100 ~ "All day",
    percent_minutes_worn < 100 & percent_minutes_worn >= 50~ "More than half day", 
    percent_minutes_worn < 50 & percent_minutes_worn > 0 ~ "Less than half day"
  ))

head(minutes_worn)


minutes_worn_percent<- minutes_worn%>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_highuse <- minutes_worn%>%
  filter (usage == "high use")%>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_moduse <- minutes_worn%>%
  filter(usage == "moderate use") %>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_lowuse <- minutes_worn%>%
  filter (usage == "low use") %>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_highuse$worn <- factor(minutes_worn_highuse$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_percent$worn <- factor(minutes_worn_percent$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_moduse$worn <- factor(minutes_worn_moduse$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_lowuse$worn <- factor(minutes_worn_lowuse$worn, levels = c("All day", "More than half day", "Less than half day"))

head(minutes_worn_percent)
head(minutes_worn_highuse)
head(minutes_worn_moduse)
head(minutes_worn_lowuse)

#plotting



ggarrange(
  ggplot(minutes_worn_percent, aes(x="",y=total_percent, fill=worn)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5), size = 3.5)+
    labs(title="Time worn per day", subtitle = "Total Users"),
  ggarrange(
    ggplot(minutes_worn_highuse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.position = "none")+
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text_repel(aes(label = labels),
                      position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "High use - Users"), 
    ggplot(minutes_worn_moduse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Moderate use - Users"), 
    ggplot(minutes_worn_lowuse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Low use - Users"), 
      ncol = 3), 
      nrow = 2)



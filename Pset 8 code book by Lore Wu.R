library(readr)
library(tidyverse)
library(ggpubr) #load the package i will use 

setwd("/Users/lorewu/Desktop") #setting the directories 
unvoting <- read.csv("unvoting copy.csv") # Load the data

######Question 1######
#subset the data with year of year of 1980 indicating before fall of berlin wall
idealpoint_1980 <- unvoting %>%
  subset(Year == 1980) 

# using the histogram to plot the distribution of idealpoint   
ggplot(idealpoint_1980, aes(idealpoint)) +
  geom_histogram(binwidth = 0.2) +
  labs(x = "Countries Ideal Point",
       title = "UN Voting Ideal Points (1980)") +
  geom_vline(xintercept = median(idealpoint_1980$idealpoint, na.rm = TRUE), color = "red") +
  annotate("text", x = median(idealpoint_1980$idealpoint + 0.3, na.rm = TRUE), 
           y = 23, label = "Median", color = "red", size = 3)
  
hist(idealpoint_1980$idealpoint, xlab = "Countries Ideal Point", main = "UN Voting Ideal Points (1980)")
abline(v = median(idealpoint_1980$idealpoint, na.rm = TRUE), col = "red", lwd = 2)  
text(x = median(idealpoint_1980$idealpoint + 0.5, na.rm = TRUE), 
     y = 23, "Median", col = "red")

#subset the data with year of year of 2000 indicating after fall of Berlin wall
idelpoint_2000 <- unvoting %>%
  subset(Year == 2000)

ggplot(idelpoint_2000, aes(idealpoint)) +
  geom_histogram(binwidth = 0.2) +
  labs(x = "Countries Ideal Point",
       title = "UN Voting Ideal Points (2000)") +
  geom_vline(xintercept = median(idelpoint_2000$idealpoint, na.rm = TRUE), color = "red") +
  annotate("text", x = median(idelpoint_2000$idealpoint + 0.3), y = 20, label = "Median", color = "red", size = 3)

hist(idelpoint_2000$idealpoint, xlab = "Countries Ideal Point", main = "UN Voting Ideal Points (2000)")
abline(v = median(idelpoint_2000$idealpoint, na.rm = TRUE), col = "red", lwd = 2) 
text(x = median(idelpoint_2000$idealpoint + 0.5, na.rm = TRUE), 
     y = 23, "Median", col = "red")


######Question 2######

# ggplot
unvoting %>%
  group_by(Year) %>%
  summarise(us_ave = mean(PctAgreeUS, na.rm = TRUE),
            ra_ave = mean(PctAgreeRUSSIA, na.rm = TRUE)) %>%
  ggplot(aes(Year)) +
  geom_line(aes(y = us_ave, color = "USA")) +
  geom_line(aes(y = ra_ave, color = "Russia")) +
  scale_color_manual(values = c("USA" = "blue", "Russia" = "red")) + # this step assisted by AI
  labs(x = "Year",
       y = "average percentageagreement",
       title = "US/Russia Agreement Over Time",
       color = "countries")

# Alternative way guided by book P100
us_ave <- tapply(unvoting$PctAgreeUS, unvoting$Year, mean, na.rm = TRUE)
ra_ave <- tapply(unvoting$PctAgreeRUSSIA, unvoting$Year, mean, na.rm = TRUE)
year <- unique(unvoting$Year)

plot(year, us_ave, col = "blue", type = "l", xlab = "Year", ylim = c(0, 1),
     ylab = "average percentageagreemen", 
     main = "US/Russia Agreement Over Time")
lines(year, ra_ave, col = "red")
text(2010, 0.6, "Russia")
text(2010, 0.1, "USA")

# Identify the countries that are consistently pro-USA and pro-Russia.....
summary_voting <- unvoting %>%
  group_by(CountryName) %>%
  summarize(pro_usa = mean(PctAgreeUS, na.rm = TRUE),
            pro_ra = mean(PctAgreeRUSSIA, na.rm = TRUE))
pro_usa <- summary_voting %>%
  filter(pro_usa > 0.7) %>%
  select(CountryName) ### I didn't get the correct answer...
pro_ra <- summary_voting %>%
  filter(pro_ra > 0.7) %>%
  select(CountryName) ### but russia looks correct 

### professor's code
unvoting_proUS <- data.frame(CountryName = unique(unvoting$CountryName),
                             Avg_US_Agreement = tapply(unvoting$PctAgreeUS, unvoting$CountryName, mean, na.rm = TRUE))
unvoting_proUS$CountryName[unvoting_proUS$Avg_US_Agreement > .7]

unvoting_proRA <- data.frame(
  CountryName = unique(unvoting$CountryName),
  Avg_RA_Agreement = tapply(unvoting$PctAgreeRUSSIA, unvoting$CountryName, mean, na.rm = TRUE))
unvoting_proRA$CountryName[unvoting_proRA$Avg_RA_Agreement > .7]

 

######Question 3######

usa_ideal <- unvoting %>%
  filter(CountryName == "United States of America") %>%
  pull(idealpoint)

ra_ideal <- unvoting %>%
  filter(CountryName == "Russia") %>%
  pull(idealpoint)

median <- tapply(unvoting$idealpoint, unvoting$Year, median, na.rm = TRUE)

voting_summary <- data.frame(year = year, usa_ideal = usa_ideal, ra_ideal = ra_ideal, median = median)

voting_summary %>% 
  ggplot(aes(year)) +
  geom_line(aes(year, usa_ideal), color = "blue") +
  geom_line(aes(year, ra_ideal), color = "red") +
  geom_line(aes(year, median), color = "black") +
  annotate("text", x = 2010, y = 2.2, label = "USA", color = "blue") +
  annotate("text", x = 2010, y = 0.2, label = "Russia", color = "red") +
  annotate("text", x = 2010, y = -0.6, label = "Median", color = "black") +
  labs(x = "year", y = "ideal point", title = "USA/Russia Ideal Point Evolution")
  

######Question 4######
postsoviet <- unvoting %>%
  filter(CountryName %in% c("Estonia", "Latvia", "Lithuania", "Belarus", "Moldova", "Ukraine", "Armenia", 
                            "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Tajikistan",
                            "Turkmenistan", "Uzbekistan", "Russia"), Year == 2012)  

nonsoviet <- unvoting %>%
  filter(!(CountryName %in% c("Estonia", "Latvia", "Lithuania", "Belarus", "Moldova", "Ukraine", "Armenia", 
                            "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Tajikistan",
                            "Turkmenistan", "Uzbekistan", "Russia")), Year == 2012)

plot(postsoviet$idealpoint, postsoviet$PctAgreeUS, col = "red", pch = 17 )
points(nonsoviet$idealpoint, nonsoviet$PctAgreeRUSSIA, col = "blue", pch = 16)

plot(postsoviet$idealpoint, postsoviet$PctAgreeUS, 
     col = "red", pch = 17, 
     xlim = range(c(postsoviet$idealpoint, nonsoviet$idealpoint), na.rm = TRUE),
     ylim = range(c(postsoviet$PctAgreeUS, nonsoviet$PctAgreeRUSSIA), na.rm = TRUE),
     xlab = "Ideal Point",
     ylab = "Percentage of Agreement with US",
     main = "Post-SU/Other Countries (2010)")
points(nonsoviet$idealpoint, nonsoviet$PctAgreeRUSSIA, col = "blue", pch = 16)
legend("topright", 
       legend = c("Post-Soviet Countries", "Other Countries"), 
       col = c("red", "blue"), 
       pch = c(17, 16), 
       bty = "n",   
       cex = 0.8,  
       pt.cex = 0.7)  # legend and adjusting the sclae fixed by the AI


######Question 5######

median_post <- unvoting %>%
  filter(CountryName %in% c("Estonia", "Latvia", "Lithuania", "Belarus", "Moldova", "Ukraine", "Armenia", 
                            "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Tajikistan",
                            "Turkmenistan", "Uzbekistan", "Russia")) %>%
           group_by(Year)  %>%
           summarize(median_post = median(idealpoint, na.rm = TRUE))

median_non <- unvoting %>%
  filter(!(CountryName %in% c("Estonia", "Latvia", "Lithuania", "Belarus", "Moldova", "Ukraine", "Armenia", 
                              "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Tajikistan",
                              "Turkmenistan", "Uzbekistan", "Russia"))) %>%
  group_by(Year)  %>%
  summarize(median_non = median(idealpoint, na.rm = TRUE))

total_median <- merge(median_post, median_non, by = "Year")

total_median %>%
  ggplot(aes(Year)) +
  geom_point(aes(Year, median_post), color = "red", shape = 17, size = 2) +
  geom_point(aes(Year, median_non), color = "blue", shape = 16, size = 2) +
  labs(title = "Median Ideal Point bewteen Post/Non SU", 
       x = "Year",
       y = "Median Ideal Point") +
  annotate("text", x = 2005, y = -0.6, label = "Non-Soviet", color = "blue", size = 4) +  
  annotate("text", x = 2005, y = 0.5, label = "Post-Soviet", color = "red", size = 4) +
  annotate("text", x = 1989, y = 1, label = "Fall of Berlin Wall", color = "Black", size = 4) +
  geom_vline(xintercept = 1989, color = "black")
  

 
  


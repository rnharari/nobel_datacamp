



library(tidyverse)
library(dplyr)
library(ggplot2)


nobel<- read_csv("nobel.csv")
write.csv(nobel, "nobel.csv")

# Taking a look at the contents of the dataset
glimpse(nobel)

# Taking a look at the first couple of winners
head(nobel,2 )

# 2. So, who gets the Nobel Prize?
# Just looking at the first couple of prize winners, or Nobel laureates as they are also called, we already see a celebrity: Wilhelm Conrad Röntgen, the guy who discovered X-rays. And actually, we see that all of the winners in 1901 were guys that came from Europe. But that was back in 1901, looking at all winners in the dataset, from 1901 to 2016, which sex and which country is the most commonly represented?

####################### Counting the number of (possibly shared) Nobel Prizes handed
# out between 1901 and 2016
nobel %>% count()

####################### Counting the number of prizes won by male and female recipients.
nobel %>% group_by(Sex) %>% count()

####################### Counting the number of prizes won by different nationalities.
nobel %>% group_by(`Birth Country`)%>% count()

####################### Calculating the proportion of USA born winners per decade
# Not so surprising perhaps: the most common Nobel laureate between 1901 and 2016 was a man born in the United States of America. But in 1901 all the laureates were European. When did the USA start to dominate the Nobel Prize charts?
# use floor function to round down the year and get decade
# getting false true we can compute mean and get proportion
prop_us<- nobel %>%
  mutate(us_born= `Birth Country`== "United States of America", decades= floor(Year/10)*10)%>%
  group_by(decades)%>% summarise(prop=mean(us_born, na.rm=T))
prop_us

####################### USA dominance, visualized

# A table is OK, but to see when the USA started to dominate the Nobel charts we need a plot.
# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)
prop_us %>% ggplot(aes(decades, prop))+ geom_line()+ geom_point()+ ylab("Us born proportation")+ theme_minimal()



####################### 5. What is the gender of a typical Nobel Prize winner?
# So the USA became the dominating winner of the Nobel Prize first in the 1930s and has kept the leading position ever since. But one group that was in the lead from the start, and never seems to let go, are men. Maybe it shouldn't come as a shock that there is some imbalance between how many male and female prize winners there are, but how significant is this imbalance? And is it better or worse within specific prize categories like physics, medicine, literature, etc.?
# Calculating the proportion of female laureates per decade
prop_female<- nobel %>% mutate(female = Sex== "Female", decade= floor(Year/10)*10)%>%
  group_by(decade)%>% summarise(prop_female= mean(female, na.rm = T))


# Plotting the proportion of female laureates per decade
# Setting the size of plots in this notebook
prop_female %>% ggplot(aes(decade, prop_female))+ geom_line()


# Plotting the proportion of female laureates per decade in different categories 
# Setting the size of plots in this notebook
prop_female_cat <- nobel %>% mutate(female = Sex== "Female", decade= floor(Year/10)*10)%>%
  group_by(decade, Category)%>% summarise(prop_female2= mean(female, na.rm=T))


prop_female_cat %>% ggplot(aes(decade, prop_female2, col= Category))+ geom_line()


####################### The first woman to win the Nobel Prize

# The plot above is a bit messy as the lines are overplotting. But it does show some interesting trends and patterns. Overall the imbalance is pretty large with physics, economics, and chemistry having the largest imbalance. Medicine has a somewhat positive trend, and since the 1990s the literature prize is also now more balanced. The big outlier is the peace prize during the 2010s, but keep in mind that this just covers the years 2010 to 2016.
# 
# Given this imbalance, who was the first woman to receive a Nobel Prize? And in what category?

# Picking out the first woman to win a Nobel Prize
head(nobel %>% filter(Sex== "Female"), 1)


#######################  How old are you when you get the prize?
# But how old are you generally when you get the prize?
library(lubridate)

# Fixing dates in the dataset
typeof(nobel$Year)# it should be date
nobel_year <- nobel %>% mutate(year_date= year(as.Date(as.character(Year), "%Y"))) # turn non-year data to year data
#year is a function from lubridate
# make sure write %Y not %y!

# Calculating the age of Nobel Prize winners
nobel_age<- nobel_year%>%
  mutate(birth_year= year(as.Date(as.character(nobel$`Birth Date`), "%Y")), age= year_date- birth_year)


nobel_age %>% ggplot(aes(year_date, age))+ geom_point()+ geom_smooth()
  
        
#######################
# Age differences between prize categories
# 
# The plot above shows us a lot! We see that people use to be around 55 when they received the price, but nowadays the average is closer to 65. But there is a large spread in the laureates' ages, and while most are 50+, some are very young.
# 
# We also see that the density of points is much high nowadays than in the early 1900s -- nowadays many more of the prizes are shared, and so there are many more winners. We also see that there was a disruption in awarded prizes around the Second World War (1939 - 1945).
# 
# Let's look at age trends within different prize categories.

# Same plot as above, but faceted by the category of the Nobel Prize


nobel_age %>% ggplot(aes(year_date, age))+ geom_point()+ geom_smooth()+ facet_wrap(~Category)



############################
# Oldest and youngest winners
# 
# We see that both winners of the chemistry, medicine, and physics prize have gotten older over time. The trend is strongest for physics: the average age used to be below 50, and now it's almost 70. Literature and economics are more stable, and we also see that economics is a newer category. But peace shows an opposite trend where winners are getting younger! </p>
# 
# In the peace category we also a winner around 2010 that seems exceptionally young. This begs the questions, who are the oldest and youngest people ever to have won a Nobel Prize?


# The oldest winner of a Nobel Prize as of 2016
oldest<- nobel_age %>% arrange(age) %>% top_n(1)
youngest<- nobel_age%>% top_n(1, desc(age))








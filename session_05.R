#Loading libraries 
library(tidyverse)
library(readxl)

#Reading in data per usual
cb_data <- read_xlsx(path = "CB_data_2023.xlsx",
                     skip = 11,
                     na = "-99")

#dplyr continued 
#Next verb: mutate
#Use mutate to create new variables (columns)
#Here creating Weight variable in kg instead of grams
cb_data %>% 
  mutate(Weight_kg = Weight / 1000) %>% 
  select(Weight, Weight_kg) %>% 
  view()

#Create multiple new columns at once
cb_data %>% 
  mutate(grams_per_m3 = Weight / Volume, 
         grams_per_m3_sq = grams_per_m3^2,
         grams_per_m3_sq_log2 = log2(grams_per_m3_sq)) %>% 
  view()
  
#Returning to the issue regarding the wonky weight obs
ggplot(cb_data, 
       aes(x = Age_in_days,
           y = Weight,
           color = Observer)) +
  geom_point()

#Lets make two plots
#One with only "raID-07"
#One with all other Observers
#plot side-by-side
p1 <- cb_data %>% 
  filter(Observer == "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight)) +
  geom_point()

p2 <- cb_data %>% 
  filter(Observer != "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight)) +
  geom_point()

cowplot::plot_grid(p1, p2)

#Use mutate() to create new variable fixing the issue
#ifelse() helps create variable based on values of other variable
cb_data_fixed <- cb_data %>% 
  mutate(Weight_fixed = ifelse(Observer == "raID-07",
                               Weight * 1000, 
                               Weight))

#plot new variable
cb_data_fixed %>%
  ggplot(aes(x = Age_in_days,
             y = Weight_fixed,
             color = Observer)) +
  geom_point()

#Steps piped together
cb_data %>% 
  mutate(Weight_fixed = ifelse(Observer == "raID-07",
                               Weight * 1000, 
                               Weight)) %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight_fixed,
             color = Observer)) +
  geom_point()

#Mutate adds new variables at the end of tibble
#Transmute keeps only new variables
cb_data %>% 
  transmute(WV_sum = Weight + Volume,
            WV_diff = Weight - Volume)

#What happens here?
cb_data %>% 
  mutate(mean_Age = mean(Age_in_days)) %>% 
  view()

#Next verb is summarise (or summarize)
#Used to calculate summary statistics
cb_data %>% 
  summarise(mean(Age_in_days))

#Output can be named (column name added in output)
cb_data %>% 
  summarise(mean_Age = mean(Age_in_days))

#Summarise is most useful together with group_by
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean_Age = mean(Age_in_days))

#Multiple summary stats can be calculated at the same time
#n() function counts number of obs per group
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean_Age = mean(Age_in_days),
            stdev_Age = sd(Age_in_days),
            n = n())

#Sometimes we want to use summarise to summarize summaries
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean_Age = mean(Age_in_days)) %>% 
  summarise(median(mean_Age))

#summarise can be used with logical operations
#The sum() functions counts number of obs that are "TRUE"
#TRUE/FALSE variables can be interpreted as numeric 
#TRUE = 1, FALSE = 0
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(sum(Age_in_days > 10))

#mean() gives proportion of "TRUE"
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean(Age_in_days > 10))

#We can also group by multiple variables
#In this case groups are now all combinations of phase and observer 
cb_data %>% 
  group_by(`Phase (color)`, Observer) %>% 
  summarise(mean(Age_in_days)) %>% 
  view()

#What happens here?
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean(Volume))

#NAs are "contagious"; if any obs NA calculations will be NA
#Solution with filter
cb_data %>% 
  filter(!is.na(Volume)) %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean(Volume))

#Use na.rm (na remove) to change default behavior
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean(Volume, na.rm = TRUE))

#group_by can be used with other functions than summarise
#Used with mutate can be helpful if summary + original data is wanted
cb_data %>% 
  group_by(Observer) %>% 
  mutate(mean_Vol = mean(Volume, na.rm = TRUE)) %>% 
  view()

cb_data %>% 
  group_by(Observer) %>% 
  count()

cb_data %>% 
  group_by(Observer) %>% 
  filter(n() > 150)

 

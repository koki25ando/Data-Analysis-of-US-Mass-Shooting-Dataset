library(tidyverse) #for data cleaning and better visualizations
library(data.table) # to import handle dataset faster
library(maps) # for map visualization
library(stringr) # to manipulate character type data variables
library(plotly) # for interactive visualizations
library(DT) # interactive data table

shoot <- fread("Mass Shootings Dataset Ver 5.csv", data.table = F)
shoot <- shoot[,-1]
shoot$Date <- as.Date(shoot$Date, "%m/%d/%Y")
names(shoot)[11] <- "Total_victims"
names(shoot)[16] <- "Mental_Health_Issues"
shoot$Age <- as.numeric(shoot$Age)

shoot %>% head()

world.map <- map_data("state")
ggplot() + 
  geom_map(data=world.map, map=world.map,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") + 
  geom_point(data = shoot, 
             aes(x = Longitude, y = Latitude, size = Fatalities), 
             colour = "red", alpha = .6) +
  xlim(-130, -65) + ylim(25,50) +
  labs(title = "Mass Shootings that happened from 1966 to 2017")

shoot <- shoot %>% 
  separate(Location, into = c("City", "State"), sep = ", ")

pattern <- c("TX|CO|MD|NV|CA|PA|WA|LA")
replacement <- c("Texas", "Colorado", "Maryland", "Nevada", "California", "Pennsylvania", "Washington", "Los Angels")
shoot$State <- shoot$State %>% 
  str_replace_all(pattern = pattern, 
                  replacement = replacement)
shoot$State <- shoot$State %>% str_replace_all(c("Texas " = "Texas", " Virginia" = "Virginia"))
shoot$State <- as.factor(shoot$State)

state_bar <- as.data.frame(table(shoot$State)) %>% 
  ggplot(aes(x = reorder(Var1, -Freq), y= Freq, fill = Var1)) + 
  geom_bar(stat = "identity", show.legend=F) + 
  labs(x = "State", y = "Count") + 
  theme(axis.text.x = element_text(angle = 90))
ggplotly(state_bar)

shoot <- shoot %>% 
  mutate(Year = year(Date), 
         Month = month(Date),
         Weekday = weekdays(Date),
         Week = week(Date),
         WeekMonth = 1+Week-min(Week),
         weekdaynumber = 
           case_when(Weekday == "Monday" ~ 1, Weekday == "Tuesday" ~ 2, Weekday == "Wednesday" ~ 3,
                     Weekday == "Thursday" ~ 4, Weekday == "Friday" ~ 5, Weekday == "Saturday" ~ 6,
                     Weekday == "Sunday" ~ 7),
         MonthChar = 
           case_when(Month == 1 ~ "Jan", Month == 2 ~ "Feb", Month == 3 ~ "Mar", Month == 4 ~ "Apr", Month == 5 ~ "May", Month == 6 ~ "Jun",  
                     Month == 7 ~ "Jul", Month == 8 ~ "Aug", Month == 9 ~ "Sep", Month == 10 ~ "Oct", Month == 11 ~ "Nov", Month == 12 ~ "Dec"))

shoot %>% 
  plot_ly(x = ~ Month, y = ~Year, z = ~Total_victims, 
          type = "heatmap", mode = 'makers', 
          hoverinfo = 'text',
          text = ~paste(Year, "/",MonthChar,
                        ' ', Total_victims, 'victims', 
                        '"', Title, '"'))

shoot %>% 
  filter(Title != "Las Vegas Strip mass shooting") %>%  
  plot_ly(x = ~ Month, y = ~Year, z = ~Total_victims, 
          type = "heatmap", mode = 'makers', 
          hoverinfo = 'text',
          text = ~paste(Year, "/",MonthChar,
                        ' ', Total_victims, 'victims', 
                        '"', Title, '"'))

shoot %>% 
  filter(Title != "Las Vegas Strip mass shooting") %>%
  arrange(desc(Total_victims)) %>% 
  select(Summary, Title:Date, -City, Age) %>% 
  head(5) %>% 
  datatable(options = 
              list(pageLength = 5,
                   lengthMenu = c(1,5)))


distribution <- shoot %>% 
  ggplot(aes(x = Age)) + 
  geom_histogram(col = "red", fill = "pink") + 
  labs(title = "Age Distibition of Criminals")
ggplotly(distribution)

shoot <- shoot %>% 
  mutate(Decade = (case_when(Year >= 1960 & Year < 1970 ~ "1960s", 
                             Year >= 1970 & Year < 1980 ~ "1970s", 
                             Year >= 1980 & Year < 1990 ~ "1980s", 
                             Year >= 1990 & Year < 2000 ~ "1990s", 
                             Year >= 2000 & Year < 2010 ~ "2000s", 
                             Year >= 2010 & Year < 2020 ~ "2010s")))

decade_boxplot <- shoot %>% 
  ggplot(aes(x = Decade, y = Age, fill = Decade)) +
  geom_boxplot() + 
  labs(x = "Each Decade", title = "Age Distribution of Each Dacade")
ggplotly(decade_boxplot)

pattern2 = c("black|Some other race|white")
replacement2 = c("Black", "Other", "White")
shoot$Race <- shoot$Race %>% 
  str_replace(pattern = pattern2, replacement = replacement2)

transition <- shoot %>% 
  ggplot(aes(x = Year, fill = Race)) + 
  geom_bar() +
  labs(title = "Transition of the races of criminals")
ggplotly(transition) %>% layout(showlegend = FALSE)

decade_trasition_race <- shoot %>% 
  ggplot(aes(x = Decade, fill = Race)) + 
  geom_histogram(stat = "count", show.legend = F, 
                 position = "fill") + 
  labs(title = "Transition of the Ratio of the Races of Criminals by Each Decade")
ggplotly(decade_trasition_race) %>% layout(showlegend = FALSE)

as.data.frame(table(shoot$Mental_Health_Issues))[-1,] %>%
  plot_ly(labels = ~Var1, values = ~Freq, type = 'pie') %>%
  layout(title = 'What were the mental health issues?',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


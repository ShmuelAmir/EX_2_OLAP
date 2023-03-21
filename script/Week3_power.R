library(dplyr)
library(ggplot2)
#library(plotly)
#library(lubridate)

#pdf('output/Week3_power.pdf')


# ---------- INPUT DATA ---------- #

A <- read.delim('data/table.tsv')

# add more readable DateTime columns,
# individual columns for date and hour
# filter to the week of 7 - 14 Feb. 2021,
# and order by DateTime
B <- A %>%
  mutate(DateTime = as.POSIXct(megawatthours, tz = "EST", format = "%H:%M EST %m/%d/%Y")) %>%
  mutate(Date = as.Date(DateTime), Hour = format(DateTime, format = "%H")) %>%
  filter(Date >= as.Date("2021-02-07"),
         Date <= as.Date("2021-02-13")) %>%
  arrange(DateTime)
  
  
# ---------- Q1 ---------- #

# Date vs mean of total net generation
C <- B %>%
  mutate(Total.net.generation = 
         Net.generation +
         Net.generation.1 +
         Net.generation.2 +
         Net.generation.3 +
         Net.generation.4 +
         Net.generation.5 +
         Net.generation.6 +
         Net.generation.7 +
         Net.generation.8 +
         Net.generation.9 +
         Net.generation.10
  ) %>%
  group_by(Date) %>%
  summarize(Mean_Power = mean(Total.net.generation))

ggplot(C, aes(x = Date, y = Mean_Power), xla) +
  xlab("Date (7-14 Feb)") +
  ylab("Mean Net Generation") +
  geom_line() + 
  ggtitle("Mean daily power generation") +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE)


# ---------- Q2 ---------- #
# גרף אחד ליום וגרף אחד ללילה לפי דקות. רק לשבוע הנ"ל.
# להכל צריך את נוסחת הרגרסיה
# data.frame חדש
# cube = matrix


# combine east coast power demand 
D <- B %>%
  mutate(east_coast_demand = Demand.9 + Demand.7 + Demand.5 + Demand.4 + Demand.2) %>%
  mutate(hour_as_num = as.numeric(Hour)) %>%
  mutate(Day_or_night = case_when(hour_as_num >= 10 & hour_as_num <= 18 ~ "Day",
                                  hour_as_num >= 20 | hour_as_num <= 3 ~ "Night")) %>%
  filter(Day_or_night == "Day" | Day_or_night == "Night") %>%
  group_by(Date, Day_or_night) %>%
  summarize(minut_power_demand = sum(east_coast_demand))
  
ggplot(D, aes(x = Date, y = minut_power_demand)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE) +
  facet_wrap(~ Day_or_night)

#dev.off()

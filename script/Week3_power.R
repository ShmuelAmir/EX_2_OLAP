library(dplyr)
library(ggplot2)
library(lubridate)

#pdf('output/Week3_power.pdf')
Sys.setlocale("LC_TIME", "English")

# ---------- INPUT DATA ---------- #

electric_data_frame <- read.delim('data/table.tsv')

slice.electric_data_frame <- electric_data_frame %>%
  mutate(DateTime = as.POSIXct(megawatthours, tz = "EST", format = "%H:%M EST %m/%d/%Y")) %>%
  mutate(Date = as.Date(DateTime), Hour = format(DateTime, format = "%H")) %>%
  select(Date, Hour, DateTime, 
         starts_with("Net.generation"),
         Demand.9, Demand.7, Demand.5, Demand.4, Demand.2
  ) %>%
  filter(Date >= as.Date("2021-02-07"), Date <= as.Date("2021-02-13")) %>%
  arrange(DateTime)

net.generation.df <- slice.electric_data_frame %>%
  select(starts_with("Net.generation"))

electric_fact <- as.matrix(net.generation.df)

net.generation.names.list <- as.list(
  sapply(names(slice.electric_data_frame)[4:14], function(x) x = NA))

# mean values
M <- net.generation.names.list
# standard deviations
S <- net.generation.names.list
# linear fit
LM <- net.generation.names.list

# calculate means and stdev
for (i in 1:length(names(net.generation.names.list))) {
  curr.net.generation <- net.generation.df[, i]
  M[[ i ]] <- mean( curr.net.generation )
  S[[ i ]] <- sd( curr.net.generation )
}

# scale and center the consumption series (normalize)
norm.net.generation.df <- t( (t(net.generation.df) - unlist(M)) / unlist(S) )
print(norm.net.generation.df)

# לתקן את אזור הזמן  ??????????
# add columns: DateTime, Date and Hour,
# select only to be used columns,
# filter to the week of 7 - 14 Feb. 2021,
# and order by DateTime
B <- A %>%
  mutate(DateTime = as.POSIXct(megawatthours, tz = "EST", format = "%H:%M EST %m/%d/%Y")) %>%
  mutate(Date = as.Date(DateTime), Hour = format(DateTime, format = "%H")) %>%
  select(Date, Hour, DateTime, 
         starts_with("Net.generation"),
         Demand.9, Demand.7, Demand.5, Demand.4, Demand.2
        ) %>%
  filter(Date >= as.Date("2021-02-07"), # slice
         Date <= as.Date("2021-02-13")) %>%
  arrange(DateTime)
  
  
# ---------- Q1 ---------- #

# sum Net.generation across US
# and calculate the mean for every day
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

# draw the chart with the regeneration line.
ggplot(C, aes(x = Date, y = Mean_Power)) +
  xlab("Date (7-14 Feb)") +
  ylab("Mean Net Generation") +
  geom_line() + 
  ggtitle("Mean daily power generation") +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se=FALSE)


# ---------- Q2 ---------- #

# combine east coast power demand columns
# and sum the demand for each date 
# for day and night.
D <- B %>%
  mutate(east_coast_demand = Demand.9 + Demand.7 + Demand.5 + Demand.4 + Demand.2) %>%
  mutate(hour_as_num = as.numeric(Hour)) %>%
  mutate(Day_or_night = case_when(hour_as_num >= 10 & hour_as_num <= 18 ~ "Day",
                                  hour_as_num >= 20 | hour_as_num <= 3 ~ "Night")) %>%
  filter(Day_or_night %in% c("Day", "Night")) %>%
  group_by(hour_as_num, Day_or_night) %>%
  summarize(minut_power_demand = sum(east_coast_demand))

# find linear line for day and night
D.day <- filter(D, Day_or_night == "Day")
D.night <- filter(D, Day_or_night == "Night")

# remove outliers (if D.day[:7,] the correlation increases even more)
coef_lm.day <- coef(lm(minut_power_demand ~ hour_as_num, data = D.day[1:8, ]))
intercept.day <- coef_lm.day[1]
slope.day <- coef_lm.day[2]

coef_lm.night <- coef(lm(
  minut_power_demand ~ factor(hour_as_num, levels = c(10:23, 0:3)), 
  data = D.night))
intercept.night <- coef_lm.night[1]
slope.night <- coef_lm.night[2]

# draw the charts with the regeneration line.
ggplot(D.day, aes(x = hour_as_num, y = minut_power_demand)) +
  xlab("Time") +
  ylab("Demand") + 
  ggtitle("Minute power demand in the east coast - Day") +
  geom_point() +
  geom_abline(intercept = intercept.day, slope = slope.day)

ggplot(D.night, aes(x = factor(hour_as_num, levels = c(10:23, 0:3)), y = minut_power_demand)) +
  xlab("Time") +
  ylab("Demand") + 
  ggtitle("Minute power demand in the east coast - Night") +
  geom_point() +
  geom_abline(intercept = intercept.night, slope = slope.night)


dev.off()

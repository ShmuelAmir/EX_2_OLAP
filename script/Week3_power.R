library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)


# ---------- INPUT DATA ---------- #

A <- read.delim('data/table.tsv')

# arrange the data
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", format = "%H:%M EST %m/%d/%Y" )
B <- A[ order(A$DateTime), ]


# ---------- Q1 ---------- #

# filter the requested week
C <- B[B$DateTime >= as.POSIXct("2021-02-07", tz = "EST") 
       & B$DateTime <= as.POSIXct("2021-02-13 23:59:59", tz = "EST") ,]

# sum net generation across the US
net_generation_cols <- c(
  "Net.generation",
  "Net.generation.1",
  "Net.generation.2",
  "Net.generation.3",
  "Net.generation.4",
  "Net.generation.5",
  "Net.generation.6",
  "Net.generation.7",
  "Net.generation.8",
  "Net.generation.9",
  "Net.generation.10"
)
C$Total_net_generation <- rowSums(C[,net_generation_cols])

# prepare the axis
C <- C %>%
    group_by(date = as.Date(DateTime)) %>%
    summarise(mean_net_generation = mean(Total_net_generation))


# ---------- Q2 ---------- #

# combine east coast power demand
east_coast_cols <- c("Demand.9", "Demand.7", "Demand.5", "Demand.4", "Demand.2")

D <- B
D$east_coast_demand <- rowSums(D[ , east_coast_cols ])

E <- na.omit(data.frame(hour = as.numeric(format(as.POSIXct(D$DateTime), format = "%H"))
                        , Demand = D$east_coast_demand))

# group by day and night
E <- E %>%
  mutate(period = case_when(hour >= 10 & hour <= 18 ~ "day",
                            hour >= 20 | hour <= 3 ~ "night")) %>%
    group_by(period) %>%
    summarise(demand = sum(Demand))

E <- na.omit(E)


# ---------- OUTPUT PLOTS ---------- #

#pdf('output/Week3_power.pdf')

# draw the first plot
#ggplot(C, aes(x = date, y = mean_net_generation)) + 
 # geom_line() +
#  labs(x = "Day", y = "Mean Net generation") +
  #scale_x_date(date_labels = "%d", date_breaks = "1 day") +
 # ggtitle("Mean daily power generation across US, 7-14 Feb")

# draw the first plot
ggplot(data=E, aes(x=period, y=demand, fill=period)) +
  geom_bar(stat = "identity")
#ggplotly(bar)

#dev.off()

library(dplyr)
library(ggplot2)

A <- read.delim('data/table.tsv')

A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", format = "%H:%M EST %m/%d/%Y" )
B <- A[ order(A$DateTime), ]

B <- B[B$DateTime >= as.POSIXct("2021-02-07", tz = "EST") 
       & B$DateTime <= as.POSIXct("2021-02-13 23:59:59", tz = "EST") ,]

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
B$Total_net_generation <- rowSums(B[,net_generation_cols])

C <- B %>%
    group_by(date = as.Date(DateTime)) %>%
    summarise(mean_net_generation = mean(Total_net_generation))

ggplot(C, aes(x = date, y = mean_net_generation)) +
  geom_line() +
  labs(x = "Day", y = "Mean Net generation") +
  scale_x_date(date_labels = "%d", date_breaks = "1 day")


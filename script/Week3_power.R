library(dplyr)
library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)
library(stringr)


pdf('output/Week3_power.pdf')

Sys.setlocale("LC_TIME", "English")


# ---------- Load the data ---------- #

origianl_data_frame <- read.delim("data/table.tsv")


# ---------- Rearrange the data ---------- #

# rows to columns
df_list <- lapply(0:10, function(i) {
  start_idx <- i * 5 + 3
  end_idx <- start_idx + 3

  df_loc <- origianl_data_frame[, c(1, start_idx:end_idx)]
  colnames(df_loc)[2:5] <- c("Demand", "Day.ahead.demand.forecast", "Net.generation", "Total.interchange")
  df_loc$location <- colnames(origianl_data_frame)[start_idx - 1]
  df_loc
})

electric_fact <- do.call(rbind, df_list)

electric_fact <- electric_fact %>%
  mutate(
    DateTime = as.POSIXct(
      megawatthours,
      tz = "EST",
      format = "%H:%M EST %m/%d/%Y"
    )
  ) %>%
  mutate(Date = as.Date(DateTime), Hour = format(DateTime, format = "%H")) %>%
  arrange(DateTime)


# ========== Question 1 ========== #

# ---------- Estimate means, variance ---------- #

# create a data cube
electric_cube <-
  tapply(electric_fact$Net.generation,
    electric_fact[, c("DateTime", "location")],
    FUN = function(x) sum(x, na.rm = TRUE)
  )

# adjust times: -3 and -1 by moving the cells
for (loc in c("PACW", "CISO", "BPAT")) {
  electric_cube[, loc] <- c(electric_cube[seq(4, length(electric_cube[, loc])), loc], NA, NA, NA)
}

for (loc in c("MISO", "ERCO")) {
  electric_cube[, loc] <- c(electric_cube[seq(2, length(electric_cube[, loc])), loc], NA)
}

# rollup by date
new_df <- data.frame(DateTime = row.names(electric_cube), electric_cube, check.names = FALSE) %>% 
  mutate(Date = as.Date(DateTime)) %>% 
  select(-DateTime) %>% 
  group_by(Date) %>% 
  summarise_all(sum)
  
rollup.electric_cube <- as.matrix(new_df[, -1])
rownames(rollup.electric_cube) <- as.character(new_df$Date)
colnames(rollup.electric_cube) <- names(new_df)[-1]

weekOfFeb <- c("2021-02-07", "2021-02-08", "2021-02-09", "2021-02-10", "2021-02-11", "2021-02-12", "2021-02-13", "2021-02-14")

slice.rollup.electric_cube <- rollup.electric_cube[weekOfFeb, ]


# ---------- Chart - date to net.generation and mean line ---------- #

means <- apply(slice.rollup.electric_cube, 1, mean)

plot_df <- data.frame(date = as.Date(weekOfFeb), mean.net.generation = means)

# mean of sum
ggplot(plot_df, aes(date, mean.net.generation)) +
  xlab("Date (7-14 Feb)") +
  ylab("Mean Net Generation") +
  geom_line() +
  ggtitle("Mean daily power generation") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  scale_x_date(date_breaks = "1 day")

# mean across all
electric_fact %>% 
  select(Date, Net.generation) %>% 
  filter(Date %in% as.Date(weekOfFeb)) %>%
  group_by(Date)%>% 
  summarize(means = mean(Net.generation)) %>%
ggplot(aes(Date, means)) +
  xlab("Date (7-14 Feb)") +
  ylab("Mean Net Generation") +
  geom_line() +
  ggtitle("Mean daily power generation") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  scale_x_date(date_breaks = "1 day")


# ========== Question 2 ========== # רגרסיה רק לשאלה 2 להעביר לחישוב מטריצות להראות נוסחה שיעבוד לפי דקה
#  חישוב נפרד לכל תחום של שעות (שעות לפי אינדקס?)

east_coast <- c("PJM", "NYIS", "ISNE", "FPL", "CPLE")
day <- c("10", "11", "12", "13", "14", "15", "16", "17", "18")
night <- c("20", "21", "22", "23", "0", "1", "2", "3")
night_pad_zero <- str_pad(night, 2, pad = 0)

# create a data cube
demand_cube <-
  tapply(electric_fact$Demand,
         electric_fact[, c("DateTime", "Hour", "location")],
         FUN = function(x) sum = sum(x, na.rm = TRUE)
  )

drilldown.demand_cube <- apply(
  demand_cube, 
  c("Hour", "location"), 
  FUN = function(x) sum(x, na.rm = TRUE)
)

dice.drilldown.demand_cube <- drilldown.demand_cube[c(day, night_pad_zero), east_coast]

sum_by_hour <- apply(dice.drilldown.demand_cube, 1, sum)
day_demand <- sum_by_hour[day]
night_demand <- sum_by_hour[night_pad_zero]


# ---------- Calculate regression fit ---------- #

day.U <- matrix(c(rep(1, 9), seq(0, 9-1)), ncol = 2)
night.U <- matrix(c(rep(1, 8), seq(0, 8-1)), ncol = 2)

day.U_T <- t(day.U)
night.U_T <- t(night.U)

day.result <- solve(day.U_T %*% day.U) %*% day.U_T %*% day_demand
night.result <- solve(night.U_T %*% night.U) %*% night.U_T %*% night_demand


# day
intercept.day <- day.result[1]
slope.day <- day.result[2]

day_demand_df <- data.frame(hours = day, demand = day_demand)

day_plot <- ggplot(day_demand_df, aes(hours, demand)) +
  xlab("Hours") +
  ylab("Demand") +
  ggtitle("Day") +
  geom_point() +
  geom_abline(intercept = intercept.day, slope = slope.day)


# night
intercept.night <- night.result[1]
slope.night <- night.result[2]

night_demand_df <- data.frame(hours = night, demand = night_demand)

night_plot <- ggplot(night_demand_df, aes(factor(night, levels = c(10:23, 0:3)), demand)) +
  xlab("Hours") +
  ylab("Demand") +
  ggtitle("Night") +
  geom_point() +
  geom_abline(intercept = intercept.night, slope = slope.night)

grid.arrange(day_plot, night_plot, ncol = 2, top = textGrob("Minute power demand in the east coast"))

dev.off()

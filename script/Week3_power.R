library(dplyr)
library(ggplot2)
library(lubridate)

# pdf('output/Week3_power.pdf')
Sys.setlocale("LC_TIME", "English")


# ---------- Load the data ---------- #

origianl_data_frame <- read.delim("data/table.tsv")


# ---------- Rearrange the data ---------- #

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

electric_cube <-
  tapply(electric_fact$Net.generation,
    electric_fact[, c("Date", "location")],
    FUN = function(x) sum = sum(x, na.rm = TRUE)
  )

weekOfFeb <- c("2021-02-07", "2021-02-08", "2021-02-09", "2021-02-10", "2021-02-11", "2021-02-12", "2021-02-13")

dice.electric_cube <- electric_cube[weekOfFeb, ]

locations <- unique(electric_fact[,"location"])

M <- as.list(setNames(rep(NA, length(locations)), locations))
S <- as.list(setNames(rep(NA, length(locations)), locations))

for ( i in 1:length(M) ) {
  M[[ i ]] <- mean( dice.electric_cube[  ,i ] )
  S[[ i ]] <- sd( dice.electric_cube[ , i ] )
}

norm.electric_cube <- t( (t(dice.electric_cube) - unlist(M)) / unlist(S) )

# ---------- Calculate regression fit ---------- #

U <- matrix(c(rep(1, length(weekOfFeb)), seq(1, length(weekOfFeb))), ncol = 2)
U_T <- t(U)

mtx <- solve(U_T %*% U) %*% U_T 
plot(1, type="n", xlab="", ylab="", xlim = c(0,12), ylim=c(-2, 2))
for ( i in 1:length(M) ) {
  LM <- a %*% norm.electric_cube[,i]

  a <- LM[[1]]
  b <- LM[[2]]
  
  abline(a, b, col = i, lw =2)
}


# ggplot(q1, aes(Date, mean.net.generation, color = location)) +
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x, se=FALSE) +
#  facet_wrap(~ location, ncol = 2, scales = "free_y") +
#  theme(legend.position="bottom")


# ========== Question 2 ========== #

q2 <- slice.electric_data_frame %>%
  filter(location %in% c("PJM", "NYIS", "ISNE", "FPL", "CPLE")) %>%
  mutate(hour_as_num = as.numeric(Hour)) %>%
  mutate(Day_or_night = case_when(
    hour_as_num >= 10 & hour_as_num <= 18 ~ "Day",
    hour_as_num >= 20 | hour_as_num <= 3 ~ "Night"
  )) %>%
  filter(Day_or_night %in% c("Day", "Night")) %>%
  group_by(hour_as_num, Day_or_night) %>%
  summarize(minut_power_demand = sum(Demand, na.rm = T))

ggplot(q2, aes(hour_as_num, minut_power_demand)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~Day_or_night, scales = "free")




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

slice.electric_data_frame <- electric_data_frame %>%
  mutate(DateTime = as.POSIXct(megawatthours, tz = "EST", format = "%H:%M EST %m/%d/%Y")) %>%
  mutate(Date = as.Date(DateTime), Hour = format(DateTime, format = "%H")) %>%
  select(
    Date, Hour, DateTime,
    starts_with("Net.generation"),
    Demand.9, Demand.7, Demand.5, Demand.4, Demand.2
  ) %>%
  filter(Date >= as.Date("2021-02-07"), Date <= as.Date("2021-02-13")) %>%
  arrange(DateTime)

net.generation.names.list <- as.list(
  sapply(names(slice.electric_data_frame)[4:14], function(x) x <- NA)
)

net.generation.df <- slice.electric_data_frame %>%
  select("Date", starts_with("Net.generation"))

rollup.net.generation.df <- net.generation.df %>%
  group_by(Date) %>%
  summarise_at(names(net.generation.names.list), mean)


electric_fact <- as.matrix(rollup.net.generation.df[, -1])

# mean values
M <- net.generation.names.list
# standard deviations
S <- net.generation.names.list
# linear fit
LM <- net.generation.names.list

# calculate means and stdev
for (i in 1:length(names(net.generation.names.list))) {
  curr.net.generation <- electric_fact[, i]
  M[[i]] <- mean(curr.net.generation)
  S[[i]] <- sd(curr.net.generation)
}

# scale and center the consumption series (normalize)
norm.electric_fact <- t((t(electric_fact) - unlist(M)) / unlist(S))

electric_cube <- data.frame(rollup.net.generation.df[, "Date"], norm.electric_fact)

ggplot(electric_cube, aes(x = Date, y = )) +
  geom_point()

# לתקן את אזור הזמן  ??????????
# add columns: DateTime, Date and Hour,
# select only to be used columns,
# filter to the week of 7 - 14 Feb. 2021,
# and order by DateTime
B <- A %>%
  mutate(DateTime = as.POSIXct(megawatthours, tz = "EST", format = "%H:%M EST %m/%d/%Y")) %>%
  mutate(Date = as.Date(DateTime), Hour = format(DateTime, format = "%H")) %>%
  select(
    Date, Hour, DateTime,
    starts_with("Net.generation"),
    Demand.9, Demand.7, Demand.5, Demand.4, Demand.2
  ) %>%
  filter(
    Date >= as.Date("2021-02-07"), # slice
    Date <= as.Date("2021-02-13")
  ) %>%
  arrange(DateTime)


# ---------- Q1 ---------- #

# sum Net.generation across US
# and calculate the mean for every day
C <- B %>%
  mutate(
    Total.net.generation =
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
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


# ---------- Q2 ---------- #

# combine east coast power demand columns
# and sum the demand for each date
# for day and night.
D <- B %>%
  mutate(east_coast_demand = Demand.9 + Demand.7 + Demand.5 + Demand.4 + Demand.2) %>%
  mutate(hour_as_num = as.numeric(Hour)) %>%
  mutate(Day_or_night = case_when(
    hour_as_num >= 10 & hour_as_num <= 18 ~ "Day",
    hour_as_num >= 20 | hour_as_num <= 3 ~ "Night"
  )) %>%
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
  data = D.night
))
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

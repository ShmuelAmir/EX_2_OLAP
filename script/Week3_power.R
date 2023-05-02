library(dplyr)
library(ggplot2)
library(lubridate)

pdf('output/Week3_power.pdf')

Sys.setlocale("LC_TIME", "English")


# ---------- functions ---------- #

# matrix linear model
mtx_lm <- function(dim, data_cube, len) {
  dim_len <- length(dim)
  
  # ---------- Estimate means, variance ---------- #
  M <- as.list(setNames(rep(NA, dim_len), dim))
  S <- as.list(setNames(rep(NA, dim_len), dim))
  
  for ( i in 1:length(M) ) {
    M[[ i ]] <- mean( data_cube[  ,i ] )
    S[[ i ]] <- sd( data_cube[ , i ] )
  }
  
  # ---------- Calculate regression fit ---------- #
  U <- matrix(c(rep(1, len), seq(1, len)), ncol = 2)
  U_T <- t(U)
  
  mtx <- solve(U_T %*% U) %*% U_T 
  
  result <- list(rep(0, length(M)))
  
  for ( i in 1:dim_len ) {
    result[[i]] <- mtx %*% data_cube[,i]
  }
  
  return(result)
}


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
    FUN = function(x) sum = sum(x, na.rm = TRUE)
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


# ---------- print the intercept and slope of linear model ---------- #
locations <- unique(electric_fact[,"location"])

print("regression fit: ")
print(mtx_lm(locations, slice.rollup.electric_cube, length(weekOfFeb)))


# ---------- Chart - date to net.generation and mean line ---------- #

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
night <- c("00", "01", "02", "03", "20", "21", "22", "23")

# create a data cube  !!!!!!!!!!!!pivot
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

dice.drilldown.demand_cube <- drilldown.demand_cube[c(day, night), east_coast]
day_demand_cube <- dice.drilldown.demand_cube[day,]
night_demand_cube <- dice.drilldown.demand_cube[night,]

mtx_lm(east_coast, day_demand_cube, length(day))
mtx_lm(east_coast, night_demand_cube, length(night))


par(mfrow=c(1,2))

# combine east coast power demand columns
# and sum the demand for each date
# for day and night.
east_coast_electric_fact <- electric_fact %>%
  filter(location %in% east_coast) %>%
  mutate(hour_as_num = as.numeric(Hour)) %>%
  mutate(Day_or_night = case_when(
    hour_as_num >= 10 & hour_as_num <= 18 ~ "Day",
    hour_as_num >= 20 | hour_as_num <= 3 ~ "Night"
  )) %>%
  filter(Day_or_night %in% c("Day", "Night")) %>%
  group_by(hour_as_num, Day_or_night) %>%
  summarize(minut_power_demand = sum(Demand, na.rm = TRUE))

# find linear line for day and night
D.day <- filter(east_coast_electric_fact, Day_or_night == "Day")
D.night <- filter(east_coast_electric_fact, Day_or_night == "Night")

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

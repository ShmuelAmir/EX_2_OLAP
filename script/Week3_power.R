# 316392323 Shmuel Amir
# GitHub link: https://github.com/ShmuelAmir/EX_2_OLAP

library(dplyr)
library(grid)

pdf('output/Week3_power.pdf')
# pdf('output/Week3_power.pdf') # in my workspace

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

electric_cube <- electric_cube[complete.cases(electric_cube), ]

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

sum_by_date <- apply(slice.rollup.electric_cube, 1, sum)
mean_line <- mean(sum_by_date)

plot(
  sum_by_date / 1000000, 
  type = "b", 
  main = "Daily power generation (in Milions)",
  ylab = "Net generation (in Millions)",
  xlab = "Date",
  xaxt = "n"
)
axis(side = 1, at = 0:8, labels = c("02/06", "02/07", "02/08", "02/09", "02/10", "02/11", "02/12", "02/13", "02/14"))
abline(h = mean_line / 1000000, col="red")


# ========== Question 2 ========== #

east_coast <- c("PJM", "NYIS", "ISNE", "FPL", "CPLE")
day <- c("10", "11", "12", "13", "14", "15", "16", "17", "18")
night <- c("20", "21", "22", "23", "0", "1", "2", "3")
night_pad_zero <- sprintf(paste0("%0", 2, "d"), as.numeric(night))

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

day_demand <- dice.drilldown.demand_cube[day,]
night_demand <- dice.drilldown.demand_cube[night_pad_zero,]


# ---------- Calculate regression fit ---------- #

# day
M <- apply(day_demand, MARGIN = 2, FUN = mean)
S <- apply(day_demand, MARGIN = 2, FUN = sd)

morn.day_demand <- t( (t(day_demand) - unlist(M)) / unlist(S) )

day.U <- matrix(c(rep(1, 9), seq(0, 9-1)), ncol = 2)

day.U_T <- t(day.U)

day.result <- solve(day.U_T %*% day.U) %*% day.U_T %*% day_demand
norm.day.result <- solve(day.U_T %*% day.U) %*% day.U_T %*% morn.day_demand

plot(1, type="n", main = "Minute power demand in the east coast - Day",
     xlab = "Hour", ylab = "Normalize demand", xlim = c(0,9), ylim=c(-2, 4), xaxt = "n")
axis(side = 1, at = 0:8, labels = day)

for (i in 1:length(colnames(day_demand))) {
  DF <- data.frame ( Hour = as.integer(day) - 10, Demand = morn.day_demand[ , i ] )
  lines( DF, col = i, type = 'b' )

  a <- norm.day.result[1,][[i]]
  b <- norm.day.result[2,][[i]]
  abline(a, b, col = i, lw =2)
}

legend( 'topleft', pch = 19,
        text.col = seq(1, length(colnames(day_demand))), 
        col = seq(1, length(colnames(day_demand))), 
        legend = paste(colnames(day_demand), ": ", 
                 "y = ", round(day.result[1,]), " + ", 
                 round(day.result[2,]), "x")
    )


# night
M.night <- apply(night_demand, MARGIN = 2, FUN = mean)
S.night <- apply(night_demand, MARGIN = 2, FUN = sd)

morn.night_demand <- t( (t(night_demand) - unlist(M.night)) / unlist(S.night) )

night.U <- matrix(c(rep(1, 8), seq(0, 8-1)), ncol = 2)
night.U_T <- t(night.U)

night.result <- solve(night.U_T %*% night.U) %*% night.U_T %*% night_demand
norm.night.result <- solve(night.U_T %*% night.U) %*% night.U_T %*% morn.night_demand

plot(1, type="n", main = "Minute power demand in the east coast - Night", 
     xlab = "Hour", ylab = "Normalize demand", xlim = c(0,8), ylim=c(-2, 2), xaxt = "n")
axis(side = 1, at = 0:7, labels = c("20", "21", "22", "23", "00", "01", "02", "03"))

for (i in 1:length(colnames(night_demand))) {
  hour <- if_else(as.integer(night) >= 20, as.integer(night)-20, as.integer(night)+4)
  DF <- data.frame ( Hour = hour, Demand = morn.night_demand[ , i ] )
  lines( DF, col = i, type = 'b' )

  a <- norm.night.result[1,][[i]]
  b <- norm.night.result[2,][[i]]
  abline(a, b, col = i, lw =2)
}

legend( 'topright', pch = 19, 
        text.col = seq(1, length(colnames(night_demand))),
        col = seq(1, length(colnames(night_demand))), 
        legend = paste(colnames(night_demand), ": ", 
                       "y = ", round(night.result[1,]), " + ", 
                       round(night.result[2,]), "x")
)


# save variables 
save(file = 'Week3_power.rdata', 
  electric_fact, electric_cube, rollup.electric_cube, slice.rollup.electric_cube,
  demand_cube, drilldown.demand_cube, dice.drilldown.demand_cube,
  weekOfFeb, day.U, day.U_T, day.result, night.U, night.U_T, night.result)

dev.off()

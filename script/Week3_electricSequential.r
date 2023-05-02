A <- read.delim('C:/Users/shmulik/Desktop/הנדסת תוכנה/שנה ג - תשפג/סמסטר ב/BIG DATA - נתוני עתק/תרגילים/EX_2_OLAP/data/table.tsv')
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
summary(A)

print(names(A))

B <- A[ order(A$DateTime), ]

print( as.integer(B[1, 'DateTime']) )
print( B[1, 'DateTime'] < B[5, 'DateTime'] )
print( c( B[1, 'DateTime'] , B[5, 'DateTime'] ) )
print (
  which ( B[, 'DateTime'] < as.POSIXct("2021-02-08 08:00:00 IST") &
	  B[, 'DateTime'] > as.POSIXct("2021-01-30 08:00:00 IST" ) )
  )


# analyze a time frame out of the power consumption data
rng <- 1497:1507

# print the range of times in our slice
print (B[rng, 'DateTime' ])

C <- with(B, cbind( Demand.1, Demand.2, Demand.3, Demand.4, Demand.5, Demand.6,
		    Demand.7 , Demand.8 , Demand.9 , Demand.10  ))
C <- C[rng, ]
print (C)


# mean values
M <- list( Demand.1 = NA, Demand.2 = NA, Demand.3 = NA, Demand.4 = NA, Demand.5 = NA, 
           Demand.6 = NA, Demand.7 = NA, Demand.8 = NA, Demand.9 = NA, Demand.10 = NA )
# standard deviations
S <- list( Demand.1 = NA, Demand.2 = NA, Demand.3 = NA, Demand.4 = NA, Demand.5 = NA,
           Demand.6 = NA, Demand.7 = NA, Demand.8 = NA, Demand.9 = NA, Demand.10 = NA )
# linear fit
LM <- list( Demand.1 = NA, Demand.2 = NA, Demand.3 = NA, Demand.4 = NA, Demand.5 = NA,
            Demand.6 = NA, Demand.7 = NA, Demand.8 = NA, Demand.9 = NA, Demand.10 = NA )

# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(0,12), ylim=c(-2, 2))

# calculate means and stdev
demands <- seq(10) # [ -c(1,4,8) ]
for ( i in demands ) {
  M[[ i ]] <- mean( C[ !is.na(C[, i]) ,i ] )
  S[[ i ]] <- sd( C[ !is.na(C[, i]), i ] )
}

# scale and center the consumption series (normalize)
norm.C <- t( (t(C) - unlist(M)) / unlist(S) )
print(norm.C)

demands <- seq(10)[ -c(1,4,5,7,10) ]
for ( i in demands ) {
   # rearrange in a new, temporary dataframe
   DF <- data.frame ( Time = rng - min(rng), Demand = norm.C[ , i ] )
   # plot
   lines( DF, col = i, type = 'b' )
   # linear fit
   LM[[ i ]] <- lm( Demand ~ Time, data = DF)
   a <- coef(LM[[ i ]])[1]
   b <- coef(LM[[ i ]])[2]
   abline(a, b, col = i, lw =2)
}
legend( 'bottomleft', col = demands, pch = 19,
       legend = sapply(demands, function(x) paste0('Demand.',x) ))

# mean regression line
lm.df <- sapply(LM[ -c(1,4,5,7,10) ], function(c) coef(c))
n <- dim(lm.df)[2]
tot <- rowSums(lm.df) 
a <- tot[1] / n
b <- tot[2] / n

# plot the mean regression line
abline(a, b, col = 'black', lw = 4, lt = 2)
time.min <- format(B[rng, 'DateTime' ][1], "%H" )
time.max <- format(B[rng, 'DateTime' ][length(rng)], "%H" )
title(paste0("Normalized demand over " , time.min, '-', time.max) )

# OLAP operation in R

library(plyr)

######################################################################################
#                         ______         _ 
#                         |  ___|       | |
#                         | |_ __ _  ___| |_ ___ 
#                         |  _/ _` |/ __| __/ __|
#                         | || (_| | (__| |_\__ \
#                         \_| \__,_|\___|\__|___/
#                    _      ______ _                          _
#                   | |     |  _  (_)                        (_)
#     __ _ _ __   __| |     | | | |_ _ __ ___   ___ _ __  ___ _  ___  _ __  ___
#    / _` | '_ \ / _` |     | | | | | '_ ` _ \ / _ \ '_ \/ __| |/ _ \| '_ \/ __|
#   | (_| | | | | (_| |     | |/ /| | | | | | |  __/ | | \__ \ | (_) | | | \__ \
#    \__,_|_| |_|\__,_|     |___/ |_|_| |_| |_|\___|_| |_|___/_|\___/|_| |_|___/
#
#####################################################################################

# Set up the dimension tables

state_table <- 
  data.frame(key = c("CA", "NY", "WA", "ON", "QU"),
             name = c("California", "new York", "Washington", "Ontario", "Quebec"),
             country = c("USA", "USA", "USA", "Canada", "Canada"))
print( state_table )

##    key       name country
##  1  CA California     USA
##  2  NY   new York     USA
##  3  WA Washington     USA
##  4  ON    Ontario  Canada
##  5  QU     Quebec  Canada

month_table <- 
  data.frame(key = 1:12,
             desc = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter = c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
print (month_table)

##     key desc quarter
##  1    1  Jan      Q1
##  2    2  Feb      Q1
##  3    3  Mar      Q1
##  4    4  Apr      Q2
##  5    5  May      Q2
##  6    6  Jun      Q2
##  7    7  Jul      Q3
##  8    8  Aug      Q3
##  9    9  Sep      Q3
##  10  10  Oct      Q4
##  11  11  Nov      Q4
##  12  12  Dec      Q4

prod_table <- 
  data.frame(key = c("Printer", "Tablet", "Laptop"),
             price = c(225, 570, 1120))
print (prod_table)

##       key price
## 1 Printer   225
## 2  Tablet   570
## 3  Laptop  1120


#
# Generate the Sales table
#
gen_sales <- function(no_of_recs) {
  
  # Generate random transaction data
  loc <- sample( state_table$key, no_of_recs, 
                 replace = T, prob = c(2,2,1,1,1) )
  time_month <- sample(month_table$key, no_of_recs, replace = T)
  time_year  <- sample(1999:2020, no_of_recs, replace = T)
  prod_       <- sample(prod_table$key, no_of_recs, replace = T, prob = c(1, 3, 2))
  unit       <- sample(c(1,2), no_of_recs, replace = T, prob = c(10, 3))
  amount     <- unit * join( data.frame( key = prod_ ), prod_table )$price
  print(amount)
  
  sales <- data.frame(month = time_month,
                      year = time_year,
                      loc = loc,
                      prod = prod_,
                      unit = unit,
                      amount = amount)
  
  # Sort the records by time order
  sales <- sales[order(sales$year, sales$month),]
  #row.names(sales) <- NULL
  return(sales)
}

# Now create the sales fact table
sales_fact <- gen_sales(5000)

# Look at a few records
print (head(sales_fact))

##    month year loc   prod unit amount
##  1     1 2012  NY Laptop    1    225
##  2     1 2012  CA Laptop    2    450
##  3     1 2012  ON Tablet    2   2240
##  4     1 2012  NY Tablet    1   1120
##  5     1 2012  NY Tablet    2   2240
##  6     1 2012  CA Laptop    1    225


#############################################################################################
#
#   ___  ___      _ _   _           _ _                          _                   _ 
#   |  \/  |     | | | (_)         | (_)                        (_)                 | |
#   | .  . |_   _| | |_ _ ______ __| |_ _ __ ___   ___ _ __  ___ _  ___  _ __   __ _| |
#   | |\/| | | | | | __| |______/ _` | | '_ ` _ \ / _ \ '_ \/ __| |/ _ \| '_ \ / _` | |
#   | |  | | |_| | | |_| |     | (_| | | | | | | |  __/ | | \__ \ | (_) | | | | (_| | |
#   \_|  |_/\__,_|_|\__|_|      \__,_|_|_| |_| |_|\___|_| |_|___/_|\___/|_| |_|\__,_|_|
#                                           _
#                                          | |
#                                 ___ _   _| |__   ___
#                                / __| | | | '_ \ / _ \
#                               | (__| |_| | |_) |  __/
#                                \___|\__,_|_.__/ \___|
#
############################################################################################


# Build a cube
revenue_cube <-
  tapply(sales_fact$amount,
         sales_fact[,c("prod", "month", "year", "loc")],
         FUN = sum )

# Show the cells of the cude
print (revenue_cube)

##  , , year = 2012, loc = CA
##  
##           month
##  prod         1    2     3    4    5    6    7    8    9   10   11   12
##    Laptop  1350  225   900  675  675   NA  675 1350   NA 1575  900 1350
##    Printer   NA 2280    NA   NA 1140  570  570  570   NA  570 1710   NA
##    Tablet  2240 4480 12320 3360 2240 4480 3360 3360 5600 2240 2240 3360
##  
##  , , year = 2013, loc = CA
##  
##           month
##  prod         1    2    3    4    5    6    7    8    9   10   11   12
##    Laptop   225  225  450  675  225  900  900  450  675  225  675 1125
##    Printer   NA 1140   NA 1140  570   NA   NA  570   NA 1140 1710 1710
##    Tablet  3360 3360 1120 4480 2240 1120 7840 3360 3360 1120 5600 4480
##  
##  , , year = 2012, loc = NY
##  
##           month
##  prod         1     2    3    4    5    6    7    8    9   10   11   12
##    Laptop   450   450   NA   NA  675  450  675   NA  225  225   NA  450
##    Printer   NA  2280   NA 2850  570   NA   NA 1710 1140   NA  570   NA
##    Tablet  3360 13440 2240 2240 2240 5600 5600 3360 4480 3360 4480 3360
##  
##  , , year = 2013, loc = NY
##  
##  .....

print (dimnames(revenue_cube))

##  $prod
##  [1] "Laptop"  "Printer" "Tablet"
##  
##  $month
##   [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12"
##  
##  $year
##  [1] "2012" "2013"
##  
##  $loc
##  [1] "CA" "NY" "ON" "QU" "WA"


#######################################################
#          _____ _ _
#         /  ___| (_)
#         \ `--.| |_  ___ ___
#          `--. \ | |/ __/ _ \
#         /\__/ / | | (_|  __/
#         \____/|_|_|\___\___|
#         
#######################################################

# Slice
# cube data in Jan, 2012
print (revenue_cube[ , "1", "2012", ])

##          loc
## prod        CA   NY   ON   QU   WA
##   Laptop  1350  450   NA  225  225
##   Printer   NA   NA   NA 1140   NA
##   Tablet  2240 3360 5600 1120 2240

# cube data in Jan, 2012
print (revenue_cube["Tablet", "1", "2012",])

##   CA   NY   ON   QU   WA
## 2240 3360 5600 1120 2240



#######################################################
#        ______ _
#        |  _  (_)
#        | | | |_  ___ ___
#        | | | | |/ __/ _ \
#        | |/ /| | (_|  __/
#        |___/ |_|\___\___|
#
#######################################################

dice <-
  revenue_cube[c("Tablet","Laptop"),
               c("1","2","3"),
               ,
               c("CA","NY")]

print (dice)

## , , year = 2012, loc = CA
## 
##         month
## prod        1    2     3
##   Tablet 2240 4480 12320
##   Laptop 1350  225   900
## 
## , , year = 2013, loc = CA
## 
##         month
## prod        1    2    3
##   Tablet 3360 3360 1120
##   Laptop  225  225  450
## 
## , , year = 2012, loc = NY
## 
##         month
## prod        1     2    3
##   Tablet 3360 13440 2240
##   Laptop  450   450   NA
## 
## , , year = 2013, loc = NY
## 
##         month
## prod        1    2    3
##   Tablet 3360 4480 6720
##   Laptop  450   NA  225



#######################################################
#       ______      _ _
#       | ___ \    | | |
#       | |_/ /___ | | |_   _ _ __
#       |    // _ \| | | | | | '_ \
#       | |\ \ (_) | | | |_| | |_) |
#       \_| \_\___/|_|_|\__,_| .__/
#                            | |
#                            |_|
#######################################################

rollup <-
  apply(revenue_cube, c("year", "prod"),
        FUN = function(x) sum(x, na.rm = TRUE) )
print (rollup)

##       prod
## year   Laptop Printer Tablet
##   2012  22275   31350 179200
##   2013  25200   33060 166880

# aggregate by decade
# find the members of each decade
up <- cut(as.integer( dimnames(revenue_cube)$year ),
          c(1989, 1999, 2009, 2019, 2029),
          dig.lab = 5)
up.group <- split( dimnames(revenue_cube)$year, up )
n.decade_levels <- length(up.group)
print (up.group)

## $`(1989,1999]`
## [1] "1999"
## 
## $`(1999,2009]`
##  [1] "2000" "2001" "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009"
## 
## $`(2009,2019]`
##  [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
## 
## $`(2019,2029]`
## [1] "2020"

# aggregate by decade ( create a list of 3-d matrices )
new.3d <- lapply(
  up.group, function(k)
    apply( revenue_cube[ , , k, ], c("prod", "month", "loc"), sum, na.rm = T )
)

# dice the original matrix along the year dim. Leave enough "year"
# levels to populate with the new "decade" values
new.revcube <- revenue_cube[,, 1:n.decade_levels, ]

dim(new.revcube)
## [1]  3 12  4  5

# apply the new values
for (i in seq(n.decade_levels))
  new.revcube[,,i,] <- new.3d[[i]]

# rectify the "year" dimension name and levels - convert it to
# the name "decade" and apply decade values to it
dimlist <- dimnames(new.revcube)
where.year <- which(names(dimlist) == "year")
names(dimlist)[where.year] <- "decade"
round10 <- function(x) as.integer(0.1 * as.integer(min(x))) * 10 
dimlist$decade <- sapply(up.group, round10 ) # round to the nearest decade:
dimnames(new.revcube) <- dimlist

print( dimnames(new.revcube) )
## $prod
## [1] "Laptop"  "Printer" "Tablet"
## 
## $month
##  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12"
## 
## $decade
## [1] "1990" "2000" "2010" "2020"
## 
## $loc
## [1] "CA" "NY" "ON" "QU" "WA"

print(new.revcube)

## , , decade = 1990, loc = CA
## 
##          month
## prod         1    2    3    4    5    6    7    8    9   10    11   12
##   Laptop     0  900  450  450  225  450  675 1350  225  225   675  450
##   Printer    0 1140    0 1140  570    0    0  570    0 1140  1710    0
##   Tablet  4480 2240 4480 2240 5600 1120 6720 5600 4480 5600 10080 7840
## 
## , , decade = 2000, loc = CA
## 
##          month
## prod          1     2     3     4     5     6     7     8     9    10    11     12
##   Laptop   4725  3150  5400  5175  4275  6975  5625  5850  4275  4050  7650   4500
##   Printer  9120  7980  3420  3990  2850  8550 10830  5700  8550  5130  7410   9690
##   Tablet  32480 38080 32480 29120 42560 29120 41440 29120 21280 30240 29120  34720
## 
## , , decade = 2010, loc = CA
## 
##          month
## prod          1     2     3     4     5     6     7     8     9    10    11     12
##   Laptop   6075  4950  7425  5850  5850  4950  3825  5625  4950  5850  4500   4950
##   Printer  5130  4560  3990  4560  8550  9120  4560  9120  5130  2280  9120   3990
##   Tablet  51520 30240 42560 40320 31360 31360 39200 25760 34720 31360 47040  34720
## 
## , , decade = 2020, loc = CA
## 
##          month
## prod         1    2    3    4     5    6    7    8    9   10   11   12
##   Laptop   900  450  900    0   675    0  225  675 1125  900    0  900
##   Printer  570 2280    0 1140  1140    0 1140  570    0 2850    0  570
##   Tablet  4480 6720 4480 6720 11200 4480 4480 2240 2240 2240 7840 3360
##
##  .....

rollup.1 <-
  apply(new.revcube, c("prod", "decade", "loc"),
        FUN = function(x) sum(x, na.rm = T))
print (rollup.1)

## , , loc = CA
## 
##          decade
## prod       1990   2000   2010  2020
##   Laptop   6075  61650  64800  6750
##   Printer  6270  83220  70110 10260
##   Tablet  60480 389760 440160 60480
## 
## , , loc = NY
## 
##          decade
## prod       1990   2000   2010  2020
##   Laptop   3375  54450  66375  4725
##   Printer  9690  71250  60420  6270
##   Tablet  43680 472640 439040 51520
## 
## , , loc = ON
## 
##          decade
## prod       1990   2000   2010  2020
##   Laptop   2925  27675  30150  2025
##   Printer  2850  33060  34200  3420
##   Tablet  25760 209440 278880 20160
## 
## , , loc = QU
## 
##          decade
## prod       1990   2000   2010  2020
##   Laptop   2700  27225  29025  2475
##   Printer  1140  38190  33630  4560
##   Tablet  28000 212800 250880 26880
## 
## , , loc = WA
## 
##          decade
## prod       1990   2000   2010  2020
##   Laptop   1800  31950  35775  2475
##   Printer  2280  34770  41040  1710
##   Tablet  22400 243040 210560 19040


#######################################################
#      ______      _ _ _     _
#      |  _  \    (_) | |   | |
#      | | | |_ __ _| | | __| | _____      ___ __
#      | | | | '__| | | |/ _` |/ _ \ \ /\ / / '_ \
#      | |/ /| |  | | | | (_| | (_) \ V  V /| | | |
#      |___/ |_|  |_|_|_|\__,_|\___/ \_/\_/ |_| |_|
#      
#######################################################

drilldown <-
  apply(revenue_cube, c("year", "month", "prod"), 
        FUN = function(x) sum(x, na.rm = TRUE) )
print (drilldown)

##  , , prod = Laptop
##  
##        month
##  year      1    2    3    4    5    6    7    8    9   10   11   12
##    2012 2250 2475 1575 1575 2250 1800 1575 1800  900 2250 1350 2475
##    2013 2250  900 1575 1575 2250 2475 2025 1800 2025 2250 3825 2250
##  
##  , , prod = Printer
##  
##        month
##  year      1    2    3    4    5    6    7    8    9   10   11   12
##    2012 1140 5700  570 3990 4560 2850 1140 2850 2850 1710 3420  570
##    2013 1140 4560 3420 4560 2850 1140  570 3420 1140 3420 3990 2850
##  
##  , , prod = Tablet
##  
##        month
##  year       1     2     3     4     5     6     7     8     9    10    11    12
##    2012 14560 23520 17920 12320 10080 14560 13440 15680 25760 12320 11200  7840
##    2013  8960 11200 10080  7840 14560 10080 29120 15680 15680  8960 12320 22400


# #################################################
#       ______ _            _
#       | ___ (_)          | |
#       | |_/ /___   _____ | |_
#       |  __/| \ \ / / _ \| __|
#       | |   | |\ V / (_) | |_
#       \_|   |_| \_/ \___/ \__|
# 
# #################################################

pivot.1 <-
  apply(revenue_cube, c("year", "month"),
        FUN = function(x) sum(x, na.rm=TRUE) )
print (pivot.1)

## 
##       month
## year       1     2     3     4     5     6     7     8     9    10    11    12
##   2012 17950 31695 20065 17885 16890 19210 16155 20330 29510 16280 15970 10885
##   2013 12350 16660 15075 13975 19660 13695 31715 20900 18845 14630 20135 27500
## 

pivot.2 <-
  apply(revenue_cube, c("prod", "loc"),
        FUN = function(x) sum(x, na.rm=TRUE) )
print (pivot.2)

## 
##          loc
## prod         CA     NY    ON    QU    WA
##   Laptop  16425   9450  7650  7425  6525
##   Printer 15390  19950  7980 10830 10260
##   Tablet  90720 117600 45920 34720 57120


# from https://stackoverflow.com/questions/26105477/2d-matrix-to-3d-stacked-array-in-r

set.seed(1)
mat <- matrix(sample(100, 12 * 5, TRUE), ncol = 5)
mat
#       [,1] [,2] [,3] [,4] [,5]
#  [1,]   27   69   27   80   74
#  [2,]   38   39   39   11   70
#  [3,]   58   77    2   73   48
#  [4,]   91   50   39   42   87
#  [5,]   21   72   87   83   44
#  [6,]   90  100   35   65   25
#  [7,]   95   39   49   79    8
#  [8,]   67   78   60   56   10
#  [9,]   63   94   50   53   32
# [10,]    7   22   19   79   52
# [11,]   21   66   83    3   67
# [12,]   18   13   67   48   41


print(t(mat))
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   68   39    1   34   87   43   14   82   59    51    97    85
# [2,]   21   54   74    7   73   79   85   37   89    37    34    89
# [3,]   44   79   33   84   35   70   74   42   38    20    28    20
# [4,]   44   87   70   40   44   25   70   39   51    42     6    24
# [5,]   32   14    2   45   18   22   78   65   70    87    70    75

`dim<-`(t(mat), c(5, 3, 4))
# , , 1
# 
#      [,1] [,2] [,3]
# [1,]   68   39    1
# [2,]   21   54   74
# [3,]   44   79   33
# [4,]   44   87   70
# [5,]   32   14    2
# 
# , , 2
# 
#      [,1] [,2] [,3]
# [1,]   34   87   43
# [2,]    7   73   79
# [3,]   84   35   70
# [4,]   40   44   25
# [5,]   45   18   22
# 
# , , 3
# 
#      [,1] [,2] [,3]
# [1,]   14   82   59
# [2,]   85   37   89
# [3,]   74   42   38
# [4,]   70   39   51
# [5,]   78   65   70
# 
# , , 4
# 
#      [,1] [,2] [,3]
# [1,]   51   97   85
# [2,]   37   34   89
# [3,]   20   28   20
# [4,]   42    6   24
# [5,]   87   70   75
# 

Sliced <- aperm(`dim<-`(t(mat), c(5, 3, 4)), c(2, 1, 3))

Sliced
# , , 1
# 
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   68   21   44   44   32
# [2,]   39   54   79   87   14
# [3,]    1   74   33   70    2
# 
# , , 2
# 
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   34    7   84   40   45
# [2,]   87   73   35   44   18
# [3,]   43   79   70   25   22
# 
# , , 3
# 
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   14   85   74   70   78
# [2,]   82   37   42   39   65
# [3,]   59   89   38   51   70
# 
# , , 4
# 
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   51   37   20   42   87
# [2,]   97   34   28    6   70
# [3,]   85   89   20   24   75

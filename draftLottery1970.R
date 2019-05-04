

#### input data ####
# install.packages("RCurl")


library(RCurl)
datafile <- getURL("https://raw.githubusercontent.com/TerrySitu/Did-The-Men-s-Dates-of-Birth-Determine-Affect-The-Order-of-Call-to-Military-Service-in-Vietnam-War-/master/draftLottery1970.txt")

lottery <- read.table(text = datafile, header=F)

# combine results into 12 months
daysInMth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
cutpts <- c(1, cumsum(daysInMth))
lottery.mth <- cut(lottery$draft.order, cutpts, include.lowest=TRUE, labels=1:12)
lottery.table.1 <- table(birth.month=lottery$month, lottery.month=lottery.mth)
print(lottery.table.1)
cat("\nRow totals:", rowSums(lottery.table.1), "\n")
cat("Col totals:", colSums(lottery.table.1), "\n\n")

# combine results into 4 quarters
daysInQtr <- c(91, 91, 92, 92)
cutpts <- c(1, cumsum(daysInQtr))
lottery.qtr <- cut(lottery$draft.order, cutpts, include.lowest=TRUE, labels=1:4)
birth.qtr <- rep(1:4, daysInQtr)
lottery.table.2 <- table(birth.quarter=birth.qtr, lottery.quarter=lottery.qtr)
print(lottery.table.2)
cat("\nRow totals:", rowSums(lottery.table.2), "\n")
cat("Col totals:", colSums(lottery.table.2), "\n\n")

# a)
quads =function(colours=c("blue","red","green","yellow")){
    limits = par()$usr
    rect(0,0,limits[2],limits[4],col=colours[1])
    rect(0,0,limits[1],limits[4],col=colours[2])
    rect(0,0,limits[1],limits[3],col=colours[3])
    rect(0,0,limits[2],limits[3],col=colours[4])
  }

head(lottery)
boxplot(lottery[,2]~lottery[,3])
plot(lottery[,1:2], type="n")
quads()
points(lottery[,2], lottery[,3])
# b)
# chi-square
chisq.indep(lottery.table.2)

# analysis of residuals
chisq.indep(lottery.table.2, verb=F)$std.res

lottery.table.2

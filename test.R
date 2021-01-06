library(readr)

wins        <- vector()
HR          <- vector()
HIT         <- vector()
AVG         <- vector()
R_score     <- vector()
OPS         <- vector()
SLG         <- vector()
wRAA        <- vector()
wOBA        <- vector()
WRC_plus    <- vector()

for (i in 1995:2019){
    str1 <- "WDATA/"
    str2 <- ".txt"
    file_name <- paste(str1, i, str2, sep = "")

    if(i<=1997){
        for(j in 1:28){
            tmp <- read.table(file_name)
            wins <- c(wins,tmp[j,2])
        }
    } else {
        for(j in 1:30){
            tmp <- read.table(file_name)
            wins <- c(wins,tmp[j,2])
        }
    }

    # Basic Data
    str1 <- "BDATA/"
    file_name <- paste(str1, i, str2, sep = "")

    if(i<=1997){
        for(j in 1:28){
            tmp     <- read.table(file_name)
            HR      <- c(HR,tmp[j,9])
            HIT     <- c(HIT,tmp[j,5])
            AVG     <- c(AVG,tmp[j,21])
            R_score <- c(R_score,tmp[j,10])
        }
    } else {
        for(j in 1:30){
            tmp <- read.table(file_name)
            HR  <- c(HR,tmp[j,9])
            HIT <- c(HIT,tmp[j,5])
            AVG <- c(AVG,tmp[j,21])
            R_score <- c(R_score,tmp[j,10])
        }
    }

    str1 <- "ADATA/"
    file_name <- paste(str1, i, str2, sep = "")

    if(i<=1997){
        for(j in 1:28){
            tmp      <- read.table(file_name)
            SLG      <- c(SLG,tmp[j,8])
            OPS      <- c(OPS,tmp[j,9])
            wRAA     <- c(wRAA,tmp[j,17])
            wOBA     <- c(wOBA,tmp[j,18])
            WRC_plus <- c(WRC_plus,tmp[j,19])
        }
    } else {
        for(j in 1:30){
            tmp      <- read.table(file_name)
            SLG      <- c(SLG,tmp[j,8])
            OPS      <- c(OPS,tmp[j,9])
            wRAA     <- c(wRAA,tmp[j,17])
            wOBA     <- c(wOBA,tmp[j,18])
            WRC_plus <- c(WRC_plus,tmp[j,19])
        }
    }
}

########################### HR ##########################
png(file = "HR-WINS.png")
plot(HR, wins,
     main="R-WINS 1995-2019",
     ylab = "WINS",
     xlab = "HR"
     )
abline(lm(wins~HR),col="red")
dev.off()


cor.test(HR,wins)

png(file = "HR-WINS-qqplot.png")
qqnorm(HR,col="blue",ylab="HR",pch=0)
qqline(HR,col="red")
dev.off()
########################### HR ##########################

########################### WRC+ ##########################
png(file = "WRC+ -WINS.png")
plot(WRC_plus, wins,
     main="R-WINS 1995-2019",
     ylab = "WINS",
     xlab = "WRC+"
     )
abline(lm(wins~WRC_plus),col="red")
dev.off()


cor.test(WRC_plus,wins)

png(file = "WRC+ - WINS-qqplot.png")
qqnorm(WRC_plus,col="blue",ylab="WRC+",pch=0)
qqline(WRC_plus,col="red")
dev.off()
########################### WRC+ ##########################


# for (i in 1:744){
#     print(AVG[i])
# }

########################### HIT ##########################
png(file = "HIT-WINS.png")
plot(HIT, wins,
     main="HIT-WINS 1995-2019",
     ylab = "WINS",
     xlab = "HIT"
     )
abline(lm(wins~HIT),col="red")
dev.off()


cor.test(HIT,wins)

png(file = "HIT-WINS-qqplot.png")
qqnorm(HIT,col="blue",ylab="HIT",pch=0)
qqline(HIT,col="red")
dev.off()
########################### HIT ##########################

########################### AVG ##########################
png(file = "AVG-WINS.png")
plot(AVG, wins,
     main="AVG-WINS 1995-2019",
     ylab = "WINS",
     xlab = "AVG"
     )
abline(lm(wins~AVG),col="red")
dev.off()


cor.test(AVG,wins)

png(file = "AVG-WINS-qqplot.png")
qqnorm(AVG,col="blue",ylab="AVG",pch=0)
qqline(AVG,col="red")
dev.off()
########################### AVG ##########################

########################### R ##########################
png(file = "R-WINS.png")
plot(R_score, wins,
     main="R-WINS 1995-2019",
     ylab = "WINS",
     xlab = "R"
     )
abline(lm(wins~R_score),col="red")
dev.off()


cor.test(R_score,wins)

png(file = "R-WINS-qqplot.png")
qqnorm(R_score,col="blue",ylab="R",pch=0)
qqline(R_score,col="red")
dev.off()
########################### R ##########################

########################### wRAA ##########################
png(file = "wRAA-WINS.png")
plot(wRAA, wins,
     main="wRAA-WINS 1995-2019",
     ylab = "WINS",
     xlab = "wRAA"
     )
abline(lm(wins~wRAA),col="red")
dev.off()


cor.test(wRAA,wins)

png(file = "wRAA-WINS-qqplot.png")
qqnorm(wRAA,col="blue",ylab="wRAA",pch=0)
qqline(wRAA,col="red")
dev.off()
########################### wRAA ##########################

########################### wOBA ##########################
png(file = "wOBA-WINS.png")
plot(wOBA, wins,
     main="wOBA-WINS 1995-2019",
     ylab = "WINS",
     xlab = "wOBA"
     )
abline(lm(wins~wOBA),col="red")
dev.off()


cor.test(wOBA,wins)

png(file = "wOBA-WINS-qqplot.png")
qqnorm(wOBA,col="blue",ylab="wOBA",pch=0)
qqline(wOBA,col="red")
dev.off()
########################### wOBA ##########################

########################### OPS ##########################
png(file = "OPS-WINS.png")
plot(OPS, wins,
     main="OPS-WINS 1995-2019",
     ylab = "WINS",
     xlab = "OPS"
     )
abline(lm(wins~OPS),col="red")
dev.off()


cor.test(OPS,wins)

png(file = "OPS-WINS-qqplot.png")
qqnorm(OPS,col="blue",ylab="OPS",pch=0)
qqline(OPS,col="red")
dev.off()
########################### OPS ##########################

########################### SLG ##########################
png(file = "SLG-WINS.png")
plot(SLG, wins,
     main="SLG-WINS 1995-2019",
     ylab = "WINS",
     xlab = "SLG"
     )
abline(lm(wins~SLG),col="red")
dev.off()


cor.test(SLG,wins)

png(file = "SLG-WINS-qqplot.png")
qqnorm(SLG,col="blue",ylab="SLG",pch=0)
qqline(SLG,col="red")
dev.off()
########################### SLG ##########################
library(readr)
library(lmtest)

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

Bat_Clutch  <- vector()
Pit_clutch  <- vector()

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


    str1 <- "BWPDATA/"
    file_name <- paste(str1, i, str2, sep = "")

    if(i<=1997){
        for(j in 1:28){
            tmp         <- read.table(file_name)
            Bat_Clutch  <- c(Bat_Clutch,tmp[j,11])
        }
    } else {
        for(j in 1:30){
            tmp         <- read.table(file_name)
            Bat_Clutch  <- c(Bat_Clutch,tmp[j,11])
        }
    }

    str1 <- "PWPDATA/"
    file_name <- paste(str1, i, str2, sep = "")

    if(i<=1997){
        for(j in 1:28){
            tmp         <- read.table(file_name)
            Pit_clutch  <- c(Pit_clutch,tmp[j,13])
        }
    } else {
        for(j in 1:30){
            tmp         <- read.table(file_name)
            Pit_clutch  <- c(Pit_clutch,tmp[j,13])
        }
    }
}



########################### HR ##########################

png(file = "HR-WINS.png")
plot(HR, wins,
     main="HR-WINS 1995-2019",
     ylab = "WINS",
     xlab = "HR"
     )
abline(lm(wins~HR),col="red")
legend("bottomright",legend=c("HR = 0.13190WINS + 57.87204",expression('r'^2~"= 0.1703")),cex=0.9)
dev.off()

summary(lm(wins~HR))
cor.test(HR,wins)
t.test(HR,wins)

png(file = "HR-WINS-qqplot.png")
qqnorm(HR,col="blue",ylab="HR",pch=0)
qqline(HR,col="red")
dev.off()


########################### HR ##########################

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")


########################### WRC+ ##########################
png(file = "WRC+ -WINS.png")
plot(WRC_plus, wins,
     main="WRC+-WINS 1995-2019",
     ylab = "WINS",
     xlab = "WRC+"
     )
abline(lm(wins~WRC_plus),col="red")
legend("bottomright",legend=c("WINS = 0.91736(WRC+)-7.71725",expression('r'^2~"= 0.4514")),cex=0.9)
dev.off()

summary(lm(wins~WRC_plus))
cor.test(WRC_plus,wins)

png(file = "WRC+ - WINS-qqplot.png")
qqnorm(WRC_plus,col="blue",ylab="WRC+",pch=0)
qqline(WRC_plus,col="red")
dev.off()

# 殘差圖
png(file = "WRC+ - WINS-res.png")
WRC_plus_data <- data.frame(x=WRC_plus,y=wins)
res <- residuals(lm(wins~WRC_plus),data=WRC_plus_data)
plot(WRC_plus,res,ylim=c(-40,40))
dev.off()

# 殘差 qqplot
png(file = "WRC+-WINS-res-qqplot.png")
qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")
dev.off()

# ncvTest
require(car)
ncvTest(lm(wins~WRC_plus))

########################### WRC+ ##########################

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

########################### HIT ##########################
png(file = "HIT-WINS.png")
plot(HIT, wins,
     main="HIT-WINS 1995-2019",
     ylab = "WINS",
     xlab = "HIT"
     )
abline(lm(wins~HIT),col="red")
legend("bottomright",legend=c("WINS = 0.049325HIT+9.469415",expression('r'^2~"= 0.1359")),cex=0.9)
dev.off()

summary(lm(wins~HIT))
cor.test(HIT,wins)

png(file = "HIT-WINS-qqplot.png")
qqnorm(HIT,col="blue",ylab="HIT",pch=0)
qqline(HIT,col="red")
dev.off()
########################### HIT ##########################

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

########################### AVG ##########################
png(file = "AVG-WINS.png")
plot(AVG, wins,
     main="AVG-WINS 1995-2019",
     ylab = "WINS",
     xlab = "AVG"
     )
abline(lm(wins~AVG),col="red")
legend("bottomright",legend=c("WINS = 335.109AVG-6.877",expression('r'^2~"= 0.1266")),cex=0.9)
dev.off()

summary(lm(wins~AVG))
cor.test(AVG,wins)

png(file = "AVG-WINS-qqplot.png")
qqnorm(AVG,col="blue",ylab="AVG",pch=0)
qqline(AVG,col="red")
dev.off()
########################### AVG ##########################

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

########################### R ##########################
png(file = "R-WINS.png")
plot(R_score, wins,
     main="R-WINS 1995-2019",
     ylab = "WINS",
     xlab = "R"
     )
abline(lm(wins~R_score),col="red")
legend("bottomright",legend=c("WINS = 0.07496R+24.57095",expression('r'^2~"= 0.3003")),cex=0.9)
dev.off()

summary(lm(wins~R_score))
cor.test(R_score,wins)

png(file = "R-WINS-qqplot.png")
qqnorm(R_score,col="blue",ylab="R",pch=0)
qqline(R_score,col="red")
dev.off()


png(file = "R-WINS-res.png")
R_score_data <- data.frame(x=R_score,y=wins)
res <- residuals(lm(wins~R_score),data=R_score_data)
plot(R_score,res,ylim=c(-40,40))
dev.off()

# 殘差 qqplot
png(file = "R-WINS-res-qqplot.png")
qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")
dev.off()

require(car)
ncvTest(lm(wins~R_score))

########################### R ##########################

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

########################### wRAA ##########################
png(file = "wRAA-WINS.png")
plot(wRAA, wins,
     main="wRAA-WINS 1995-2019",
     ylab = "WINS",
     xlab = "wRAA"
     )
abline(lm(wins~wRAA),col="red")
legend("bottomright",legend=c("WINS = 0.101155wRAA+80.874021",expression('r'^2~"= 0.3613")),cex=0.9)
dev.off()

summary(lm(wins~wRAA))
cor.test(wRAA,wins)

png(file = "wRAA-WINS-qqplot.png")
qqnorm(wRAA,col="blue",ylab="wRAA",pch=0)
qqline(wRAA,col="red")
dev.off()

png(file = "wRAA-WINS-res.png")
wRAA_data <- data.frame(x=wRAA,y=wins)
res <- residuals(lm(wins~wRAA),data=wRAA_data)
plot(wRAA,res,ylim=c(-40,40))
dev.off()


png(file = "wRAA-WINS-res-qqplot.png")
qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")
dev.off()

require(car)
ncvTest(lm(wins~wRAA))

########################### wRAA ##########################

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

########################### wOBA ##########################
png(file = "wOBA-WINS.png")
plot(wOBA, wins,
     main="wOBA-WINS 1995-2019",
     ylab = "WINS",
     xlab = "wOBA"
     )
abline(lm(wins~wOBA),col="red")
legend("bottomright",legend=c("WINS = 369.75wOBA-39.57",expression('r'^2~"= 0.2486")),cex=0.9)
dev.off()

summary(lm(wins~wOBA))
cor.test(wOBA,wins)

png(file = "wOBA-WINS-qqplot.png")
qqnorm(wOBA,col="blue",ylab="wOBA",pch=0)
qqline(wOBA,col="red")
dev.off()


png(file = "wOBA-WINS-res.png")
wOBA_data <- data.frame(x=wOBA,y=wins)
res <- residuals(lm(wins~wOBA),data=wOBA_data)
plot(wOBA,res)
dev.off()


png(file = "wOBA-WINS-res-qqplot.png")
qqnorm(res,col="blue",ylab="wOBA",pch=0)
qqline(res,col="red")
dev.off()

tmp  <- lm(wins~wOBA)
tmp2 <- bptest(tmp)
print(tmp2)

require(car)
print(ncvTest(tmp))

########################### wOBA ##########################

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

########################### OPS ##########################
png(file = "OPS-WINS.png")
plot(OPS, wins,
     main="OPS-WINS 1995-2019",
     ylab = "WINS",
     xlab = "OPS"
     )
abline(lm(wins~OPS),col="red")
legend("bottomright",legend=c("WINS = 153.518OPS-33.993",expression('r'^2~"= 0.2649")),cex=0.9)
dev.off()

summary(lm(wins~OPS))
cor.test(OPS,wins)

png(file = "OPS-WINS-qqplot.png")
qqnorm(OPS,col="blue",ylab="OPS",pch=0)
qqline(OPS,col="red")
dev.off()


png(file = "OPS-WINS-res.png")
OPS_data <- data.frame(x=OPS,y=wins)
res <- residuals(lm(wins~OPS),data=OPS_data)
plot(OPS,res,ylim=c(-40,40))
dev.off()


png(file = "OPS-WINS-res-qqplot.png")
qqnorm(res,col="blue",ylab="res",pch=0)
qqline(res,col="red")
dev.off()

require(car)
ncvTest(lm(wins~OPS))


########################### OPS ##########################

cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")

########################### SLG ##########################
png(file = "SLG-WINS.png")
plot(SLG, wins,
     main="SLG-WINS 1995-2019",
     ylab = "WINS",
     xlab = "SLG"
     )
abline(lm(wins~SLG),col="red")
legend("bottomright",legend=c("WINS = 210.982SLG-7.414",expression('r'^2~"= 0.2339")),cex=0.9)
dev.off()

summary(lm(wins~SLG))
cor.test(SLG,wins)

png(file = "SLG-WINS-qqplot.png")
qqnorm(SLG,col="blue",ylab="SLG",pch=0)
qqline(SLG,col="red")
dev.off()
########################### SLG ##########################


cat("\n")
cat("\n")
print("----------------------------------------------------------")
cat("\n")
cat("\n")


total_clutch <- vector()
total_clutch <- Pit_clutch+Bat_Clutch

# for (i in 1:744){
#     print(total_clutch[i])
# }

png(file = "Clutch-WINS.png")
plot(total_clutch, wins,
     main="Clutch-WINS 1995-2019",
     ylab = "WINS",
     xlab = "Clutch"
     )
abline(lm(wins~total_clutch),col="red")
dev.off()

summary(total_clutch)

cor.test(total_clutch,wins)

png(file = "Clutch-WINS-qqplot.png")
qqnorm(total_clutch,col="blue",ylab="Clutch",pch=0)
qqline(total_clutch,col="red")
dev.off()
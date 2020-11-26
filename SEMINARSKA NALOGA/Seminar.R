data <- read.table("dataSem1.txt", sep=",", header=TRUE, stringsAsFactors = T)
head(data)
nrow(data)
summary(data)

data$datum <- as.Date(data$datum)
data$ura <- as.factor(data$ura)

sel <- data$ura == "11" & data$stavba == "1"
plot(data$datum[sel], data$poraba[sel], type = "l")
#the higher air temperature cuses more energy use
plot(data$temp_zraka[sel], data$poraba[sel])

plot(data$datum[sel], data$poraba[sel], type="l")


selSummer <- data$namembnost == "izobrazevalna" & data$datum >= as.Date("2016-06-01") & data$datum <= as.Date("2016-09-01")
selWinter <- data$namembnost == "izobrazevalna" & data$datum <= as.Date("2016-05-30") | data$datum >=as.Date("2016-09-02")

selSchools <- data$namembnost == "izobrazevalna"

schools <- data[selSchools,]


#we select a sample from schools and plot them to see if there is any correlation btw the summer break and the school year
sample <- sample(1:nrow(schools), 4)
schoolsStavbe <- schools[sample,"stavba"]
plot(schools$datum[schools$stavba == "17" & schools$ura == "11"], schools$poraba[schools$stavba == "17" & schools$ura == "11"], type="l")
plot(schools$datum[schools$stavba == "33" & schools$ura == "11"], schools$poraba[schools$stavba == "33" & schools$ura == "11"], type="l")
plot(schools$datum[schools$stavba == "39" & schools$ura == "11"], schools$poraba[schools$stavba == "39" & schools$ura == "11"], type="l")
plot(schools$datum[schools$stavba == "174" & schools$ura == "11"], schools$poraba[schools$stavba == "174" & schools$ura == "11"], type="l")
plot(data$datum[data$stavba == "3" & data$ura == "11"], data$poraba[data$stavba == "3" & data$ura == "11"], type="l")

boxplot(data$poraba[selSummer], data$poraba[selWinter], names = c("Summer break", "Scholl working"))

#checking if there is any corr btw the weekend and working days
selWeekend <- weekdays(data$datum) %in% c("sobota", "nedelja")
selWeekDays <- weekdays(data$datum) %in% c("ponedeljek", "torek", "sreda", "četrtek", "petek")
boxplot(data$poraba[selWeekend] / 2, data$poraba[selWeekDays] / 5,names=c("Weekend", "Weekdays") ,main="Boxplot showing the differences of eng use btw the weekend and weekdays")
abline(h=median(data$poraba[selWeekend] / 2), col="green")
abline(h=median(data$poraba[selWeekDays] / 5), col="blue")

selWeekendHouses <- data$namembnost == "stanovanjska" & selWeekend
selWeekendJavno <- data$namembnost == "javno_storitvena" & selWeekend

boxplot(data$poraba[selWeekendHouses], data$poraba[selWeekendOffice], ylab = "Poraba elektrike(kWh)" ,names=c("Stanovanjska", "Javno storitvena"), main="Elektrika, porabljena ob koncu tedna.")
abline(h=median(data$poraba[selWeekendHouses]), col="red")
abline(h=median(data$poraba[selWeekendOffice]), col="blue")
legend("topleft", legend=c("Median stanovanj", "Median javno storitvena"),
       col=c("red", "blue"), lty=1:1, cex=0.8,
       box.lty=2, box.lwd=2, box.col="green")

selWeekHouses <- data$namembnost == "stanovanjska" & selWeekDays
#doesn't make sence bc there are multiple weekdays and 2 days for weekend
boxplot(data$poraba[selWeekendHouses] / 2, data$poraba[selWeekHouses] / 5, ylab = "Poraba elektrike(kWh)", names =c("Weekend", "Weekdays"), main="Stanovanja")
abline(h=mean(data$poraba[selWeekendHouses] / 2), col="red")
abline(h=mean(data$poraba[selWeekHouses] / 5), col="blue")

barplot(data$datum, data$norm_poraba)

tab <- table(data$datum, data$norm_poraba)
tabAll <- sum(colSums(tab))
ratioNizka <- (tab[,1] + tab[,4]) / tabAll
barplot(ratioNizka)

ratioVZeloV <- (tab[,3] + tab[,5]) / tabAll
barplot(ratio)
plot(x=seq(as.Date("2015-12-31"), as.Date("2016-12-31"), by="days"), y=ratioVZeloV, type="l",
     xlab="Dates", ylab="(VISOKA + ZELOVISOKA) / ALL")
plot(x=seq(as.Date("2015-12-31"), as.Date("2016-12-31"), by="days"), y=ratioNizka, type="l",
     xlab="Dates", ylab="(NIZKA) / ALL", main="The ratio of LOW(NIZKA, ZELONIZKA) and electricity usage 
     through the year")
#suggesting that we should add an atribute for the season

addPreviosDay <- function(data){
  if(data$datum > "2015-01-01"){
    #temp <- table(data$datum -1, data$temp_zraka)
    temp <- aggregate(temp_zraka ~ datum - 1, data=data,mean)
  }
}
sel <- data$datum > "2016-01-01"
temp <- aggregate(temp_zraka ~ (datum - 1) + stavba, data=data[sel,],mean)



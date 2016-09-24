myfunction <- function(x){
	y <- rnorm(100)
	mean(y)
}

loadData <- function(){
	f <- dir(".","hw1*.csv")
	t <- read.csv(f)
	t
}

loadhw <- function(){
	read.csv("hw1_data.csv")
}


ex18 <- function(mydata){
	good <- mydata[complete.cases(mydata$Ozone, mydata$Solar.R, mydata$Temp),]
	ozone31 <- good[good$Ozone > 31,]
	temp90 <- ozone31[ozone31$Temp > 90,]
	result <- mean(temp90$Solar.R)
	result
}

ex19 <- function(mydata){
	good <- mydata[complete.cases(mydata$Temp, mydata$Month),]
	month6 <- good[good$Month == 6,]
	result <- mean(month6$Temp)
	result
}

ex20 <- function(mydata){
	good <- mydata[complete.cases(mydata$Ozone, mydata$Month),]
	month5 <- good[good$Month == 5,]
	max(month5$Ozone)
}

ex3 <- function(x){
	g <- function(y) {
		y + z
	}
	z <- 4
	x + g(x)
}



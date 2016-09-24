loadFiles <- function(dir = "specdata", ids = 1:332, ignoreNA = TRUE, pol = "sulfate"){
	ids <- formatC(ids, width = 3, format = "d", flag = "0")
	allFiles <- paste(paste(dir, ids, sep = "/"), "csv", sep = ".")
	require(data.table)
	fullData <- rbindlist(lapply(allFiles, read.csv))
	if(!ignoreNA){
		fullData
	} else {
		good <- fullData[complete.cases(fullData[[pol]]),]
		good
	}
}

pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:332){
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!

	good <- loadFiles(ids = id, pol = pollutant)
	mean(good[[pollutant]])
}

complete <- function(directory = "specdata", id = 1:332){
	## Write a function that reads a directory full of files and
	## reports the number of completely observed cases in each data
	## file. The function should return a data frame where the first
	## column is the name of the file and the second column is the
	## number of complete cases.

	## All you need to do to get the number of complete cases is read
	## the file and run sum() on complete.cases().

	rowsNum <- length(id)
	filesList <- paste(paste(directory, formatC(id, width = 3, format = "d", flag = "0"), sep = "/"), "csv", sep = ".")
	filesData <- lapply(filesList, read.csv)
	idC <- numeric(rowsNum)
	nobsC <- numeric(rowsNum)
	for( i in 1:rowsNum){
		idC[i] <- id[i]
		nobsC[i] <- sum(complete.cases(filesData[[i]]))
	}
	data.frame(id = idC, nobs = nobsC)
}


corr <- function(directory = "specdata", threshold = 0){
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations(on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0

	## Return a numeric vector of correlations
	## NOTE: Do not round the result!

	## So, you can loop through the files and if complete cases > threshold then
	## calculate cor() and add it to a vector. If you initialize your vector to
	## numeric() and no files have complete cases then when you return the vector
	## it's an empty numeric vector which is what the instructions call for.
	id <- 1:332
	rowsNum <- length(id)
	filesList <- paste(paste(directory, formatC(id, width = 3, format = "d", flag = "0"),sep = "/"), "csv", sep = ".")
	filesData <- lapply(filesList, read.csv)

	myvector <- numeric()

	for( i in 1:rowsNum){
		cases <- sum(complete.cases(filesData[[i]]))
		if(cases > threshold){
			myvector[i] <- cor(filesData[[i]]$sulfate, filesData[[i]]$nitrate, use = "na.or.complete")
		}
	}
	myvector[!is.na(myvector)]
}


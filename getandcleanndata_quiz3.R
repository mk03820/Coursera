library(sqldf)
library(RCurl)
Sys.setenv(https_proxy = "http://localhost:3128")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "acs.csv", method = "auto")
dateDownloaded <- date()
acs <- read.table("./acs.csv",sep=",",header=TRUE)
head(acs)
# households on greater than 10 acres who sold more than $10,000 
#worth of agriculture products ACR=3 AND AGS=6

agricultureLogical<-(acs$ACR=='3' & acs$AGS=='6')
which(agricultureLogical)
class(agricultureLogical)

install.packages("jpeg")
library(jpeg)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl, destfile = "jeff.jpg", method = "auto")
img.n<-readJPEG("jeff.jpg",TRUE)
quantile(img.n,probs=c(0.3,0.8))

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl, destfile = "gdp.csv", method = "auto")
gdp <- read.csv("./gdp.csv")
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl1, destfile = "edu.csv", method = "auto")
edu <- read.csv("./edu.csv")
X="CountryCode"
names(gdp)
names(edu)
head(gdp)
head(edu)
gdpclean<-gdp[5:194,]
mergedData=as.data.frame(merge(gdpclean,edu,by.x="X",by.y="CountryCode"))
mergedData$Gross.domestic.product.2012 = as.numeric(as.character(mergedData$Gross.domestic.product.2012))
summary(mergedData[mergedData$Income.Group=="High income: OECD",])


quantile(mergedData$Gross.domestic.product.2012,probs=c(0.2,0.4,0.6,0.8,1))
library(Hmisc)
mergedData$gdp=cut2(mergedData$Gross.domestic.product.2012,g=5)
table(mergedData$Income.Group,mergedData$gdp)




# Getting and Cleaning Data
# Coursera
# John Hopkins University

# Bastiaan Quast
# bquast@gmail.com

# write the file url and file destination to an object
file.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
file.dest <- 'GDP.csv'

# download from the URL
download.file(file.url, file.dest )

# specify the right lines
rowNames <- seq(10,200, 2)

# read the data
gdp <- read.csv('GDP.csv', header=F, skip=5, nrows=190)
View(gdp)

# second data file
file.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
file.dest <- 'GDP2.csv'

# download from the URL
download.file(file.url, file.dest )

# read second file
fed <- read.csv('GDP2.csv')
View(fed)

# merge datasets
combined <- merge(gdp, fed, by.x='V1', by.y='CountryCode', sort=TRUE)
View(combined)

# Q3.
# sort the data
combined[with(combined, order(-V2) )]

# Q4.
# OECD
mean(combined[combined$Income.Group=='High income: OECD',]$V2)
# non OECD
mean(combined[combined$Income.Group=='High income: nonOECD',]$V2)

# Q5.
# assign quentile values
quentile <- c(0.2,0.4,0.6,0.8,1)
q <- quantile(combined$V2, quentile)
q1 <- combined$V2 <= 38

xtabs(q1 ~ combined$Income.Group)

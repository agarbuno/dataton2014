library(data.table)
library(lubridate)
library(plyr)

library(doParallel)
registerDoParallel(cores=detectCores())

twitter <- read.csv('/Users/alfredogarbuno/dataton/redes-sociales/Twitter.csv', sep = ',')
foursq <- read.csv('/Users/alfredogarbuno/dataton/redes-sociales/Foursquare.csv', sep = ',')

twitter[,4] <- NULL
foursq[,4]  <- NULL

twitter <- data.table(twitter)
foursq <- data.table(foursq)

setnames(twitter, names(twitter), c('timestamp', 'lat', 'lon', 'user'))
setnames(foursq, names(foursq), c('timestamp', 'lat', 'lon', 'user'))

twitter$social <- 'twitter'
foursq$social <- 'foursq'

social <- rbind(twitter, foursq)
social$timestamp <- sub('T', ' ', social$timestamp)
social$timestamp <- sub('Z', ' ', social$timestamp)
time <- strptime(social$timestamp, "%Y-%m-%d %H:%M:%S")

rm('twitter')
rm('foursq')

social$date <- as.Date(time)
social$time <- hour(time) + minute(time)/60 + second(time)/3600

users <- social[, list(count = .N, min.date = min(date), max.date = max(date), days = as.numeric(max(date) - min(date))
                       , ratio = .N/as.numeric(max(date) - min(date))), by = list(user)]
users <- users[order(count)]

res <- social[,data.table(kmeans(cbind(lat,lon),centers=1)$centers, count = .N),by=user]
res

#res <- ddply(social, 'user', summarise, mean = mean(time))

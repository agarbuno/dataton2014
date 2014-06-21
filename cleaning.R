#install.packages(c('data.table', 'lubridate', 'plyr', 'doParallel'))
#install.packages('stringi')

library(data.table)
library(lubridate)
library(plyr)
library(stringi)

library(doParallel)
registerDoParallel(cores=detectCores())

twitter <- read.csv('~/dataton/redes-sociales/Twitter.csv', sep = ',')
foursq <- read.csv('~/dataton/redes-sociales/Foursquare.csv', sep = ',')

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
social$hour <- paste(substr(formatC(hour(time), digits = 2, flag = 0), 2,3)
                     , substr(formatC(minute(time), digits = 2, flag = 0), 2,3)
                     , substr(formatC(second(time), digits = 2, flag = 0), 2,3)
                     , sep = ':')
social

users <- social[, list(count = .N, min.date = min(date), max.date = max(date), days = as.numeric(max(date) - min(date) + 1 )
                       , ratio = .N/as.numeric(max(date) - min(date) + 1)), by = list(user)]
users <- users[order(count)]
users <- subset(users, days <= 10)

social$day <- wday(social$date, label = TRUE, abbr = FALSE)
turistas <- subset(social, user %in% users$user)

#res <- social[,data.table(kmeans(cbind(lat,lon),centers=1)$centers, count = .N),by=user]
#res <- ddply(social, 'user', summarise, mean = mean(time))

rm(time)

# Codigo para tratamientos de texto en foursquare.... no utilizado

# foursq <- read.csv('~/dataton/redes-sociales/Foursquare.csv', sep = ',', encoding = 'UTF-8')
# foursq <- data.table(foursq)
# setnames(foursq, names(foursq), c('timestamp', 'lat', 'lon', 'text', 'user'))
# 
# fsq <- subset(foursq, user %in% users$user)
# fsq$text <- as.character(fsq$text)
# fsq$id <- seq(1,nrow(fsq))
# 
# fsq <- fsq[, list(id, user, text)]
# #write.csv(fsq, '~/dataton/redes-sociales/fsq.csv', sep = '|', row.names = FALSE)
# 
# fsq.clean <- read.csv('~/dataton/redes-sociales/fsq-clean.csv', header = FALSE, sep = ',')
# fsq.clean <- data.table(fsq.clean)
# setnames(fsq.clean, c('id', 'user', 'venue'))
# fsq.clean <- fsq.clean[order(id)]
# fsq.clean$venue <- sub("\"", "", fsq.clean$venue)
# fsq.clean$venue <- sub("\"", "", fsq.clean$venue)
# fsq.clean <- unique(fsq.clean)
# 
# tmp <- fsq.clean[, list(count = .N), by = id]
# tmp <- subset(tmp, count > 1)

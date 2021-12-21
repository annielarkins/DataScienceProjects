library(aplpack)
library(tidyverse)

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")

top80s <- read.csv('top_80.csv')
top80s_filled <- cbind(top80s, rep(0, length(top80s$Track.Name)))
top80s_filled <- cbind(top80s_filled, rep(0, length(top80s$Track.Name)))
top80s_filled <- cbind(top80s_filled, rep(0, length(top80s$Track.Name)))
name_pieces <- str_split(top80s$Track.Name, '-')
song_labels <- sapply(name_pieces,"[[",1)
faces(top80s_filled[,19:33], labels=song_labels, ncol.plot = 4, cex = 1.5, face.type = 0)



fall <- read.csv('fall_2021.csv')

fall[,19:30]

drake <- read.csv('this_is_drake.csv')

beatles <- read.csv('abbey_road.csv')

name_pieces <- str_split(beatles$Track.Name, '-')
song_labels <- sapply(song_labels,"[[",1)
faces(beatles[,19:30], labels=song_labels, ncol.plot = 4, print.info = TRUE, face.type = 1)


faces(crime[,2:8], nrow.plot = 4)

crime_filled <- cbind(crime[,1:6], rep(0, length(crime$state)), crime[,7:8])

faces(crime_filled[,2:8], labels=crime_filled$state)

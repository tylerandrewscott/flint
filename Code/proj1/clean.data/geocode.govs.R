library(ggmap)

library(stringr)
sd.df = read.csv('Input/officials.data/cleaned/sd.master.csv')
sd.df = sd.df[!is.na(sd.df$SD) & !duplicated(sd.df$SD),]
sd.df$mailingzip = str_extract(sd.df$mailingzip,'^[0-9]{5}')
sd.df$code.query = paste(
  sd.df$mailingaddress,sd.df$mailingcity,
  sd.df$mailingstate,sd.df$mailingzip,sep=',')

lg.df = read.csv('Input/officials.data/cleaned/locgov.master.csv')
lg.df = lg.df[!duplicated(lg.df$Org.Name),]
lg.df$code.query = paste(lg.df$Org.Name,'Georgia',sep=',')


all.loc.query = c(sd.df$code.query,lg.df$code.query)

library(doParallel)

library(parallel)

sd.geo.locs = ggmap::geocode(sd.df$code.query,output='more')
lg.geo.locs = ggmap::geocode(lg.df$code.query,output='more')


sd.df$code.query

client.id = '29813640204-4t8db4q0pbe1kkh7ln1mb8rv3tgr00a7.apps.googleusercontent.com'
secret = 'ExzKTQ2-uNPK5DG6TwnDUbIx'
load('start.geocode.RData')
save.image('start.geocode.RData')
last.part.lg = ggmap::geocode(lg.df$code.query[218:length(lg.df$code.query)])
last.part.lg
tail(lg.geo.locs$lon,600)
head(lg.df[is.na(lg.geo.locs$lon),])
head(lg.geo.locs[is.na(lg.geo.locs$lon),])
ggmap::geocodeQueryCheck()
ggmap::geocode('Ludowici City,Georgia',client = client.id,signature = secret)

geomat.key  = 'AIzaSyDloZmP_tIxnaM17fc3ebUNQCdxYKUX5lA'
qbase = 'https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins='
origins = paste(test, collapse="|")
destinations = paste(test, collapse="|")
qstring = paste0(qbase,origins,'&destinations=',destinations,'&key=',geomat.key)

return = getURL(qstring)


1104 * 678
library()


test = sd.df$code.query[1:3]



library(RCurl)
library(jsonlite)
tt = fromJSON(qstring,)
qstring
tt

RCurl::getURL(qstring)
origins=Washington,DC&destinations=New+York+City,NY&key=YOUR_API_KEY
library(plyr);library(dplyr)



qstring


library(plyr);library(dplyr);library(readxl)


list.of.files = list.files('Input/fiscal.data/DCA Authority Data/dca_authorities')
list.of.files = list.of.files[grep('xlsx',list.of.files)]
list.of.files = as.list(paste0('Input/fiscal.data/DCA Authority Data/dca_authorities/',list.of.files))


list.data.sheet1 = lapply(list.of.files,read_excel)
all.data.sheet1 = do.call(rbind,list.data.sheet1)
all.data.sheet1 = all.data.sheet1 %>% 
  rename(total.current.assets = P2N204,
         total.noncurrent.assets = P2N212,
         total.assets = P2N213,
         net.assets = P2N228)

list.data.sheet2 = lapply(list.of.files,read_excel,sheet=2)
all.data.sheet2 = do.call(rbind,list.data.sheet2)
all.data.sheet2 = all.data.sheet2 %>% rename(
  from.state = P3N311,
from.fed = P3N312,P2AuthName = P3AuthName,
from.loc = P3N313,
total.non.op.rev = P3N317,
total.op.rev = P3N304) %>% mutate(total.rev = total.op.rev + total.non.op.rev)

test = join(all.data.sheet1,all.data.sheet2,type='full')

write.csv(test,'Input/fiscal.data/cleaned.fiscal.csv')
head(test)
test = 
  
  all.data.sheet2 %>% group_by(P2AuthName,Year,AuthType) %>% 
  summarise(m1 = from.loc/total.rev,m2 = total.op.rev/total.rev) %>% filter(Year==2011)


summary(all.data.sheet2$total.rev)

summary(all.data.sheet2$P3N315)
summary(all.data.sheet2$P3N316)
names(all.data)

all.data.sheet2$P3N314

s (own source rev/total revenue). Let's also do: (local govt transfers/total revenue) 

summary(all.data.sheet2$total.non.op.rev)
summary(all.data.sheet2$total.op.rev)
summary(all.data.sheet2$)

list.of.files[[1]]
 = read.xlsx2(list.of.files[1],sheet = 1)

library(readxl)
list.of.files
test = readxl::read_excel(list.of.files[1])
head(test)
list.of.files[1]
list.of.files
test = read.csv('Input/fiscal.data/exp5.csv')
head(test)



accessloc<-'Input/fiscal.data/RLGFData2014WC_Final.accdb'
# PostgreSQL: 'case' should be detected automatically
channel <- odbcConnect(dsn = 'Input/fiscal.data/RLGFData2014WC_Final.accdb')


read.csv('Input/fiscal.data/RLGFData2014WC_Final.accd')
openxlsx::openXL('Input/')

expd = read.csv('Input/fiscal.data/exp5.csv')
jurs = read.csv('Input/fiscal.data/ga.jurisdictions.csv')

head(expd)

expd$NAMEGOV




library(RODBC)
db<-file.path("Input/fiscal.data/RLGFData2014WC_Final.accdb")
channel<-odbcDriverConnect(db)
data<-sqlFetch(channel,"stud")
data

fisc = RODBC::odbcConnect()

db <- 'Input/fiscal.data/RLGF Data 2014WC_Final.accdb'
con2 <- odbcConnectAccess2007(db)
RODBC::


#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

library(RCurl)
library(RJSONIO)
library(plyr)


#create a variable which is a channel to the database - we call it channel1 for want of a better name - note the function used is specific to Access2007 and

channel1<-odbcConnectAccess2007(accessloc)
# we now work through a few function of RODBC to show you how it works. note that the connection is live, so any SQL command you give will work,
#including deleting data and database tables and databases!
#lets see what tables we have in the database
sqlTables(channel1)
#note that there you will have at least 13 tables, all but one are system tables that you wont be aware off, but you should see tblname in the last row of
your result.
# lets fetch just that table
sqlFetch(channel1,"tblname")
#you should see your rows of data which 



reg.01.12 = read.csv('Input/officials.data/RegistrationData_2001_2012.csv')
reg.13.15 = read.csv('Input/officials.data/RegistrationData_2013_2015.csv')

reg.01.12[reg.01.12$MailingAddress=='Post Office Box 550',]

paste(reg.01.12$MailingAddress,reg.01.12$MailingCity,reg.01.12$MailingState,reg.01.12$MailingZip)

grep('Twiggs',reg.01.12$authority_name,value=T)

reg.01.12[reg.01.12$authority_name=='Development Authority of the City of Jeffersonville and Twiggs County',]

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

##Test with a single address
#address <- geoCode("The White House, Washington, DC")
#address
#[1] "38.8976831"
#[2] "-77.0364972"
#[3] "APPROXIMATE"
#[4] "The White House, 1600 Pennsylvania Avenue Northwest, Washington, D.C., DC 20500, USA"

gsub('Bill Mitchell,','Bill Mitchell, ')

vecs = grep('Jackie Bullard|Bill Mitchell|Andy Hutto|Lynward lindsey|Lisa Summers|Maureen Stubbs|Dick Schneider|Diann Adams|Kathyjo Gordon|Matthew Chancey',
     sd.df$Contact.)
sd.df$Contact.[vecs] = 

sd.df$Contact. = gsub('Two ','2 ',sd.df$Contact.)
sd.df$Contact. = gsub('One ','2 ',sd.df$Contact.)
sd.df$Address = NA
sd.df$Address[grep('Box',sd.df$Contact.)] = str_extract(sd.df$Contact.[grep('Box',sd.df$Contact.)],'Box+.*[0-9]{5}')
sd.df$Address[grep('Box',sd.df$Contact.,invert=T)] = str_extract(sd.df$Contact.[grep('Box',sd.df$Contact.,invert=T)],'[0-9]+.*[0-9]{5}')
sd.df$Zip.Code = str_extract(sd.df$Contact.,'[0-9]{5}')
sd.df$Phone = str_extract(sd.df$Contact.,'[0-9]{3}-[0-9]{3}-[0-9]{4}')
sd.df$Director = gsub(',.*$','',sd.df$Contact.)


sd.df[sd.df$Address=='Box 2291973 Martin Luther King Jr. Drive  Soperton, Georgia 30457',]
uq.address = unique(sd.df$Address)

uq.geo = sapply(uq.address,geoCode)

uq.address


grep('Adel',city.df$City,value=T)
get.addresses = ddply(sd.df,.(Address),geoCode)

geoCode('1515 Lower Fayetteville Road  Newnan, Georgia 30265')

sd.df$Contact.

sd.df$Phone
[0-9]{3}-[0-9]{3}-[0-9]{4}

sd.df$Contact.[158:161]
  gsub("[^,]*, ","  ",sd.df$Contact.)   
                           
sd.df = read.csv('Input/officials.data/ga.local.authority.detailed.database.csv')   
sd.df$Contact. = as.character(sd.df$Contact.)
vecs = grep('Jackie Bullard|Bill Mitchell|Andy Hutto|Lynward lindsey|Lisa Summers|Maureen Stubbs|Dick Schneider|Diann Adams|Kathyjo Gordon|Matthew Chancey',
            sd.df$Contact.)                        

gsub("[^,]*","  ",sd.df$Contact.[vecs])  


sd.df$Contact.[vecs]

sd.df$Contact. = gsub("[^,]*","  ",sd.df$Contact.)
)
geoCode(unique(sd.df$Contact.)[1])
sd.df$Contact.[1]

split.contact = stringr::str_split(sd.df$Contact,"  ")
unique(split.contact[unlist(lapply(split.contact,length))<4])
sd.df$Contact.[unlist(lapply(split.contact,length))<4]
table(unlist(lapply(split.contact,length)))
sd.df[unlist(lapply(split.contact,length))<4,]
sd.df[1,]
geoCode(sd.df$Contact.[1])

geoCode('154 Vine St, Athens, GA')

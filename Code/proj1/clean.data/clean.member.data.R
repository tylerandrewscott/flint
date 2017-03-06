rm(list=ls())
test.string = 'The Development Authority of the City of Camilla'
library(stringr)
year.vec.sd = 2011:2014
year.vec.lg = 2011:2014
library(plyr);library(dplyr);library(foreach)
city.df = read.csv('Input/officials.data/raw/ga.city.officials.2011.2015.csv')
county.df = read.csv('Input/officials.data/raw/ga.county.officials.2010.2015.csv')
city.df = city.df[!duplicated(city.df),]
city.df = city.df %>% select(-X)
county.df = county.df %>% select(-X,-Base)
#county.df = county.df %>% select(-X,-Link,-Date)
county.df = county.df[grep('Vacant|VACANT',county.df$Name,invert=T),]

county.df = county.df %>% dplyr::rename(Org.Name = County,Title = Position)
county.df$Org.Name = paste(county.df$Org.Name,'County',sep=' ')

city.df = city.df[city.df$Name != '',]
city.df = city.df %>% dplyr::rename(Org.Name = City)
city.df$Org.Name = paste(city.df$Org.Name,'City', sep=' ')

county.df$Type = 'County'
city.df$Type = 'City'
city.df = city.df %>% filter(Name != ' '&Name!='')
county.df = county.df %>% filter(Name != ' '&Name!='')
#city.df$Name = city.df$Name %>% as.character(.) %>% gsub("[[:space:]]", "", .)
county.df$Name = county.df$Name %>% gsub('<b>|</b>','',.) %>% gsub('<..>|\\n|</..>|','',.)

library(readxl)
sd.df = readxl::read_excel('Input/officials.data/raw/RegistrationData_2010_2015.xlsx',sheet=1)
sd.df2 = readxl::read_excel('Input/officials.data/raw/RegistrationData_2010_2015.xlsx',sheet=2)

names(sd.df) = gsub('\\.|-| ','_',tolower(names(sd.df)))
names(sd.df2) = gsub('\\.|-| ','_',tolower(names(sd.df2)))
sd.df = sd.df %>% filter(year>=2010)

sd.df = sd.df %>% select(-board) %>% dplyr::rename(board_members = clean_board_characters,member_governments = namegov)


sd.df = sd.df %>% filter(!is.na(sd.df$authority_name)) %>% filter(board_members!='NA')
sd.df = sd.df[grep('active|Active',sd.df$board_members,invert=T),]

#sd.df = read.table('Input/officials.data/RegistrationData_2001_2012.txt')
#sd.df2 = read.csv('Input/officials.data/RegistrationData_2013_2015.csv')

sd.df2 = sd.df2 %>% select(-board) %>% dplyr::rename(contact_phone = phone,contact_title = title,mailingaddress = address,mailingzip = zip,
                           mailingcity=city,mailingstate=state,board_members = clean_board_characters,
                           juris_type = single_or_multi_jurisdictional)


sd.df = full_join(sd.df,sd.df2)

sd.df = sd.df %>% filter(!is.na(sd.df$year)) %>% dplyr::rename(SD = authority_name)


sd.df$juris_type[grep('Single|single',sd.df$juris_type)] = 'single'
sd.df$juris_type[grep('Single|single',sd.df$juris_type,invert=T)] = 'multi'

sd.df = sd.df %>% dplyr::rename(Year = year) %>% filter(grepl('Test',SD)==F)
sd.df = sd.df %>% filter(Year %in% year.vec.sd)

sd.df$Type = basic.info$Type[match(tolower(sd.df$SD), tolower(basic.info$Authority.Name))]
sd.df$Type[is.na(sd.df$Type)&grepl('Hospital',sd.df$SD)] = 5
sd.df$Type[is.na(sd.df$Type)&grepl('Downtown Development',sd.df$SD)] = 4
sd.df$Type[is.na(sd.df$Type)&grepl('Joint Development',sd.df$SD)] = 8
sd.df$Type[is.na(sd.df$Type)&grepl('Industrial Development',sd.df$SD)] = 7
sd.df$Type[is.na(sd.df$Type)&grepl('Housing',sd.df$SD)] = 6
sd.df$Type[is.na(sd.df$Type)&
                 grepl('Development',sd.df$SD) &
                 grepl('Downtown',sd.df$SD)==F &
                 grepl('Joint',sd.df$SD)==F &
                 grepl('Industrial',sd.df$SD)==F] = 3
sd.df$Type[is.na(sd.df$Type)&grepl('Building',sd.df$SD)] = 2
sd.df$Type[is.na(sd.df$Type)&grepl('911|Public Safety',sd.df$SD)] = 23
sd.df$Type[is.na(sd.df$Type)&grepl('Recreation',sd.df$SD)] = 12
sd.df$Type[is.na(sd.df$Type)&grepl('Water',sd.df$SD)&grepl('Sewer|Sewage',sd.df$SD)] = 18
sd.df$Type[is.na(sd.df$Type)&grepl('Waste Management',sd.df$SD)] = 17
sd.df$Type[is.na(sd.df$Type)&grepl('Public Facilities',sd.df$SD)] = 10
sd.df$Type[is.na(sd.df$Type)&grepl('Economic',sd.df$SD)] = 3
sd.df$Type[is.na(sd.df$Type)&grepl('Residential Care',sd.df$SD)] = 14



type.guide = data.frame(Type = c('01','02','03','04','23','05','06','07','08','22','20','09','10','11','12','13','14','15',
  '17','16','19','21','18'),
Type.Name = c('Airport','Building','Development','Downtown Development','E-911','Hospital',
'Housing','Industrial Development','Joint Development','Landbank','Other','Parking',
'Public Service','Public Transit','Recreation','Regional Jail','Residential Care of the Elderly',
'Resource Recovery','Solid Waste Management','Stadium and Coliseum','Tourism',
'Urban Redevelopment','Water and Sewer'))

sd.df = join(sd.df,type.guide)

sd.df$board_members = gsub('[\\(.*\\)]','',sd.df$board_members)
sd.df$board_members = gsub('  ',' ',sd.df$board_members)
sd.df$board_members = gsub('".*"','',sd.df$board_members)
sd.df$board_members = gsub("'.*'",'',sd.df$board_members)

#sd.df$board_zzz = gsub("", "\\1zzz\\2", sd.df$board_members)
sd.df$zzz_count = str_count(sd.df$board_members,'ZZZ')
board_split = str_split(sd.df$board_members,'ZZZ')


member.sd.pairs = sapply(1:length(board_split), function(x) data.frame(SD = sd.df$SD[x],
                                                                             Name = board_split[[x]],
                                                                             Year = sd.df$Year[x]),simplify=FALSE)
sd.boards.df = do.call('rbind',member.sd.pairs)
sd.boards.df$Name = as.character(sd.boards.df$Name)
sd.boards.df$Name = gsub(' $','',sd.boards.df$Name)
sd.boards.df$Name = gsub('^ ','',sd.boards.df$Name)
sd.boards.df$Name = gsub('\\,$','',sd.boards.df$Name)
sd.boards.df = sd.boards.df[sd.boards.df$Name!='',]

sd.boards.df = join(sd.boards.df,sd.df,match='first')
sd.boards.df = sd.boards.df[!duplicated(paste(sd.boards.df$Name,sd.boards.df$SD,sd.boards.df$Year)),]

loc.govs = full_join(city.df,county.df)
loc.govs$Name = gsub('Robert Snead','Robert Sneed',loc.govs$Name)


loc.gov.names = sort(unique(loc.govs$Org.Name))

sd.names = sort(unique(sd.df$authority_name))

temp.lg  = loc.govs %>% filter(Year %in% year.vec.lg)
temp.lg$Org.Name = gsub('/','-',temp.lg$Org.Name)

temp.lg$Org.Name[grep('Cusseta-Chattahoochee|Athens-Clarke|Macon-Bibb|
                      Columbus-Muscogee|Statenville-Echols|Augusta-Richmond|Georgetown-Quitman|Preston-Webster',temp.lg$Org.Name)] = 
  gsub('County City','CG',
       temp.lg$Org.Name[grep('Cusseta-Chattahoochee|Athens-Clarke|Macon-Bibb|
                             Columbus-Muscogee|Statenville-Echols|Augusta-Richmond|Georgetown-Quitman|Preston-Webster',temp.lg$Org.Name)])

temp.lg$Org.Name[grep('Cusseta-Chattahoochee|Athens-Clarke|Macon-Bibb|
                      Columbus-Muscogee|Statenville-Echols|Augusta-Richmond|Georgetown-Quitman|Preston-Webster',temp.lg$Org.Name)] = 
  gsub('County|City','CG',
       temp.lg$Org.Name[grep('Cusseta-Chattahoochee|Athens-Clarke|Macon-Bibb|
                             Columbus-Muscogee|Statenville-Echols|Augusta-Richmond|Georgetown-Quitman|Preston-Webster',temp.lg$Org.Name)])


temp.lg$Type[grep('CG',temp.lg$Org.Name)]  = 'CG'

temp.lg$Org.Name = gsub('City City','City',temp.lg$Org.Name)


temp.lg = temp.lg[!duplicated(paste(temp.lg$Name,temp.lg$Org.Name,temp.lg$Year)),]

sd.boards.df$Name = gsub(' {2,}',' ',sd.boards.df$Name)

#lg.master = read.csv('Input/officials.data/cleaned/locgov.master.csv',row.names=1)
write.csv(temp.lg,'Input/officials.data/cleaned/locgov.master.csv')
write.csv(sd.boards.df,'Input/officials.data/cleaned/sd.master.csv')


#write.csv(person.ties.11.15,'Input/officials.data/cleaned/person.ties.by.year.2011.2014.csv')
#write.csv(person.ties.total.in.period,'Input/officials.data/cleaned/person.ties.total.2011.2014.csv')

#five.year.avg = person.ties.11.15 %>% group_by(Loc.Gov,SD) %>% summarize(five.year.avg = sum(n)/5)

#write.csv(five.year.avg,'Input/officials.data/cleaned/person.ties.2011.2014.avg.csv')




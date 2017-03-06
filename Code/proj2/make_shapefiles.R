library(tidyverse)
library(readxl)
library(rgeos)
library(rgdal)

### Load base reference for SDs
# basic.info = fread('Input/officials.data/raw/ga.local.authority.reference.csv')
# basic.info = as.data.frame(basic.info)
# basic.info = basic.info %>% filter(!grepl('DCA Test',authority_name))
# basic.info$AuthID = gsub('&.*$','',gsub('^.*ssid=','',basic.info$href))
#basic.info$authority_name = gsub(' County','',basic.info$authority_name)

lg_members_of_sd_01_12 = read_excel('Input/officials.data/raw/RegistrationData_2001_2015.xlsx',sheet='01_12')
lg_members_of_sd_13_15 = read_excel('Input/officials.data/raw/RegistrationData_2001_2015.xlsx',sheet='13_15')
lg_members_of_sd = full_join(lg_members_of_sd_01_12,lg_members_of_sd_13_15)

lg_members_of_sd = lg_members_of_sd %>% filter(!is.na(authority_name) & !grepl('Test ',authority_name))

lg_members_of_sd$NAMEGOV = gsub('\\/','-',lg_members_of_sd$NAMEGOV)
lg_members_of_sd$NAMEGOV = gsub('Town$','City',lg_members_of_sd$NAMEGOV)
lg_members_of_sd$authority_name = gsub('  ',' ',lg_members_of_sd$authority_name)

#lg_members_of_sd = lg_members_of_sd %>% dplyr::filter(authority_name %in% basic.info$authority_name)
#lg_members_of_sd$AuthID = basic.info$AuthID[match(lg_members_of_sd$authority_name,basic.info$authority_name)]

#### load city and county data #####
library(sp)
library(maptools)
counties = readOGR(dsn='spatial_data',layer='PVS_15_v1_county_13')
counties@data$NAMELSAD = str_to_title(counties@data$NAMELSAD)
places = readOGR(dsn='spatial_data',layer = 'PVS_15_v1_place_13')
places@data$NAMELSAD = gsub(' \\(balance\\)','',places@data$NAMELSAD)
places@data$NAMELSAD  = gsub(' County unified government',' CG',places@data$NAMELSAD)
places@data$NAMELSAD  = gsub(' County consolidated government',' CG',places@data$NAMELSAD)
names(counties)[names(counties) =='COUNTYFP'] = 'UNITFP'
names(places)[names(places)=='PLACEFP'] = 'UNITFP'
names(counties)[names(counties) =='COUNTYNS'] = 'UNITNS'
names(places)[names(places)=='PLACENS'] = 'UNITNS'
places@data = places@data %>% dplyr::select(-PARTFLG)
places = sp::spChFIDs(places,as.character(places$UNITFP))
counties = sp::spChFIDs(counties,as.character(counties$UNITFP))
#all_local = spRbind(places,counties)
places$NAMELSAD = gsub(' town$',' city',places$NAMELSAD)

library(stringr)

places$NAMELSAD[places$NAMELSAD == 'De Soto city'] = 'Desoto city'
places$NAMELSAD[places$NAMELSAD == 'Du Pont city'] = 'Dupont city'
places$NAMELSAD[places$NAMELSAD == 'Pine Lake city'] = 'Pinelake city'
places$NAMELSAD[places$NAMELSAD == 'Edge Hill city'] = 'Edgehill city'

places$NAMELSAD[places$NAMELSAD == 'Macon-Bibb County'] = 'Macon-Bibb County CG'
places$NAMELSAD[places$NAMELSAD == 'Webster CG'] = 'Preston-Webster CG'
places$NAMELSAD[places$NAMELSAD == 'Columbus city'] = 'Columbus-Muscogee CG'
places$NAMELSAD[places$NAMELSAD == 'Echols County'] = "Statenville-Echols CG"
places$NAMELSAD = gsub('McRae','Mcrae',places$NAMELSAD)
places$NAMELSAD = str_to_title(places$NAMELSAD)
places$NAMELSAD = gsub('Cg$','CG',places$NAMELSAD)
consols = places[grep('CG',places$NAMELSAD),]
places = places[grep('CG',places$NAMELSAD,invert=T),]

lg_members_of_sd$NAMEGOV = str_to_title(lg_members_of_sd$NAMEGOV)
lg_members_of_sd$NAMEGOV = gsub('Cg$','CG',lg_members_of_sd$NAMEGOV)
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV=="Macon-Bibb County"] = 'Macon-Bibb County CG'
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV=="Pine Lake City"] = 'Pinelake City'
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV=="Echols County CG"] = "Echols County"
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV=="Webster County Unified" ] ='Preston-Webster CG'

comp_set = sort(c(counties$NAMELSAD,places$NAMELSAD,consols$NAMELSAD))

lg_members_of_sd$NAMEGOV[!lg_members_of_sd$NAMEGOV %in% comp_set & (paste0(lg_members_of_sd$NAMEGOV,' City') %in% comp_set)] = 
  paste0(lg_members_of_sd$NAMEGOV[!lg_members_of_sd$NAMEGOV %in% c(counties$NAMELSAD,places$NAMELSAD) & (paste0(lg_members_of_sd$NAMEGOV,' City') %in% comp_set)],' City')



#### Make SD polygons based upon government members of each sd (e.g., if SD is Baldwin and Jackson counies, SD polygon is union of Baldwin and Jackson polygons)
all_lg = spRbind(spRbind(places,counties),consols)
all_lg$NAMELSAD = str_to_title(all_lg$NAMELSAD)
all_lg$NAMELSAD = gsub('Cg$','CG',all_lg$NAMELSAD)

lg_members_of_sd = as.data.frame(lg_members_of_sd)
# sd_poly_list = lapply(sort(unique(lg_members_of_sd$authority_name)),function(sd)  
#   try(unionSpatialPolygons(all_lg[all_lg$NAMELSAD %in% lg_members_of_sd$member_governments[lg_members_of_sd$authority_name==sd],],
#                                      IDs = rep(sd,sum(all_lg$NAMELSAD %in% lg_members_of_sd$member_governments[lg_members_of_sd$authority_name==sd])))))

sd_poly_list = lapply(sort(unique(lg_members_of_sd$authority_name)),function(spd)  
  unionSpatialPolygons(all_lg[all_lg$NAMELSAD %in% lg_members_of_sd$NAMEGOV[lg_members_of_sd$authority_name==spd],],
                           IDs = rep(which(spd == sort(unique(lg_members_of_sd$authority_name))),sum(all_lg$NAMELSAD %in% lg_members_of_sd$NAMEGOV[lg_members_of_sd$authority_name==spd]))))

sdp_list = lapply(sd_poly_list,function(x) x@polygons[[1]])
sdp_sp = SpatialPolygons(sdp_list)
proj4string(sdp_sp) =  CRS(proj4string(all_lg))   

basics_to_add = lg_members_of_sd %>% dplyr::select(-Year,-CONTACT_PERSON,-NAMEGOV,-CONTACT_TITLE,
                                                   -CONTACT_PHONE,
                                            -MailingAddress,-MailingCity,
                                            -MailingState,-MailingZip,-BOARD,
                                            -Clean_Board_Characters) %>%
 mutate(authority_name = as.character(authority_name)) %>% filter(!duplicated(authority_name))

sd_spdf = SpatialPolygonsDataFrame(sdp_sp,
            data = data.frame(authority_name = 
         as.character(sort(unique(lg_members_of_sd$authority_name)))))
                    
sd_spdf@data = left_join(sd_spdf@data,basics_to_add,type='full')


library(rgdal)
writeOGR(sd_spdf, dsn = 'spatial_data/custom', layer = 'georgia_sd', driver = "ESRI Shapefile")
#writeOGR(places, dsn = 'spatial_data/custom', layer = 'georgia_places', driver = "ESRI Shapefile")
#writeOGR(consols, dsn = 'spatial_data/custom', layer = 'georgia_consols', driver = "ESRI Shapefile")
#writeOGR(counties, dsn = 'spatial_data/custom', layer = 'georgia_counties', driver = "ESRI Shapefile")





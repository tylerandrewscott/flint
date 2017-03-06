


##### THIS SECTION LOADS 3 DATA SETS #########
####1: DCA SD REGISTRATION DATA THAT LINKS SD TO CONSTITUENT GOVS (SO WE KNOW WHAT COUNTY TO AGGREGATE TO)
####2: COUNTY SHAPEFILES
####3: PLACE SHAPEFILES
##### CODE IN THIS SECTION RECTIFIES NAMES, ADDS COUNTY_FIPS REFERENCE TO SD BASE DATA FOR AGGREGATING FISCAL DATA LATER
library(tidyverse)
library(readxl)
library(rgeos)
library(rgdal)
library(stringr)
set_year = 2004:2016
#### Load special district local gov institutional affiliates
lg_members_of_sd_01_12 = read_excel('Input/officials.data/raw/RegistrationData_2001_2015.xlsx',sheet='01_12')
lg_members_of_sd_13_15 = read_excel('Input/officials.data/raw/RegistrationData_2001_2015.xlsx',sheet='13_15')

lg_members_of_sd = full_join(lg_members_of_sd_01_12,lg_members_of_sd_13_15) %>% filter(Year %in% set_year)
lg_members_of_sd = lg_members_of_sd %>% filter(!is.na(authority_name) & !grepl('Test ',authority_name))
lg_members_of_sd$NAMEGOV = gsub('\\/','-',lg_members_of_sd$NAMEGOV)
lg_members_of_sd$NAMEGOV = gsub('Town$','City',lg_members_of_sd$NAMEGOV)
lg_members_of_sd$authority_name = gsub('  ',' ',lg_members_of_sd$authority_name)
lg_members_of_sd$NAMEGOV = str_to_title(lg_members_of_sd$NAMEGOV)
lg_members_of_sd$NAMEGOV = gsub('Cg$','CG',lg_members_of_sd$NAMEGOV)
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV=="Macon-Bibb County"] = 'Macon-Bibb County CG'
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV=="Pine Lake City"] = 'Pinelake City'
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV=="Echols County CG"] = "Echols County"
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV=="Webster County Unified" ] ='Preston-Webster CG'
lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV %in% c("Union City","Peachtree City","Garden City",'Lumber City','Mountain City','Twin City')] = 
  paste0(lg_members_of_sd$NAMEGOV[lg_members_of_sd$NAMEGOV %in% c("Union City","Peachtree City","Garden City",'Lumber City','Mountain City','Twin City')],' City')
lg_members_of_sd$authority_name = gsub(' GA$| Georgia$| Georgi$| Georg$| Ga\\.| Ga$| GA\\.$','',lg_members_of_sd$authority_name)
lg_members_of_sd$authority_name = gsub('\\,$','',lg_members_of_sd$authority_name)
#load city and county data
library(sp)
library(maptools)
counties = readOGR(dsn='spatial_data',layer='PVS_15_v1_county_13')
counties@data$NAMELSAD = str_to_title(counties@data$NAMELSAD)
names(counties)[names(counties) =='COUNTYFP'] = 'UNITFP'
names(counties)[names(counties) =='COUNTYNS'] = 'UNITNS'
counties@data$COUNTY_FIPS = paste0(counties@data$STATEFP,counties@data$UNITFP)
counties = sp::spChFIDs(counties,as.character(counties$UNITFP))
counties@data$COUNTY_FIPS = paste0(counties@data$STATEFP,counties@data$UNITFP)
counties@data$NAMELSAD[counties@data$NAMELSAD=='Clarke County'] = 'Athens-Clarke CG'
counties@data$NAMELSAD[counties@data$NAMELSAD=='Bibb County'] = 'Macon-Bibb County CG'
counties@data$NAMELSAD[counties@data$NAMELSAD=='Richmond County'] = 'Augusta-Richmond CG'
counties@data$NAMELSAD[counties@data$NAMELSAD=="Chattahoochee County"] = 'Cusseta-Chattahoochee CG'
counties@data$NAMELSAD[counties@data$NAMELSAD=="Webster County"] = 'Preston-Webster CG'
counties@data$NAMELSAD[counties@data$NAMELSAD=="Quitman County"] = 'Georgetown-Quitman CG'
counties@data$NAMELSAD[counties@data$NAMELSAD=="Muscogee County"] = 'Columbus-Muscogee CG'
places = readOGR(dsn='spatial_data',layer = 'PVS_15_v1_place_13')
places@data$NAMELSAD = gsub(' \\(balance\\)','',places@data$NAMELSAD)
places@data$NAMELSAD  = gsub('County unified government|County consolidated government','CG',places@data$NAMELSAD)
places = places[!grepl('CG|Macon-Bibb',places@data$NAMELSAD),]
names(places)[names(places)=='PLACEFP'] = 'UNITFP'
names(places)[names(places)=='PLACENS'] = 'UNITNS'
places@data = places@data %>% dplyr::select(-PARTFLG)
places = sp::spChFIDs(places,as.character(places$UNITFP))
places$NAMELSAD = gsub(' town$',' city',places$NAMELSAD)
places$NAMELSAD[places$NAMELSAD == 'De Soto city'] = 'Desoto city'
places$NAMELSAD[places$NAMELSAD == 'Du Pont city'] = 'Dupont city'
places$NAMELSAD[places$NAMELSAD == 'Pine Lake city'] = 'Pinelake city'
places$NAMELSAD[places$NAMELSAD == 'Edge Hill city'] = 'Edgehill city'
places$NAMELSAD = str_to_title(places$NAMELSAD)
place_in_county = gWithin(gCentroid(places,byid = T),counties,byid = TRUE,returnDense = F)
places@data$COUNTY_FIPS = paste0(counties@data$COUNTY_FIPS[unlist(place_in_county)])
ref_place_to_cfips = full_join(
counties@data[,c('COUNTY_FIPS','NAMELSAD')],
places@data[,c('COUNTY_FIPS','NAMELSAD')])
### record each county fips associated with a special district by matching gov names
lg_members_of_sd$COUNTY_FIPS = ref_place_to_cfips$COUNTY_FIPS[match(lg_members_of_sd$NAMEGOV,ref_place_to_cfips$NAMELSAD)]



############### THIS SECTION LOADS SD FISCAL DATA #########
fiscal_data_subd = 'Input/fiscal.data/tima/'
code_book = read_excel(paste0(fiscal_data_subd,'GAAuthCleaned.xlsx'),sheet = 'VariableCodes',skip=1)
names(code_book) = c('Row','VAR','Var_Name','Notes')
#SE balance sheet
### TotCurrentAssets: short term assets (cash)
### TotCapAssets: long term assets
### TotNCA: total non-current assets
### TotalAssets: cash + non-cash
### TotCL: current liabilities (cash owed within next fiscal year)
### TotNCL: non-current liabilities (stuff that you owe in long term like pension funds)
### NetAssets: total assets - total liabilities (are you in deficit)
### Need to create: NetCurrentAssets = TotCurrentAssets - TotCL
bal_df = read_excel(paste0(fiscal_data_subd,'GAAuthCleaned.xlsx'),sheet = 'BAL')
#peach county ha shows up twice for a couple years for some reason, under a different name
bal_df  = bal_df %>% dplyr::rename(AuthID = P2AuthID,AuthName = P2AuthName) %>% filter(AuthName !='Peach County Hospital Authority')
#Yearly statement
### TotOR: Operating revenue
### TotOE: Operating expenses
### OperateInc: Net operating profit
stmt_df = read_excel(paste0(fiscal_data_subd,'GAAuthCleaned.xlsx'),sheet = 'STMT')
stmt_df = stmt_df %>% dplyr::rename(AuthID = P3AuthID,AuthName = P3AuthName) %>%
  filter(AuthName != 'Peach County Hospital Authority') %>%
  dplyr::select(-AuthType,-ordern3,-AuthName)
#Bond/debt
bonds_df = read_excel(paste0(fiscal_data_subd,'GAAuthCleaned.xlsx'),sheet = 'Bonds')
bonds_df = bonds_df %>% dplyr::rename(AuthID = P4AuthID,AuthName = P4AuthName) %>%
  filter(AuthName != 'Peach County Hospital Authority') %>%
  dplyr::select(-AuthType,-AuthName,-DateIssued,-ordern4)
leases_df = read_excel(paste0(fiscal_data_subd,'GAAuthCleaned.xlsx'),sheet = 'Leases')
leases_df = leases_df %>% dplyr::select(-DateLease) %>% 
  dplyr::rename(AuthID = P5AuthID,AuthName = P5AuthName) %>%
  filter(AuthName != 'Peach County Hospital Authority') %>%
  dplyr::select(-AuthType,-AuthName,-ordern5)

sd_fiscal_df = left_join(left_join(left_join(bal_df,stmt_df),bonds_df),leases_df)




sd_fiscal_df = sd_fiscal_df %>% mutate(RevFromLG = LocGovGrantsNORE / {TotOE + (TotNORE - LessIntExpenseNORE)})%>% 
  filter(Year %in% set_year) %>%
  dplyr::select(AuthID,AuthName,Year,AuthType,EndBalanceBonds,CapLeaseLiability,TotOE,RevFromLG) %>%
  rename(authority_name = AuthName) %>% mutate(Year = as.character(Year))

sd_fiscal_df$authority_name[grepl('Athens',sd_fiscal_df$authority_name) & grepl('Public',sd_fiscal_df$authority_name)] = 'Athens-Clarke County Public Facilities Authority'
sd_fiscal_df$authority_name[grepl('Atlanta Development Authority',sd_fiscal_df$authority_name)] = "The Atlanta Development Authority d/b/a Invest Atlanta"
sd_fiscal_df$authority_name[grepl("Downtown Development Authority of Smyrna",sd_fiscal_df$authority_name)] = "Downtown Smyrna Development Authority"
sd_fiscal_df$authority_name[grepl("Decatur School Building Authority",sd_fiscal_df$authority_name)] = "Decatur County School Building Authority"
sd_fiscal_df$authority_name[grepl("Hospital Authority of the City of Bainbridge and D",sd_fiscal_df$authority_name)] =  "Hospital Authority of the City of Bainbridge and Decatur County"
sd_fiscal_df$authority_name[grepl("Joint Development Authority of Brooks, Colquitt, Grady, Mitchell, and Thomas Counties",sd_fiscal_df$authority_name)] =  "Joint Development Authority of Colquitt, Mitchell, Grady, Thomas and Brooks Counties"
sd_fiscal_df$authority_name[grepl("City of Stockbridge, Georgia Downtown Development Authority" ,sd_fiscal_df$authority_name)] =  "City of Stockbridge, Downtown Development Authority"
sd_fiscal_df$authority_name[grepl("Carroll City/County Hospital Authority",sd_fiscal_df$authority_name)] = "The Carroll City_County Hospital Authority"
sd_fiscal_df$authority_name[grepl("City of Alpharetta Development Authority",sd_fiscal_df$authority_name)] = "Development Authority of Alpharetta"
sd_fiscal_df$authority_name[grepl("Clay County Development Authority",sd_fiscal_df$authority_name)] = "Development Authority of Clay County"
sd_fiscal_df$authority_name[grepl("Pulaski County Hospital Authority",sd_fiscal_df$authority_name)] = "Hospital Authority of Pulaski County"
sd_fiscal_df$authority_name[grepl("Winder Downtown Development Authority" ,sd_fiscal_df$authority_name)] = "The Winder Downtown Development Authority" 
sd_fiscal_df$authority_name[grepl("Dahlonega Downtown Development Authority" ,sd_fiscal_df$authority_name)] =  "Downtown Development Authority of the City of Dahlonega"
sd_fiscal_df$authority_name[grepl("Crisp County Hospital Authority" ,sd_fiscal_df$authority_name)] =  "Hospital Authority of Crisp County" 
sd_fiscal_df$authority_name[grepl("Doraville Downtown Development Authority" ,sd_fiscal_df$authority_name)] =  "Downtown Development Authority of the City of Doraville"
sd_fiscal_df$authority_name[grepl( "Downtown Development Authority of the City of Doraville" ,sd_fiscal_df$authority_name)] = "Development Authority of Cumming"  
sd_fiscal_df$authority_name[grepl("Development Authority of Emanuel County and the Ci" ,sd_fiscal_df$authority_name)] =  "Development Authority of Emanuel County and the City of Swainsboro" 
sd_fiscal_df$authority_name[grepl("City of Cumming Development Authority"  ,sd_fiscal_df$authority_name)] =  "Development Authority of Cumming"  
sd_fiscal_df$authority_name[grepl("Liberty County Industrial Development Authority"  ,sd_fiscal_df$authority_name)] =  "Liberty County Industrial Authority"  
sd_fiscal_df$authority_name[grepl("Grady County Joint Development Authority" ,sd_fiscal_df$authority_name)] =  "Joint Grady County Development Authority" 
sd_fiscal_df$authority_name[grepl("Henry County Water and Sewerage Authority" ,sd_fiscal_df$authority_name)] =  "Henry County Water Authority"  
sd_fiscal_df$authority_name[grepl( "Union County Hospital Authority"   ,sd_fiscal_df$authority_name)] =  "Hospital Authority of Union County"
sd_fiscal_df$authority_name[grepl( "Milledgeville MainStreet/The Downtown Development"  ,sd_fiscal_df$authority_name)] = "Milledgeville MainStreet/The Downtown Development Authority of the City of Milledgeville"
sd_fiscal_df$authority_name[grepl( "Union County Hospital Authority"   ,sd_fiscal_df$authority_name)] =  "Hospital Authority of Union County"
sd_fiscal_df$authority_name[grepl(  "Towns County Water and Sewer Authority"    ,sd_fiscal_df$authority_name)] =  "Towns County Water and Sewerage Authority"
sd_fiscal_df$authority_name[grepl( "Swainsboro Emanuel County Parks and Recreation Aut"   ,sd_fiscal_df$authority_name)] = "Swainsboro Emanuel County Parks and Recreation Authority"
sd_fiscal_df$authority_name[grepl(  "Towns County Water and Sewer Authority"    ,sd_fiscal_df$authority_name)] =  "Towns County Water and Sewerage Authority"
sd_fiscal_df$authority_name[grepl( "Dalton-Whitfield Joint Development Authority"    ,sd_fiscal_df$authority_name)] =   "Dalton-Whitfield County Joint Development Authority" 
sd_fiscal_df$authority_name[grepl( "Fort Oglethorpe Downtown Development Authority"    ,sd_fiscal_df$authority_name)] = "Downtown Development Authority of the City of Fort Oglethorpe"
sd_fiscal_df$authority_name[grepl( "Joint Development Authority of Carroll, Haralson, Polk, Heard and Troup Counties"    ,sd_fiscal_df$authority_name)] = "Greater West Georgia Joint Development Authority"
sd_fiscal_df$authority_name =  gsub(' Georgia$| Georgi$| Georg$| Ga\\.$| GA$| Ga$| GA\\.','',sd_fiscal_df$authority_name)
sd_fiscal_df$authority_name = gsub('Auth$','Authority',sd_fiscal_df$authority_name)
sd_fiscal_df$authority_name = gsub('\\,$','',sd_fiscal_df$authority_name)
sd_fiscal_df$authority_name = gsub('  ',' ',sd_fiscal_df$authority_name)
sd_fiscal_df$authority_name = gsub('Mt\\.','Mount',sd_fiscal_df$authority_name)
sd_fiscal_df$authority_name =  gsub(' Monr$',' Monroe',sd_fiscal_df$authority_name)
sd_fiscal_df$authority_name =  gsub(' Forest Par$',' Forest Park',sd_fiscal_df$authority_name)


sd_fiscal_df$observed = 1

sd_fiscal_df = left_join(expand.grid(unique(sd_fiscal_df$authority_name),as.character(2004:2016)) %>% rename(authority_name = Var1,Year = Var2),sd_fiscal_df)

sd_fiscal_df$Year = as.numeric(sd_fiscal_df$Year)


for (i in 1:nrow(sd_fiscal_df))
{
  if(is.na(sd_fiscal_df$observed[i])&!sd_fiscal_df$Year[i]%in%c(2004,2016))
  {
    tempfill = sd_fiscal_df %>% filter(Year %in% c(Year[i]-1,Year[i]+1) & AuthID %in% AuthID[i] & !is.na(observed))
    if(nrow(tempfill)==2){
      sd_fiscal_df$EndBalanceBonds[i] = mean(tempfill$EndBalanceBonds)
      sd_fiscal_df$CapLeaseLiability[i] = mean(tempfill$CapLeaseLiability)
      sd_fiscal_df$RevFromLG[i] = mean(tempfill$CapLeaseLiability)
      sd_fiscal_df$TotOE[i] = mean(tempfill$TotOE)
      sd_fiscal_df$observed[i] = 2
    }
    }
}


sd_fiscal_df = sd_fiscal_df %>% mutate(Year = as.character(Year)) %>% filter(Year!='2004'&Year!='2016') %>% filter(!is.na(observed)) 


lg_members_of_sd$authority_name[grepl('Fulton County Hospital Authority',lg_members_of_sd$authority_name)] = "Hospital Authority of Fulton County"
lg_members_of_sd$authority_name[grepl('Quitman Development Authority',lg_members_of_sd$authority_name)] = "Quitman County Development Authority"
lg_members_of_sd$authority_name[grepl("Downtown Development Authority of Conyers"  ,lg_members_of_sd$authority_name)] = "Conyers Downtown Development Authority"
lg_members_of_sd$authority_name[grepl("The Housing Authority of the City of Mount Vernon" ,lg_members_of_sd$authority_name)] = "Housing Authority of the City of Mount Vernon"

##check for fiscal records without DCA name match
grep('Hospital',sort(unique(sd_fiscal_df$authority_name[!sd_fiscal_df$authority_name %in% lg_members_of_sd$authority_name])),value=T)
###AUTHORITIES WITH NO FISCAL MATCH
sort(unique(lg_members_of_sd$authority_name[!lg_members_of_sd$authority_name %in% sd_fiscal_df$authority_name]))

##unmatched money 
sum(sd_fiscal_df$EndBalanceBonds[!sd_fiscal_df$authority_name %in% lg_members_of_sd$authority_name],na.rm=T)
#$25,066,990
sum(sd_fiscal_df$CapLeaseLiability[!sd_fiscal_df$authority_name %in% lg_members_of_sd$authority_name],na.rm=T)
#$1,156,939
n_counties_df = lg_members_of_sd %>% group_by(authority_name,Year) %>% summarise(n_counties = length(unique(COUNTY_FIPS)))
lg_members_of_sd = left_join(lg_members_of_sd,n_counties_df)
n_govs_df = lg_members_of_sd %>% group_by(authority_name,Year) %>% summarise(n_govs = n())
lg_members_of_sd = left_join(lg_members_of_sd,n_govs_df)





######  Merge sd member refernces with sd fiscal data ####
#drop double counting instances for county-level aggregation, divide figures across member counties
sd_df = left_join(lg_members_of_sd,sd_fiscal_df) %>% filter(!duplicated(paste(authority_name,COUNTY_FIPS,Year))) %>% mutate(EndBalanceBonds = EndBalanceBonds/n_counties,
                                                                                                                            CapLeaseLiability = CapLeaseLiability/n_counties,
                                                                                                                            TotOE = TotOE/n_counties,RevFromLG = RevFromLG/n_counties)


#### load city and county fiscal data #####
library(stringr)
lg_rev = read_excel('Input/fiscal.data/tima/GA_LG_AccountingData_1985-2014.xlsx',sheet='Rev1')
lg_rev = lg_rev %>% filter(fiscyear %in% set_year) %>% 
  mutate(govname = str_to_title(govname)) %>% dplyr::select(-popgroup)

lg_debt1 = read_excel('Input/fiscal.data/tima/GA_LG_AccountingData_1985-2014.xlsx',sheet='Debt1')
lg_debt1 = lg_debt1 %>% filter(fiscyear %in% set_year) %>%
  dplyr::select(-govtype,-pop,-popgroup,-govname)

lg_debt2 = read_excel('Input/fiscal.data/tima/GA_LG_AccountingData_1985-2014.xlsx',sheet='Debt2')
lg_debt2 = lg_debt2 %>% filter(fiscyear %in% set_year) %>% 
  dplyr::select(-govname,-govtype,-pop,-popgroup)
lg_debt_temp = left_join(lg_debt1,lg_debt2)

lg_debt3 = read_excel('Input/fiscal.data/tima/GA_LG_AccountingData_1985-2014.xlsx',sheet='Debt3')
lg_debt3 = lg_debt3 %>% filter(fiscyear %in% set_year) %>% 
  dplyr::select(-govname,-govtype,-pop,-popgroup)
lg_debt_temp = left_join(lg_debt_temp,lg_debt3)

lg_debt4 = read_excel('Input/fiscal.data/tima/GA_LG_AccountingData_1985-2014.xlsx',sheet='Debt4')
lg_debt4 = lg_debt4 %>% filter(fiscyear %in% set_year) %>% 
  dplyr::select(-govname,-govtype,-pop,-popgroup)
lg_debt_temp = left_join(lg_debt_temp,lg_debt4)

lg_debt = lg_debt_temp

lg_exp1 = read_excel('Input/fiscal.data/tima/GA_LG_AccountingData_1985-2014.xlsx',sheet='Expend1')
lg_exp1 = lg_exp1 %>% filter(fiscyear %in% set_year) %>% 
  dplyr::select(-govname,-govtype,-pop,-popgroup)
lg_exp2 = read_excel('Input/fiscal.data/tima/GA_LG_AccountingData_1985-2014.xlsx',sheet='Expend3')
lg_exp2 = lg_exp2 %>% filter(fiscyear %in% set_year) %>% 
  dplyr::select(-govname,-govtype,-pop,-popgroup)
lg_exp = left_join(lg_exp1,lg_exp2)

lg_all = left_join(left_join(lg_rev,lg_debt),lg_exp) %>% filter(!is.na(govtype))

lg_all$govtype[lg_all$govid=='2134007'] = 2
lg_all$govtype[lg_all$govid=='2014002'] = 2
lg_all$govid[lg_all$govid %in% c("3152152","1152152")] = "3152152"
lg_all$govtype[lg_all$govid=='3152152'] = 3
lg_all$govname[lg_all$govid=="3152152"] = 'Preston-Webster CG'

lg_all$govid[lg_all$govid %in% c("1050050","3050050")] = "1050050"
lg_all$govtype[lg_all$govid=='1050050'] = 3
lg_all$govname[lg_all$govid=="1050050"] = 'Echols County'

lg_all$govtype[lg_all$govid=="2004001"] = 2
lg_all$govname[lg_all$govid=="2004001"] = 'Newton City'

lg_all$govtype[lg_all$govid=="2129003"] = 2

lg_all$govname[lg_all$govid=="3121121"] = 'Augusta-Richmond CG'
lg_all$govname[lg_all$govid=="3029029"] = 'Athens-Clarke CG'
lg_all$govname[lg_all$govid== "3106002"] = 'Columbus-Muscogee CG'

lg_all$govname = gsub('Chattahooche$','Chattahoochee',lg_all$govname)

lg_all$govname[lg_all$govtype==1 & !grepl('County',lg_all$govname)]= paste0(lg_all$govname[lg_all$govtype==1 & !grepl('County',lg_all$govname)],' County')

lg_all$govname[lg_all$govid== "3026026"] = 'Cusseta-Chattahoochee CG'

lg_all$govid[lg_all$govid %in% c("1011011","3011011")] = "3011011"
lg_all$govname[lg_all$govid=="3011011"] = "Macon-Bibb County CG"

lg_all$govid[lg_all$govid %in% c("2118001","1118118","3118118")] ="3118118"
lg_all$govtype[lg_all$govid=="3118118"] = 3
lg_all$govname[lg_all$govid=="3118118"] = 'Georgetown-Quitman CG'

lg_all$govname = gsub('Cit$|Ci$|To$|Town$','City',lg_all$govname)
lg_all$govname = gsub('\\/','-',lg_all$govname)


#lg_all$govname[lg_all$govtype==3] =  paste0(str_to_title(gsub(' CG$|C$','',lg_all$govname[lg_all$govtype==3])),' CG')
lg_all$govname[lg_all$govtype==2&!grepl('City',lg_all$govname)] = 
  paste0(lg_all$govname[lg_all$govtype==2&!grepl('City',lg_all$govname)],' City')

lg_all$govname[grep('Mcrae|Helena',lg_all$govname)] = "Mcrae-Helena City"
lg_all$govname[grep('Webster Unif|Webster County Unif',lg_all$govname)] = "Preston-Webster CG"
lg_all$govname = gsub('De Soto','Desoto',lg_all$govname)
lg_all$govname = gsub('Pine Lake','Pinelake',lg_all$govname)
lg_all$govname = gsub('Gum Branch','Gumbranch',lg_all$govname)
lg_all$govname = gsub('  ',' ',lg_all$govname)
lg_all$govname = gsub('Mt\\.','Mount',lg_all$govname)
lg_all$govname = gsub('Ft\\.','Fort',lg_all$govname)
lg_all$govname = gsub('Ft\\.','Fort',lg_all$govname)
lg_all$govname[lg_all$govname %in% c("Sale City","Union City","Peachtree City","Iron City","Garden City",'Lumber City',"Junction City",'Ray City','Lake City','Mountain City','Twin City')] = 
  paste0(lg_all$govname[lg_all$govname %in% c("Sale City","Union City","Peachtree City","Garden City","Iron City","Junction City",'Lumber City','Mountain City','Twin City','Ray City','Lake City')],' City')
  
lg_all = lg_all %>% filter(!govname %in% c('Coleman City','Weston City')) %>% filter(!duplicated(.)) %>% filter(!(govname=='Echols County'&is.na(pop)))



lg_fiscal_df =   lg_all %>% rename(TotOE = TE_currentoper) %>% dplyr::select(govid,govname,fiscyear,TotOE,
                                        RBDebt_outstand,GODebt_outstand,
                                        LP_outstand,OLTDebt_outstand,RBDebt_issued,GODebt_issued,OLTDebt_issued,LP_issued) %>%
  mutate(Year = as.character(fiscyear)) %>%
  group_by(govid,govname,Year) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  rename(NAMELSAD = govname) %>% dplyr::select(-fiscyear)


#### impute missing county fiscals for now 

check_missings = expand.grid(counties@data$NAMELSAD,2005:2014) %>% rename(NAMELSAD = Var1,Year = Var2) %>% mutate(Check = paste(NAMELSAD,Year,sep='_'))
check_missings$Missing = !check_missings$Check %in% paste(lg_fiscal_df$NAMELSAD,lg_fiscal_df$Year,sep='_')

temp = lapply(1:nrow(check_missings),function(i) if(check_missings$Missing[i])
{
  lg_fiscal_df %>% filter(NAMELSAD == check_missings$NAMELSAD[i] & Year == check_missings$Year[i]+1) %>% 
    mutate( RBDebt_outstand = RBDebt_outstand - RBDebt_issued,
            OLTDebt_outstand = OLTDebt_outstand - OLTDebt_issued,
            GODebt_outstand = GODebt_outstand - GODebt_issued,
            LP_outstand = LP_outstand - LP_issued)})
temp = do.call(rbind,temp)

temp$RBDebt_outstand = ifelse(temp$RBDebt_outstand<0,0,temp$RBDebt_outstand)
temp$OLTDebt_outstand = ifelse(temp$OLTDebt_outstand<0,0,temp$OLTDebt_outstand)
temp$GODebt_outstand = ifelse(temp$GODebt_outstand<0,0,temp$GODebt_outstand)
temp$LP_outstand = ifelse(temp$LP_outstand<0,0,temp$LP_outstand)
temp$LP_issued = NA
temp$RBDebt_issued = NA
temp$GODebt_issued = NA
temp$OLTDebt_issued = NA
temp$Year = as.character(as.numeric(temp$Year) - 1)

lg_fiscal_df = rbind(lg_fiscal_df,temp)


lg_fiscal_df = left_join(lg_fiscal_df,ref_place_to_cfips)
lg_fiscal_df$COUNTY_FIPS[is.na(lg_fiscal_df$COUNTY_FIPS)&lg_fiscal_df$NAMELSAD == 'Macon City'] = "13021"
lg_fiscal_df$COUNTY_FIPS[is.na(lg_fiscal_df$COUNTY_FIPS)&lg_fiscal_df$NAMELSAD == 'Preston City'] = "13307"
lg_fiscal_df$general_purpose = 1

lg_fiscal_df = lg_fiscal_df %>% group_by(NAMELSAD,Year)

sd_df$general_purpose = 0
sd_df = sd_df %>% dplyr::select(-NAMEGOV,-CONTACT_TITLE,-CONTACT_PHONE,-MailingAddress,-MailingCity,-MailingState,-BOARD,-Clean_Board_Characters,-CONTACT_PERSON,-OFFICIAL_CITATION)

sd_df = sd_df %>% mutate(authority_name = str_to_title(authority_name))
sd_df$AuthType[grepl('Housing|Residential',sd_df$authority_name)] = 'Housing'
sd_df$AuthType[grepl('Hospital',sd_df$authority_name)] = 'Hospital'
sd_df$AuthType[grepl('Water|Sewer',sd_df$authority_name)] = 'Water and Sewer'
sd_df$AuthType[grepl('Development And Renewal|Economic|Payroll',sd_df$authority_name)] = 'Development'
sd_df$AuthType[grepl('Urban Development Authority|Downtown|Inner City Authority|Pine Lake Downtown Development Authority',sd_df$authority_name)] = 'Downtown Development'
sd_df$AuthType[grepl('Industr',sd_df$authority_name)] = 'Industrial Development'
sd_df$AuthType[grepl('Transit|Transportation',sd_df$authority_name)] = 'Public Transit'
sd_df$AuthType[grepl('Redevelopment',sd_df$authority_name)] = 'Urban Redevelopment'
sd_df$AuthType[grepl('Resource Recovery',sd_df$authority_name)] = 'Resource Recovery'
sd_df$AuthType[grepl('Regional Development|Joint Development|Join Development|Canal|Unified Development',sd_df$authority_name)] = 'Joint Development'
sd_df$AuthType[grepl('Airport',sd_df$authority_name)] = 'Airport'
sd_df$AuthType[grepl('Building',sd_df$authority_name)] = 'Building'
sd_df$AuthType[grepl('Waste',sd_df$authority_name)] = 'Solid Waste'
sd_df$AuthType[grepl('Parking',sd_df$authority_name)] = 'Parking'
sd_df$AuthType[grepl('Public Facilit',sd_df$authority_name)] = 'Public Facilities'
sd_df$AuthType[grepl('Recreation|Park ',sd_df$authority_name)] = 'Recreation'
sd_df$AuthType[grepl('Land Bank|Landbank',sd_df$authority_name)] = 'Land Bank'
sd_df$AuthType[grepl('911|Liberty County Fire Authority',sd_df$authority_name)] = 'E-911'
sd_df$AuthType[grepl('Touri|Visitor|Convention|Center',sd_df$authority_name)] = 'Tourism'
sd_df$AuthType[grepl('Utilities|Utility District|Stormwater|Gas',sd_df$authority_name)] = 'Water and Sewer'
sd_df$AuthType[grepl('Public Service|Governmental Service',sd_df$authority_name)] = 'Public Service'
sd_df$AuthType[grepl('Elder',sd_df$authority_name)] = 'Residential Care of the Elderly'
sd_df$AuthType[grepl('Stadium|Coliseum',sd_df$authority_name)] = 'Stadium and Coliseum'
sd_df$AuthType[grepl('Jail',sd_df$authority_name)] = 'Regional Jail'
sd_df$AuthType[grepl('Families|Youth|Improvements|Judicial|Lake|Agribusiness|Theater|Technology|Cemetery|Arts|Homeless|Impoundment|Radio|Family|Livestock',sd_df$authority_name)] = 'Other'
sd_df$AuthType[grepl("Lake Oconee Area Development Authority|West Point Lake Development Authority",sd_df$authority_name)] = "Development"
sd_df$AuthType[grepl('^Development Authority|County Development Authority',sd_df$authority_name)] = 'Development'
sd_df$AuthType[grepl('Development Authority',sd_df$authority_name)&is.na(sd_df$AuthType)] = 'Development'

lg_fiscal_df$authority_name = NA
lg_fiscal_df$authority_name = as.character(lg_fiscal_df$authority_name)

all_fiscal_df = full_join(lg_fiscal_df,sd_df)



write.csv(all_fiscal_df,'Input/ready_to_model/debt_by_gov_year.csv')
writeOGR(counties, dsn = 'spatial_data/custom', layer = 'georgia_counties', driver = "ESRI Shapefile")

library(lubridate)
library(grid);library(lattice);library(gridExtra)

library(ggplot2)
library(ggthemes)

create_date = sd_df %>% filter(!duplicated(authority_name)) %>% dplyr::select(DATE_CREATED) %>% 
  mutate(Created = ymd(DATE_CREATED),Year = year(Created)) 

d1 = data.frame(x = year(create_date$Created),y = NA)
d2 = data.frame(x = (min(year(create_date$Created))-1):max(year(create_date$Created)),
                y = sapply((min(year(create_date$Created))-1):max(year(create_date$Created)), function(x) sum(x > year(create_date$Created))))
year.vec = seq(1940,2015,25)

d3 = d2[d2$x %in% year.vec,]

p1 <- ggplot(data = d1,aes(x=x)) + geom_dotplot(binwidth=1,dotsize=.5) + 
  scale_y_continuous(name = 'Established',limits=c(0,1))+ theme_tufte(ticks=F) +
  scale_x_continuous(name='')+ 
  theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.title=element_text(size=20),plot.margin=unit(c(.5,1,-0.5,1), "cm"))
p2 <- ggplot(data = d2,aes(x=x,y=y)) + geom_line(stat='identity')+ theme_tufte(ticks=F) +
  scale_y_continuous(name = 'Active Total') + #,breaks=c(250,500,750,1000)) +
  scale_x_continuous(name='',breaks =  year.vec)+ 
  theme(#axis.text.y = element_text(size=18),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=18),axis.title=element_text(size=20),plot.margin=unit(c(-0.5,1,.5,1), "cm")) +
  geom_point(aes(x=x,y=y),data=d3) + geom_label(aes(x=x,y=y,label = y),data=d3) 
grid.arrange(p1, p2, ncol=1)


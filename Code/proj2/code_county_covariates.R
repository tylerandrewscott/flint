
library(tidyverse)
library(rgdal)
library(stringr)
set_year = 2004:2015
ga_counties = readOGR('spatial_data/custom','georgia_counties')
ga_counties@data = ga_counties@data %>% rename(COUNTY_FIPS = COUNTY_,NAMELSAD = NAMELSA)

cgrid_ref = expand.grid(ga_counties$COUNTY_FIPS,as.character(set_year)) %>% rename(COUNTY_FIPS = Var1,Year = Var2)
county_temp = ga_counties@data  %>%
 dplyr::select(-UNITFP,-UNITNS,-LSAD,-CLASSFP,-VINTAGE,-FORM_ID,-EFF_DAT,-FUNCSTA,
                    -STATEFP,-RELATE,-CHNG_TY,-AUTHTYP,-DOCU,-AREA,-JUSTIFY,-NAME)

county_temp = merge(county_temp,data.frame(Year = set_year))

###load and add tax base (assessed value)
ct_taxb = readr::read_csv('Input/ready_to_model/cleaned_county_taxbase.csv') %>% filter(Year >= 2004)
ct_taxb$COUNTY_FIPS = ga_counties$COUNTY_FIPS[match(ct_taxb$NAMELSAD,ga_counties$NAMELSAD)]

county_temp = left_join(county_temp,ct_taxb)

pop_2000_2009 = read_csv('Input/census/CO-EST00INT-01-13.csv',skip=3) %>% filter(grepl('County',X1)&!grepl('Georgia',X1)) %>% dplyr::select(-X2,-X13,-X14) %>% rename(NAMELSAD = X1) %>%
  mutate(NAMELSAD = gsub('^\\.','',NAMELSAD)) %>% gather(Year,Population,-NAMELSAD)

pop_2010_2015 = read_csv('Input/census/PEP_2015_PEPANNRES.csv',skip = 1) %>% rename(NAMELSAD = Geography,COUNTY_FIPS  = Id2) %>% mutate(NAMELSAD = gsub('\\, Georgia$','',NAMELSAD)) %>%
  dplyr::select(-contains('April')) %>% gather(Year,Population,-Id,-COUNTY_FIPS,-NAMELSAD) %>% mutate(Year = str_extract(Year,"[0-9]{4}"))

pop_2000_2015 = full_join(pop_2010_2015 %>% dplyr::select(-COUNTY_FIPS),pop_2000_2009) %>% mutate(COUNTY_FIPS = as.character(pop_2010_2015$COUNTY_FIPS[match(NAMELSAD,pop_2010_2015$NAMELSAD)])) %>%
  arrange(NAMELSAD,Year) %>% mutate(last_pop = lag(Population,1),pop_growth_percent = 100 * ((Population-last_pop)/last_pop)) %>% dplyr::select(-Id,-NAMELSAD) 

county_temp$Year = as.character(county_temp$Year)

county_temp = left_join(county_temp,pop_2000_2015) %>% mutate(Pop10k = Population/10000) 

census_ed = lapply(paste0('Input/census/ACS_education/',grep('S1501_with',list.files('Input/census/ACS_education/'),value=T)),function(x) read_csv(x) %>% .[-1,] %>% mutate(Year = x))
bach_by_county = do.call(rbind,lapply(census_ed, function(x) x %>% mutate(NAMELSAD = gsub('\\, Georgia$',"",`GEO.display-label`)) %>% rename(COUNTY_FIPS = GEO.id2,PERC_BACH_OVER25 = HC01_EST_VC05) %>% 
                                        dplyr::select(COUNTY_FIPS,NAMELSAD,PERC_BACH_OVER25,Year) %>% mutate(Year = gsub('ACS_','20',str_extract(Year,'ACS_[0-9]{2}')),PERC_BACH_OVER25 = as.numeric(PERC_BACH_OVER25))))

year_bach_grid = expand.grid(ga_counties$COUNTY_FIPS,set_year) %>% rename(COUNTY_FIPS = Var1,Year = Var2) %>% mutate(Year = as.character(Year))
year_bach_grid = left_join(year_bach_grid,bach_by_county %>% arrange(NAMELSAD,Year) %>% dplyr::select(-NAMELSAD)) %>% group_by(COUNTY_FIPS)
fips_bach_mean = year_bach_grid %>% group_by(COUNTY_FIPS) %>% summarise(FIPS_MEAN = mean(PERC_BACH_OVER25,na.rm=T))
year_bach_grid$PERC_BACH_OVER25[is.na(year_bach_grid$PERC_BACH_OVER25)] = fips_bach_mean$FIPS_MEAN[match(year_bach_grid$COUNTY_FIPS[is.na(year_bach_grid$PERC_BACH_OVER25)],fips_bach_mean$COUNTY_FIPS)]

county_temp = left_join(county_temp,year_bach_grid)


old_2000_2009 = read_csv('Input/census/CO-EST00INT-ALLDATA-13.csv')  %>%
  filter(AGEGRP>=14&AGEGRP<99&(!YEAR%in%c(1,12))) %>%
  mutate(COUNTY_FIPS = paste0(STATE,COUNTY)) %>%
  dplyr::select(COUNTY_FIPS,AGEGRP,YEAR,TOT_POP) %>% group_by(COUNTY_FIPS,YEAR) %>%
  summarise(Pop_Over65 = sum(TOT_POP)) %>% rename(Year = YEAR) %>%
  mutate(Year = Year + 1998) %>% filter(Year!=2011) %>% mutate(Year = as.character(Year))

old_2010_2015 = read_csv('Input/census/PEP_2015_PEPAGESEX_with_ann.csv') %>% .[-1,] %>% 
  dplyr::select(GEO.id2,contains('sex0_age65plus')) %>% dplyr::select(-contains('420')) %>%
  rename(COUNTY_FIPS = GEO.id2) %>% gather(Year,Pop_Over65,-COUNTY_FIPS) %>%
  mutate(Year = paste0('20',gsub('sex.*','',gsub('^est720','',Year))),Pop_Over65 = as.numeric(Pop_Over65))

old_2000_2015 = full_join(old_2000_2009,old_2010_2015)

county_temp = left_join(county_temp,old_2000_2015) %>%
  mutate(Perc_Over65 = 100 * (Pop_Over65/Population))



unemp_df = read_csv('Input/misc/GA_county_unemp_rates.csv') %>% gather(Year,UnempR,-series_ID) %>%
  mutate(COUNTY_FIPS = str_extract(gsub('LAUCN','',series_ID),'^[0-9]{5}')) %>%
  dplyr::select(-series_ID)

county_temp = left_join(county_temp,unemp_df)

inc_per_df = read_csv('Input/misc/CA1_1969_2014_GA.csv') %>% filter(GeoFIPS != 13000 & LineCode==3) %>% 
  dplyr::select(-Region,-Table,-IndustryClassification,-GeoName,-LineCode,-Description) %>%
  gather(Year,Income_Per_Capita,-GeoFIPS) %>% filter(Year>=2004) %>% rename(COUNTY_FIPS = GeoFIPS) %>%
  mutate(COUNTY_FIPS = as.character(COUNTY_FIPS))

county_temp = left_join(county_temp,inc_per_df)


chouse_05_09 = do.call(rbind,lapply(paste0('Input/census/ACS_Households/',
                                           intersect(grep('ACS_0',list.files('Input/census/ACS_Households/'),value=T),
                                                     grep('S1101_with_ann',list.files('Input/census/ACS_Households/'),value=T))),
                                    function(x)  read_csv(x) %>% .[-1,] %>% dplyr::select(GEO.id2, HC01_EST_VC02,HC01_EST_VC08)       %>%  
                                      rename(COUNTY_FIPS = GEO.id2,Total_Households = HC01_EST_VC02, Households_With_Children = HC01_EST_VC08) %>%
                                      mutate(prop_house_with_children = 100 * as.numeric(Households_With_Children)/as.numeric(Total_Households),Year = x)
))


chouse_10_12 = do.call(rbind,lapply(paste0('Input/census/ACS_Households/',
                                           intersect(grep('ACS_1[0-2]',list.files('Input/census/ACS_Households/'),value=T),
                                                     grep('S1101_with_ann',list.files('Input/census/ACS_Households/'),value=T))),
                                    function(x)  read_csv(x) %>% .[-1,] %>% dplyr::select(GEO.id2, HC01_EST_VC02,HC01_EST_VC11)       %>%  
                                      rename(COUNTY_FIPS = GEO.id2,Total_Households = HC01_EST_VC02, Households_With_Children = HC01_EST_VC11) %>%
                                      mutate(prop_house_with_children = 100 * as.numeric(Households_With_Children)/as.numeric(Total_Households),
                                             Year = x)))


chouse_13_15 = do.call(rbind,lapply(paste0('Input/census/ACS_Households/',
                                           intersect(grep('ACS_1[3-5]',list.files('Input/census/ACS_Households/'),value=T),
                                                     grep('S1101_with_ann',list.files('Input/census/ACS_Households/'),value=T))),
                                    function(x)  read_csv(x) %>% .[-1,] %>% dplyr::select(GEO.id2, HC01_EST_VC02,HC01_EST_VC10)       %>%  
                                      rename(COUNTY_FIPS = GEO.id2,Total_Households = HC01_EST_VC02, Households_With_Children = HC01_EST_VC10) %>%
                                      mutate(prop_house_with_children = 100 * as.numeric(Households_With_Children)/as.numeric(Total_Households),
                                             Year = x)))

chouse = full_join(full_join(chouse_05_09,chouse_10_12),chouse_13_15)
chouse$Year = paste0('20',gsub('_.*','',gsub('.*ACS_','',chouse$Year)))
chouse = full_join(chouse,cgrid_ref)

mean_impute_chouse = chouse %>% group_by(COUNTY_FIPS) %>% 
summarise(mean_prop = mean(prop_house_with_children,na.rm=T))

chouse$prop_house_with_children[is.na(chouse$prop_house_with_children)] = 
mean_impute_chouse$mean_prop[match(chouse$COUNTY_FIPS[is.na(chouse$prop_house_with_children)],mean_impute_chouse$COUNTY_FIPS)]

county_temp = left_join(county_temp,chouse)


metro_2003 = read_csv('Input/misc/ruralurbancodes2003.csv') %>% filter(State=='GA') %>%
mutate(nonmetro = `2003 Rural-urban Continuum Code` %in% c(4:7),
       metro = `2003 Rural-urban Continuum Code` %in% c(1:3),
       rural = `2003 Rural-urban Continuum Code` %in% c(8:9)) %>% 
  rename(COUNTY_FIPS = `FIPS Code`) %>% dplyr::select(COUNTY_FIPS,nonmetro,metro,rural) %>%
  merge(.,data.frame(Year = as.character(2003:2012)))

metro_2013 =  read_csv('Input/misc/ruralurbancodes2013.csv') %>% filter(State=='GA') %>%
  mutate(nonmetro = RUCC_2013 %in% c(4:7),
         metro = RUCC_2013 %in% c(1:3),
         rural = RUCC_2013 %in% c(8:9)) %>% 
  rename(COUNTY_FIPS = FIPS) %>% dplyr::select(COUNTY_FIPS,nonmetro,metro,rural) %>%
  merge(.,data.frame(Year = as.character(2013:2015)))

metro = full_join(metro_2003,metro_2013)



county_temp = left_join(county_temp,metro) %>% filter(Year!=2004) %>%
  mutate(urbanized = ifelse(metro,'Metro',ifelse(rural,'Rural','Suburban'))) %>% 
  dplyr::select(-Total_Households,-Households_With_Children,-X1,-rural,-metro,-nonmetro)

mean_center = function(x){return(x - mean(x,na.rm=T))}

county_temp = county_temp %>% mutate(Pop10k_mc = mean_center(Population/10000),
                                     pop_growth_percent_mc = mean_center(pop_growth_percent),
                                     Perc_Over65_mc = mean_center(Perc_Over65),
                                     prop_house_with_children_mc = mean_center(prop_house_with_children),
                                     Income_Per_Capita_1k_mc = mean_center(Income_Per_Capita/1000),
                                     PERC_BACH_OVER25_mc = mean_center(PERC_BACH_OVER25),
                                     UnempR_mc = mean_center(UnempR))


write_csv(county_temp,'Input/ready_to_model/county_covariates.csv')



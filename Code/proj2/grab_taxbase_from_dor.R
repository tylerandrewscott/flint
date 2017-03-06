
library(readxl)
loc = 'Input/fiscal.data/digests/'
year_digests = lapply(paste0(loc,list.files(loc)),read_excel)
set_year = 2005:2015
library(data.table)
library(dplyr)
library(stringr)
first_set = lapply(year_digests[1:14], function(x) x %>% filter(`txpyr-dist-did`==1 & !grepl('FIRE',`txpyr-dist-name`)) %>%
                     dplyr::select(  `txpyr-name`, 
                                    `rtn-period-taxYr`,pCnt,  
                                  `tax-valAmt-bnd`)    %>%
  rename(NAMELSAD = `txpyr-name`, 
  Tax_Year =    `rtn-period-taxYr`,
  Total_Parcels = pCnt,  
  Total_Assessed_Value = `tax-valAmt-bnd`))

second_set = lapply(year_digests[15:16], function(x) x %>% dplyr::filter(`District Code` == 1) %>%
                      dplyr::select(`County Name`,`Tax Year`,  `Total Parcels`,
                                                      `Total Assessed Value-Bond`) %>%
                                                rename(NAMELSAD = `County Name`,
                                                       Tax_Year = `Tax Year`,
                                                      Total_Parcels = `Total Parcels`,
                                                      Total_Assessed_Value =  `Total Assessed Value-Bond` ))

set1 = do.call(rbind,first_set)
set2 = do.call(rbind,second_set)
tax_base_df = full_join(set1,set2) %>% mutate(NAMELSAD = str_to_title(NAMELSAD))
tax_base_df$NAMELSAD[!grepl('County',tax_base_df$NAMELSAD)] = paste0(tax_base_df$NAMELSAD[!grepl('County',tax_base_df$NAMELSAD)],' County')

tax_base_df = tax_base_df %>% rename(Year = Tax_Year) %>% filter(Year %in% set_year)

tax_base_df$NAMELSAD[tax_base_df$NAMELSAD=='Clarke County'] = 'Athens-Clarke CG'
tax_base_df$NAMELSAD[tax_base_df$NAMELSAD=='Bibb County'] = 'Macon-Bibb County CG'
tax_base_df$NAMELSAD[tax_base_df$NAMELSAD=='Richmond County'] = 'Augusta-Richmond CG'
tax_base_df$NAMELSAD[tax_base_df$NAMELSAD=="Chattahoochee County"] = 'Cusseta-Chattahoochee CG'
tax_base_df$NAMELSAD[tax_base_df$NAMELSAD=="Webster County"] = 'Preston-Webster CG'
tax_base_df$NAMELSAD[tax_base_df$NAMELSAD=="Quitman County"] = 'Georgetown-Quitman CG'
tax_base_df$NAMELSAD[tax_base_df$NAMELSAD=="Muscogee County"] = 'Columbus-Muscogee CG'


fill_all = expand.grid(unique(tax_base_df$NAMELSAD),set_year) %>% rename(NAMELSAD = Var1, Year = Var2)

tax_base_df = left_join(fill_all,tax_base_df)

###use average change from 2013-2014 to fill Wayne County
avg_2014_change = tax_base_df %>% filter(NAMELSAD != 'Wayne County') %>% group_by(NAMELSAD) %>% mutate(perc_increase = Total_Assessed_Value/lag(Total_Assessed_Value,order_by = Year)) %>%
  filter(Year == 2014)
tax_base_df$Total_Assessed_Value[tax_base_df$NAMELSAD=='Wayne County'&tax_base_df$Year==2014] = 
mean(avg_2014_change$perc_increase) * tax_base_df$Total_Assessed_Value[tax_base_df$NAMELSAD=='Wayne County'&tax_base_df$Year==2013]
##same for 2015
avg_2015_change = tax_base_df %>% filter(NAMELSAD != 'Wayne County') %>% group_by(NAMELSAD) %>% mutate(perc_increase = Total_Assessed_Value/lag(Total_Assessed_Value,order_by = Year)) %>%
  filter(Year == 2015)

tax_base_df$Total_Assessed_Value[tax_base_df$NAMELSAD=='Wayne County'&tax_base_df$Year==2015] = mean(avg_2015_change$perc_increase) * tax_base_df$Total_Assessed_Value[tax_base_df$NAMELSAD=='Wayne County'&tax_base_df$Year==2014]


write.csv(tax_base_df , 'Input/ready_to_model/cleaned_county_taxbase.csv')

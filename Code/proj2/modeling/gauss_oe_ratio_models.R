
library(tidyverse)
library(rgdal)
library(stringr)
library(forcats)
library(ggthemes)
set_year = 2005:2014

mean_center = function(x){return(x - mean(x,na.rm=T))}
ga_counties = readOGR('spatial_data/custom','georgia_counties')

debt_df  = read_csv('Input/ready_to_model/debt_by_gov_year.csv') %>% filter(Year %in% set_year)

library(knitr)
kable(as.data.frame(table(debt_df$AuthType[debt_df$general_purpose==0&!duplicated(debt_df$authority_name)])))


debt_df$TotOE[is.na(debt_df$TotOE)] = 0
debt_df = debt_df %>% mutate(JURIS_TYPE = ifelse(grepl('Multi',JURIS_TYPE),"Multi","Single"))

debt_df$HighCost = 'Low'
debt_df$HighCost[grepl('Airport|Jail|Water|Sewer|Hospitals',debt_df$AuthType)] = 'High'

ga_counties@data = ga_counties@data %>% rename(COUNTY_FIPS = COUNTY_,NAMELSAD = NAMELSA)

county_grid = expand.grid(ga_counties@data$COUNTY_FIPS,set_year) %>% rename(COUNTY_FIPS = Var1,Year = Var2)

county_df = read_csv('Input/ready_to_model/county_covariates.csv') %>% 
  mutate(COUNTY_FIPS = as.character(COUNTY_FIPS))

data_temp = left_join(county_grid,county_df)


data_temp = left_join(data_temp,    debt_df  %>% dplyr::select(Year,general_purpose,COUNTY_FIPS,
                                                               EndBalanceBonds,CapLeaseLiability,LP_outstand,GODebt_outstand,
                                                               RBDebt_outstand,OLTDebt_outstand,RevFromLG,TotOE) %>%
                        mutate(COUNTY_FIPS = as.character(COUNTY_FIPS),
                               TotOE = as.numeric(TotOE),
                               EndBalanceBonds = as.numeric(EndBalanceBonds),RevFromLG = as.numeric(RevFromLG),
                               CapLeaseLiability = as.numeric(CapLeaseLiability)) %>%
                        group_by(Year,COUNTY_FIPS) %>% summarise_each(funs(sum(.,na.rm=T))) %>%
                        mutate(LG_DEBT = RBDebt_outstand + GODebt_outstand + LP_outstand +  OLTDebt_outstand,
                               SD_DEBT = CapLeaseLiability + EndBalanceBonds,
                               TOTAL_DEBT = LG_DEBT + SD_DEBT,
                               Linear_Year = Year - 2005))


oe_type_sum = debt_df %>% filter(Year>2004) %>%   mutate(COUNTY_FIPS = as.character(COUNTY_FIPS)) %>% 
  group_by(COUNTY_FIPS,Year,general_purpose) %>% summarise(TotOE = sum(TotOE,na.rm=T)) %>% 
  spread(general_purpose,TotOE) %>% rename(TotOE_LG = `1`,TotOE_SD = `0`) %>% 
  mutate(TotOE_LG = ifelse(is.na(TotOE_LG),0,TotOE_LG),TotOE_SD =  ifelse(is.na(TotOE_SD),0,TotOE_SD)) %>%
  complete(COUNTY_FIPS, Year, fill = list(TotOE_SD = 0,TotOE_LG = 0))

data_temp = left_join(data_temp,oe_type_sum) %>% mutate(OE_Ratio_SD_All = 100*(TotOE_SD / TotOE))


data_temp = left_join(data_temp, debt_df %>% mutate(special_purpose = abs(general_purpose-1)) %>%
                        mutate(COUNTY_FIPS = as.character(COUNTY_FIPS)) %>%
                        group_by(COUNTY_FIPS,Year) %>% summarise(sd_count = sum(special_purpose),
                                                                 lg_count = sum(general_purpose)) %>%
                        complete(COUNTY_FIPS, Year, fill = list(sd_count = 0,lg_count = 0)))

data_temp = data_temp %>%
  mutate(Debt_Ratio_LGonly = (LG_DEBT/Total_Assessed_Value) * 100,
         Debt_Ratio_SDonly = (SD_DEBT/Total_Assessed_Value) * 100,
         Debt_Ratio_Total = (TOTAL_DEBT/Total_Assessed_Value) * 100,
         Debt_Ratio_Difference = Debt_Ratio_Total - Debt_Ratio_LGonly,
         Linear_Year_Sq = Linear_Year ^ 2,
         TotOE1m_mc = mean_center(TotOE/1000000),TotOE_LG1m_mc = mean_center(TotOE_LG/1000000),TotOE_SD1m_mc = mean_center(TotOE_SD/1000000))

sd_juris = debt_df %>% filter(general_purpose!=1) %>% mutate(COUNTY_FIPS = as.character(COUNTY_FIPS)) %>% group_by(Year,COUNTY_FIPS,JURIS_TYPE) %>% summarise(sd_count = n()) %>% 
  spread(JURIS_TYPE,sd_count) %>% complete(COUNTY_FIPS,Year,fill=list(Multi = 0,Single=0)) %>% rename(sd_count_multi = Multi,sd_count_single = Single)
data_temp = left_join(data_temp,sd_juris)

sd_dep = debt_df %>% filter(general_purpose!=1) %>% mutate(COUNTY_FIPS = as.character(COUNTY_FIPS)) %>% group_by(Year,COUNTY_FIPS,DEPENDENCY) %>% summarise(sd_count = n()) %>% 
  spread(DEPENDENCY,sd_count) %>% complete(COUNTY_FIPS,Year,fill=list(Dependent = 0,Independent=0)) %>% rename(sd_count_dep = Dependent,sd_count_ind = Independent)
data_temp = left_join(data_temp,sd_dep)

sd_cost = debt_df %>% filter(general_purpose!=1) %>% mutate(COUNTY_FIPS = as.character(COUNTY_FIPS)) %>% group_by(Year,COUNTY_FIPS,HighCost) %>% summarise(sd_count = n()) %>% 
  spread(HighCost,sd_count) %>% complete(COUNTY_FIPS,Year,fill=list(High = 0,Low=0)) %>% rename(sd_count_high = High,sd_count_low = Low)
data_temp = left_join(data_temp,sd_cost)

data_temp[,grepl('sd_count',colnames(data_temp))][is.na(data_temp[,grepl('sd_count',colnames(data_temp))])] = 0


#### compute OE ratios for diff sd types

oe_juris_sum = debt_df %>% filter(Year>2004&general_purpose==0) %>%   
  mutate(COUNTY_FIPS = as.character(COUNTY_FIPS)) %>% 
  group_by(COUNTY_FIPS,Year,JURIS_TYPE) %>% summarise(TotOE = sum(TotOE,na.rm=T)) %>% 
  spread(JURIS_TYPE,TotOE) %>% rename(TotOE_SD_Multi = Multi,TotOE_SD_Single = Single) %>% 
  mutate(TotOE_SD_Multi =  ifelse(is.na(TotOE_SD_Multi),0,TotOE_SD_Multi)) %>%
  mutate(TotOE_SD_Single =  ifelse(is.na(TotOE_SD_Single),0,TotOE_SD_Single)) %>%
  complete(COUNTY_FIPS, Year, fill = list(TotOE_SD_Multi = 0,TotOE_SD_Single = 0))


oe_dep_sum = debt_df %>% filter(Year>2004&general_purpose==0) %>%   
  mutate(COUNTY_FIPS = as.character(COUNTY_FIPS)) %>% 
  group_by(COUNTY_FIPS,Year,DEPENDENCY) %>% summarise(TotOE = sum(TotOE,na.rm=T)) %>% 
  spread(DEPENDENCY,TotOE) %>% rename(TotOE_SD_Ind = Independent,TotOE_SD_Dep = Dependent) %>% 
  mutate(TotOE_SD_Ind =  ifelse(is.na(TotOE_SD_Ind),0,TotOE_SD_Ind)) %>%
  mutate(TotOE_SD_Dep =  ifelse(is.na(TotOE_SD_Dep),0,TotOE_SD_Dep)) %>%
  complete(COUNTY_FIPS, Year, fill = list(TotOE_SD_Ind = 0,TotOE_SD_Dep = 0))

oe_asset_sum = debt_df %>% filter(Year>2004&general_purpose==0) %>%   
  mutate(COUNTY_FIPS = as.character(COUNTY_FIPS)) %>% 
  group_by(COUNTY_FIPS,Year,HighCost) %>% summarise(TotOE = sum(TotOE,na.rm=T)) %>% 
  spread(HighCost,TotOE) %>% rename(TotOE_SD_High = High,TotOE_SD_Low = Low) %>% 
  mutate(TotOE_SD_High =  ifelse(is.na(TotOE_SD_High),0,TotOE_SD_High)) %>%
  mutate(TotOE_SD_Low =  ifelse(is.na(TotOE_SD_Low),0,TotOE_SD_Low)) %>%
  complete(COUNTY_FIPS, Year, fill = list(TotOE_SD_High = 0,TotOE_SD_Low = 0))

data_temp = data_temp %>% left_join(.,oe_asset_sum) %>% left_join(.,oe_juris_sum) %>% left_join(.,oe_dep_sum)

data_temp = data_temp %>% mutate(TotOE_SD_Low_Over_TotOE = 100* (TotOE_SD_Low/TotOE),
                     TotOE_SD_High_Over_TotOE = 100* (TotOE_SD_High/TotOE),
                     TotOE_SD_Ind_Over_TotOE = 100* (TotOE_SD_Ind/TotOE),
                     TotOE_SD_Dep_Over_TotOE = 100* (TotOE_SD_Dep/TotOE),
                     TotOE_SD_Multi_Over_TotOE = 100* (TotOE_SD_Multi/TotOE),
                     TotOE_SD_Single_Over_TotOE =  100* (TotOE_SD_Single/TotOE))



##total households:HC01_EST_VC02
##total households w/ under18: HC01_EST_VC08
library(ggthemes)
library(viridis)
library(INLA)
#Load libraries
library(spdep)
ga_counties@data$ID = 1:nrow(ga_counties)
#Create adjacency matrix
ga_county_mat <- poly2nb(ga_counties,queen=F,row.names = as.character(ga_counties$COUNTY_FIPS))
#Convert the adjacency matrix into a file in the INLA format
nb2INLA("ga_county.adj", ga_county_mat)

#Create areas IDs to match the values in nc.adj
data_temp$ID = ga_counties@data$ID[match(data_temp$COUNTY_FIPS,ga_counties@data$COUNTY_FIPS)]

form_oe_allsd = Debt_Ratio_Total ~ Pop10k_mc + pop_growth_percent_mc + 
  PERC_BACH_OVER25_mc + Perc_Over65_mc + UnempR_mc +  Income_Per_Capita_1k_mc + 
  prop_house_with_children_mc+
  TotOE_SD1m_mc +
  urbanized +
  lg_count +
  OE_Ratio_SD_All +
  f(Linear_Year,model='ar1',group=ID)+ 
  f(ID, model="bym", graph="ga_county.adj")

form_oe_dep = Debt_Ratio_Total ~  Pop10k_mc + pop_growth_percent_mc + 
  PERC_BACH_OVER25_mc + Perc_Over65_mc + UnempR_mc +  Income_Per_Capita_1k_mc + 
  prop_house_with_children_mc+
  TotOE_SD1m_mc +
  urbanized +
  lg_count +
  TotOE_SD_Dep_Over_TotOE+TotOE_SD_Ind_Over_TotOE +
  f(Linear_Year,model='ar1',group=ID)+ 
  f(ID, model="bym", graph="ga_county.adj")

form_oe_juris = Debt_Ratio_Total ~  Pop10k_mc + pop_growth_percent_mc + 
  PERC_BACH_OVER25_mc + Perc_Over65_mc + UnempR_mc +  Income_Per_Capita_1k_mc + 
  prop_house_with_children_mc+
  TotOE_SD1m_mc +
  urbanized +
  lg_count +
  TotOE_SD_Multi_Over_TotOE+TotOE_SD_Single_Over_TotOE +
  f(Linear_Year,model='ar1',group=ID)+ 
  f(ID, model="bym", graph="ga_county.adj")

form_oe_asset = Debt_Ratio_Total ~   Pop10k_mc + pop_growth_percent_mc + 
  PERC_BACH_OVER25_mc + Perc_Over65_mc + UnempR_mc +  Income_Per_Capita_1k_mc + 
  prop_house_with_children_mc+
  TotOE_SD1m_mc +
  urbanized +
  lg_count +
  TotOE_SD_High_Over_TotOE+TotOE_SD_Low_Over_TotOE +
  f(Linear_Year,model='ar1',group=ID)+ 
  f(ID, model="bym", graph="ga_county.adj")

form_list_oe = lapply(grep('form_oe',ls(),value=T),get)

mod_list_gauss_oe = lapply(form_list_oe,function(x) inla(x,family = 'gaussian', data = data_temp,
             control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),verbose=T))


save.image('Scratch/temp_results_oe_ratio_2_noi.RData')
# 
# 
# 
# 
# 
# model_fixed_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.fixed) %>% 
#                                      mutate(Model = x,Coef = mod_list_gauss[[x]]$names.fixed,Effect='Fixed'))) %>% filter(!grepl('Intercept',Coef))  %>% dplyr::select(-kld)
# 
# model_random_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.hyperpar) %>% 
#                                            mutate(Model = x,Coef = rownames(mod_list_gauss[[x]]$summary.hyperpar),Effect='Random'))) %>% 
#   filter(!grepl('Intercept',Coef))
# 
# model_coefs = rbind(model_fixed_coefs,model_random_coefs)
# 
# m1_coefs = model_coefs %>% filter(Model==1)
# m1_coefs$Coef = fct_inorder(m1_coefs$Coef)
# 
# 
# ggplot(data = model_coefs %>% filter(Model == 1)) + 
#   geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round') + 
#   theme_tufte(ticks=F) 
# 
# 
#   geom_point(aes(x=mean,y=Coef),size=5)+
#   scale_y_discrete(limits = rev(levels(m1_coefs$Coef)),drop=T,
#                    labels=c('Suburban','Rural','# special districts','SD Operating Expenses ($1M)',
#                             '% households w/ children','Income per capita ($1k)','Unemployment %','% pop. over age 65',
#                             "% pop. with Bachelor's","Yearly population growth %",'Population (10k)'))+
#   #      labels=c('Pop')) + 
#   scale_x_continuous(name = '95% credible interval') +
#   geom_vline(xintercept=0,lty=2) + theme(axis.title.y = element_blank(),
#                                          axis.text = element_text(size=16),
#                                          axis.title.x=element_text(size=18))


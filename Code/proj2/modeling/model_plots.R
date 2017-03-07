rm(list=ls())
library(tidyverse)
library(ggthemes)
library(INLA)
rm(list=ls())
load('Scratch/temp_results_total_ratio_ar1_noi.RData')
library(broom)
library(forcats)

model_fixed_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.fixed) %>%
                                     mutate(Model = x,Coef = mod_list_gauss[[x]]$names.fixed,Effect='Fixed'))) %>% filter(!grepl('Intercept',Coef))  %>% dplyr::select(-kld)

model_random_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.hyperpar) %>%
                                           mutate(Model = x,Coef = rownames(mod_list_gauss[[x]]$summary.hyperpar),Effect='Random'))) %>%
  filter(!grepl('Intercept',Coef))


model_fixed_coefs = model_fixed_coefs %>% mutate(Model = ifelse(Model==1,'Total',
               ifelse(Model==2,'Asset specificity',
                      ifelse(Model==3,'Status','Jurisdiction'
                      ))),
               SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))



model_fixed_coefs$Coef = fct_recode(model_fixed_coefs$Coef,
`Population (10k)` = 'Pop10k_mc',
`Yearly population growth %` = 'pop_growth_percent_mc',
`% pop. with degree` = "PERC_BACH_OVER25_mc",
`% pop. over age 65` = "Perc_Over65_mc",
`Unemployment %` = "UnempR_mc",
`Income per capita ($1k)` = "Income_Per_Capita_1k_mc",
`% households w/ children` = 'prop_house_with_children_mc',
`SD Operating Expenses ($1M)` = "TotOE_SD1m_mc",
`Rural` = "urbanizedRural",
`Suburban` = "urbanizedSuburban",
`# local governments` = "lg_count",
`# special districts` = 'sd_count',
`# single-juris. SDs` = 'sd_count_single',
`# multi-juris. SDs` = 'sd_count_multi',
`# high AS SDs` = 'sd_count_high',
`# low AS SDs` = 'sd_count_low',
`# dep. SDs` = 'sd_count_dep',
`# ind. SDs` = 'sd_count_ind')



model_fixed_coefs$Coef = fct_relevel(model_fixed_coefs$Coef,c(
 'Population (10k)'  ,
  'Yearly population growth %'  ,
     '% pop. over age 65'  ,
 '% households w/ children'  ,
      'Unemployment %'  ,
    'Income per capita ($1k)'  ,
 '% pop. with degree'  ,
 'Rural'  ,
 'Suburban'  ,
  'SD Operating Expenses ($1M)'  ,
   '# local governments'  ,
    '# special districts'  ,
      '# single-juris. SDs'  ,
    '# multi-juris. SDs'  ,
      '# high AS SDs'  ,
'# low AS SDs'  ,
   '# dep. SDs'  ,
     '# ind. SDs'  ))

model_fixed_coefs$Model = fct_relevel(model_fixed_coefs$Model,
                                      c('Total'))


#model_coefs = rbind(model_fixed_coefs,model_random_coefs)


ggplot(data = model_fixed_coefs) +
  geom_vline(xintercept=0,lty=2) +
  geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd = 2,lineend = 'round') +
  theme_tufte(ticks=F) + 
  geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=2,shape=21)+
  scale_x_continuous(name = '95% credible interval',
                     breaks=seq(-0.5,0.5,.5),labels=seq(-0.5,.5,0.5)) +
  scale_fill_manual(values=c('white','black'),guide=FALSE) +
  facet_wrap(~Model,nrow=1) +
  scale_y_discrete(limits = rev(levels(model_fixed_coefs$Coef))) +
  theme(axis.title.y=element_blank(),axis.text=element_text(size=14),
        strip.text=element_text(size=14))

table_coefs = model_fixed_coefs %>% select(-mode,-sd,-Effect,-SIG,-`0.5quant`)  %>%
  mutate(Mean = signif(mean,2),`0.025quant` = signif(`0.025quant`,2),`0.975quant` = signif(`0.975quant`,2)) %>%
  mutate(Print = paste0(Mean,' (',`0.025quant`,', ',`0.975quant`,')')) %>%
  select(Coef,Print,Model)

table_coefs_ranef = model_random_coefs %>% select(-mode,-sd,-Effect,-`0.5quant`)  %>%
  mutate(Mean = signif(mean,2),`0.025quant` = signif(`0.025quant`,2),`0.975quant` = signif(`0.975quant`,2)) %>%
  mutate(Print = paste0(Mean,' (',`0.025quant`,', ',`0.975quant`,')')) %>%
  select(Coef,Print,Model)

table_coefs$Coef = fct_relevel(table_coefs$Coef,c(
  'Population (10k)'  ,
  'Yearly population growth %'  ,
  '% pop. over age 65'  ,
  '% households w/ children'  ,
  'Unemployment %'  ,
  'Income per capita ($1k)'  ,
  '% pop. with degree'  ,
  'Rural'  ,
  'Suburban'  ,
  'SD Operating Expenses ($1M)'  ,
  '# local governments'  ,
  '# special districts'  ,
  '# single-juris. SDs'  ,
  '# multi-juris. SDs'  ,
  '# high AS SDs'  ,
  '# low AS SDs'  ,
  '# dep. SDs'  ,
  '# ind. SDs'   ))
library(knitr)
kable(table_coefs %>% spread(Model,Print),format = 'html',
      align = c('l','c','c','c','c'))

kable(table_coefs_ranef %>% spread(Model,Print),format = 'html',
      align = c('l','c','c','c','c'))


mod_names = c('Total','Asset specificity',
      'Status','Jurisdiction')

cpo_vals = lapply(1:length(mod_list_gauss),function(x) 
  data.frame(CPO = mod_list_gauss[[x]]$cpo$cpo,Model = mod_names[x],i = 1:length(mod_list_gauss[[x]]$cpo$cpo))) %>%
  do.call(rbind,.)

library(viridis)
ggplot(cpo_vals,aes(x=i,y=CPO,colour=Model)) + geom_point(pch=1) + theme_tufte(ticks=F) + 
  scale_color_viridis(discrete=T,option = 'D') + scale_x_continuous(name='Observation') + 
  theme(axis.text.x=element_blank(),legend.position = 'bottom',
        axis.text.y=element_text(size=16),axis.title=element_text(size=18),
        legend.title=element_text(size=18),legend.text=element_text(size=16))


rm(list=ls())
load('Scratch/temp_results_oe_ratio_2_noi.RData')


model_fixed_coefs = do.call(rbind,lapply(1:length(mod_list_gauss_oe),function(x) as.data.frame(mod_list_gauss[[x]]$summary.fixed) %>%
                                           mutate(Model = x,Coef = mod_list_gauss_oe[[x]]$names.fixed,Effect='Fixed'))) %>% filter(!grepl('Intercept',Coef))  %>% dplyr::select(-kld)

model_random_coefs = do.call(rbind,lapply(1:length(mod_list_gauss_oe),function(x) as.data.frame(mod_list_gauss_oe[[x]]$summary.hyperpar) %>%
                                            mutate(Model = x,Coef = rownames(mod_list_gauss_oe[[x]]$summary.hyperpar),Effect='Random'))) %>%
  filter(!grepl('Intercept',Coef))


model_fixed_coefs = model_fixed_coefs %>% mutate(Model = ifelse(Model==1,'Total',
                                                                ifelse(Model==2,'Asset specificity',
                                                                       ifelse(Model==3,'Status','Jurisdiction'
                                                                       ))),
                                                 SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))



model_fixed_coefs$Coef = fct_recode(model_fixed_coefs$Coef,
                                    `Population (10k)` = 'Pop10k_mc',
                                    `Yearly population growth %` = 'pop_growth_percent_mc',
                                    `% pop. with degree` = "PERC_BACH_OVER25_mc",
                                    `% pop. over age 65` = "Perc_Over65_mc",
                                    `Unemployment %` = "UnempR_mc",
                                    `Income per capita ($1k)` = "Income_Per_Capita_1k_mc",
                                    `% households w/ children` = 'prop_house_with_children_mc',
                                    `SD Operating Expenses ($1M)` = "TotOE_SD1m_mc",
                                    `Rural` = "urbanizedRural",
                                    `Suburban` = "urbanizedSuburban",
                                    `# local governments` = "lg_count",
                                    `OE special districts/Total OE` = 'OE_Ratio_SD_All',
                                    `OE single-juris. SDs/Total OE` = 'TotOE_SD_Single_Over_TotOE',
                                    `OE multi-juris. SDs/Total OE` = 'TotOE_SD_Multi_Over_TotOE',
                                    `OE high AS SDs/Total OE` = 'TotOE_SD_High_Over_TotOE',
                                    `OE low AS SDs/Total OE` = 'TotOE_SD_Low_Over_TotOE',
                                    `OE dep. SDs/Total OE` = 'TotOE_SD_Dep_Over_TotOE',
                                    `OE ind. SDs/Total OE` = 'TotOE_SD_Ind_Over_TotOE')



model_fixed_coefs$Coef = fct_relevel(model_fixed_coefs$Coef,c(
  'Population (10k)'  ,
  'Yearly population growth %'  ,
  '% pop. over age 65'  ,
  '% households w/ children'  ,
  'Unemployment %'  ,
  'Income per capita ($1k)'  ,
  '% pop. with degree'  ,
  'Rural'  ,
  'Suburban'  ,
  'SD Operating Expenses ($1M)'  ,
  '# local governments'  ,
  'OE special districts/Total OE',
  'OE single-juris. SDs/Total OE',
  'OE multi-juris. SDs/Total OE',
  'OE high AS SDs/Total OE',
  'OE low AS SDs/Total OE' ,
  'OE dep. SDs/Total OE',
  'OE ind. SDs/Total OE'  ))

model_fixed_coefs$Model = fct_relevel(model_fixed_coefs$Model,
                                      c('Total'))


#model_coefs = rbind(model_fixed_coefs,model_random_coefs)


ggplot(data = model_fixed_coefs) +
  geom_vline(xintercept=0,lty=2) +
  geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd = 2,lineend = 'round') +
  theme_tufte(ticks=F) + 
  geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=2,shape=21)+
  scale_x_continuous(name = '95% credible interval',
                     breaks=seq(-0.5,0.5,.5),labels=seq(-0.5,.5,0.5)) +
  scale_fill_manual(values=c('white','black'),guide=FALSE) +
  facet_wrap(~Model,nrow=1) +
  scale_y_discrete(limits = rev(levels(model_fixed_coefs$Coef))) +
  theme(axis.title.y=element_blank(),axis.text=element_text(size=14),
        strip.text=element_text(size=14))



table_coefs_oe = model_fixed_coefs %>% select(-mode,-sd,-Effect,-SIG,-`0.5quant`)  %>%
  mutate(Mean = signif(mean,2),`0.025quant` = signif(`0.025quant`,2),`0.975quant` = signif(`0.975quant`,2)) %>%
  mutate(Print = paste0(Mean,' (',`0.025quant`,', ',`0.975quant`,')')) %>%
  select(Coef,Print,Model)


table_coefs_oe$Coef = fct_relevel(table_coefs_oe$Coef,c(
  'Population (10k)'  ,
  'Yearly population growth %'  ,
  '% pop. over age 65'  ,
  '% households w/ children'  ,
  'Unemployment %'  ,
  'Income per capita ($1k)'  ,
  '% pop. with degree'  ,
  'Rural'  ,
  'Suburban'  ,
  'SD Operating Expenses ($1M)'  ,
  '# local governments'  ,
  'OE special districts/Total OE',
  'OE single-juris. SDs/Total OE',
  'OE multi-juris. SDs/Total OE',
  'OE high AS SDs/Total OE',
  'OE low AS SDs/Total OE' ,
  'OE dep. SDs/Total OE',
  'OE ind. SDs/Total OE'  ))
library(knitr)
kable(table_coefs_oe %>% spread(Model,Print),format = 'html',
      align = c('l','c','c','c'))

table_coefs_ranef = model_random_coefs %>% select(-mode,-sd,-Effect,-`0.5quant`)  %>%
  mutate(Mean = signif(mean,2),`0.025quant` = signif(`0.025quant`,2),`0.975quant` = signif(`0.975quant`,2)) %>%
  mutate(Print = paste0(Mean,' (',`0.025quant`,', ',`0.975quant`,')')) %>%
  select(Coef,Print,Model)

kable(table_coefs_ranef %>% spread(Model,Print),format = 'html',
      align = c('l','c','c','c','c'))




###############

mod_all =  model_fixed_coefs %>% filter(Model == 1)
mod_all$Coef = fct_inorder(mod_all$Coef)


 ggplot(data = mod_all) +
  geom_vline(xintercept=0,lty=2) +
     geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=1:nrow(mod_all),yend=1:nrow(mod_all)),lwd=3,lineend = 'round') +
     theme_tufte(ticks=F) + geom_point(aes(x=mean,y=1:nrow(mod_all),fill=as.factor(SIG)),size=5,shape=21)+
     scale_x_continuous(name = '95% credible interval',breaks=seq(-1,1,0.25),labels=seq(-1,1,0.25)) +
     scale_fill_manual(values=c('white','black'),guide=FALSE)+
     theme(axis.title.y = element_blank(),
                                              axis.text = element_text(size=16),
                                                      axis.title.x=element_text(size=18)) +
     scale_y_reverse(breaks=c(1:nrow(mod_all)),labels = c('Population (10k)',"Yearly population growth %", "% pop. with Bachelor's",'% pop. over age 65',
                                                                      'Unemployment %','Income per capita ($1k)',  '% households w/ children','SD Operating Expenses ($1M)','Rural',
                                                                            'Suburban','# local governments','# special districts'))+
     annotate('text',x = 0.6,y=7.5,label=
                          paste(paste0('DIC: ',round(mod_list_gauss[[1]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[1]]$waic$waic,2))),size=7)


 ggplot(data = model_fixed_coefs) +
   geom_vline(xintercept=0,lty=2) +
   geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef,lwd=3),lineend = 'round') +
   theme_tufte(ticks=F) + geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+
   scale_x_continuous(name = '95% credible interval',breaks=seq(-1,1,0.25),labels=seq(-1,1,0.25)) +
   scale_fill_manual(values=c('white','black'),guide=FALSE) +
   facet_wrap(~Model,nrow=1)
 
 
   theme(axis.title.y = element_blank(),
         axis.text = element_text(size=16),
         axis.title.x=element_text(size=18)) +
   scale_y_reverse(breaks=c(1:nrow(mod_all)),labels = c('Population (10k)',"Yearly population growth %", "% pop. with Bachelor's",'% pop. over age 65',
                                                        'Unemployment %','Income per capita ($1k)',  '% households w/ children','SD Operating Expenses ($1M)','Rural',
                                                        'Suburban','# local governments','# special districts'))+
   annotate('text',x = 0.6,y=7.5,label=
              paste(paste0('DIC: ',round(mod_list_gauss[[1]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[1]]$waic$waic,2))),size=7)
 
 
 
 
 
 sd_coefs_only = model_fixed_coefs %>% filter(grepl('sd_count',model_fixed_coefs$Coef))
 sd_coefs_only$Coef = fct_inorder(sd_coefs_only$Coef)
 
sd_coefs_only = sd_coefs_only %>% mutate(Coef = gsub('sd_count_','',Coef),
                                          Model = ifelse(Model==1,'Total',
                                                  ifelse(Model==2,'Asset specificity',
                                                  ifelse(Model==3,'Status','Jurisdiction'
                                                  ))))
 
 sd_coefs_only$Coef = fct_recode(as.factor(sd_coefs_only$Coef),`Total` = 'sd_count',
           `# High` = 'high',`# Low` = 'low',`# Low * # High` = 'high:low',
           `# Dependent` = 'ind',`# Independent`='ind',`# Ind. * # Dep.` = 'dep:ind',
           `# Multi-jurisdiction` = 'multi',`# Single jurisdiction` = 'single',
           `# Single * # Multi` = 'multi:single')
 sd_coefs_only$Coef
 
 
 ggplot(data = sd_coefs_only %>% filter(Model!='Total')) +
   geom_vline(xintercept=0,lty=2) +
   geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round') +
   #geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round') 
   theme_tufte(ticks=F) + 
   geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21) +
   facet_wrap(~Model,nrow=2,scales='free')  +
   scale_fill_manual(values=c('white','black'),guide=FALSE)+
   theme(axis.title.y = element_blank(),
                                     axis.text = element_text(size=16),
                                     axis.title.x=element_text(size=18),
                                     legend.position = c(0.75,0.25))
 
 
   #geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+
     scale_y_reverse(breaks=c(1:9),labels = c('High asset specificity SDs','Low asset specificity SDs',
                                               'High asset specificity SDs\nx Low asset specificity SDs',
                                              'Dependent SDs','Independent SDs','Dependent SDs\nx Independent SDs',
                                             'Multi-jurisdictional SDs','Single-jurisdictional SDs','Multi-jurisdictional SDs\nx Single-jurisdictional SDs')) +
   scale_x_continuous(name = '95% credible interval: (LG debt + SD debt) / Total assessed value ~ .') +
   scale_fill_manual(values=c('white','black'),guide=FALSE)+
   theme(axis.title.y = element_blank(),
         axis.text = element_text(size=16),
         axis.title.x=element_text(size=18),
         legend.position = c(0.75,0.25)) +
   facet_wrap(~Model,scale='free')
 
   geom_hline(yintercept=c(1.5,4.5,7.5),lwd=2,col='grey50')
 
 


m_asset = model_fixed_coefs %>% filter(Model==2)
m_asset$Coef = fct_inorder(m_asset$Coef)
ggplot(data = m_asset %>% filter(grepl('sd',Coef))) +
  geom_vline(xintercept=0,lty=2) + 
  geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round')+
theme_tufte(ticks=F)  + geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+
  scale_fill_manual(values=c('white','black'),guide=F)+
  scale_y_discrete(limits = rev(levels(m_asset$Coef)),drop=T,
                   labels=c('High asset specificity SDs x\nlow asset specificity SDs','Low asset specificity SDs','High asset specificity SDs')) +
  scale_x_continuous(name = '95% credible interval',limits=c(-0.1,0.8),breaks=c(0,0.2,0.4,0.6,0.8)) +
  theme(axis.title.y = element_blank(),
                                         axis.text = element_text(size=18),
                                         axis.title.x=element_text(size=18)) +
  annotate('text',x = 0.6,y=1,label=
             paste(paste0('DIC: ',round(mod_list_gauss[[2]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[2]]$waic$waic,2))),size=7)




m_dep = model_fixed_coefs %>% filter(Model==3)
m_dep$Coef = fct_inorder(m_dep$Coef)
ggplot(data = m_dep %>% filter(grepl('sd',Coef))) +
  geom_vline(xintercept=0,lty=2) + 
  geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round')+
  theme_tufte(ticks=F)+ geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+
  scale_fill_manual(values=c('white','black'),guide=F)+
  scale_y_discrete(limits = rev(levels(m_dep$Coef)),drop=T,
 labels=c('Independent SDs x\nDependent SDs','Dependent SDs','Independent SDs')) +
  scale_x_continuous(name = '95% credible interval',limits=c(-0.1,0.8),breaks=c(0,0.2,0.4,0.6,0.8)) +
  theme(axis.title.y = element_blank(),
                                         axis.text = element_text(size=18),
                                         axis.title.x=element_text(size=18)) +
  annotate('text',x = 0.60,y=1,label=
             paste(paste0('DIC: ',round(mod_list_gauss[[3]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[3]]$waic$waic,2))),size=7)


m_juris = model_fixed_coefs %>% filter(Model==4)
m_juris$Coef = fct_inorder(m_juris$Coef)

ggplot(data = m_juris %>% filter(grepl('sd',Coef))) +
  geom_vline(xintercept=0,lty=2) + 
  geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=rev(Coef),yend=rev(Coef)),lwd=3,lineend = 'round') +
  theme_tufte(ticks=F)+ geom_point(aes(x=mean,y=rev(Coef),fill=as.factor(SIG)),size=5,shape=21)+
  scale_fill_manual(values=c('white','black'),guide=F)+
  scale_y_discrete(drop=T,labels=c(
                                   "Multi-jurisdictional SDs x\nSingle jurisdictional SDs",
                                   'Multi-jurisdictional SDs',   'Single jurisdication SDs'))+
 # scale_y_discrete(limits = rev(levels(m_juris$Coef)),drop=T,
  #            labels=c('Multi-jurisdictional SDs x\nSingle jurisdictional SDs','Low juris specificity SDs','High juris specificity SDs')) +
  scale_x_continuous(name = '95% credible interval',limits=c(-0.2,0.8),breaks=c(0,0.2,0.4,0.6,0.8)) +
  theme(axis.title.y = element_blank(),
                                         axis.text = element_text(size=18),
                                         axis.title.x=element_text(size=18)) +
  annotate('text',x = 0.60,y=1,label=
             paste(paste0('DIC: ',round(mod_list_gauss[[2]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[2]]$waic$waic,2))),size=7)


library(texreg)
library(stargazer)

summary(mod_list_gauss[[3]])

mod_names = c('All',"Asset",'Dep','Juris')

cpo_vals = lapply(1:length(mod_list_gauss),function(x) 
  data.frame(CPO = mod_list_gauss[[x]]$cpo$cpo,Model = mod_names[x],i = 1:length(mod_list_gauss[[x]]$cpo$cpo))) %>%
  do.call(rbind,.)

library(viridis)
ggplot(cpo_vals,aes(x=i,y=CPO,colour=Model)) + geom_point(pch=1) + theme_tufte(ticks=F) + 
  scale_color_viridis(discrete=T) + scale_x_continuous(name='Observation') + 
  theme(axis.text.x=element_blank(),axis.text.y=element_text(size=16),axis.title=element_text(size=18),legend.title=element_text(size=18),legend.text=element_text(size=16))



formt = Debt_Ratio_Total ~ Pop10k_mc + pop_growth_percent_mc + 
  PERC_BACH_OVER25_mc + Perc_Over65_mc + UnempR_mc +  Income_Per_Capita_1k_mc + 
  prop_house_with_children_mc+
  TotOE_SD1m_mc +
  urbanized +
  lg_count +
  sd_count  +
  f(Linear_Year,model='rw1',group=ID)
  f(ID, model="bym", graph="ga_county.adj")

test = inla(formt,family = 'gaussian', data = data_temp,
     control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),verbose=T)
plot(mod_list_gauss[[1]])

plot(test)
test
tmat = as.data.frame(mod_list_gauss[[1]]$model.matrix)
tmat$CPO = mod_list_gauss[[1]]$cpo$cpo
tmat$county = rep(1:159,10)
tmat$Year = rep(2005:2014,each=159)
tmat = tmat %>% arrange(county) %>% mutate(county_ordering = 1:nrow(.)) %>% arrange(Year) %>% mutate(year_ordering = 1:nrow(.))


ggplot(tmat,aes(x=year_ordering,y=CPO)) + geom_point(pch=1) + theme_tufte(ticks=F) + 
   scale_x_continuous(name='Observation') + 
  theme(axis.text.x=element_blank(),axis.text.y=element_text(size=16),axis.title=element_text(size=18),legend.title=element_text(size=18),legend.text=element_text(size=16))


ggplot(tmat,aes(x=county_ordering,y=CPO)) + geom_point(pch=1) + theme_tufte(ticks=F) + 
  scale_x_continuous(name='Observation') + 
  theme(axis.text.x=element_blank(),axis.text.y=element_text(size=16),axis.title=element_text(size=18),legend.title=element_text(size=18),legend.text=element_text(size=16))



159*2
dim(mod_list_gauss[[1]]$summary.fitted.values)
mod_list_gauss[[1]]$model.matrix[c(1,160,319),]
mod_list_gauss[[1]]$
table(cpo_vals$CPO<0.05)

####### OE Ratio instead of count Results
rm(list=ls())
load('Scratch/temp_results_oe_ratio_2.RData')
library(tidyverse)
library(INLA)
model_fixed_coefs = do.call(rbind,lapply(1:length(mod_list_gauss_oe),function(x) as.data.frame(mod_list_gauss_oe[[x]]$summary.fixed) %>%
                                           mutate(Model = x,Coef = mod_list_gauss_oe[[x]]$names.fixed,Effect='Fixed'))) %>% filter(!grepl('Intercept',Coef))  %>% dplyr::select(-kld)

model_random_coefs = do.call(rbind,lapply(1:length(mod_list_gauss_oe),function(x) as.data.frame(mod_list_gauss_oe[[x]]$summary.hyperpar) %>%
                                            mutate(Model = x,Coef = rownames(mod_list_gauss_oe[[x]]$summary.hyperpar),Effect='Random'))) %>%
  filter(!grepl('Intercept',Coef))


model_fixed_coefs$SIG = ifelse(model_fixed_coefs$`0.025quant`<0&model_fixed_coefs$`0.975quant`>0,0,1)

#model_coefs = rbind(model_fixed_coefs,model_random_coefs)
library(forcats)
library(ggthemes)


sd_coefs_only = model_fixed_coefs %>% filter(grepl('Over_TotOE|SD_All',model_fixed_coefs$Coef))
sd_coefs_only$Coef = fct_inorder(sd_coefs_only$Coef)

ggplot(data = sd_coefs_only) +
  geom_vline(xintercept=0,lty=2) +
  geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round') +
  theme_tufte(ticks=F) + geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)


ggplot(data = sd_coefs_only) +
  geom_vline(xintercept=0,lty=2) +
  geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=1:nrow(sd_coefs_only),yend=1:nrow(sd_coefs_only)),lwd=3,lineend = 'round') +
  theme_tufte(ticks=F) + geom_point(aes(x=mean,y=1:nrow(sd_coefs_only),fill=as.factor(SIG)),size=5,shape=21)+
  #scale_y_discrete(limits = rev(levels(sd_coefs_only$Coef)),drop=T)+
#labels=c('Total # SDs'))+
  scale_y_reverse(breaks=c(1:10),labels = c('SD OE / Total OE','High asset specificity SD OE / Total OE','Low asset specificity SD OE / Total OE',
                                            'High asset specificity SD OE / Total OE\nx Low asset specificity SD OE / Total OE',
                                            'Dependent SD OE / Total OE','Independent SD OE / Total OE','Dependent SD OE / Total OE\nx Independent SD OE / Total OE',
                                            'Multi-jurisdictional SD OE / Total OE','Single-jurisdictional SD OE / Tot OE','Multi-jurisdictional SD OE / Total OE\nx Single-jurisdictional SD OE / Tot OE')) +
  scale_x_continuous(name = '95% credible interval: (Total LG + SD debt) / Total assessed value ~.') +
  scale_fill_manual(values=c('white','black'),guide=FALSE)+
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x=element_text(size=18)) +
  geom_hline(yintercept=c(1.5,4.5,7.5),lwd=2,col='grey50')

  
library(knitr)

kable(as.data.frame(table(data_temp$COUNTY_FIPS,data_temp$Over_Limit)) %>% filter(Var2==1 & Freq>0) %>% rename(Times_Over = Freq) %>% dplyr::select(-Var2))




rm(list=ls())
load('Scratch/temp_results_lggo_sd_ratio.RData')
library(tidyverse)
library(INLA)
model_fixed_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.fixed) %>%
                                           mutate(Model = x,Coef = mod_list_gauss[[x]]$names.fixed,Effect='Fixed'))) %>% filter(!grepl('Intercept',Coef))  %>% dplyr::select(-kld)

model_random_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.hyperpar) %>%
                                            mutate(Model = x,Coef = rownames(mod_list_gauss[[x]]$summary.hyperpar),Effect='Random'))) %>%
  filter(!grepl('Intercept',Coef))


model_fixed_coefs$SIG = ifelse(model_fixed_coefs$`0.025quant`<0&model_fixed_coefs$`0.975quant`>0,0,1)

#model_coefs = rbind(model_fixed_coefs,model_random_coefs)
library(forcats)
library(ggthemes)


sd_coefs_only = model_fixed_coefs %>% filter(grepl('sd_count',model_fixed_coefs$Coef))
sd_coefs_only$Coef = fct_inorder(sd_coefs_only$Coef)

  
  ggplot(data = sd_coefs_only) +
  geom_vline(xintercept=0,lty=2) +
  geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=1:nrow(sd_coefs_only),yend=1:nrow(sd_coefs_only)),lwd=3,lineend = 'round') +
  theme_tufte(ticks=F) + geom_point(aes(x=mean,y=1:nrow(sd_coefs_only),fill=as.factor(SIG)),size=5,shape=21)+
  scale_y_reverse(breaks=c(1:10),labels = c('# Total SDs','High asset specificity SDs','Low asset specificity SDs',
                                            'High asset specificity SDs\nx Low asset specificity SDs',
                                            'Dependent SDs','Independent SDs','Dependent SDs\nx Independent SDs',
                                            'Single-jurisdictional SDs','Multi-jurisdictional SDs','Multi-jurisdictional SDs\nx Single-jurisdictional SDs')) +
  scale_x_continuous(name = '95% credible interval: (LG GO debt + SD debt) / Total assessed value ~ .') +
  scale_fill_manual(values=c('white','black'),guide=FALSE)+
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size=16),
        axis.title.x=element_text(size=18)) +
  geom_hline(yintercept=c(1.5,4.5,7.5),lwd=2,col='grey50')



  
  rm(list=ls())
  load('Scratch/temp_results_total_ratio_rw1.RData')
  library(tidyverse)
  library(INLA)
  model_fixed_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.fixed) %>%
                                             mutate(Model = x,Coef = mod_list_gauss[[x]]$names.fixed,Effect='Fixed'))) %>% filter(!grepl('Intercept',Coef))  %>% dplyr::select(-kld)
  
  model_random_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.hyperpar) %>%
                                              mutate(Model = x,Coef = rownames(mod_list_gauss[[x]]$summary.hyperpar),Effect='Random'))) %>%
    filter(!grepl('Intercept',Coef))
  
  
  model_fixed_coefs$SIG = ifelse(model_fixed_coefs$`0.025quant`<0&model_fixed_coefs$`0.975quant`>0,0,1)
  
  lapply(mod_list_gauss,function(x) x$waic$waic)
  
  summary(mod_list_gauss[[1]])
  
  #model_coefs = rbind(model_fixed_coefs,model_random_coefs)
  library(forcats)
  library(ggthemes)
  
  
  sd_coefs_only = model_fixed_coefs %>% filter(grepl('sd_count',model_fixed_coefs$Coef))
  sd_coefs_only$Coef = fct_inorder(sd_coefs_only$Coef)

  ggplot(data = sd_coefs_only) +
    geom_vline(xintercept=0,lty=2) +
    geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=1:nrow(sd_coefs_only),yend=1:nrow(sd_coefs_only)),lwd=3,lineend = 'round') +
    theme_tufte(ticks=F) + geom_point(aes(x=mean,y=1:nrow(sd_coefs_only),fill=as.factor(SIG)),size=5,shape=21)+
 #   scale_y_reverse(breaks=c(1:10),labels = c('# Total SDs','High asset specificity SDs','Low asset specificity SDs',
  #                                            'High asset specificity SDs\nx Low asset specificity SDs',
   #                                           'Dependent SDs','Independent SDs','Dependent SDs\nx Independent SDs',
    #                                          'Single-jurisdictional SDs','Multi-jurisdictional SDs','Multi-jurisdictional SDs\nx Single-jurisdictional SDs')) +
    scale_x_continuous(name = '95% credible interval: (LG debt + SD debt) / Total assessed value ~ .') +
    scale_fill_manual(values=c('white','black'),guide=FALSE)+
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size=16),
          axis.title.x=element_text(size=18)) +
    geom_hline(yintercept=c(1.5,4.5,7.5),lwd=2,col='grey50')
  
  
  
  
  mod_names = c('All',"Asset",'Dep','Juris')
  
  cpo_vals = lapply(1:length(mod_list_gauss),function(x) 
    data.frame(CPO = mod_list_gauss[[x]]$cpo$cpo,Model = mod_names[x],i = 1:length(mod_list_gauss[[x]]$cpo$cpo))) %>%
    do.call(rbind,.)
  
  library(viridis)
  ggplot(cpo_vals,aes(x=i,y=CPO,colour=Model)) + geom_point(pch=1) + theme_tufte(ticks=F) + 
    scale_color_viridis(discrete=T) + scale_x_continuous(name='Observation') + 
    theme(axis.text.x=element_blank(),axis.text.y=element_text(size=16),axis.title=element_text(size=18),legend.title=element_text(size=18),legend.text=element_text(size=16))
  

##############   AR1 Plots ###############
  rm(list=ls())
  load('Scratch/temp_results_total_ratio_ar1.RData')
  library(tidyverse)
  library(INLA)
  model_fixed_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.fixed) %>%
                                             mutate(Model = x,Coef = mod_list_gauss[[x]]$names.fixed,Effect='Fixed'))) %>% filter(!grepl('Intercept',Coef))  %>% dplyr::select(-kld)
  
  model_random_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.hyperpar) %>%
                                              mutate(Model = x,Coef = rownames(mod_list_gauss[[x]]$summary.hyperpar),Effect='Random'))) %>%
    filter(!grepl('Intercept',Coef))
  
  
  model_fixed_coefs$SIG = ifelse(model_fixed_coefs$`0.025quant`<0&model_fixed_coefs$`0.975quant`>0,0,1)
  
  lapply(mod_list_gauss,function(x) x$waic$waic)

  mod_names = c('All',"Asset",'Dep','Juris')
  
  cpo_vals = lapply(1:length(mod_list_gauss),function(x) 
    data.frame(CPO = mod_list_gauss[[x]]$cpo$cpo,Model = mod_names[x],i = 1:length(mod_list_gauss[[x]]$cpo$cpo))) %>%
    do.call(rbind,.)
  
  library(viridis)
  ggplot(cpo_vals,aes(x=i,y=CPO,colour=Model)) + geom_point(pch=1) + theme_tufte(ticks=F) + 
    scale_color_viridis(discrete=T) + scale_x_continuous(name='Observation') + 
    theme(axis.text.x=element_blank(),axis.text.y=element_text(size=16),axis.title=element_text(size=18),legend.title=element_text(size=18),legend.text=element_text(size=16))
  
  #model_coefs = rbind(model_fixed_coefs,model_random_coefs)
  library(forcats)
  library(ggthemes)
  
  sd_coefs_only = model_fixed_coefs %>% filter(grepl('sd_count',model_fixed_coefs$Coef))
  sd_coefs_only$Coef = fct_inorder(sd_coefs_only$Coef)
  
  ggplot(data = sd_coefs_only) +
    geom_vline(xintercept=0,lty=2) +
   geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=1:nrow(sd_coefs_only),yend=1:nrow(sd_coefs_only)),lwd=3,lineend = 'round') +
    #geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round') +
    theme_tufte(ticks=F) + 
   geom_point(aes(x=mean,y=1:nrow(sd_coefs_only),fill=as.factor(SIG)),size=5,shape=21)+
  #  geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+

       scale_y_reverse(breaks=c(1:10),labels = c('# Total SDs','High asset specificity SDs','Low asset specificity SDs',
                                                'High asset specificity SDs\nx Low asset specificity SDs',
                                               'Dependent SDs','Independent SDs','Dependent SDs\nx Independent SDs',
                                              'Multi-jurisdictional SDs','Single-jurisdictional SDs','Multi-jurisdictional SDs\nx Single-jurisdictional SDs')) +
    scale_x_continuous(name = '95% credible interval: (LG debt + SD debt) / Total assessed value ~ .') +
    scale_fill_manual(values=c('white','black'),guide=FALSE)+
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size=16),
          axis.title.x=element_text(size=18)) +
    geom_hline(yintercept=c(1.5,4.5,7.5),lwd=2,col='grey50')
  

  m_asset = model_fixed_coefs %>% filter(Model==2)
  m_asset$Coef = fct_inorder(m_asset$Coef)
  ggplot(data = m_asset %>% filter(grepl('sd',Coef))) +
    geom_vline(xintercept=0,lty=2) + 
    geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round')+
    theme_tufte(ticks=F)  + geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+
    scale_fill_manual(values=c('white','black'),guide=F)+
    scale_y_discrete(limits = rev(levels(m_asset$Coef)),drop=T,
                     labels=c('High asset specificity SDs x\nlow asset specificity SDs','Low asset specificity SDs','High asset specificity SDs')) +
    scale_x_continuous(name = '95% credible interval',limits=c(-0.1,0.8),breaks=c(0,0.2,0.4,0.6,0.8)) +
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size=18),
          axis.title.x=element_text(size=18)) +
    annotate('text',x = 0.6,y=1,label=
               paste(paste0('DIC: ',round(mod_list_gauss[[2]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[2]]$waic$waic,2))),size=7)
  

  m_dep = model_fixed_coefs %>% filter(Model==3)
  m_dep$Coef = fct_inorder(m_dep$Coef)
  ggplot(data = m_dep %>% filter(grepl('sd',Coef))) +
    geom_vline(xintercept=0,lty=2) + 
    geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round')+
    theme_tufte(ticks=F)+ geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+
    scale_fill_manual(values=c('white','black'),guide=F)+
    scale_y_discrete(limits = rev(levels(m_dep$Coef)),drop=T,
                     labels=c('Independent SDs x\nDependent SDs','Independent SDs','Dependent SDs')) +
    scale_x_continuous(name = '95% credible interval',limits=c(-0.1,0.8),breaks=c(0,0.2,0.4,0.6,0.8)) +
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size=18),
          axis.title.x=element_text(size=18)) +
    annotate('text',x = 0.60,y=1,label=
               paste(paste0('DIC: ',round(mod_list_gauss[[3]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[3]]$waic$waic,2))),size=7)
  
  
  m_juris = model_fixed_coefs %>% filter(Model==4)
  m_juris$Coef = fct_inorder(m_juris$Coef)
  
  ggplot(data = m_juris %>% filter(grepl('sd',Coef))) +
    geom_vline(xintercept=0,lty=2) + 
    geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=rev(Coef),yend=rev(Coef)),lwd=3,lineend = 'round') +
    theme_tufte(ticks=F)+ geom_point(aes(x=mean,y=rev(Coef),fill=as.factor(SIG)),size=5,shape=21)+
    scale_fill_manual(values=c('white','black'),guide=F)+
    scale_y_discrete(drop=T,labels=c(
      "Multi-jurisdictional SDs x\nSingle jurisdictional SDs",
        'Single jurisdication SDs','Multi-jurisdictional SDs' ))+
    # scale_y_discrete(limits = rev(levels(m_juris$Coef)),drop=T,
    #            labels=c('Multi-jurisdictional SDs x\nSingle jurisdictional SDs','Low juris specificity SDs','High juris specificity SDs')) +
    scale_x_continuous(name = '95% credible interval',limits=c(-0.2,0.8),breaks=c(0,0.2,0.4,0.6,0.8)) +
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size=18),
          axis.title.x=element_text(size=18)) +
    annotate('text',x = 0.60,y=1,label=
               paste(paste0('DIC: ',round(mod_list_gauss[[2]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[2]]$waic$waic,2))),size=7)
  
  
####### AR1 LG GO + SD Debt
  
  rm(list=ls())
  load('Scratch/temp_results_lggo_sd_ratio_ar1.RData')
  
  
  model_fixed_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.fixed) %>%
                                             mutate(Model = x,Coef = mod_list_gauss[[x]]$names.fixed,Effect='Fixed'))) %>% filter(!grepl('Intercept',Coef))  %>% dplyr::select(-kld)
  
  model_random_coefs = do.call(rbind,lapply(1:length(mod_list_gauss),function(x) as.data.frame(mod_list_gauss[[x]]$summary.hyperpar) %>%
                                              mutate(Model = x,Coef = rownames(mod_list_gauss[[x]]$summary.hyperpar),Effect='Random'))) %>%
    filter(!grepl('Intercept',Coef))
  
  
  model_fixed_coefs$SIG = ifelse(model_fixed_coefs$`0.025quant`<0&model_fixed_coefs$`0.975quant`>0,0,1)
  
  #model_coefs = rbind(model_fixed_coefs,model_random_coefs)
  library(forcats)
  
  mod_all =  model_fixed_coefs %>% filter(Model == 1)
  mod_all$Coef = fct_inorder(mod_all$Coef)
  
  
  ggplot(data = mod_all) +
    geom_vline(xintercept=0,lty=2) +
    geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=1:nrow(mod_all),yend=1:nrow(mod_all)),lwd=3,lineend = 'round') +
    theme_tufte(ticks=F) + geom_point(aes(x=mean,y=1:nrow(mod_all),fill=as.factor(SIG)),size=5,shape=21)+
    scale_x_continuous(name = '95% credible interval',breaks=seq(-1,1,0.25),labels=seq(-1,1,0.25)) +
    scale_fill_manual(values=c('white','black'),guide=FALSE)+
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size=16),
          axis.title.x=element_text(size=18)) +
    scale_y_reverse(breaks=c(1:nrow(mod_all)),labels = c('Population (10k)',"Yearly population growth %", "% pop. with Bachelor's",'% pop. over age 65',
                                                         'Unemployment %','Income per capita ($1k)',  '% households w/ children','SD Operating Expenses ($1M)','Rural',
                                                         'Suburban','# local governments','# special districts'))+
    annotate('text',x = 0.3,y=7.5,label=
               paste(paste0('DIC: ',round(mod_list_gauss[[1]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[1]]$waic$waic,2))),size=7)

  
  sd_coefs_only = model_fixed_coefs %>% filter(grepl('sd_count',model_fixed_coefs$Coef))
  sd_coefs_only$Coef = fct_inorder(sd_coefs_only$Coef)
  
  sd_coefs_only
  ggplot(data = sd_coefs_only) +
    geom_vline(xintercept=0,lty=2) +
    geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=1:nrow(sd_coefs_only),yend=1:nrow(sd_coefs_only)),lwd=3,lineend = 'round') +
    #geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round') +
    theme_tufte(ticks=F) + 
    geom_point(aes(x=mean,y=1:nrow(sd_coefs_only),fill=as.factor(SIG)),size=5,shape=21)+
    #geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+
    scale_y_reverse(breaks=c(1:10),labels = c('# Total SDs','High asset specificity SDs','Low asset specificity SDs',
                                               'High asset specificity SDs\nx Low asset specificity SDs',
                                               'Dependent SDs','Independent SDs','Dependent SDs\nx Independent SDs',
                                               'Multi-jurisdictional SDs','Single-jurisdictional SDs','Multi-jurisdictional SDs\nx Single-jurisdictional SDs')) +
    scale_x_continuous(name = '95% credible interval: (LG GO + SD debt) / Tot. assessed value ~ .') +
    scale_fill_manual(values=c('white','black'),guide=FALSE)+
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size=16),
          axis.title.x=element_text(size=18)) +
    geom_hline(yintercept=c(1.5,4.5,7.5),lwd=2,col='grey50')
  
  
  



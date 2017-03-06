
library(tidyverse)
library(ggthemes)
install.packages("INLA", repos="https://www.math.ntnu.no/inla/R/stable")
rm(list=ls())
load('Scratch/temp_results_total_ratio_ar1.RData')


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
     annotate('text',x = 0.6,y=7.5,label=
                          paste(paste0('DIC: ',round(mod_list_gauss[[1]]$dic$dic,2)),'\n',paste0('WAIC: ',round(mod_list_gauss[[1]]$waic$waic,2))),size=7)


 
 
 sd_coefs_only = model_fixed_coefs %>% filter(grepl('sd_count',model_fixed_coefs$Coef))
 sd_coefs_only$Coef = fct_inorder(sd_coefs_only$Coef)
 
 ggplot(data = sd_coefs_only) +
   geom_vline(xintercept=0,lty=2) +
   geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=1:nrow(sd_coefs_only),yend=1:nrow(sd_coefs_only)),lwd=3,lineend = 'round') +
   #geom_segment(aes(x=`0.025quant`,xend=`0.975quant`,y=Coef,yend=Coef),lwd=3,lineend = 'round') 
   theme_tufte(ticks=F) + 
   geom_point(aes(x=mean,y=1:nrow(sd_coefs_only),fill=as.factor(SIG)),size=5,shape=21)+
   #geom_point(aes(x=mean,y=Coef,fill=as.factor(SIG)),size=5,shape=21)+
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
  
  
  



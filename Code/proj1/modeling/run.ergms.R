rm(list=ls())
library(broom)
library(statnet)
library(scales)
library(ggthemes)
library(plyr);library(dplyr)
library(texreg)
library(ggplot2)
require(ggmcmc)
library(gdata)
library(car)
library(ggvis)
library("network") # tested with version 1.11.3
library("statnet") # tested with version 2014.2.0
#library("xergm") # tested with version 1.4.13
library("texreg")# tested with version 1.35.2

library(snow)
#library(rlecuyer)
library(parallel)

library(snow)
library(Rmpi)
np <- 4
cluster <- makeMPIcluster(np)
cluster
clusterEvalQ(cluster, library(ergm.count))
#pties = read.csv('Input/officials.data/cleaned/person.ties.total.2011.2014.csv',row.names=1)
sd.master = read.csv('../../../Input/officials.data/cleaned/sd.master.csv')
lg.master = read.csv('../../../Input/officials.data/cleaned/locgov.master.csv')


lg.master %>% filter(!(duplicated(Org.Name))) %>% group_by(Type) %>% summarise(n())


head(lg.master)

all.names = join(sd.master %>% filter(!is.na(Name)) %>% select(Name,SD,Year) %>% rename(Org.Name = SD) %>% 
                    mutate(Name = tolower(Name),Gov.Type = 'SD'),
                  lg.master %>% select(Name,Org.Name,Year,Type) %>% rename(Gov.Type = Type) %>% mutate(Name = tolower(Name)),type='full')

all.names = all.names[all.names$Name!='no_page',]
all.names$Org.Name = as.character(all.names$Org.Name)
all.names$Name = as.character(all.names$Name)

n.orgs = length(sort(unique(all.names$Org)))
n.people = length(sort(unique(all.names$Name)))
org.names = as.character(sort(unique(all.names$Org)))
people.names = as.character(sort(unique(all.names$Name)))

all.names.uq = all.names %>% filter(!duplicated(paste0(Name,Org.Name))) 

bip.mat = as.matrix(table(all.names.uq[,c('Name',"Org.Name")]))
org.mat = tcrossprod(t(bip.mat))


org.network = as.network(org.mat,type='adjacency',ignore.eval = FALSE,names.eval = "Common.Persons",directed=F)
set.vertex.attribute(org.network,'Gov.Type',value = all.names$Gov.Type[match(network.vertex.names(org.network),all.names$Org.Name)])

#### Compute distance between


sd_geo = read.csv('../../../Input/officials.data/cleaned/sd_geocode.csv')
lg_geo = read.csv('../../../Input/officials.data/cleaned/lg_geocode.csv')

all.geo = rbind(lg_geo %>% select(Org.Name,lat,lon) %>% mutate(Org.Name = as.character(Org.Name)), 
                sd_geo %>% filter(SD!='') %>% select(SD,lat,lon) %>% rename(Org.Name = SD) %>% mutate(Org.Name = as.character(Org.Name)))

all.geo = all.geo %>% arrange(Org.Name)
gdist = geosphere::distm(cbind(all.geo$lon,all.geo$lat))
gdist = gdist/100000
colnames(gdist) = rownames(gdist) = all.geo$Org.Name


# lg.fin = read.csv('../../../Input/fiscal.data/cleaned/localgov_fiscal_2014.csv')
#sd.fin = read.csv('Input/fiscal.data/cleaned/sd_fiscal_2011-2014.csv')
# lg.fin = lg.fin %>% rename(fromloc = V255)
# 
# all.fin = rbind(sd.fin %>% select(p2authname,fromloc) %>% rename(Org.Name = p2authname),
#                 lg.fin %>% select(NAMEGOV,fromloc) %>% rename(Org.Name = NAMEGOV))
# all.fin$Org.Name = gsub(' Town$',' City',all.fin$Org.Name)
# all.fin$Org.Name = gsub('Webster County Unified','Webster County',all.fin$Org.Name)
# all.fin$Org.Name = gsub('Tallapoosa Development Authority',"The Development Authority of the City of Tallapoosa",all.fin$Org.Name)

burnin <- 10000   # MCMC burnin
sampsize <- 50000  # MCMC sample size
maxit <- 40       # number of MCMC MLE iterations
nsim <- 10000       # number of simulated networks for the GOF assessment
seed <- 24 # random seed for exact reproducibility
interval = 1500
set.seed(seed)

edge.count = length(org.network$mel)
#"sum  +  nonzero + mutual(form='min') + transitiveweights('min','max','min') + 
#nodecov('meetings.attended')"

custom.control = control.ergm(
  #MCMC.prop.weights="0inflated",
  #MCMC.prop.args=list(p0=0.5),
  #MCMC.prop.weights="TNT",
  #MCMLE.trustregion=1000,
  MCMLE.density.guard = 4* edge.count,
  MCMC.runtime.traceplot=F,seed=seed,
  MPLE.max.dyad.types=1e+7,
  CD.maxit = maxit,
  MCMLE.maxit=maxit,
  #MCMC.addto.se=T,
  # init.method = 'zeros',
  parallel = cluster,
  #parallel.type="PSOCK",parallel=np,
  parallel.version.check=FALSE,
  MCMC.samplesize=sampsize,
  MCMC.burnin=burnin,MCMC.interval=interval)

verb = FALSE
getloglik = FALSE




network.vertex.names(org.network)[1782]
library(xergm)
list.edge.attributes(org.network)




### Model version with 1 level, assign structural zero to lg-lg and sd-sd ties
form.0.3 = org.network ~ sum +nonzero+nodesqrtcovar(center=TRUE)+
transitiveweights(twopath="min",combine="max",affect="min")+
 edgecov(gdist,form='sum') + nodefactor('Gov.Type')

m1 <- sum(org.network %e% "Common.Persons")/network.dyadcount(org.network) 
init.sum.net1 <- log(1 - 1/(m1+1))
custom.control$init = c(init.sum.net1,rep(0,7))


library(ergm.count)
mod.test = ergm(form.0.3,response = 'Common.Persons',reference=~Poisson,
                      eval.loglik=getloglik,verbose=TRUE,control=custom.control)
                    #  offset.coef=-Inf,
                    #  control = custom.control,verbose=verb,response = 'Common.Persons',reference=~Poisson)


save.image('../../../Scratch/test_ergmcount_results.RData')

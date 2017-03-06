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
clusterEvalQ(cluster, library(ergm.count))
#pties = read.csv('Input/officials.data/cleaned/person.ties.total.2011.2014.csv',row.names=1)
sd.master = read.csv('Input/officials.data/cleaned/sd.master.csv')
lg.master = read.csv('Input/officials.data/cleaned/locgov.master.csv')

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

uq.orgs = all.names.uq %>% select(-Name) %>% filter(!duplicated(Org.Name)) %>% arrange(Org.Name)

rownames(uq.orgs) = uq.orgs$Org.Name
uq.orgs$Gov.Type[uq.orgs$Gov.Type=='CG'] = 'County'


cut = 10
org.mat = org.mat[1:cut,1:cut]



nsim = 50000
burn = 10000
thin.by = 1/100
test =  GERGM::gergm(org.mat~edges(alpha=1,method = 'endogenous'),#+ nodemix(Gov.Type,base = "County"), 
                     covariate_data = uq.orgs[1:cut,], network_is_directed = F, normalization_type = "division",
                     number_of_networks_to_simulate = nsim,MCMC_burnin = burn,seed = 24,transformation_type = 'LogCauchy',
              output_directory = '../../../Scratch',output_name = 'test_gergm',verbose = F, use_stochastic_MH =T, estimate_model = T,
              parallel = TRUE,parallel_statistic_calculation=TRUE,cores = 4,thin = thin.by,proposal_variance = 0.01,
              slackr_integration_list = 
                list(model_name = "test_fit", channel = "#gergm",
                     incoming_webhook_url = "https://hooks.slack.com/services/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"))



set.seed(12345)


net <- matrix(sample(1:10,100,replace=T),10,10)
colnames(net) <- rownames(net) <- letters[1:10]
net[lower.tri(net)] <- t(net)[lower.tri(net)]
net[net<8]= 0


net


test_net = unname(org.mat, force = FALSE)
colnames(test_net) <- rownames(test_net) <- letters[1:10]
test_net = test_net+0.5

########################### 1. No Covariates #############################
# Preparing an unbounded network without covariates for gergm estimation #
set.seed(12345)
net <- matrix(sample(c(1,2),100,replace=T),10,10)
colnames(net) <- rownames(net) <- letters[1:10]
formula <- net ~ edges(method = "endogenous") 
test <- gergm(formula,
              normalization_type = "division",
              network_is_directed = TRUE,
              number_of_networks_to_simulate = 40000,
              thin = 1/10,
              proposal_variance = 0.2,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.01,
              force_x_theta_update = 4,verbose=F)

set.seed(12345)
net <- matrix(sample(1:3,100,replace=T),10,10)
net[net==2] = 0
colnames(net) <- rownames(net) <- letters[1:10]
formula <- net ~ edges(method = "endogenous") 
test <- gergm(formula,
              normalization_type = "division",
              network_is_directed = TRUE,
              number_of_networks_to_simulate = 40000,
              thin = 1/10,
              proposal_variance = 0.2,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.01,
              force_x_theta_update = 4,verbose=F)


net[net<8] = 8


net

formula <- net  ~ edges(method = "endogenous")
test <- gergm(formula,
              normalization_type = "division",
              network_is_directed = FALSE,
            transformation_type = 'Gaussian',
              number_of_networks_to_simulate = 100000,
              thin = 1/10,
              #proposal_variance = 0.02,
              MCMC_burnin = 10000,
              seed = 456,
              convergence_tolerance = 0.01,
              force_x_theta_update = 4)



table(diag.remove(test@network,0))

table(test@network)

test@network
dim(org.mat)


all.names = all.names[all.names$Org.Name %in% org.names & all.names$Year==2011,]


bip.mat = as.matrix(table(all.names[,c('Name',"Org.Name")]))

colSums(bip.mat)
tcrossprod(t(empty_bip))
tcrossprod(empty_bip)

tcrossprod(t(bip.mat))

all.names[all.names$Org.Name=='Acworth Area Convention and Visitors Bureau Authority',]

org.mat <- t(bip.mat) %*% bip.mat

for (i in 1:nrow(all.names)) {empty_bip[rownames(empty_bip)==all.names$Name[i],colnames(empty_bip)==all.names$Org.Name[i]] = 
  empty_bip[rownames(empty_bip)==all.names$Name[i],colnames(empty_bip)==all.names$Org.Name[i]]+ 1}


sum(empty_bip[,colnames(empty_bip)=='Abbeville Housing Authority'])


sum(all.names$Name %in% all.names$Name[all.names$Org.Name=='Abbeville Housing Authority'])


table(all.names$Org.Name)

test <- tcrossprod(t(empty_bip))
tcrossprod(t(as.matrix(table(all.names[,c('Name',"Org.Name")]))))


all.names$Org.Name



sum(all.names$Org.Name=='Abbeville Housing Authority')


all.names[all.names$Org.Name=='Abbeville Housing Authority',]


dim(all.names[all.names$Org.Name=='Zebulon City',])
sum(bip.mat[,colnames(bip.mat)=='Zebulon City'])

bip.mat[rownames(bip.mat)=='phylis crawford',]

org.mat[1782,1782]

head(colnames(org.mat))
head(rownames(org.mat))
table(bip.mat)
dim(org.mat)
table(org.mat)










test = pbsapply(1:nrow(all.names),function(i){
  em)
library(pbapply)


lapply(1:nrow(all.names), function(x) all.names$Name[x]))





intersect(all.names$Name[grepl('Zebulon',all.names$Org.Name)],all.names$Name[grepl('Young Harris',all.names$Org.Name)])


org.mat[1781,1782]

which( as.sociomatrix(org.network,"Common.Persons") == 16, arr.ind=T )


gergm(org.network~edges)






all.names = all.names %>% mutate(Org.Name = as.character(Org.Name)) %>% arrange(Org.Name)
bip.mat = as.matrix(table(all.names[,c('Name',"Org.Name")]))
org.mat <- tcrossprod(t(bip.mat))

org.network = as.network(org.mat,type='adjacency',ignore.eval = FALSE,names.eval = "Common.Persons",directed=F)
set.vertex.attribute(org.network,'Gov.Type',value = all.names$Gov.Type[match(network.vertex.names(org.network),all.names$Org.Name)])

#### Compute distance between
sd_geo = read.csv('Input/officials.data/cleaned/sd_geocode.csv')
lg_geo = read.csv('Input/officials.data/cleaned/lg_geocode.csv')

all.geo = rbind(lg_geo %>% select(Org.Name,lat,lon) %>% mutate(Org.Name = as.character(Org.Name)), 
              sd_geo %>% select(SD,lat,lon) %>% rename(Org.Name = SD) %>% mutate(Org.Name = as.character(Org.Name)))
all.geo = all.geo %>% arrange(Org.Name) %>% filter(Org.Name!='')
gdist = geosphere::distm(cbind(all.geo$lon,all.geo$lat))
gdist = gdist/100000
colnames(gdist) = rownames(gdist) = sort(all.geo$Org.Name)

lg.fin = read.csv('Input/fiscal.data/cleaned/localgov_fiscal_2014.csv')
sd.fin = read.csv('Input/fiscal.data/cleaned/sd_fiscal_2011-2014.csv')
lg.fin = lg.fin %>% rename(fromloc = V255)

all.fin = rbind(sd.fin %>% select(p2authname,fromloc) %>% rename(Org.Name = p2authname),
      lg.fin %>% select(NAMEGOV,fromloc) %>% rename(Org.Name = NAMEGOV))
all.fin$Org.Name = gsub(' Town$',' City',all.fin$Org.Name)
all.fin$Org.Name = gsub('Webster County Unified','Webster County',all.fin$Org.Name)
all.fin$Org.Name = gsub('Tallapoosa Development Authority',"The Development Authority of the City of Tallapoosa",all.fin$Org.Name)





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
  MCMC.prop.args=list(p0=0.5),
  MCMC.prop.weights="TNT",
  MCMLE.trustregion=1000,
  MCMLE.density.guard = 4* edge.count,
  MCMC.runtime.traceplot=F,seed=seed,
  MPLE.max.dyad.types=1e+7,
  CD.maxit = maxit,
  MCMLE.maxit=maxit,
  #MCMC.addto.se=T,
  # init.method = 'zeros',
 # parallel = cluster,
  # parallel.type="PSOCK",parallel=np,
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
                      eval.loglik=getloglik,verbose=verb,control=custom.control)
                    #  offset.coef=-Inf,
                    #  control = custom.control,verbose=verb,response = 'Common.Persons',reference=~Poisson)

ergm::search.ergmTerms('temporal')



save.image('Output/temp.results3.RData')

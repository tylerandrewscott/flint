base = read.csv('data/1_online_egos.csv')
hyper_wget_edges = read.csv('data/hyperlinksWget_edgelist.csv',header=F)
hyper_ic_edges = read.csv('data/hyperlinksIC2_edgelist.csv',header=F)
twitter_M_edges = read.csv('data/twitterM_edgelist_all.csv',header=F)
twitter_NRT_edges = read.csv('data/twitterM_edgelist_NoRT.csv',header=F)
twitter_NRT_edges$V2 = gsub('^ ','',twitter_NRT_edges$V2)
twitter_egos = read.csv('data/twitter_egos.csv',header=F)

edgelist_list = list(hyper_ic_edges,hyper_wget_edges,twitter_M_edges,twitter_NRT_edges)
base.network = network.initialize(max(base$Node))
emlist = rep(list(base.network), 4)
for (i in 1:4)
{
  emlist[[i]][cbind(base$Node[match(edgelist_list[[i]]$V1,base$Name)],
                    base$Node[match(edgelist_list[[i]]$V2,base$Name)])] =1  
  emlist[[i]] %e% "count" <- edgelist_list[[i]]$V3
  emlist[[i]]%v%"Name" = as.character(base$Name)
}

custom.control = control.ergm(MCMLE.maxit = 30,
                              MCMC.burnin = 7500,
                              MCMC.prop.weights =  'TNT',
                              MCMC.samplesize=50000,MCMC.interval=1000
                                )
library(parallel)

base.mods = lapply(emlist,function(net) ergm(net ~ edges + mutual))

unres.mods = mclapply(emlist,function(net) ergm(net ~ edges + mutual + 
                                 gwidegree(1.5,fixed=T) + 
                                 gwesp(0.5,fixed=T),control = custom.control),
mc.preschedule = FALSE,mc.cores = getOption("mc.cores", 4L),
mc.cleanup=TRUE,
mc.set.seed = FALSE)

degreedist(emlist[[3]])

mcmc.diagnostics(test)
test = ergm(emlist[[3]] ~ edges + mutual + 
             # gwidegree(0.25,fixed=T) + gwodegree(0.25,fixed=T) +
              gwesp(0.5,fixed=T),control = custom.control,verbose=T)

emlist[[3]]
unres.mods = lapply(emlist,function(net) ergm(net ~ edges + mutual + 
                                                gwidegree(1.5,fixed=T) + 
                                                gwesp(0.5,fixed=T)))

library(rgeos)
library(rgdal)
library(dplyr)

library(readxl)


georgia_outline = readOGR('spatial_data','PVS_15_v1_state_13')
georgia_grid = SpatialPoints(sp::spsample(georgia_outline,n=100000,type='hexagonal'),proj4string=CRS(proj4string(georgia_outline)))

goutline = fortify(georgia_outline)
gplace = readOGR('spatial_data/custom','georgia_places')
gcounty= readOGR('spatial_data/custom','georgia_counties')
gsd = readOGR('spatial_data/custom','georgia_sd')
over_place = over(georgia_grid,gplace,returnList = TRUE)
over_county = over(georgia_grid,gcounty ,returnList = TRUE)
over_sd = over(georgia_grid,gsd,returnList = TRUE)

georgia_grid_df = as.data.frame(georgia_grid)
georgia_grid_df$over_place = unlist(lapply(over_place,nrow))
georgia_grid_df$over_sd = unlist(lapply(over_sd,nrow))
georgia_grid_df$over_county = unlist(lapply(over_county,nrow))

georgia_grid_df = georgia_grid_df %>% mutate(over_govs = over_place + over_sd + over_county)


library(ggthemes)
library(viridis)

 ggplot() + geom_point(data = georgia_grid_df,aes(x = x,y =y,colour=over_govs),size=0.6,shape = 19) + scale_color_viridis(name = '# active LGs + SDs',option='magma') + 
  theme_tufte(ticks=F) + theme(axis.text=element_blank(),axis.title=element_blank(),legend.position = c(0.8,0.8)) +
   geom_path(aes(x=long,y=lat,group=id),data = goutline,lwd=1)


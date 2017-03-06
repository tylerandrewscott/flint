library(readxl);library(plyr);library(dplyr)
sd.df = readxl::read_excel('Input/temp/temp_sd_file.csv')

unique.sd = sd.df %>% filter(Year %in% c(2011:2015)) %>% filter(!duplicated(paste(authority_name)))
unique.sd$Type = basic.info$Type[match(tolower(unique.sd$authority_name), tolower(basic.info$Authority.Name))]


table(unique.sd$created_by)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(grid);library(lattice);library(gridExtra)

d1 = data.frame(x = year(unique.sd$date_created),y = NA)
d2 = data.frame(x = (min(year(unique.sd$date_created))-1):max(year(unique.sd$date_created)),
                y = sapply((min(year(unique.sd$date_created))-1):max(year(unique.sd$date_created)), function(x) sum(x > year(unique.sd$date_created))))
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


gg = read.csv('Input/temp/board.member.ties.scratch.csv',row.names=1)

person.ties.11.15 = tally(group_by(gg,Loc.Gov,SD), sort = TRUE)
person.ties.11.15 = person.ties.11.15 %>% mutate(avg.year = n/5)
sd.df$juris_type[grep('Single',sd.df$juris_type)] = 'Single-Jurisidictional'
sd.df$juris_type[grep('Single',sd.df$juris_type,invert=T)] = 'Multi-jurisdictional'
person.ties.11.15$juris.type = sd.df$juris_type[match(person.ties.11.15$SD, sd.df$authority_name)]
person.ties.11.15$depen = sd.df$dependency[match(person.ties.11.15$SD, sd.df$authority_name)]
person.ties.11.15$type = sd.df$Type[match(person.ties.11.15$SD, sd.df$authority_name)]



ggplot(person.ties.11.15) + geom_dotplot(aes(fill=as.factor(type),x=avg.year),binwidth=1) + 
  scale_color_colorblind()






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

pties = read.csv('Input/officials.data/cleaned/person.ties.total.2011.2014.csv',row.names=1)
sd.master = read.csv('Input/officials.data/cleaned/sd.master.csv',row.names=1)
lg.master = read.csv('Input/officials.data/cleaned/locgov.master.csv',row.names=1)

n.member.govs = sd.master %>% group_by(SD,Year) %>% summarise(number.member.govs = n())

n.loc.gov = length(unique(lg.master$Org.Name))
n.sd = length(unique(sd.master$SD))
loc.gov.names = as.character(unique(sort(lg.master$Org.Name)))
n.all = n.loc.gov+n.sd
all.names = sort(c(as.character(unique(sd.master$SD)),as.character(unique(lg.master$Org.Name))))

board.seats = sd.master %>% filter(!duplicated(paste(SD,Year))) %>% group_by(SD) %>% 
  summarise(total.board.seats = sum(board.count))

board.connections = pties %>% group_by(SD) %>% summarise(total.connections = sum(n))

board.values = join(board.connections,board.seats) 
board.values$Proportion = board.values$total.connections/board.values$total.board.seats

board.values  = join(board.values,sd.master)
board.values = join(board.values,n.member.govs)

board.values = board.values%>% filter(!duplicated(SD))

#lg.master$Name[nchar(as.character(lg.master$Name))<8]



library(ggvis)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(board.values,aes(y=total.connections,x=total.board.seats,colour = factor(number.member.govs),size=factor(number.member.govs))) +
  geom_jitter(shape=1,alpha=0.5) + 
  #scale_size_manual(name = 'Member/ngovernments') + 
  geom_abline(intercept=0,slope=1,lty=2,colour="#999999") +
  theme_tufte(ticks=F) +
  #geom_text(aes(label=SD),size=3)+
  theme(legend.position = c(0.9,.3),axis.text=element_text(size=16),
        axis.title=element_text(size=18),legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.direction='vertical') +
  xlab('Total Board Seats') + ylab('# Unique Connections')+ guides(colour=guide_legend()) +
  scale_colour_manual(name = 'Member\ngovernments',breaks=c('1','5','10','15'),values = c(cbPalette[1],rep(cbPalette[2],13))) +
  scale_size_manual(name = 'Member\ngovernments',breaks=c('1','5','10','15'),values = sqrt(seq_len(15))) +
  scale_y_continuous(breaks=c(20,50,80)) + scale_x_continuous(breaks=c(20,50,80),limits=c(0,100)) +
  guides(colour = guide_legend(title.position = "top"))


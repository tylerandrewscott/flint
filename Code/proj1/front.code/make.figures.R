Y = seq(0,100,0.01)
X = seq(0,100,0.01)

library(ggplot2)


ggplot() + geom_line(aes(x=X,y=-sqrt(Y)),lwd=2) + theme_bw() +
  scale_y_continuous(expand=c(0,0),breaks=-c(07.5,05.0,2.5),labels=c('Low','Medium','High')) + 
  scale_x_continuous(expand=c(0,0),breaks=c(025,050,75),labels=c('Low','Medium','High')) +
  ylab('Special District Autonomy') + xlab('Financial/Political Dependence on General Purpose Government') +
  theme(axis.ticks=element_blank(),axis.title=element_text(size=18),axis.text=element_text(size=16))


-sqrt(Y)

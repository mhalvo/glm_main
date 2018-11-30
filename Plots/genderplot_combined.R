setwd("~/Downloads")
plotmales<-readRDS("plotmale.rds")
plotfemales<-readRDS("plotfemale.rds")

require(ggplot2)

dfmale<-plotmales$dfplot
dfmale$sex<-"Male"
dffemale<-plotfemales$dfplot
dffemale$sex<-"Female"
dfplot0<-rbind(dfmale,dffemale)

dfplot<-dfplot0[which(dfplot$part=="count"),]

pointsmale<-plotmales$plotpoints
pointsmale$sex<-"Male"
pointsfemale<-plotfemales$plotpoints
pointsfemale$sex<-"Female"

dfpoints<-rbind(pointsmale,pointsfemale)


(plotcombined<-ggplot() +
  geom_ribbon(data=dfplot, aes(x=focal.seq, y=pe, ymin = lower, ymax = upper,fill=sex), alpha = .25) +
  geom_smooth(data = dfplot, se=F,aes(focal.seq, pe, by=sex, color=sex), size=1) +
  geom_point(data=dfpoints,aes(x=pred,y=y,shape=sex, color=sex),alpha=.5) +
  
  #thematic specifications of my graphic
  theme(text=element_text(family="Helvetica",size=10, color="black"),
        legend.position="none",
        panel.background = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(colour="black"),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(colour="black"),
        axis.title.y=element_text(size=14),
        axis.line = element_line(size=.4),
        panel.grid.major = element_blank(),
        plot.background=element_rect(fill='white'))
)

ggsave("plotcombined.pdf",plotcombined,width = 5, height = 5, units = "in")
              
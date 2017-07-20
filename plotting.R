#d=read.csv('pixcount.csv')
library(ggplot2)
library(gsheet)

setwd('../turtleAnt_dataAnalysis/')
urlList=c('https://docs.google.com/spreadsheets/d/1cvECaZLMiSOCoJlqv3W8xpVNIWY5Nf5zsw8b7khrvSA/edit#gid=1236070938',
          'https://docs.google.com/spreadsheets/d/1cvECaZLMiSOCoJlqv3W8xpVNIWY5Nf5zsw8b7khrvSA/edit#gid=943324237',
          'https://docs.google.com/spreadsheets/d/1cvECaZLMiSOCoJlqv3W8xpVNIWY5Nf5zsw8b7khrvSA/edit#gid=1576199558',
          'https://docs.google.com/spreadsheets/d/1cvECaZLMiSOCoJlqv3W8xpVNIWY5Nf5zsw8b7khrvSA/edit#gid=0',
          'https://docs.google.com/spreadsheets/d/1cvECaZLMiSOCoJlqv3W8xpVNIWY5Nf5zsw8b7khrvSA/edit#gid=922087983',
          'https://docs.google.com/spreadsheets/d/1cvECaZLMiSOCoJlqv3W8xpVNIWY5Nf5zsw8b7khrvSA/edit#gid=1728631886')

colData=data.frame()
colonies=c('T1','T2','T3','V1','V2','V3')
for(i in 1:length(urlList)){
  coli=read.csv(text=gsheet2text(urlList[i], format='csv'))[,1:8]
  coli$colony=colonies[i]
  coli$Number.Workers=as.numeric(as.character(coli$Number.Workers))
  print(names(coli))
  colData=rbind(colData,coli)
}
colData=colData[colData$Box!='O',]
#colDataSplit=split(colData, colData$colony)
dim(colData)
colData=na.omit(colData)

splitted=t(matrix(unlist(strsplit(as.character(colData$Date),'/')),nrow=3))
colData$Date=sprintf(paste(paste0('0',splitted[,1]),"%02d",paste0('20',splitted[,3]), sep='/'), as.numeric(splitted[,2]))


time=paste(colData$Date, colData$Time)
colData$Posix=as.POSIXct(time , format = "%m/%d/%Y %I:%M %p")

colData$PosixTime=as.POSIXct(colData$Time , format = "%I:%M %p")

summary(colData)
colData$Number.Workers=as.numeric(as.character(colData$Number.Workers))

for(i in 1:nrow(colData)){
  colData$Brood[i]=(colData$Number.Larva[i]!=0|colData$Number.Eggs[i]!=0)
}

install.packages('gridGraphics')
install.packages('png')

library(gridGraphics)
library(png)

system('mkdir finalPoster')
for(i in colonies){
  png(paste0('finalPoster/line_workers_col',i,'.png'))
  p=ggplot(data=colData[colData$colony==i,], aes(x=Posix, y=Number.Workers, group=Box, color=Box, shape=Brood, size=Brood))+
    geom_point()+
    geom_line(size=1.3)+
    scale_shape_manual(values=c(16, 18))+
    scale_size_manual(values=c(1,4.5))+
    labs(x='Time')+
    scale_colour_manual(values=c( 'dodgerblue2','deepskyblue1','dodgerblue4', 'firebrick3', 'firebrick1', 'firebrick4'))+
    theme_classic()+
    theme(axis.title=element_text(size=13),axis.text=element_text(size=12, angle=45, hjust=1), legend.title=element_text(size=14),legend.text=element_text(size=12))
  #geom_bar(stat='identity')
  plot(p)
  graphics.off()
}
# 
# 
# ggplot(data=colData, aes(x=Posix, y=Number.Workers, group=Box, color=Box))+
#   geom_point()+
#   geom_line()+
#   scale_color_brewer(palette='RdBu')+
#   facet_grid(colony~.)+
#   theme_classic()

for(i in colonies){
  png(paste0('7-12-17/bar_workers_col',i,'.png'))
  p=ggplot(data=colData[colData$colony==i,], aes(x=Posix, y=Number.Workers, group=Box, fill=Box))+
    geom_bar(stat='identity')+
    scale_fill_manual(values=c( 'dodgerblue2','deepskyblue1','dodgerblue4', 'firebrick3', 'firebrick1', 'firebrick4'))+
    theme_classic()+
    labs(x='Time')+
    theme(axis.title=element_text(size=13),axis.text=element_text(size=12, angle=45, hjust=1), legend.title=element_text(size=14),legend.text=element_text(size=12))
    plot(p)
  graphics.off()
}

head(colData)



colData$DR=as.factor(t(matrix(unlist(strsplit(as.character(colData$Box), '')),nrow=2))[,1])
png('finalPoster/final_counts.png')
ggplot(colData[colData$Time=='8:00 PM',], aes(y=Number.Workers, x=as.factor(colony), fill=DR))+
  geom_bar(stat='identity', position='dodge')+
  scale_fill_manual(values=c('dodgerblue2','firebrick'),name='Section')+
  labs(x='Colony')+
  theme_classic()+
  theme(axis.title=element_text(size=13),axis.text=element_text(size=12, angle=45, hjust=1), legend.title=element_text(size=14),legend.text=element_text(size=12))
graphics.off()




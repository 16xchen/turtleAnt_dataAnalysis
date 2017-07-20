urls=c(
  #'https://docs.google.com/spreadsheets/d/1OCKoUgQ_A-do18Uc_ulIzoQ_P22nb8lim9wBlwROR4c/edit#gid=1849028624',
  'https://docs.google.com/spreadsheets/d/1OCKoUgQ_A-do18Uc_ulIzoQ_P22nb8lim9wBlwROR4c/edit#gid=0',
  'https://docs.google.com/spreadsheets/d/1OCKoUgQ_A-do18Uc_ulIzoQ_P22nb8lim9wBlwROR4c/edit#gid=433819656',
  'https://docs.google.com/spreadsheets/d/1OCKoUgQ_A-do18Uc_ulIzoQ_P22nb8lim9wBlwROR4c/edit#gid=1138436910')

colData=data.frame()
colonies=c('V1','V2','V3')
for(i in 1:length(urls)){
  coli=read.csv(text=gsheet2text(urls[i], format='csv'))[-1,1:14]
  coli$colony=colonies[i]
  print(names(coli))
  colData=rbind(colData,coli)
}

summary(colData)
dim(colData)

colData=na.omit(colData)

colData$colony


#splitted=t(matrix(unlist(strsplit(as.character(colData$Date),'/')),nrow=3))
#colData$Date=sprintf(paste(paste0('0',splitted[,1]),"%02d",paste0('20',splitted[,3]), sep='/'), as.numeric(splitted[,2]))
#time=paste(colData$Date, colData$Time)
colData$Posix=as.POSIXct(colData$Time , format = "%I:%M:%S")

colData$Enter.Box=as.numeric(as.character(colData$Enter.Box))
colData$Box=as.factor(colData$Box)
colData$colony=as.factor(colData$colony)

names(colData)
colData$Nest
colData=na.omit(colData)

head(colData)



system('mkdir 7-17-17')
for(i in colonies){
  png(paste0('7-17-17/enterNest_col',i,'png'))
p=ggplot(colData[c(colData$colony==i&colData$Nest!=colData$Nest[1]),], aes(y=Enter.Nest, x=Posix, fill=Nest, color=Nest))+
  #geom_bar(stat='identity')+
  geom_line(size=1.5)+
  labs(x='Time')+
  scale_colour_manual(values=c( 'dodgerblue2','deepskyblue1','dodgerblue4', 'firebrick3', 'firebrick1', 'firebrick4'))+
  theme(axis.title=element_text(size=14), legend.title=element_text(size=14),legend.text=element_text(size=12))+
  theme_classic()
plot(p)
graphics.off()
}



BoxNest=as.factor(paste(colData$Box, colData$Nest))

for(i in colonies){
  png(paste0('7-17-17/enterBox_col',i,'png'))
p=ggplot(colData[c(colData$colony==i),], aes(y=Enter.Box, x=Posix, fill=Box, color=Box))+
  #geom_bar(stat='identity')+
  scale_colour_manual(values=c('gray', 'gray','deepskyblue1','gray', 'dodgerblue2','dodgerblue4', 'gray', 'firebrick1','gray','firebrick3','gray', 'firebrick4'),
                      labels=c('1','2','3 (R2)','4','5 (R1)','6 (R3)','7','8 (D2)','9','10 (D1)','11','12 (D3)'))+
  #scale_fill_manual(values=c('gray', 'gray','deepskyblue1','gray', 'dodgerblue2','dodgerblue4', 'gray', 'firebrick1','gray','firebrick3','gray', 'firebrick4'))+
  geom_line(size=1.5)+
  labs(x='Time')+
  theme_classic()
plot(p)
graphics.off()
}

library(reshape2)
colData$Enter.Nest=as.numeric(as.character(colData$Enter.Nest))
colData$Exit.Nest=as.numeric(as.character(colData$Exit.Nest))


colData_s=colData[,c('Posix','Nest','Enter.Nest','Exit.Nest','colony')]
colData_s=colData_s[colData_s$Nest!=colData_s$Nest[1],]
colData_s$Enter.Nest=as.numeric(as.character(colData_s$Enter.Nest))
colData_s$Exit.Nest=as.numeric(as.character(colData_s$Exit.Nest))
colData_s$colony=as.factor(colData_s$colony)

sumdf=cbind(melt(acast(colData_s[,c(2,5,3)], Nest~colony, sum)),
      melt(acast(colData_s[,c(2,5,4)], Nest~colony, sum))$value)

names(sumdf)=c('Nest','Colony','Enter','Exit')

sumdf=melt(sumdf, id.vars=c('Nest','Colony'))

png('finalPoster//leaveNestBar.png')
ggplot(sumdf, aes(x=Nest, y=value,fill=variable, group=variable))+
  geom_bar(stat='identity', position='dodge')+
  scale_fill_manual(values=c('black','grey'))+
  facet_wrap(~Colony)+
  labs(y='# of Ants')+
  theme_classic()
graphics.off()

###


r=c(2,3,5,6)
d=c(7,8,9,10,11,12)

for(i in 1:nrow(colData)){
  if(colData$Box[i]%in%r){
    colData$Section[i]='R'
  }
  else if(colData$Box[i]%in%d){
    colData$Section[i]='D'
  }
}
colData_sec=colData[!is.na(colData$Section),]






colData_s=colData_sec[,c('Posix','Section','Enter.Box','Exit.Box','colony')]
#colData_s=colData_s[colData_s$Box!=colData_s$Box[1],]
colData_s$Enter.Box=as.numeric(as.character(colData_s$Enter.Box))
colData_s$Exit.Box=as.numeric(as.character(colData_s$Exit.Box))
colData_s$colony=as.factor(colData_s$colony)

sumdf=cbind(melt(acast(colData_s[,c(2,5,3)], Section~colony, sum)),
            melt(acast(colData_s[,c(2,5,4)], Section~colony, sum))$value)

names(sumdf)=c('Section','Colony','Enter','Exit')

sumdf=melt(sumdf, id.vars=c('Section','Colony'))


png('finalPoster/leaveBoxBar.png')
ggplot(sumdf, aes(x=Section, y=value,fill=Section, group=variable, alpha=variable))+
  geom_bar(stat='identity', position='dodge')+
  scale_alpha_discrete(range = c(0.7, 1))+
  scale_fill_manual(values=c('dodgerblue2','firebrick'),name=c('Section','Activity'))+
  facet_wrap(~Colony)+
  labs(y='Ant Activity',x='Box #')+
  theme_classic()+
  theme(title=element_text(size=14, face='bold'),axis.title=element_text(size=13), legend.title=element_text(size=13),legend.text=element_text(size=12))+
graphics.off()




colData_s=colData_sec[,c('Posix','Section','Enter.Box','Exit.Box','colony')]
#colData_s=colData_s[colData_s$Box!=colData_s$Box[1],]
colData_s$Enter.Box=as.numeric(as.character(colData_s$Enter.Box))
colData_s$Exit.Box=as.numeric(as.character(colData_s$Exit.Box))
colData_s$colony=as.factor(colData_s$colony)

colData_split=split(colData_s, colData_s$colony)

cols=c('V1','V2','V3')
fullSum=data.frame()
for(i in 1:length(colData_split)){
sumdf=cbind(melt(acast(colData_split[[i]][,c(1,2,3)], Section~Posix, sum)),
            melt(acast(colData_split[[i]][,c(1,2,4)], Section~Posix, sum))$value)
sumdf$Colony=cols[i]
names(sumdf)=c('Section','Time','Exit','Enter','Colony')
fullSum=rbind(fullSum,sumdf)
}

fullSum=melt(fullSum, id.vars=c('Section','Colony','Time'))

head(fullSum)
fullSum$Time=as.POSIXct(fullSum$Time)


png('finalPoster/activity_ct.png')
ggplot(fullSum,aes(y=value, x=Time, color=Section, group=Section))+
  geom_line(size=1.3)+
  geom_point()+
  facet_wrap(~Colony)+
  scale_color_manual(values=c('dodgerblue2','firebrick'),name=c('Section'))+
  theme(title=element_text(size=14, face='bold'),axis.title=element_text(size=13), 
        axis.text=element_text(size=12, angle=45, hjust=1),
        legend.title=element_text(size=13),legend.text=element_text(size=12))+
  labs(y='Ant Activity (# of entries and exits')+
  theme_classic()
graphics.off()
##



colData=data.frame()
colonies=c(4,5,6)
for(i in 1:length(urls)){
  coli=read.csv(text=gsheet2text(urls[i], format='csv'))[-1,c(2,5,16:17)]
  coli$colony=colonies[i]
  print(names(coli))
  colData=rbind(colData,coli)
}

summary(colData)
dim(colData)

colDataOR=na.omit(colData[,-3])
colDataOD=na.omit(colData[,-4])

colData$Posix=as.POSIXct(colData$Time , format = "%I:%M:%S")

#colData$Enter.Box=as.numeric(as.character(colData$Enter.Box))
#colData$Box=as.factor(colData$Box)
colData$colony=as.factor(colData$colony)

dts=na.omit(melt(colData[,-c(1:2)], id.vars=c("Posix",'colony')))

head(dts)
ggplot(dts, aes(x=Posix,y=value, color=as.factor(variable)))+
  geom_point()+
  geom_line()+
  facet_wrap(~colony)+
  scale_color_manual(values=c('dodgerblue2','firebrick'),name=c('Section'))+
  theme(title=element_text(size=14, face='bold'),axis.title=element_text(size=13), legend.title=element_text(size=13),legend.text=element_text(size=12))+
  theme_classic()
  








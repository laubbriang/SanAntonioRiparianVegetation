##load in packages  
library(vegan)

##NMDS community analysis
#canopy cover
can.relative.abundance<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(can.relative.abundance)

can.prop.mtx<-data.frame(can.relative.abundance[,3:43], row.names = creek.name ) 
can.prop.mtx #this call up the matrix for viewing 

can.nmds<-metaMDS(can.prop.mtx, distance ='bray',autotransform = FALSE) 
can.nmds.score<-scores(can.nmds, display = 'sites') 
can.nmds #shows results 
ordiplot(can.nmds.score,type="text")

#basal area
ba.relative.abundance<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(ba.relative.abundance)

ba.prop.mtx<-data.frame(ba.relative.abundance[,3:39], row.names = creek.name ) 
ba.prop.mtx #this call up the matrix for viewing 

ba.nmds<-metaMDS(ba.prop.mtx, distance ='bray',autotransform = FALSE) 
ba.nmds.score<-scores(ba.nmds, display = 'sites') 
ba.nmds #shows results 
ordiplot(ba.nmds.score,type="text")

#stem count
stem.relative.abundance<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(stem.relative.abundance)

stem.species.mtx<-data.frame(stem.relative.abundance[,3:39], row.names = creek.name ) 
stem.species.mtx #this call up the matrix for viewing 

stem.nmds<-metaMDS(stem.species.mtx, distance ='bray',autotransform = FALSE) 
stem.nmds.score<-scores(stem.nmds, display = 'sites') 
stem.nmds #shows results 
ordiplot(stem.nmds.score,type="text")

##Environmental fitting
land.use.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') #load in data called: land use and water part 2, this has the updated metrics from Dr. Laub
attach(land.use.data)
ag.cover<-land.use.data[,33]+land.use.data[,34]
land.use.mtx<-data.frame(cbind(ag.cover,land.use.data[,c(4,26,31,35,39,40)]),row.names=Site.Name)

#canopy cover
env.fit.can<-envfit(can.nmds,land.use.mtx) #name object for envfit 
env.fit.can #shows envfit results
can.envfit.scores<-scores(env.fit.can,display="vectors")

#basal area
env.fit.ba<-envfit(ba.nmds,land.use.mtx) #name object for envfit 
env.fit.ba #shows envfit results 
ba.envfit.scores<-scores(env.fit.ba,display="vectors")

#stem count
env.fit.stem<-envfit(stem.nmds,land.use.mtx) #name object for envfit 
env.fit.stem#shows envfit results, no significance 
stem.envfit.scores<-scores(env.fit.stem,display="vectors")

##Environmental fitting of trait variables
#Canopy cover
can.trait.by.site<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(can.trait.by.site)

can.trait.mtx<-data.frame(can.trait.by.site[,3:6],row.names=Site)

env.fit.can.trait<-envfit(can.nmds,can.trait.mtx) #name object for envfit 
env.fit.can.trait #shows envfit results 
can.trait.envfit.scores<-scores(env.fit.can.trait,display="vectors")

#Basal Area
ba.trait.by.site<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(ba.trait.by.site)

ba.trait.mtx<-data.frame(ba.trait.by.site[,3:6],row.names=Site)

env.fit.ba.trait<-envfit(ba.nmds,ba.trait.mtx) #name object for envfit 
env.fit.ba.trait #shows envfit results
ba.trait.envfit.scores<-scores(env.fit.ba.trait,display="vectors")

#Stem Count
stem.trait.by.site<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(stem.trait.by.site)

stem.trait.mtx<-data.frame(stem.trait.by.site[,3:6],row.names=Site)

env.fit.stem.trait<-envfit(stem.nmds,stem.trait.mtx) #name object for envfit 
env.fit.stem.trait #shows envfit results 
stem.trait.envfit.scores<-scores(env.fit.stem.trait,display="vectors")

##Plot creation of NMDS results with environmental and trait variables
#Canopy cover
par(las=1)
plot(can.nmds,type = 'n',xlim=c(-1,1), ylim=c(-1,1),main="Canopy Cover") #plain plot ready for the listed data 
points(x=can.nmds.score[1,1],y=can.nmds.score[1,2],pch=16,col="gold",cex=1.5)
points(x=can.nmds.score[2,1],y=can.nmds.score[2,2],pch=16,col="lightgreen",cex=1.5)
points(x=can.nmds.score[3,1],y=can.nmds.score[3,2],pch=16,col="darkred",cex=1.5)
points(x=can.nmds.score[4,1],y=can.nmds.score[4,2],pch=16,col="red",cex=1.5)
points(x=can.nmds.score[5,1],y=can.nmds.score[5,2],pch=16,col="forestgreen",cex=1.5)
points(x=can.nmds.score[6,1],y=can.nmds.score[6,2],pch=16,col="darkgreen",cex=1.5)
points(x=can.nmds.score[7,1],y=can.nmds.score[7,2],pch=16,col="darkorange",cex=1.5)
points(x=can.nmds.score[8,1],y=can.nmds.score[8,2],pch=16,col="gold",cex=1.5)
arrows(x0=0,y0=0,x1=can.envfit.scores[3,1], y1=can.envfit.scores[3,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[3,1], y=can.envfit.scores[3,2],col = 'grey0', labels = 'Percent Impervious',pos=4,font=3)
arrows(x0=0,y0=0,x1=can.envfit.scores[6,1], y1=can.envfit.scores[6,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[6,1], y=can.envfit.scores[6,2],col = 'grey0', labels = 'Floods per Year',pos=1,font=3)
arrows(x0=0,y0=0,x1=can.envfit.scores[7,1], y1=can.envfit.scores[7,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[7,1], y=can.envfit.scores[7,2],col = 'grey0', labels = 'Time Between Floods',pos=4,font=3)
text(x=0.9,y=1,labels = 'Stress = 0.059',font=3)
arrows(x0=0,y0=0,x1=can.trait.envfit.scores[1,1], y1=can.trait.envfit.scores[1,2],lwd=2,col = 'grey0')
text(x=can.trait.envfit.scores[1,1], y=can.trait.envfit.scores[1,2],col = 'grey0', labels = 'Drought Tolerance',pos=1,font=3)
arrows(x0=0,y0=0,x1=can.trait.envfit.scores[3,1], y1=can.trait.envfit.scores[3,2],lwd=2,col = 'grey0')
text(x=can.trait.envfit.scores[3,1], y=can.trait.envfit.scores[3,2],col = 'grey0', labels = 'Water Use',pos=3,font=3)
legend("topleft",legend=c("Gov Canyon Trib (0.0%)","Gov Canyon (1.0%)","Salado (12.1%)","Maverick (18.1%)","Leon (18.1%)","Huesta (19.4%)","French (41.2%)","Leon Trib (45.2%)"),col=c("dark green","forest green","light green","gold","gold","dark orange","red","dark red"),pch=16,pt.cex=1.25,bty="n")

#Basal area
par(las=1)
plot(ba.nmds.score[,1],ba.nmds.score[,2],type = 'n',xlim=c(-0.6,0.6), ylim=c(-1,1),xlab="NMDS1",ylab="NMDS2",main="Basal Area") #plain plot ready for the listed data 
points(x=ba.nmds.score[1,1],y=ba.nmds.score[1,2],pch=16,col="gold",cex=1.5)
points(x=ba.nmds.score[2,1],y=ba.nmds.score[2,2],pch=16,col="lightgreen",cex=1.5)
points(x=ba.nmds.score[3,1],y=ba.nmds.score[3,2],pch=16,col="darkred",cex=1.5)
points(x=ba.nmds.score[4,1],y=ba.nmds.score[4,2],pch=16,col="red",cex=1.5)
points(x=ba.nmds.score[5,1],y=ba.nmds.score[5,2],pch=16,col="forestgreen",cex=1.5)
points(x=ba.nmds.score[6,1],y=ba.nmds.score[6,2],pch=16,col="darkgreen",cex=1.5)
points(x=ba.nmds.score[7,1],y=ba.nmds.score[7,2],pch=16,col="darkorange",cex=1.5)
points(x=ba.nmds.score[8,1],y=ba.nmds.score[8,2],pch=16,col="gold",cex=1.5)
arrows(x0=0,y0=0,x1=ba.envfit.scores[3,1], y1=ba.envfit.scores[3,2],lwd=2,col = 'grey0')
text(x=ba.envfit.scores[3,1], y=ba.envfit.scores[3,2]-0.05,col = 'grey0', labels = 'Percent Impervious',font=3)
text(x=0.5,y=1,labels = 'Stress = 0.054',font=3)
arrows(x0=0,y0=0,x1=ba.trait.envfit.scores[1,1], y1=ba.trait.envfit.scores[1,2],lwd=2,col = 'grey0')
text(x=ba.trait.envfit.scores[1,1], y=ba.trait.envfit.scores[1,2],col = 'grey0', labels = 'Drought Tolerance',pos=2,font=3)
arrows(x0=0,y0=0,x1=ba.trait.envfit.scores[2,1], y1=ba.trait.envfit.scores[2,2],lwd=2,col = 'grey0')
text(x=ba.trait.envfit.scores[2,1], y=ba.trait.envfit.scores[2,2],col = 'grey0', labels = 'Heat Tolerance',pos=1,font=3)
legend("topleft",legend=c("Gov Canyon Trib (0.0%)","Gov Canyon (1.0%)","Salado (12.1%)","Maverick (18.1%)","Leon (18.1%)","Huesta (19.4%)","French (41.2%)","Leon Trib (45.2%)"),col=c("dark green","forest green","light green","gold","gold","dark orange","red","dark red"),pch=16,pt.cex=1.25,bty="n")

#Stem count 
par(las=1)
plot(stem.nmds,type = 'n',xlim=c(-0.4,0.4), ylim=c(-0.4,0.4),main="Stem Count") #plain plot ready for the listed data 
points(x=stem.nmds.score[1,1],y=stem.nmds.score[1,2],pch=16,col="gold",cex=1.5)
points(x=stem.nmds.score[2,1],y=stem.nmds.score[2,2],pch=16,col="lightgreen",cex=1.5)
points(x=stem.nmds.score[3,1],y=stem.nmds.score[3,2],pch=16,col="darkred",cex=1.5)
points(x=stem.nmds.score[4,1],y=stem.nmds.score[4,2],pch=16,col="red",cex=1.5)
points(x=stem.nmds.score[5,1],y=stem.nmds.score[5,2],pch=16,col="forestgreen",cex=1.5)
points(x=stem.nmds.score[6,1],y=stem.nmds.score[6,2],pch=16,col="darkgreen",cex=1.5)
points(x=stem.nmds.score[7,1],y=stem.nmds.score[7,2],pch=16,col="darkorange",cex=1.5)
points(x=stem.nmds.score[8,1],y=stem.nmds.score[8,2],pch=16,col="gold",cex=1.5)
text(x=0.37,y=0.4,labels = 'Stress = 0.056',font=3)
legend("topleft",legend=c("Gov Canyon Trib (0.0%)","Gov Canyon (1.0%)","Salado (12.1%)","Maverick (18.1%)","Leon (18.1%)","Huesta (19.4%)","French (41.2%)","Leon Trib (45.2%)"),col=c("dark green","forest green","light green","gold","gold","dark orange","red","dark red"),pch=16,pt.cex=1.25,bty="n")

##Trait analyses (sig. relationships have r2 and p-value listed)
#Basic regressions with impervious surface cover
trait.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(trait.data)

#canopy cover
imp.v.native.can<-lm(native.can~PctImpervious2019Ws)
summary(imp.v.native.can)

imp.v.droughttoleranceveryhigh.can<-lm(dt.vh.can~PctImpervious2019Ws)
summary(imp.v.droughttoleranceveryhigh.can)

avgdurationdry.v.droughttoleranceveryhigh.can<-lm(dt.vh.can~avgdurationdry)
summary(avgdurationdry.v.droughttoleranceveryhigh.can)

eventsperyear.v.droughttoleranceveryhigh.can<-lm(dt.vh.can~eventsperyear)
summary(eventsperyear.v.droughttoleranceveryhigh.can)

imp.v.droughttolerancehigh.can<-lm(dt.h.can~PctImpervious2019Ws)
summary(imp.v.droughttolerancehigh.can)

avgdurationdry.v.droughttolerancehigh.can<-lm(dt.h.can~avgdurationdry)
summary(avgdurationdry.v.droughttolerancehigh.can)

eventsperyear.v.droughttolerancehigh.can<-lm(dt.h.can~eventsperyear)
summary(eventsperyear.v.droughttolerancehigh.can)

imp.v.droughttolerancemediumhigh.can<-lm(dt.mh.can~PctImpervious2019Ws)
summary(imp.v.droughttolerancemediumhigh.can)

imp.v.droughttolerancemedium.can<-lm(dt.m.can~PctImpervious2019Ws)
summary(imp.v.droughttolerancemedium.can)
	#close: r2 = 0.3921, p = 0.0967

imp.v.droughttolerancelow.can<-lm(dt.l.can~PctImpervious2019Ws)
summary(imp.v.droughttolerancelow.can)

imp.v.droughttoleranceoverall.can<-lm(dt.overall.can~PctImpervious2019Ws)
summary(imp.v.droughttoleranceoverall.can)

imp.v.heattoleranceveryhigh.can<-lm(ht.vh.can~PctImpervious2019Ws)
summary(imp.v.heattoleranceveryhigh.can)

imp.v.heattolerancehigh.can<-lm(ht.h.can~PctImpervious2019Ws)
summary(imp.v.heattolerancehigh.can)

imp.v.heattolerancemediumhigh.can<-lm(ht.mh.can~PctImpervious2019Ws)
summary(imp.v.heattolerancemediumhigh.can)

imp.v.heattolerancemedium.can<-lm(ht.m.can~PctImpervious2019Ws)
summary(imp.v.heattolerancemedium.can)

imp.v.heattolerancelow.can<-lm(ht.l.can~PctImpervious2019Ws)
summary(imp.v.heattolerancelow.can)

imp.v.wateruselowhigh.can<-lm(wu.lh.can~PctImpervious2019Ws)
summary(imp.v.wateruselowhigh.can)

imp.v.wateruselowmedhigh.can<-lm(wu.lmh.can~PctImpervious2019Ws)
summary(imp.v.wateruselowmedhigh.can)

imp.v.waterusehigh.can<-lm(wu.h.can~PctImpervious2019Ws)
summary(imp.v.waterusehigh.can)
	
imp.v.waterusemed.can<-lm(wu.m.can~PctImpervious2019Ws)
summary(imp.v.waterusemed.can)

imp.v.wateruselowmed.can<-lm(wu.lm.can~PctImpervious2019Ws)
summary(imp.v.wateruselowmed.can)
	#close: r2 = 0.3864, p = 0.0999

imp.v.wateruselow.can<-lm(wu.l.can~PctImpervious2019Ws)
summary(imp.v.wateruselow.can)

#basal area
imp.v.native.ba<-lm(native.ba~PctImpervious2019Ws)
summary(imp.v.native.ba)

imp.v.droughttoleranceveryhigh.ba<-lm(dt.vh.ba~PctImpervious2019Ws)
summary(imp.v.droughttoleranceveryhigh.ba)

imp.v.droughttolerancehigh.ba<-lm(dt.h.ba~PctImpervious2019Ws)
summary(imp.v.droughttolerancehigh.ba)

imp.v.droughttolerancemediumhigh.ba<-lm(dt.mh.ba~PctImpervious2019Ws)
summary(imp.v.droughttolerancemediumhigh.ba)

imp.v.droughttolerancemedium.ba<-lm(dt.m.ba~PctImpervious2019Ws)
summary(imp.v.droughttolerancemedium.ba)
	#close: r2 = 0.4712, p = 0.06

imp.v.droughttolerancelow.ba<-lm(dt.l.ba~PctImpervious2019Ws)
summary(imp.v.droughttolerancelow.ba)

imp.v.droughttoleranceoverall.ba<-lm(dt.overall.ba~PctImpervious2019Ws)
summary(imp.v.droughttoleranceoverall.ba)

imp.v.heattoleranceveryhigh.ba<-lm(ht.vh.ba~PctImpervious2019Ws)
summary(imp.v.heattoleranceveryhigh.ba)

imp.v.heattolerancehigh.ba<-lm(ht.h.ba~PctImpervious2019Ws)
summary(imp.v.heattolerancehigh.ba)

imp.v.heattolerancemediumhigh.ba<-lm(ht.mh.ba~PctImpervious2019Ws)
summary(imp.v.heattolerancemediumhigh.ba)

imp.v.heattolerancemedium.ba<-lm(ht.m.ba~PctImpervious2019Ws)
summary(imp.v.heattolerancemedium.ba)

imp.v.heattolerancelow.ba<-lm(ht.l.ba~PctImpervious2019Ws)
summary(imp.v.heattolerancelow.ba)

imp.v.wateruselowhigh.ba<-lm(wu.lh.ba~PctImpervious2019Ws)
summary(imp.v.wateruselowhigh.ba)

imp.v.wateruselowmedhigh.ba<-lm(wu.lmh.ba~PctImpervious2019Ws)
summary(imp.v.wateruselowmedhigh.ba)

imp.v.waterusehigh.ba<-lm(wu.h.ba~PctImpervious2019Ws)
summary(imp.v.waterusehigh.ba)
	
imp.v.waterusemed.ba<-lm(wu.m.ba~PctImpervious2019Ws)
summary(imp.v.waterusemed.ba)

imp.v.wateruselowmed.ba<-lm(wu.lm.ba~PctImpervious2019Ws)
summary(imp.v.wateruselowmed.ba)
	#sig: r2 = 0.5257, p = 0.04185
	#relationship negative (-0.004213)

imp.v.wateruselow.ba<-lm(wu.l.ba~PctImpervious2019Ws)
summary(imp.v.wateruselow.ba)
	#sig: r2 = 0.551, p = 0.03495
	#relationship positive (0.005241)

#stem counts
imp.v.native.stem<-lm(native.stem~PctImpervious2019Ws)
summary(imp.v.native.stem)

imp.v.droughttoleranceveryhigh.stem<-lm(dt.vh.stem~PctImpervious2019Ws)
summary(imp.v.droughttoleranceveryhigh.stem)

imp.v.droughttolerancehigh.stem<-lm(dt.h.stem~PctImpervious2019Ws)
summary(imp.v.droughttolerancehigh.stem)

imp.v.droughttolerancemediumhigh.stem<-lm(dt.mh.stem~PctImpervious2019Ws)
summary(imp.v.droughttolerancemediumhigh.stem)

imp.v.droughttolerancemedium.stem<-lm(dt.m.stem~PctImpervious2019Ws)
summary(imp.v.droughttolerancemedium.stem)

imp.v.droughttolerancelow.stem<-lm(dt.l.stem~PctImpervious2019Ws)
summary(imp.v.droughttolerancelow.stem)

imp.v.droughttoleranceoverall.stem<-lm(dt.overall.stem~PctImpervious2019Ws)
summary(imp.v.droughttoleranceoverall.stem)

imp.v.heattoleranceveryhigh.stem<-lm(ht.vh.stem~PctImpervious2019Ws)
summary(imp.v.heattoleranceveryhigh.stem)

imp.v.heattolerancehigh.stem<-lm(ht.h.stem~PctImpervious2019Ws)
summary(imp.v.heattolerancehigh.stem)

imp.v.heattolerancemediumhigh.stem<-lm(ht.mh.stem~PctImpervious2019Ws)
summary(imp.v.heattolerancemediumhigh.stem)

imp.v.heattolerancemedium.stem<-lm(ht.m.stem~PctImpervious2019Ws)
summary(imp.v.heattolerancemedium.stem)

imp.v.heattolerancelow.stem<-lm(ht.l.stem~PctImpervious2019Ws)
summary(imp.v.heattolerancelow.stem)

imp.v.wateruselowhigh.stem<-lm(wu.lh.stem~PctImpervious2019Ws)
summary(imp.v.wateruselowhigh.stem)

imp.v.wateruselowmedhigh.stem<-lm(wu.lmh.stem~PctImpervious2019Ws)
summary(imp.v.wateruselowmedhigh.stem)

imp.v.waterusehigh.stem<-lm(wu.h.stem~PctImpervious2019Ws)
summary(imp.v.waterusehigh.stem)
	
imp.v.waterusemed.stem<-lm(wu.m.stem~PctImpervious2019Ws)
summary(imp.v.waterusemed.stem)

imp.v.wateruselowmed.stem<-lm(wu.lm.stem~PctImpervious2019Ws)
summary(imp.v.wateruselowmed.stem)
	#sig: r2 = 0.5301, p = 0.04056
	#relationship negative (-0.0049)

imp.v.wateruselow.stem<-lm(wu.l.stem~PctImpervious2019Ws)
summary(imp.v.wateruselow.stem)
	#close: r2 = 0.4371, p = 0.07421
	#relationship positive (0.003398)

##Geomorphic surfaces analysis and plotting
#Canopy cover
can.geo.surface<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(can.geo.surface)

can.geo.mtx<-data.frame(can.geo.surface[,5:45],row.names=creek.sample)

can.geo.nmds<-metaMDS(can.geo.mtx, distance = 'bray',autotransform=FALSE)
can.geo.nmds.site.score<-scores(can.geo.nmds, display = 'sites')
can.geo.nmds

par(las=1)
plot(can.geo.nmds.site.score, type = 'n',xlim=c(-1,1),ylim=c(-1,1),main="Canopy Cover") #this plotting code makes a plot by geomorphic unit
points(can.geo.nmds.site.score[c(1,7,13,18,24,29,34,40),1], can.geo.nmds.site.score[c(1,7,13,18,24,29,34,40),2], pch=22,cex=2,bg="skyblue") #active channel
points(can.geo.nmds.site.score[c(4,10,15,21,26,31,37,43),1], can.geo.nmds.site.score[c(4,10,15,21,26,31,37,43),2], pch=23,cex=2,bg="gold") #active channel shelf
points(can.geo.nmds.site.score[c(3,9,20,36,42),1], can.geo.nmds.site.score[c(3,9,20,36,42),2], pch=24,cex=2,bg="orange") #secondary active channel
points(can.geo.nmds.site.score[c(5,11,16,22,27,32,38,44),1], can.geo.nmds.site.score[c(5,11,16,22,27,32,38,44),2], pch=21,cex=2,bg="forestgreen") #floodplain
points(can.geo.nmds.site.score[c(6,12,17,23,28,33,39,45),1], can.geo.nmds.site.score[c(6,12,17,23,28,33,39,45),2], pch=25,cex=2) #riparian terrace
points(can.geo.nmds.site.score[c(2,8,14,19,25,30,35,41),1], can.geo.nmds.site.score[c(2,8,14,19,25,30,35,41),2], pch=21,cex=2,bg="purple") #islands
text(0.65,1, labels="Stress = 0.2",pos=4,font=3)
legend(x='topleft', legend = c('Active Channel','Active Channel Shelf','Secondary Active Channel','Floodplain','Riparian Terrace','Island'),pch= c(22,23,24,21,25,21),pt.bg=c("skyblue","gold","orange","forestgreen","white","purple"),cex=1,bty="n",ncol=2,pt.cex=1.25)

par(las=1)
plot(can.geo.nmds.site.score, type = 'n',xlim=c(-1,1),ylim=c(-1,1),main="Canopy Cover") #this plotting code makes a plot by site
points(can.geo.nmds.site.score[1:6,1], can.geo.nmds.site.score[1:6,2],pch=15,cex=2,col="gold")
points(can.geo.nmds.site.score[7:12,1], can.geo.nmds.site.score[7:12,2],pch=15,cex=2,col="light green")
points(can.geo.nmds.site.score[13:17,1], can.geo.nmds.site.score[13:17,2], pch=17,cex=2,col="dark red")
points(can.geo.nmds.site.score[18:23,1], can.geo.nmds.site.score[18:23,2], pch=18,cex=2,col="red")
points(can.geo.nmds.site.score[24:28,1], can.geo.nmds.site.score[24:28,2], pch=19,cex=2,col="forest green")
points(can.geo.nmds.site.score[29:33,1], can.geo.nmds.site.score[29:33,2], pch=25,cex=2,col="dark green",bg="dark green")
points(can.geo.nmds.site.score[34:39,1], can.geo.nmds.site.score[34:39,2],pch=15,cex=2,col="dark orange")
points(can.geo.nmds.site.score[40:45,1], can.geo.nmds.site.score[40:45,2],pch=16,cex=2,col="gold")
text(0.65,1, labels="Stress = 0.2",pos=4,font=3)
legend(x='topleft', legend = c('Gov Canyon Trib','Gov Canyon','Salado','Maverick','Leon','Huesta','French','Leon Trib'),col = c("dark green","forest green","light green","gold","gold","dark orange","red","dark red"),pch= c(25,19,15,15,16,15,18,17),pt.bg="dark green",cex=1,pt.cex=1.25,ncol=2,bty="n")

#Canopy cover permanova analysis
#Both site and geomorphic surface
perm.result.can.both<-adonis2(can.geo.mtx~creek.name+geo.type, method = 'bray',by="margin")
perm.result.can.both

#Geomorphic surface only
perm.result.can.geoonly<-adonis2(can.geo.mtx~geo.type, method = 'bray')
perm.result.can.geoonly
	
#Basal Area
ba.geo.surface<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(ba.geo.surface)

ba.geo.mtx<-data.frame(ba.geo.surface[,5:41],row.names=creek.sample)

ba.geo.nmds<-metaMDS(ba.geo.mtx, distance = 'bray',autotransform=FALSE)
ba.geo.nmds.site.score<-scores(ba.geo.nmds, display = 'sites')
ba.geo.nmds

par(las=1)
plot(ba.geo.nmds.site.score, type = 'n',xlim=c(-1.5,2),ylim=c(-1.5,2),main="Basal Area") #this plotting code makes a plot by geomorphic unit
points(ba.geo.nmds.site.score[c(1,5,11,15,20,25,30,36),1], ba.geo.nmds.site.score[c(1,5,11,15,20,25,30,36),2], pch=22,cex=2,bg="skyblue") #Active Channel
points(ba.geo.nmds.site.score[c(2,8,12,17,22,27,33,38),1], ba.geo.nmds.site.score[c(2,8,12,17,22,27,33,38),2], pch=23,cex=2,bg="gold") #Active Channel Shelf
points(ba.geo.nmds.site.score[c(7,16,32),1], ba.geo.nmds.site.score[c(7,16,32),2], pch=24,cex=2,bg="orange") #Secondary Active Channel
points(ba.geo.nmds.site.score[c(3,9,13,18,23,28,34,39),1], ba.geo.nmds.site.score[c(3,9,13,18,23,28,34,39),2], pch=21,cex=2,bg="forestgreen") #Floodplain
points(ba.geo.nmds.site.score[c(4,10,14,19,24,29,35,40),1], ba.geo.nmds.site.score[c(4,10,14,19,24,29,35,40),2], pch=25,cex=2) #Riparian Terrace
points(ba.geo.nmds.site.score[c(6,21,26,31,37),1], ba.geo.nmds.site.score[c(6,21,26,31,37),2], pch=21,cex=2,bg="purple") #Island
text(1.3,2, labels="Stress = 0.17",pos=4,font=3)
legend(x='topright', legend = c('Active Channel','Active Channel Shelf','Secondary Active Channel','Floodplain','Riparian Terrace','Island'),pch= c(22,23,24,21,25,21),pt.bg=c("skyblue","gold","orange","forestgreen","white","purple"),cex=1)

par(las=1)
plot(ba.geo.nmds.site.score, type = 'n',xlim=c(-1.5,2),ylim=c(-1.5,2),main="Basal Area") #this plotting code makes a plot by site
points(ba.geo.nmds.site.score[1:4,1], ba.geo.nmds.site.score[1:4,2],pch=15,cex=2,col="gold")
points(ba.geo.nmds.site.score[5:10,1], ba.geo.nmds.site.score[5:10,2],pch=15,cex=2,col="light green")
points(ba.geo.nmds.site.score[11:14,1], ba.geo.nmds.site.score[11:14,2], pch=17,cex=2,col="dark red")
points(ba.geo.nmds.site.score[15:19,1], ba.geo.nmds.site.score[15:19,2], pch=18,cex=2,col="red")
points(ba.geo.nmds.site.score[20:24,1], ba.geo.nmds.site.score[20:24,2], pch=19,cex=2,col="forest green")
points(ba.geo.nmds.site.score[25:29,1], ba.geo.nmds.site.score[25:29,2], pch=25,cex=2,col="dark green",bg="darkgreen")
points(ba.geo.nmds.site.score[30:35,1], ba.geo.nmds.site.score[30:35,2],pch=15,cex=2,col="dark orange")
points(ba.geo.nmds.site.score[36:40,1], ba.geo.nmds.site.score[36:40,2],pch=16,cex=2,col="gold")
text(1.3,2, labels="Stress = 0.17",pos=4,font=3)
legend(x='topleft', legend = c('Gov Canyon Trib','Gov Canyon','Salado','Maverick','Leon','Huesta','French','Leon Trib'),col = c("dark green","forest green","light green","gold","gold","dark orange","red","dark red"),pch= c(25,19,15,15,16,15,18,17),pt.bg="darkgreen",cex=1,pt.cex=1.25,ncol=2,bty="n")

#Basal area permanova analysis
#Both site and geomorphic surface
perm.result.ba.both<-adonis2(ba.geo.mtx~creek.name+geo.type, method = 'bray',by="margin")
perm.result.ba.both

#Geomorphic surface only
perm.result.ba.geoonly<-adonis2(ba.geo.mtx~geo.type, method = 'bray')
perm.result.ba.geoonly

#Stem Count
stem.geo.surface<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(stem.geo.surface)

stem.geo.mtx<-data.frame(stem.geo.surface[,5:41],row.names=creek.sample)

stem.geo.nmds<-metaMDS(stem.geo.mtx, distance = 'bray',autotransform=FALSE)
stem.geo.nmds.site.score<-scores(stem.geo.nmds, display = 'sites')
stem.geo.nmds

par(las=1)
plot(stem.geo.nmds.site.score, type = 'n',xlim=c(-2,2),ylim=c(-1,1.5),main="Stem Count") #this plotting code makes a plot by geomorphic unit
points(stem.geo.nmds.site.score[c(1,5,11,15,20,25,30,36),1], stem.geo.nmds.site.score[c(1,5,11,15,20,25,30,36),2], pch=22,cex=2,bg="skyblue") #Active channel
points(stem.geo.nmds.site.score[c(2,8,12,17,22,27,33,38),1], stem.geo.nmds.site.score[c(2,8,12,17,22,27,33,38),2], pch=23,cex=2,bg="gold") #Active channel shelf
points(stem.geo.nmds.site.score[c(7,16,32),1], stem.geo.nmds.site.score[c(7,16,32),2], pch=24,cex=2,bg="orange") #Secondary active channel
points(stem.geo.nmds.site.score[c(3,9,13,18,23,28,34,39),1], stem.geo.nmds.site.score[c(3,9,13,18,23,28,34,39),2], pch=21,cex=2,bg="forestgreen") #Floodplain
points(stem.geo.nmds.site.score[c(4,10,14,19,24,29,35,40),1], stem.geo.nmds.site.score[c(4,10,14,19,24,29,35,40),2], pch=25,cex=2) #Riparian terrace
points(stem.geo.nmds.site.score[c(6,21,26,31,37),1], stem.geo.nmds.site.score[c(6,21,26,31,37),2], pch=21,cex=2,bg="purple") #Island
text(1.3,1.5, labels="Stress = 0.15",pos=4,font=3)
legend(x='topright', legend = c('Active Channel','Active Channel Shelf','Secondary Active Channel','Floodplain','Riparian Terrace','Island'),pch= c(22,23,24,21,25,21),pt.bg=c("skyblue","gold","orange","forestgreen","white","purple"),cex=1)

par(las=1)
plot(stem.geo.nmds.site.score, type = 'n',xlim=c(-2,2),ylim=c(-1,1.5),main="Stem Count") #this plotting code makes a plot by site
points(stem.geo.nmds.site.score[1:4,1], stem.geo.nmds.site.score[1:4,2],pch=15,cex=2,col="gold")
points(stem.geo.nmds.site.score[5:10,1], stem.geo.nmds.site.score[5:10,2],pch=15,cex=2,col="light green")
points(stem.geo.nmds.site.score[11:14,1], stem.geo.nmds.site.score[11:14,2], pch=17,cex=2,col="dark red")
points(stem.geo.nmds.site.score[15:19,1], stem.geo.nmds.site.score[15:19,2], pch=18,cex=2,col="red")
points(stem.geo.nmds.site.score[20:24,1], stem.geo.nmds.site.score[20:24,2], pch=19,cex=2,col="forest green")
points(stem.geo.nmds.site.score[25:29,1], stem.geo.nmds.site.score[25:29,2], pch=25,cex=2,col="dark green",bg="dark green")
points(stem.geo.nmds.site.score[30:35,1], stem.geo.nmds.site.score[30:35,2],pch=15,cex=2,col="dark orange")
points(stem.geo.nmds.site.score[36:40,1], stem.geo.nmds.site.score[36:40,2],pch=16,cex=2,col="gold")
text(1.3,1.5, labels="Stress = 0.15",pos=4,font=3)
legend(x='topleft', legend = c('Gov Canyon Trib','Gov Canyon','Salado','Maverick','Leon','Huesta','French','Leon Trib'),col = c("dark green","forest green","light green","gold","gold","dark orange","red","dark red"),pch= c(25,19,15,15,16,15,18,17),pt.bg="dark green",cex=1,pt.cex=1.25,ncol=2,bty="n")

#Stem count permanova analysis
#Both site and geomorphic surface
perm.result.stem.both<-adonis2(stem.geo.mtx~creek.name+geo.type, method = 'bray',by="margin")
perm.result.stem

#Geomorphic surface only
perm.result.stem.geoonly<-adonis2(stem.geo.mtx~geo.type, method = 'bray')
perm.result.stem.geoonly

##Diversity - land use - flow tests
div.lu.flow.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(div.lu.flow.data)

rich.imp.can<-lm(rich.can~imp.cover)
summary(rich.imp.can)

div.imp.can<-lm(div.can~imp.cover)
summary(div.imp.can)
plot(imp.cover,div.can)

rich.imp.ba<-lm(rich.ba~imp.cover)
summary(rich.imp.ba)

div.imp.ba<-lm(div.ba~imp.cover)
summary(div.imp.ba)

rich.imp.stem<-lm(rich.stem~imp.cover)
summary(rich.imp.stem)

div.imp.stem<-lm(div.stem~imp.cover)
summary(div.imp.stem)

rich.dryduration.can<-lm(rich.can~avgdurationdry)
summary(rich.dryduration.can)

div.dryduration.can<-lm(div.can~avgdurationdry)
summary(div.dryduration.can)

rich.dryduration.ba<-lm(rich.ba~avgdurationdry)
summary(rich.dryduration.ba)

div.dryduration.ba<-lm(div.ba~avgdurationdry)
summary(div.dryduration.ba)

rich.dryduration.stem<-lm(rich.stem~avgdurationdry)
summary(rich.dryduration.stem)

div.dryduration.stem<-lm(div.stem~avgdurationdry)
summary(div.dryduration.stem)

rich.eventsperyear.can<-lm(rich.can~eventsperyear)
summary(rich.eventsperyear.can)

div.eventsperyear.can<-lm(div.can~eventsperyear)
summary(div.eventsperyear.can)

rich.eventsperyear.ba<-lm(rich.ba~eventsperyear)
summary(rich.eventsperyear.ba)

div.eventsperyear.ba<-lm(div.ba~eventsperyear)
summary(div.eventsperyear.ba)

rich.eventsperyear.stem<-lm(rich.stem~eventsperyear)
summary(rich.eventsperyear.stem)

div.eventsperyear.stem<-lm(div.stem~eventsperyear)
summary(div.eventsperyear.stem)

rich.area.can<-lm(rich.can~wsarea)
summary(rich.area.can)

div.area.can<-lm(div.can~wsarea)
summary(div.area.can)

rich.area.ba<-lm(rich.ba~wsarea)
summary(rich.area.ba)

div.area.ba<-lm(div.ba~wsarea)
summary(div.area.ba)

rich.area.stem<-lm(rich.stem~wsarea)
summary(rich.area.stem)

div.area.stem<-lm(div.stem~wsarea)
summary(div.area.stem)

##Traits vs flow metrics
#Canopy cover
avgdurationdry.v.native.can<-lm(native.can~avgdurationdry)
summary(avgdurationdry.v.native.can)

eventsperyear.v.native.can<-lm(native.can~eventsperyear)
summary(eventsperyear.v.native.can)

avgdurationdry.v.droughttoleranceveryhigh.can<-lm(dt.vh.can~avgdurationdry)
summary(avgdurationdry.v.droughttoleranceveryhigh.can)

eventsperyear.v.droughttoleranceveryhigh.can<-lm(dt.vh.can~eventsperyear)
summary(eventsperyear.v.droughttoleranceveryhigh.can)

avgdurationdry.v.droughttolerancehigh.can<-lm(dt.h.can~avgdurationdry)
summary(avgdurationdry.v.droughttolerancehigh.can)

eventsperyear.v.droughttolerancehigh.can<-lm(dt.h.can~eventsperyear)
summary(eventsperyear.v.droughttolerancehigh.can)

avgdurationdry.v.droughttolerancemediumhigh.can<-lm(dt.mh.can~avgdurationdry)
summary(avgdurationdry.v.droughttolerancemediumhigh.can)

eventsperyear.v.droughttolerancemediumhigh.can<-lm(dt.mh.can~eventsperyear)
summary(eventsperyear.v.droughttolerancemediumhigh.can)

avgdurationdry.v.droughttolerancemedium.can<-lm(dt.m.can~avgdurationdry)
summary(avgdurationdry.v.droughttolerancemedium.can)

eventsperyear.v.droughttolerancemedium.can<-lm(dt.m.can~eventsperyear)
summary(eventsperyear.v.droughttolerancemedium.can)

avgdurationdry.v.droughttolerancelow.can<-lm(dt.l.can~avgdurationdry)
summary(avgdurationdry.v.droughttolerancelow.can)

eventsperyear.v.droughttolerancelow.can<-lm(dt.l.can~eventsperyear)
summary(eventsperyear.v.droughttolerancelow.can)

avgdurationdry.v.droughttoleranceoverall.can<-lm(dt.overall.can~avgdurationdry)
summary(avgdurationdry.v.droughttoleranceoverall.can)

eventsperyear.v.droughttoleranceoverall.can<-lm(dt.overall.can~eventsperyear)
summary(eventsperyear.v.droughttoleranceoverall.can)

avgdurationdry.v.wateruselowhigh.can<-lm(wu.lh.can~avgdurationdry)
summary(avgdurationdry.v.wateruselowhigh.can)

eventsperyear.v.wateruselowhigh.can<-lm(wu.lh.can~eventsperyear)
summary(eventsperyear.v.wateruselowhigh.can)

avgdurationdry.v.wateruselowmedhigh.can<-lm(wu.lmh.can~avgdurationdry)
summary(avgdurationdry.v.wateruselowmedhigh.can)

eventsperyear.v.wateruselowmedhigh.can<-lm(wu.lmh.can~eventsperyear)
summary(eventsperyear.v.wateruselowmedhigh.can)

avgdurationdry.v.waterusehigh.can<-lm(wu.h.can~avgdurationdry)
summary(avgdurationdry.v.waterusehigh.can)

eventsperyear.v.waterusehigh.can<-lm(wu.h.can~eventsperyear)
summary(eventsperyear.v.waterusehigh.can)
	
avgdurationdry.v.waterusemed.can<-lm(wu.m.can~avgdurationdry)
summary(avgdurationdry.v.waterusemed.can)

eventsperyear.v.waterusemed.can<-lm(wu.m.can~eventsperyear)
summary(eventsperyear.v.waterusemed.can)

avgdurationdry.v.wateruselowmed.can<-lm(wu.lm.can~avgdurationdry)
summary(avgdurationdry.v.wateruselowmed.can)
	#sig: r2 = 0.69, p = 0.01, coefficient = 0.0003

eventsperyear.v.wateruselowmed.can<-lm(wu.lm.can~eventsperyear)
summary(eventsperyear.v.wateruselowmed.can)

avgdurationdry.v.wateruselow.can<-lm(wu.l.can~avgdurationdry)
summary(avgdurationdry.v.wateruselow.can)

eventsperyear.v.wateruselow.can<-lm(wu.l.can~eventsperyear)
summary(eventsperyear.v.wateruselow.can)

#Basal Area
avgdurationdry.v.native.ba<-lm(native.ba~avgdurationdry)
summary(avgdurationdry.v.native.ba)

eventsperyear.v.native.ba<-lm(native.ba~eventsperyear)
summary(eventsperyear.v.native.ba)

avgdurationdry.v.droughttoleranceveryhigh.ba<-lm(dt.vh.ba~avgdurationdry)
summary(avgdurationdry.v.droughttoleranceveryhigh.ba)

eventsperyear.v.droughttoleranceveryhigh.ba<-lm(dt.vh.ba~eventsperyear)
summary(eventsperyear.v.droughttoleranceveryhigh.ba)

avgdurationdry.v.droughttolerancehigh.ba<-lm(dt.h.ba~avgdurationdry)
summary(avgdurationdry.v.droughttolerancehigh.ba)

eventsperyear.v.droughttolerancehigh.ba<-lm(dt.h.ba~eventsperyear)
summary(eventsperyear.v.droughttolerancehigh.ba)

avgdurationdry.v.droughttolerancemediumhigh.ba<-lm(dt.mh.ba~avgdurationdry)
summary(avgdurationdry.v.droughttolerancemediumhigh.ba)

eventsperyear.v.droughttolerancemediumhigh.ba<-lm(dt.mh.ba~eventsperyear)
summary(eventsperyear.v.droughttolerancemediumhigh.ba)

avgdurationdry.v.droughttolerancemedium.ba<-lm(dt.m.ba~avgdurationdry)
summary(avgdurationdry.v.droughttolerancemedium.ba)
	#sig: r2 = 0.53, p = 0.04, coefficient = 0.00036

eventsperyear.v.droughttolerancemedium.ba<-lm(dt.m.ba~eventsperyear)
summary(eventsperyear.v.droughttolerancemedium.ba)

avgdurationdry.v.droughttolerancelow.ba<-lm(dt.l.ba~avgdurationdry)
summary(avgdurationdry.v.droughttolerancelow.ba)

eventsperyear.v.droughttolerancelow.ba<-lm(dt.l.ba~eventsperyear)
summary(eventsperyear.v.droughttolerancelow.ba)

avgdurationdry.v.droughttoleranceoverall.ba<-lm(dt.overall.ba~avgdurationdry)
summary(avgdurationdry.v.droughttoleranceoverall.ba)

eventsperyear.v.droughttoleranceoverall.ba<-lm(dt.overall.ba~eventsperyear)
summary(eventsperyear.v.droughttoleranceoverall.ba)

avgdurationdry.v.wateruselowhigh.ba<-lm(wu.lh.ba~avgdurationdry)
summary(avgdurationdry.v.wateruselowhigh.ba)

eventsperyear.v.wateruselowhigh.ba<-lm(wu.lh.ba~eventsperyear)
summary(eventsperyear.v.wateruselowhigh.ba)
	#close: r2 = 0.39, p = 0.1, coefficient = 0.0026

avgdurationdry.v.wateruselowmedhigh.ba<-lm(wu.lmh.ba~avgdurationdry)
summary(avgdurationdry.v.wateruselowmedhigh.ba)

eventsperyear.v.wateruselowmedhigh.ba<-lm(wu.lmh.ba~eventsperyear)
summary(eventsperyear.v.wateruselowmedhigh.ba)

avgdurationdry.v.waterusehigh.ba<-lm(wu.h.ba~avgdurationdry)
summary(avgdurationdry.v.waterusehigh.ba)

eventsperyear.v.waterusehigh.ba<-lm(wu.h.ba~eventsperyear)
summary(eventsperyear.v.waterusehigh.ba)
	
avgdurationdry.v.waterusemed.ba<-lm(wu.m.ba~avgdurationdry)
summary(avgdurationdry.v.waterusemed.ba)

eventsperyear.v.waterusemed.ba<-lm(wu.m.ba~eventsperyear)
summary(eventsperyear.v.waterusemed.ba)

avgdurationdry.v.wateruselowmed.ba<-lm(wu.lm.ba~avgdurationdry)
summary(avgdurationdry.v.wateruselowmed.ba)
	#sig: r2 = 0.72, p = 0.008, coefficient = 0.0004

eventsperyear.v.wateruselowmed.ba<-lm(wu.lm.ba~eventsperyear)
summary(eventsperyear.v.wateruselowmed.ba)

avgdurationdry.v.wateruselow.ba<-lm(wu.l.ba~avgdurationdry)
summary(avgdurationdry.v.wateruselow.ba)

eventsperyear.v.wateruselow.ba<-lm(wu.l.ba~eventsperyear)
summary(eventsperyear.v.wateruselow.ba)

#Stem count
avgdurationdry.v.native.stem<-lm(native.stem~avgdurationdry)
summary(avgdurationdry.v.native.stem)
	
eventsperyear.v.native.stem<-lm(native.stem~eventsperyear)
summary(eventsperyear.v.native.stem)
	#close: r2 = 0.45, p = 0.07, coeff. = -0.004

avgdurationdry.v.droughttoleranceveryhigh.stem<-lm(dt.vh.stem~avgdurationdry)
summary(avgdurationdry.v.droughttoleranceveryhigh.stem)

eventsperyear.v.droughttoleranceveryhigh.stem<-lm(dt.vh.stem~eventsperyear)
summary(eventsperyear.v.droughttoleranceveryhigh.stem)

avgdurationdry.v.droughttolerancehigh.stem<-lm(dt.h.stem~avgdurationdry)
summary(avgdurationdry.v.droughttolerancehigh.stem)

eventsperyear.v.droughttolerancehigh.stem<-lm(dt.h.stem~eventsperyear)
summary(eventsperyear.v.droughttolerancehigh.stem)

avgdurationdry.v.droughttolerancemediumhigh.stem<-lm(dt.mh.stem~avgdurationdry)
summary(avgdurationdry.v.droughttolerancemediumhigh.stem)

eventsperyear.v.droughttolerancemediumhigh.stem<-lm(dt.mh.stem~eventsperyear)
summary(eventsperyear.v.droughttolerancemediumhigh.stem)

avgdurationdry.v.droughttolerancemedium.stem<-lm(dt.m.stem~avgdurationdry)
summary(avgdurationdry.v.droughttolerancemedium.stem)

eventsperyear.v.droughttolerancemedium.stem<-lm(dt.m.stem~eventsperyear)
summary(eventsperyear.v.droughttolerancemedium.stem)

avgdurationdry.v.droughttolerancelow.stem<-lm(dt.l.stem~avgdurationdry)
summary(avgdurationdry.v.droughttolerancelow.stem)

eventsperyear.v.droughttolerancelow.stem<-lm(dt.l.stem~eventsperyear)
summary(eventsperyear.v.droughttolerancelow.stem)

avgdurationdry.v.droughttoleranceoverall.stem<-lm(dt.overall.stem~avgdurationdry)
summary(avgdurationdry.v.droughttoleranceoverall.stem)

eventsperyear.v.droughttoleranceoverall.stem<-lm(dt.overall.stem~eventsperyear)
summary(eventsperyear.v.droughttoleranceoverall.stem)

avgdurationdry.v.wateruselowhigh.stem<-lm(wu.lh.stem~avgdurationdry)
summary(avgdurationdry.v.wateruselowhigh.stem)

eventsperyear.v.wateruselowhigh.stem<-lm(wu.lh.stem~eventsperyear)
summary(eventsperyear.v.wateruselowhigh.stem)

avgdurationdry.v.wateruselowmedhigh.stem<-lm(wu.lmh.stem~avgdurationdry)
summary(avgdurationdry.v.wateruselowmedhigh.stem)

eventsperyear.v.wateruselowmedhigh.stem<-lm(wu.lmh.stem~eventsperyear)
summary(eventsperyear.v.wateruselowmedhigh.stem)

avgdurationdry.v.waterusehigh.stem<-lm(wu.h.stem~avgdurationdry)
summary(avgdurationdry.v.waterusehigh.stem)

eventsperyear.v.waterusehigh.stem<-lm(wu.h.stem~eventsperyear)
summary(eventsperyear.v.waterusehigh.stem)
	
avgdurationdry.v.waterusemed.stem<-lm(wu.m.stem~avgdurationdry)
summary(avgdurationdry.v.waterusemed.stem)

eventsperyear.v.waterusemed.stem<-lm(wu.m.stem~eventsperyear)
summary(eventsperyear.v.waterusemed.stem)

avgdurationdry.v.wateruselowmed.stem<-lm(wu.lm.stem~avgdurationdry)
summary(avgdurationdry.v.wateruselowmed.stem)
	#sig: r2 = 0.76, p = 0.005, coefficient = 0.0005

eventsperyear.v.wateruselowmed.stem<-lm(wu.lm.stem~eventsperyear)
summary(eventsperyear.v.wateruselowmed.stem)

avgdurationdry.v.wateruselow.stem<-lm(wu.l.stem~avgdurationdry)
summary(avgdurationdry.v.wateruselow.stem)
	#close: r2 = 0.46, p = 0.06, coefficient = -0.0003
	plot(avgdurationdry,wu.l.stem)

eventsperyear.v.wateruselow.stem<-lm(wu.l.stem~eventsperyear)
summary(eventsperyear.v.wateruselow.stem)




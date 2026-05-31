##############Correlations between land use and hydrological variables
#bring in the diversity and environmental variables dataset
rich.div.envar.mat<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(rich.div.envar.mat)

#####Creation of a correlogram plot

#Load needed package
library(corrplot)

#Make the correlation matrix
x.mat<-rich.div.envar.mat[,c(12,11,10,14:26,29:30,33)]
y.mat<-rich.div.envar.mat[,c(12,11,10,14:26,29:30,33)]
env.var.cor.mat<-cor(x.mat,y.mat)
rownames(env.var.cor.mat)<-c("Time Between Floods","Flood Frequency","Flood Duration","Watershed Area","Open Water","Urban Open Space","Urban Low Density","Urban Medium Density","Urban High Density","Impervious Cover","Barren","Deciduous Forest","Coniferous Forest","Mixed Forest","Shrubland","Grassland","Agriculture","Woody Wetlands","Impervious Cover in 100m Riparian Buffer")
colnames(env.var.cor.mat)<-c("Time Between Floods","Flood Frequency","Flood Duration","Watershed Area","Open Water","Urban Open Space","Urban Low Density","Urban Medium Density","Urban High Density","Impervious Cover","Barren","Deciduous Forest","Coniferous Forest","Mixed Forest","Shrubland","Grassland","Agriculture","Woody Wetlands","Impervious Cover in 100m Riparian Buffer")

#make a matrix of the p-values of correlations
sig.mat<-matrix(0,19,19)
sig.mat[5,4]<-1
sig.mat[7,6]<-1
sig.mat[7,7]<-1
sig.mat[8,6]<-1
sig.mat[8,7]<-1
sig.mat[9,6]<-1
sig.mat[9,8]<-1
sig.mat[10,6:9]<-1
sig.mat[11,4:5]<-1
sig.mat[12,4:5]<-1
sig.mat[13,6:10]<-1
sig.mat[14,8]<-1
sig.mat[15,4:5]<-1
sig.mat[15,11:12]<-1
sig.mat[16,4]<-1
sig.mat[16,15]<-1
sig.mat[18,8]<-1
sig.mat[18,14]<-1
sig.mat[19,6]<-1
sig.mat[19,8:10]<-1
sig.mat[19,13]<-1
rownames(sig.mat)<-c("Time Between Floods","Flood Frequency","Flood Duration","Watershed Area","Open Water","Urban Open Space","Urban Low Density","Urban Medium Density","Urban High Density","Impervious Cover","Barren","Deciduous Forest","Coniferous Forest","Mixed Forest","Shrubland","Grassland","Agriculture","Woody Wetlands","Impervious Cover in 100m Riparian Buffer")
colnames(sig.mat)<-c("Time Between Floods","Flood Frequency","Flood Duration","Watershed Area","Open Water","Urban Open Space","Urban Low Density","Urban Medium Density","Urban High Density","Impervious Cover","Barren","Deciduous Forest","Coniferous Forest","Mixed Forest","Shrubland","Grassland","Agriculture","Woody Wetlands","Impervious Cover in 100m Riparian Buffer")

#define the color palette going from red to blue instead of blue to red
elect.col.pal<-colorRampPalette(colors=c("Blue","Red"))

#set up exporting as a tiff
tiff(filename="C:\\Users\\laubs\\Documents\\Figure3_corrplot.tif", width=6.24, height=4, units="in", pointsize=12, res=500)

#make the plot
env.var.cor.plot<-corrplot(env.var.cor.mat,type="lower",col=elect.col.pal(200),is.corr=TRUE,bg="grey",addgrid.col="white",diag=FALSE,tl.cex=0.5,tl.col="black",tl.srt=45,p.mat=sig.mat,sig.level=0.05,pch=8,pch.cex=1)

#stop graphics device
dev.off()


##############Richness and diversity relationships to land use and hydrological variables
#load required package
library(lmerTest)

#bring in the diversity and environmental variables dataset
rich.div.envar.mat<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(rich.div.envar.mat)

#define the full mixed effect models
can.rich.mod.full<-lmer(rich.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS+(1|geotype))
can.div.mod.full<-lmer(div.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS+(1|geotype))
ba.rich.mod.full<-lmer(rich.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS+(1|geotype))
ba.div.mod.full<-lmer(div.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS+(1|geotype))
stem.div.mod.full<-lmer(div.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS+(1|geotype))

#perform the backward stepwise procedure for each model
can.rich.step.mod<-step(can.rich.mod.full,reduce.random=FALSE) 
can.div.step.mod<-step(can.div.mod.full,reduce.random=FALSE) 
ba.rich.step.mod<-step(ba.rich.mod.full,reduce.random = FALSE) 
ba.div.step.mod<-step(ba.div.mod.full,reduce.random=FALSE) 
stem.div.step.mod<-step(stem.div.mod.full,alpha.fixed=0.1,reduce.random = FALSE) #yay, this was the same model I got (although if don't tell it to keep random effect, it also drops that)

#perform forward stepwise procedure for canopy cover richness
can.rich.mod.null<-lmer(rich.can~1+(1|geotype))
can.rich.mod.timebtwnfloodsonly<-lmer(rich.can~AvgPeriodBtwnFloods+(1|geotype))
can.rich.mod.floodfreqonly<-lmer(rich.can~FloodFrequency+(1|geotype))
can.rich.mod.floodduronly<-lmer(rich.can~AvgFloodDuration+(1|geotype))
can.rich.mod.areaonly<-lmer(rich.can~WatershedAreaSqKm+(1|geotype))
can.rich.mod.imponly<-lmer(rich.can~PctImpervious2019Ws+(1|geotype))
can.rich.mod.mixforestonly<-lmer(rich.can~PctMxFst2019Ws+(1|geotype))
can.rich.mod.agonly<-lmer(rich.can~PctAg2019WS+(1|geotype))
extractAIC(can.rich.mod.null)
extractAIC(can.rich.mod.timebtwnfloodsonly)
extractAIC(can.rich.mod.floodfreqonly)
extractAIC(can.rich.mod.floodduronly)
extractAIC(can.rich.mod.areaonly)
extractAIC(can.rich.mod.imponly)
extractAIC(can.rich.mod.mixforestonly)
extractAIC(can.rich.mod.agonly)
can.rich.mod.imp.timebtwnfloods<-lmer(rich.can~PctImpervious2019Ws+AvgPeriodBtwnFloods+(1|geotype))
can.rich.mod.imp.floodfreq<-lmer(rich.can~PctImpervious2019Ws+FloodFrequency+(1|geotype))
can.rich.mod.imp.flooddur<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+(1|geotype))
can.rich.mod.imp.area<-lmer(rich.can~PctImpervious2019Ws+WatershedAreaSqKm+(1|geotype))
can.rich.mod.imp.mixforest<-lmer(rich.can~PctImpervious2019Ws+PctMxFst2019Ws+(1|geotype))
can.rich.mod.imp.ag<-lmer(rich.can~PctImpervious2019Ws+PctAg2019WS+(1|geotype))
extractAIC(can.rich.mod.imp.timebtwnfloods)
extractAIC(can.rich.mod.imp.floodfreq)
extractAIC(can.rich.mod.imp.flooddur)
extractAIC(can.rich.mod.imp.area)
extractAIC(can.rich.mod.imp.mixforest)
extractAIC(can.rich.mod.imp.ag)
can.rich.mod.imp.flooddur.timebtwnfloods<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+AvgPeriodBtwnFloods+(1|geotype))
can.rich.mod.imp.flooddur.floodfreq<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+FloodFrequency+(1|geotype))
can.rich.mod.imp.flooddur.area<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+WatershedAreaSqKm+(1|geotype))
can.rich.mod.imp.flooddur.mixedforest<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+PctMxFst2019Ws+(1|geotype))
can.rich.mod.imp.flooddur.ag<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+PctAg2019WS+(1|geotype))
extractAIC(can.rich.mod.imp.flooddur.timebtwnfloods)
extractAIC(can.rich.mod.imp.flooddur.floodfreq)
extractAIC(can.rich.mod.imp.flooddur.area)
extractAIC(can.rich.mod.imp.flooddur.mixedforest)
extractAIC(can.rich.mod.imp.flooddur.ag)
can.rich.mod.imp.flooddur.timebtwnfloods.floodfreq<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+AvgPeriodBtwnFloods+FloodFrequency+(1|geotype))
can.rich.mod.imp.flooddur.timebtwnfloods.area<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+AvgPeriodBtwnFloods+WatershedAreaSqKm+(1|geotype))
can.rich.mod.imp.flooddur.timebtwnfloods.mixedforest<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+AvgPeriodBtwnFloods+PctMxFst2019Ws+(1|geotype))
can.rich.mod.imp.flooddur.timebtwnfloods.ag<-lmer(rich.can~PctImpervious2019Ws+AvgFloodDuration+AvgPeriodBtwnFloods+PctAg2019WS+(1|geotype))
extractAIC(can.rich.mod.imp.flooddur.timebtwnfloods.floodfreq)
extractAIC(can.rich.mod.imp.flooddur.timebtwnfloods.area)
extractAIC(can.rich.mod.imp.flooddur.timebtwnfloods.mixedforest)
extractAIC(can.rich.mod.imp.flooddur.timebtwnfloods.ag)

#perform forward stepwise procedure for canopy cover Shannon diversity
can.div.mod.null<-lmer(div.can~1+(1|geotype))
can.div.mod.timebtwnfloodsonly<-lmer(div.can~AvgPeriodBtwnFloods+(1|geotype))
can.div.mod.floodfreqonly<-lmer(div.can~FloodFrequency+(1|geotype))
can.div.mod.floodduronly<-lmer(div.can~AvgFloodDuration+(1|geotype))
can.div.mod.areaonly<-lmer(div.can~WatershedAreaSqKm+(1|geotype))
can.div.mod.imponly<-lmer(div.can~PctImpervious2019Ws+(1|geotype))
can.div.mod.mixforestonly<-lmer(div.can~PctMxFst2019Ws+(1|geotype))
can.div.mod.agonly<-lmer(div.can~PctAg2019WS+(1|geotype))
extractAIC(can.div.mod.null)
extractAIC(can.div.mod.timebtwnfloodsonly)
extractAIC(can.div.mod.floodfreqonly)
extractAIC(can.div.mod.floodduronly)
extractAIC(can.div.mod.areaonly)
extractAIC(can.div.mod.imponly)
extractAIC(can.div.mod.mixforestonly)
extractAIC(can.div.mod.agonly)
can.div.mod.imp.timebtwnfloods<-lmer(div.can~PctImpervious2019Ws+AvgPeriodBtwnFloods+(1|geotype))
can.div.mod.imp.floodfreq<-lmer(div.can~PctImpervious2019Ws+FloodFrequency+(1|geotype))
can.div.mod.imp.flooddur<-lmer(div.can~PctImpervious2019Ws+AvgFloodDuration+(1|geotype))
can.div.mod.imp.area<-lmer(div.can~PctImpervious2019Ws+WatershedAreaSqKm+(1|geotype))
can.div.mod.imp.mixforest<-lmer(div.can~PctImpervious2019Ws+PctMxFst2019Ws+(1|geotype))
can.div.mod.imp.ag<-lmer(div.can~PctImpervious2019Ws+PctAg2019WS+(1|geotype))
extractAIC(can.div.mod.imp.timebtwnfloods)
extractAIC(can.div.mod.imp.floodfreq)
extractAIC(can.div.mod.imp.flooddur)
extractAIC(can.div.mod.imp.area)
extractAIC(can.div.mod.imp.mixforest)
extractAIC(can.div.mod.imp.ag)

#perform forward stepwise procedure for basal area/stem richness
ba.rich.mod.null<-lmer(rich.ba~1+(1|geotype))
ba.rich.mod.timebtwnfloodsonly<-lmer(rich.ba~AvgPeriodBtwnFloods+(1|geotype))
ba.rich.mod.floodfreqonly<-lmer(rich.ba~FloodFrequency+(1|geotype))
ba.rich.mod.floodduronly<-lmer(rich.ba~AvgFloodDuration+(1|geotype))
ba.rich.mod.areaonly<-lmer(rich.ba~WatershedAreaSqKm+(1|geotype))
ba.rich.mod.imponly<-lmer(rich.ba~PctImpervious2019Ws+(1|geotype))
ba.rich.mod.mixforestonly<-lmer(rich.ba~PctMxFst2019Ws+(1|geotype))
ba.rich.mod.agonly<-lmer(rich.ba~PctAg2019WS+(1|geotype))
extractAIC(ba.rich.mod.null)
extractAIC(ba.rich.mod.timebtwnfloodsonly)
extractAIC(ba.rich.mod.floodfreqonly)
extractAIC(ba.rich.mod.floodduronly)
extractAIC(ba.rich.mod.areaonly)
extractAIC(ba.rich.mod.imponly)
extractAIC(ba.rich.mod.mixforestonly)
extractAIC(ba.rich.mod.agonly)
ba.rich.mod.floodfreq.timebtwnfloods<-lmer(rich.ba~FloodFrequency+AvgPeriodBtwnFloods+(1|geotype))
ba.rich.mod.floodfreq.flooddur<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+(1|geotype))
ba.rich.mod.floodfreq.area<-lmer(rich.ba~FloodFrequency+WatershedAreaSqKm+(1|geotype))
ba.rich.mod.floodfreq.imp<-lmer(rich.ba~FloodFrequency+PctImpervious2019Ws+(1|geotype))
ba.rich.mod.floodfreq.mixforest<-lmer(rich.ba~FloodFrequency+PctMxFst2019Ws+(1|geotype))
ba.rich.mod.floodfreq.ag<-lmer(rich.ba~FloodFrequency+PctAg2019WS+(1|geotype))
extractAIC(ba.rich.mod.floodfreq.timebtwnfloods)
extractAIC(ba.rich.mod.floodfreq.flooddur)
extractAIC(ba.rich.mod.floodfreq.area)
extractAIC(ba.rich.mod.floodfreq.imp)
extractAIC(ba.rich.mod.floodfreq.mixforest)
extractAIC(ba.rich.mod.floodfreq.ag)
ba.rich.mod.floodfreq.flooddur.timebtwnfloods<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+AvgPeriodBtwnFloods+(1|geotype))
ba.rich.mod.floodfreq.flooddur.area<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+(1|geotype))
ba.rich.mod.floodfreq.flooddur.imp<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+PctImpervious2019Ws+(1|geotype))
ba.rich.mod.floodfreq.flooddur.mixforest<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+PctMxFst2019Ws+(1|geotype))
ba.rich.mod.floodfreq.flooddur.ag<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+PctAg2019WS+(1|geotype))
extractAIC(ba.rich.mod.floodfreq.flooddur.timebtwnfloods)
extractAIC(ba.rich.mod.floodfreq.flooddur.area)
extractAIC(ba.rich.mod.floodfreq.flooddur.imp)
extractAIC(ba.rich.mod.floodfreq.flooddur.mixforest)
extractAIC(ba.rich.mod.floodfreq.flooddur.ag)
ba.rich.mod.floodfreq.flooddur.area.timebtwnfloods<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+AvgPeriodBtwnFloods+(1|geotype))
ba.rich.mod.floodfreq.flooddur.area.imp<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+(1|geotype))
ba.rich.mod.floodfreq.flooddur.area.mixforest<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctMxFst2019Ws+(1|geotype))
ba.rich.mod.floodfreq.flooddur.area.ag<-lmer(rich.ba~FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctAg2019WS+(1|geotype))
extractAIC(ba.rich.mod.floodfreq.flooddur.area.timebtwnfloods)
extractAIC(ba.rich.mod.floodfreq.flooddur.area.imp)
extractAIC(ba.rich.mod.floodfreq.flooddur.area.mixforest)
extractAIC(ba.rich.mod.floodfreq.flooddur.area.ag)

#perform forward stepwise procedure for basal area Shannon diversity
ba.div.mod.null<-lmer(div.ba~1+(1|geotype))
ba.div.mod.timebtwnfloodsonly<-lmer(div.ba~AvgPeriodBtwnFloods+(1|geotype))
ba.div.mod.floodfreqonly<-lmer(div.ba~FloodFrequency+(1|geotype))
ba.div.mod.floodduronly<-lmer(div.ba~AvgFloodDuration+(1|geotype))
ba.div.mod.areaonly<-lmer(div.ba~WatershedAreaSqKm+(1|geotype))
ba.div.mod.imponly<-lmer(div.ba~PctImpervious2019Ws+(1|geotype))
ba.div.mod.mixforestonly<-lmer(div.ba~PctMxFst2019Ws+(1|geotype))
ba.div.mod.agonly<-lmer(div.ba~PctAg2019WS+(1|geotype))
extractAIC(ba.div.mod.null)
extractAIC(ba.div.mod.timebtwnfloodsonly)
extractAIC(ba.div.mod.floodfreqonly)
extractAIC(ba.div.mod.floodduronly)
extractAIC(ba.div.mod.areaonly)
extractAIC(ba.div.mod.imponly)
extractAIC(ba.div.mod.mixforestonly)
extractAIC(ba.div.mod.agonly)
ba.div.mod.floodfreq.timebtwnfloods<-lmer(div.ba~FloodFrequency+AvgPeriodBtwnFloods+(1|geotype))
ba.div.mod.floodfreq.flooddur<-lmer(div.ba~FloodFrequency+AvgFloodDuration+(1|geotype))
ba.div.mod.floodfreq.area<-lmer(div.ba~FloodFrequency+WatershedAreaSqKm+(1|geotype))
ba.div.mod.floodfreq.imp<-lmer(div.ba~FloodFrequency+PctImpervious2019Ws+(1|geotype))
ba.div.mod.floodfreq.mixforest<-lmer(div.ba~FloodFrequency+PctMxFst2019Ws+(1|geotype))
ba.div.mod.floodfreq.ag<-lmer(div.ba~FloodFrequency+PctAg2019WS+(1|geotype))
extractAIC(ba.div.mod.floodfreq.timebtwnfloods)
extractAIC(ba.div.mod.floodfreq.flooddur)
extractAIC(ba.div.mod.floodfreq.area)
extractAIC(ba.div.mod.floodfreq.imp)
extractAIC(ba.div.mod.floodfreq.mixforest)
extractAIC(ba.div.mod.floodfreq.ag)
ba.div.mod.floodfreq.area.timebtwnfloods<-lmer(div.ba~FloodFrequency+WatershedAreaSqKm+AvgPeriodBtwnFloods+(1|geotype))
ba.div.mod.floodfreq.area.flooddur<-lmer(div.ba~FloodFrequency+WatershedAreaSqKm+AvgFloodDuration+(1|geotype))
ba.div.mod.floodfreq.area.imp<-lmer(div.ba~FloodFrequency+WatershedAreaSqKm+PctImpervious2019Ws+(1|geotype))
ba.div.mod.floodfreq.area.mixforest<-lmer(div.ba~FloodFrequency+WatershedAreaSqKm+PctMxFst2019Ws+(1|geotype))
ba.div.mod.floodfreq.area.ag<-lmer(div.ba~FloodFrequency+WatershedAreaSqKm+PctAg2019WS+(1|geotype))
extractAIC(ba.div.mod.floodfreq.area.timebtwnfloods)
extractAIC(ba.div.mod.floodfreq.area.flooddur)
extractAIC(ba.div.mod.floodfreq.area.imp)
extractAIC(ba.div.mod.floodfreq.area.mixforest)
extractAIC(ba.div.mod.floodfreq.area.ag)

#perform forward stepwise procedure for stem count Shannon diversity
stem.div.mod.null<-lmer(div.stem~1+(1|geotype))
stem.div.mod.timebtwnfloodsonly<-lmer(div.stem~AvgPeriodBtwnFloods+(1|geotype))
stem.div.mod.floodfreqonly<-lmer(div.stem~FloodFrequency+(1|geotype))
stem.div.mod.floodduronly<-lmer(div.stem~AvgFloodDuration+(1|geotype))
stem.div.mod.areaonly<-lmer(div.stem~WatershedAreaSqKm+(1|geotype))
stem.div.mod.imponly<-lmer(div.stem~PctImpervious2019Ws+(1|geotype))
stem.div.mod.mixforestonly<-lmer(div.stem~PctMxFst2019Ws+(1|geotype))
stem.div.mod.agonly<-lmer(div.stem~PctAg2019WS+(1|geotype))
extractAIC(stem.div.mod.null)
extractAIC(stem.div.mod.timebtwnfloodsonly)
extractAIC(stem.div.mod.floodfreqonly)
extractAIC(stem.div.mod.floodduronly)
extractAIC(stem.div.mod.areaonly)
extractAIC(stem.div.mod.imponly)
extractAIC(stem.div.mod.mixforestonly)
extractAIC(stem.div.mod.agonly)
stem.div.mod.mixforest.timebtwnfloods<-lmer(div.stem~PctMxFst2019Ws+AvgPeriodBtwnFloods+(1|geotype))
stem.div.mod.mixforest.floodfreq<-lmer(div.stem~PctMxFst2019Ws+FloodFrequency+(1|geotype))
stem.div.mod.mixforest.flooddur<-lmer(div.stem~PctMxFst2019Ws+AvgFloodDuration+(1|geotype))
stem.div.mod.mixforest.area<-lmer(div.stem~PctMxFst2019Ws+WatershedAreaSqKm+(1|geotype))
stem.div.mod.mixforest.imp<-lmer(div.stem~PctMxFst2019Ws+PctImpervious2019Ws+(1|geotype))
stem.div.mod.mixforest.ag<-lmer(div.stem~PctMxFst2019Ws+PctAg2019WS+(1|geotype))
extractAIC(stem.div.mod.mixforest.timebtwnfloods)
extractAIC(stem.div.mod.mixforest.floodfreq)
extractAIC(stem.div.mod.mixforest.flooddur)
extractAIC(stem.div.mod.mixforest.area)
extractAIC(stem.div.mod.mixforest.imp)
extractAIC(stem.div.mod.mixforest.ag)

###############NMDS and environmental fitting of species data at geomorphic surface level

###Canopy cover

#Load required package
library(vegan)

#Load data
can.geo.surface<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(can.geo.surface)

#Define species by site matrix
can.geo.mtx<-data.frame(can.geo.surface[,5:45],row.names=creek.sample)

#Run the NMDS model
can.geo.nmds<-metaMDS(can.geo.mtx, distance = 'bray',autotransform=FALSE)
can.geo.nmds

##Fit landuse and hydrological variables to NMDS
#First bring in the land use and hydrological data
land.use.hydro.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') #load in data called: land use and water part 2, this has the updated metrics from Dr. Laub
attach(land.use.hydro.data)

#Then define the land use and hydrological variable matrix
land.use.hydro.mtx.can<-data.frame(land.use.hydro.data[,c(10:12,14,20,24,29)],row.names=creeksample)

#Then conduct the environmental fitting analysis
env.fit.can<-envfit(can.geo.nmds,land.use.hydro.mtx.can)  
env.fit.can 

#Store the environmental fitting scores for later plotting
can.envfit.scores<-scores(env.fit.can,display="vectors")

#Permanova analysis by site
perm.result.can.siteonly<-adonis2(can.geo.mtx~creek.name,method="bray")
perm.result.can.siteonly

##Making an ordination plot of the canopy cover NMDS with significant land use and hydrological variables 

#set up exporting as a tiff
tiff(filename="C:\\Users\\laubs\\Documents\\Figure4a_speciesNMDScan.tif", width=7.8, height=5, units="in", pointsize=12, res=500)

#First define a blank plot
ordiplot(can.geo.nmds,type="none",xlim=c(-1,2))

#Then add each site as an ellipse
ordiellipse(can.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark orange",alpha=200,show.groups="Huesta")
ordiellipse(can.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="red",alpha=200,show.groups="French")
ordiellipse(can.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Maverick")
ordiellipse(can.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="light green",alpha=200,show.groups="Salado")
ordiellipse(can.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark red",alpha=200,show.groups="Leon Creek Trib")
ordiellipse(can.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="forest green",alpha=200,show.groups="Gov Canyon")
ordiellipse(can.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark green",alpha=200,show.groups="Gov Canyon Trib")
ordiellipse(can.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Leon")

#Labels for the site ellipses
text(0.8,0.4,labels="Gov Canyon Trib")
text(0.13,0.5,labels="Gov Canyon")
text(0.08,-0.26,labels="Salado")
text(0.55,-0.075,labels="Maverick")
text(-0.7,0.2,labels="Leon")
text(0.2,-0.55,labels="Huesta")
text(-0.4,-0.55,labels="French")
text(0.85,-0.6,labels="Leon Trib")

#Add the stress value
text(1.75,1, labels="Stress = 0.19",pos=4,font=3)

#Add a legend to the plot
legend(x=-1.65,y=1, legend = c('0','1','12','18','19','41','45'),col = c("dark green","forest green","light green","gold","dark orange","red","dark red"),pch= c(19,19,19,19,19,19,19),pt.bg="dark green",cex=1,pt.cex=1.25,ncol=1,horiz=TRUE,bty="n")
text(-1.1,1,labels="Percent Impervious")

#add the land use and hydrologcial variables as arrows with labels
arrows(x0=0,y0=0,x1=can.envfit.scores[2,1], y1=can.envfit.scores[2,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[2,1], y=can.envfit.scores[2,2],col = 'grey0', labels = 'Flood Frequency',pos=2,font=3)
arrows(x0=0,y0=0,x1=can.envfit.scores[3,1], y1=can.envfit.scores[3,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[3,1], y=can.envfit.scores[3,2],col = 'grey0', labels = 'Time Between Floods',pos=4,font=3)
arrows(x0=0,y0=0,x1=can.envfit.scores[4,1], y1=can.envfit.scores[4,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[4,1], y=can.envfit.scores[4,2],col = 'grey0', labels = 'Watershed Area',pos=2,font=3)
arrows(x0=0,y0=0,x1=can.envfit.scores[5,1], y1=can.envfit.scores[5,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[5,1], y=can.envfit.scores[5,2],col = 'grey0', labels = 'Impervious Cover',pos=1,font=3)
arrows(x0=0,y0=0,x1=can.envfit.scores[6,1], y1=can.envfit.scores[6,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[6,1], y=can.envfit.scores[6,2],col = 'grey0', labels = 'Mixed Forest',font=3,adj=c(1,1.2))
arrows(x0=0,y0=0,x1=can.envfit.scores[7,1], y1=can.envfit.scores[7,2],lwd=2,col = 'grey0')
text(x=can.envfit.scores[7,1], y=can.envfit.scores[7,2],col = 'grey0', labels = 'Agriculture',pos=2,font=3)

#stop graphics device
dev.off()

###Basal Area

#Load required package
library(vegan)

#Load data
ba.geo.surface<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(ba.geo.surface)

#Define species by site matrix
ba.geo.mtx<-data.frame(ba.geo.surface[,5:41],row.names=creek.sample)

#Run the NMDS model
ba.geo.nmds<-metaMDS(ba.geo.mtx, distance = 'bray',autotransform=FALSE)
ba.geo.nmds

##Fit landuse and hydrological variables to NMDS
#First bring in the land use and hydrological data
land.use.hydro.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') #load in data called: land use and water part 2, this has the updated metrics from Dr. Laub
attach(land.use.hydro.data)

#Then define the land use and hydrological variable matrix
land.use.hydro.mtx.bastem<-data.frame(land.use.hydro.data[c(1,4:13,15:18,20:41,43:45),c(10:12,14,20,24,29)],row.names=creeksample[c(1,4:13,15:18,20:41,43:45)])

#Then conduct the environmental fitting analysis
env.fit.ba<-envfit(ba.geo.nmds,land.use.hydro.mtx.bastem) 
env.fit.ba  

#Store the environmental fitting scores for later plotting
ba.envfit.scores<-scores(env.fit.ba,display="vectors")

#Permanova analysis by site
perm.result.ba.siteoonly<-adonis2(ba.geo.mtx~creek.name, method = 'bray')
perm.result.ba.siteoonly

##Making an ordination plot of the canopy cover NMDS with significant land use and hydrological variables 

#set up exporting as a tiff
tiff(filename="C:\\Users\\laubs\\Documents\\Figure4b_speciesNMDSba.tif", width=7.8, height=5, units="in", pointsize=12, res=500)

#First define a blank plot
ordiplot(ba.geo.nmds,type="none") 

#Then add each site as an ellipse
ordiellipse(ba.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="light green",alpha=200,show.groups="Salado")
ordiellipse(ba.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="red",alpha=200,show.groups="French")
ordiellipse(ba.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark orange",alpha=200,show.groups="Huesta")
ordiellipse(ba.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="forest green",alpha=200,show.groups="Gov Canyon")
ordiellipse(ba.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark green",alpha=200,show.groups="Gov Canyon Trib")
ordiellipse(ba.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Maverick")
ordiellipse(ba.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark red",alpha=200,show.groups="Leon Creek Trib")
ordiellipse(ba.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Leon")

#Labels for the site ellipses
text(-1.5,0.075,labels="Gov Canyon Trib")
text(-0.1,1.25,labels="Gov Canyon")
text(0.5,0.7,labels="Salado")
text(-0.7,-0.1,labels="Maverick")
text(-0.6,-0.9,labels="Leon")
text(0.5,0.3,labels="Huesta")
text(0.42,-1,labels="French")
text(-0.4,0.7,labels="Leon Trib")

#Add the stress value
text(2.25,1.5, labels="Stress = 0.17",pos=4,font=3)

#Add a legend to the plot
legend(x=-3.2,y=1.5, legend = c('0','1','12','18','19','41','45'),col = c("dark green","forest green","light green","gold","dark orange","red","dark red"),pch= c(19,19,19,19,19,19,19),pt.bg="dark green",cex=1,pt.cex=1.25,ncol=1,bty="n")
text(-2.35,1.5,labels="Percent Impervious")

#add the land use and hydrologcial variables as arrows with labels
arrows(x0=0,y0=0,x1=ba.envfit.scores[2,1], y1=ba.envfit.scores[2,2],lwd=2,col = 'grey0')
text(x=ba.envfit.scores[2,1], y=ba.envfit.scores[2,2],col = 'grey0', labels = 'Flood Frequency',pos=4,font=3)
arrows(x0=0,y0=0,x1=ba.envfit.scores[3,1], y1=ba.envfit.scores[3,2],lwd=2,col = 'grey0')
text(x=ba.envfit.scores[3,1], y=ba.envfit.scores[3,2],col = 'grey0', labels = 'Time Between Floods',font=3,adj=c(1,0))
arrows(x0=0,y0=0,x1=ba.envfit.scores[4,1], y1=ba.envfit.scores[4,2],lwd=2,col = 'grey0')
text(x=ba.envfit.scores[4,1], y=ba.envfit.scores[4,2],col = 'grey0', labels = 'Watershed Area',pos=2,font=3)
arrows(x0=0,y0=0,x1=ba.envfit.scores[5,1], y1=ba.envfit.scores[5,2],lwd=2,col = 'grey0')
text(x=ba.envfit.scores[5,1], y=ba.envfit.scores[5,2],col = 'grey0', labels = 'Impervious Cover',font=3,adj=c(-0.05,-0.5))
arrows(x0=0,y0=0,x1=ba.envfit.scores[6,1], y1=ba.envfit.scores[6,2],lwd=2,col = 'grey0')
text(x=ba.envfit.scores[6,1], y=ba.envfit.scores[6,2],col = 'grey0', labels = 'Mixed Forest',pos=1,font=3)

#stop graphics device
dev.off()


###Stem Count

#Load required package
library(vegan)

#Load data
stem.geo.surface<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(stem.geo.surface)

#Define species by site matrix
stem.geo.mtx<-data.frame(stem.geo.surface[,5:41],row.names=creek.sample)

#Run the NMDS model
stem.geo.nmds<-metaMDS(stem.geo.mtx, distance = 'bray',autotransform=FALSE)
stem.geo.nmds

##Fit landuse and hydrological variables to NMDS
#First bring in the land use and hydrological data
land.use.hydro.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') #load in data called: land use and water part 2, this has the updated metrics from Dr. Laub
attach(land.use.hydro.data)

#Then define the land use and hydrological variable matrix
land.use.hydro.mtx.bastem<-data.frame(land.use.hydro.data[c(1,4:13,15:18,20:41,43:45),c(10:12,14,20,24,29)],row.names=creeksample[c(1,4:13,15:18,20:41,43:45)])

#Then conduct the environmental fitting analysis
env.fit.stem<-envfit(stem.geo.nmds,land.use.hydro.mtx.bastem) 
env.fit.stem

#Store the environmental fitting scores for later plotting
stem.envfit.scores<-scores(env.fit.stem,display="vectors")

#Permanova analysis by site
perm.result.stem.siteonly<-adonis2(stem.geo.mtx~creek.name, method = 'bray')
perm.result.stem.siteonly

##Making an ordination plot of the canopy cover NMDS with significant land use and hydrological variables 

#set up exporting as a tiff
tiff(filename="C:\\Users\\laubs\\Documents\\Figure4c_speciesNMDSstem.tif", width=7.8, height=5, units="in", pointsize=12, res=500)

#First define a blank plot
ordiplot(stem.geo.nmds,type="none") 

#Then add each site as an ellipse
ordiellipse(stem.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Leon")
ordiellipse(stem.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="red",alpha=200,show.groups="French")
ordiellipse(stem.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark orange",alpha=200,show.groups="Huesta")
ordiellipse(stem.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="light green",alpha=200,show.groups="Salado")
ordiellipse(stem.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="forest green",alpha=200,show.groups="Gov Canyon")
ordiellipse(stem.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark green",alpha=200,show.groups="Gov Canyon Trib")
ordiellipse(stem.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Maverick")
ordiellipse(stem.geo.nmds,groups=creek.name,kind="ehull",draw="polygon",col="dark red",alpha=200,show.groups="Leon Creek Trib")

#Labels for the site ellipses
text(-0.6,-0.8,labels="Gov Canyon Trib")
text(0.8,-0.4,labels="Gov Canyon")
text(0.4,0.2,labels="Salado")
text(-0.55,-0.15,labels="Maverick")
text(-0.8,0.82,labels="Leon")
text(0.1,0.9,labels="Huesta")
text(0.85,0.75,labels="French")
text(-0.32,0,labels="Leon Trib")

#Add the stress value
text(1.3,1, labels="Stress = 0.15",pos=4,font=3)

#Add a legend to the plot
legend(x=-2.6,y=1, legend = c('0','1','12','18','19','41','45'),col = c("dark green","forest green","light green","gold","dark orange","red","dark red"),pch= c(19,19,19,19,19,19,19),pt.bg="dark green",cex=1,pt.cex=1.25,ncol=1,bty="n")
text(-2,1,labels="Percent Impervious")

#add the land use and hydrologcial variables as arrows with labels
arrows(x0=0,y0=0,x1=stem.envfit.scores[2,1], y1=stem.envfit.scores[2,2],lwd=2,col = 'grey0')
text(x=stem.envfit.scores[2,1], y=stem.envfit.scores[2,2],col = 'grey0', labels = 'Flood Frequency',pos=3,font=3)
arrows(x0=0,y0=0,x1=stem.envfit.scores[3,1], y1=stem.envfit.scores[3,2],lwd=2,col = 'grey0')
text(x=stem.envfit.scores[3,1], y=stem.envfit.scores[3,2],col = 'grey0', labels = 'Time Between Floods',pos=4,font=3)
arrows(x0=0,y0=0,x1=stem.envfit.scores[5,1], y1=stem.envfit.scores[5,2],lwd=2,col = 'grey0')
text(x=stem.envfit.scores[5,1], y=stem.envfit.scores[5,2],col = 'grey0', labels = 'Impervious Cover',pos=2,font=3)
arrows(x0=0,y0=0,x1=stem.envfit.scores[6,1], y1=stem.envfit.scores[6,2],lwd=2,col = 'grey0')
text(x=stem.envfit.scores[6,1], y=stem.envfit.scores[6,2],col = 'grey0', labels = 'Mixed Forest',pos=4,font=3)

#stop graphics device
dev.off()

##############Stepwise linear modeling of CWM trait data

###Bring in data

#Trait data
cwm.trait.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(cwm.trait.data)

#Land use and hydrological variables
land.use.hydro.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') #load in data called: land use and water part 2, this has the updated metrics from Dr. Laub
attach(land.use.hydro.data)


###Each set of lines below defines a full model, runs the step procedure, then prints results
#Nonnatives - canopy cover - first line creates the nonnative CWM vector
nonnat.can<-1-native.can
can.nonnat.full<-lm(nonnat.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.nonnat.step<-step(can.nonnat.full,scope=can.nonnat.full,direction="both")
summary(can.nonnat.step)

#Very high drought tolerance - canopy cover
can.dt.vh.full<-lm(dt.vh.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.dt.vh.step<-step(can.dt.vh.full,scope=can.dt.vh.full,direction="both")
summary(can.dt.vh.step)

#High drought tolerance - canopy cover
can.dt.h.full<-lm(dt.h.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.dt.h.step<-step(can.dt.h.full,scope=can.dt.h.full,direction="both")
summary(can.dt.h.step)

#Medium drought tolerance - canopy cover
can.dt.m.full<-lm(dt.m.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.dt.m.step<-step(can.dt.m.full,scope=can.dt.m.full,direction="both")
summary(can.dt.m.step)

#Very high heat tolerance - canopy cover
can.ht.vh.full<-lm(ht.vh.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.ht.vh.step<-step(can.ht.vh.full,scope=can.ht.vh.full,direction="both")
summary(can.ht.vh.step)

#High heat tolerance - canopy cover
can.ht.h.full<-lm(ht.h.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.ht.h.step<-step(can.ht.h.full,scope=can.ht.h.full,direction="both")
summary(can.ht.h.step)

#Medium-high heat tolerance - canopy cover
can.ht.mh.full<-lm(ht.mh.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.ht.mh.step<-step(can.ht.mh.full,scope=can.ht.mh.full,direction="both")
summary(can.ht.mh.step)

#Medium heat tolerance - canopy cover
can.ht.m.full<-lm(ht.m.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.ht.m.step<-step(can.ht.m.full,scope=can.ht.m.full,direction="both")
summary(can.ht.m.step)

#Low water use - canopy cover
can.wu.l.full<-lm(wu.l.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.wu.l.step<-step(can.wu.l.full,scope=can.wu.l.full,direction="both")
summary(can.wu.l.step)

#Medium-low water use - canopy cover
can.wu.lm.full<-lm(wu.lm.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.wu.lm.step<-step(can.wu.lm.full,scope=can.wu.lm.full,direction="both")
summary(can.wu.lm.step)

#Medium water use - canopy cover
can.wu.m.full<-lm(wu.m.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.wu.m.step<-step(can.wu.m.full,scope=can.wu.m.full,direction="both")
summary(can.wu.m.step)

#No anaerobic tolerance - canopy cover
can.at.n.full<-lm(at.n.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.at.n.step<-step(can.at.n.full,scope=can.at.n.full,direction="both")
summary(can.at.n.step)

#Low anaerobic tolerance - canopy cover
can.at.l.full<-lm(at.l.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.at.l.step<-step(can.at.l.full,scope=can.at.l.full,direction="both")
summary(can.at.l.step)

#Medium anaerobic tolerance - canopy cover
can.at.m.full<-lm(at.m.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.at.m.step<-step(can.at.m.full,scope=can.at.m.full,direction="both")
summary(can.at.m.step)

#Resprout ability - canopy cover
can.ra.y.full<-lm(ra.y.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.ra.y.step<-step(can.ra.y.full,scope=can.ra.y.full,direction="both")
summary(can.ra.y.step)

#Propagated by cuttings - canopy cover
can.pbc.y.full<-lm(pbc.y.can~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
can.pbc.y.step<-step(can.pbc.y.full,scope=can.pbc.y.full,direction="both")
summary(can.pbc.y.step)

#Nonnatives - basal area - First line creates the nonnative CWM vector
nonnat.ba<-1-native.ba
ba.nonnat.full<-lm(nonnat.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.nonnat.step<-step(ba.nonnat.full,scope=ba.nonnat.full,direction="both")
summary(ba.nonnat.step)

#Very high drought tolerance - basal area
ba.dt.vh.full<-lm(dt.vh.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.dt.vh.step<-step(ba.dt.vh.full,scope=ba.dt.vh.full,direction="both")
summary(ba.dt.vh.step)

#High drought tolerance - basal area
ba.dt.h.full<-lm(dt.h.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.dt.h.step<-step(ba.dt.h.full,scope=ba.dt.h.full,direction="both")
summary(ba.dt.h.step)

#Medium drought tolerance - basal area
ba.dt.m.full<-lm(dt.m.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.dt.m.step<-step(ba.dt.m.full,scope=ba.dt.m.full,direction="both")
summary(ba.dt.m.step)

#Very high heat tolerance - basal area
ba.ht.vh.full<-lm(ht.vh.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.ht.vh.step<-step(ba.ht.vh.full,scope=ba.ht.vh.full,direction="both")
summary(ba.ht.vh.step)

#High heat tolerance - basal area
ba.ht.h.full<-lm(ht.h.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.ht.h.step<-step(ba.ht.h.full,scope=ba.ht.h.full,direction="both")
summary(ba.ht.h.step)

#Medium-high heat tolerance - basal area
ba.ht.mh.full<-lm(ht.mh.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.ht.mh.step<-step(ba.ht.mh.full,scope=ba.ht.mh.full,direction="both")
summary(ba.ht.mh.step)

#Low water use - basal area
ba.wu.l.full<-lm(wu.l.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.wu.l.step<-step(ba.wu.l.full,scope=ba.wu.l.full,direction="both")
summary(ba.wu.l.step)

#Medium-low water use - basal area
ba.wu.lm.full<-lm(wu.lm.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.wu.lm.step<-step(ba.wu.lm.full,scope=ba.wu.lm.full,direction="both")
summary(ba.wu.lm.step)

#Medium water use - basal area
ba.wu.m.full<-lm(wu.m.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.wu.m.step<-step(ba.wu.m.full,scope=ba.wu.m.full,direction="both")
summary(ba.wu.m.step)

#No anaerobic tolerance - basal area
ba.at.n.full<-lm(at.n.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.at.n.step<-step(ba.at.n.full,scope=ba.at.n.full,direction="both")
summary(ba.at.n.step)

#Low anaerobic tolerance - basal area
ba.at.l.full<-lm(at.l.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.at.l.step<-step(ba.at.l.full,scope=ba.at.l.full,direction="both")
summary(ba.at.l.step)

#Medium anaerobic tolerance - basal area
ba.at.m.full<-lm(at.m.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.at.m.step<-step(ba.at.m.full,scope=ba.at.m.full,direction="both")
summary(ba.at.m.step)

#Resprout ability - basal area
ba.ra.y.full<-lm(ra.y.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.ra.y.step<-step(ba.ra.y.full,scope=ba.ra.y.full,direction="both")
summary(ba.ra.y.step)

#Propagated by cuttings - basal area
ba.pbc.y.full<-lm(pbc.y.ba~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
ba.pbc.y.step<-step(ba.pbc.y.full,scope=ba.pbc.y.full,direction="both")
summary(ba.pbc.y.step)

#Nonnatives - stem count - First line creates the nonnative CWM vector
nonnat.stem<-1-native.stem
stem.nonnat.full<-lm(nonnat.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.nonnat.step<-step(stem.nonnat.full,scope=stem.nonnat.full,direction="both")
summary(stem.nonnat.step)

#Very high drought tolerance - stem count
stem.dt.vh.full<-lm(dt.vh.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.dt.vh.step<-step(stem.dt.vh.full,scope=stem.dt.vh.full,direction="both")
summary(stem.dt.vh.step)

#High drought tolerance - stem count
stem.dt.h.full<-lm(dt.h.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.dt.h.step<-step(stem.dt.h.full,scope=stem.dt.h.full,direction="both")
summary(stem.dt.h.step)

#Medium drought tolerance - stem count
stem.dt.m.full<-lm(dt.m.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.dt.m.step<-step(stem.dt.m.full,scope=stem.dt.m.full,direction="both")
summary(stem.dt.m.step)

#Very high heat tolerance - stem count
stem.ht.vh.full<-lm(ht.vh.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.ht.vh.step<-step(stem.ht.vh.full,scope=stem.ht.vh.full,direction="both")
summary(stem.ht.vh.step)

#High heat tolerance - stem count
stem.ht.h.full<-lm(ht.h.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.ht.h.step<-step(stem.ht.h.full,scope=stem.ht.h.full,direction="both")
summary(stem.ht.h.step)

#Medium-high heat tolerance - stem count
stem.ht.mh.full<-lm(ht.mh.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.ht.mh.step<-step(stem.ht.mh.full,scope=stem.ht.mh.full,direction="both")
summary(stem.ht.mh.step)

#Low water use - stem count
stem.wu.l.full<-lm(wu.l.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.wu.l.step<-step(stem.wu.l.full,scope=stem.wu.l.full,direction="both")
summary(stem.wu.l.step)

#Medium-low water use - stem count
stem.wu.lm.full<-lm(wu.lm.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.wu.lm.step<-step(stem.wu.lm.full,scope=stem.wu.lm.full,direction="both")
summary(stem.wu.lm.step)

#Medium water use - stem count
stem.wu.m.full<-lm(wu.m.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.wu.m.step<-step(stem.wu.m.full,scope=stem.wu.m.full,direction="both")
summary(stem.wu.m.step)

#No anaerobic tolerance - stem count
stem.at.n.full<-lm(at.n.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.at.n.step<-step(stem.at.n.full,scope=stem.at.n.full,direction="both")
summary(stem.at.n.step)

#Low anaerobic tolerance - stem count
stem.at.l.full<-lm(at.l.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.at.l.step<-step(stem.at.l.full,scope=stem.at.l.full,direction="both")
summary(stem.at.l.step)

#Medium anaerobic tolerance - stem count
stem.at.m.full<-lm(at.m.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.at.m.step<-step(stem.at.m.full,scope=stem.at.m.full,direction="both")
summary(stem.at.m.step)

#Resprout ability - stem count
stem.ra.y.full<-lm(ra.y.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.ra.y.step<-step(stem.ra.y.full,scope=stem.ra.y.full,direction="both")
summary(stem.ra.y.step)

#Propagated by cuttings - stem count
stem.pbc.y.full<-lm(pbc.y.stem~AvgPeriodBtwnFloods+FloodFrequency+AvgFloodDuration+WatershedAreaSqKm+PctImpervious2019Ws+PctMxFst2019Ws+PctAg2019WS)
stem.pbc.y.step<-step(stem.pbc.y.full,scope=stem.pbc.y.full,direction="both")
summary(stem.pbc.y.step)

###############NMDS and environmental fitting of CWM trait data at geomorphic surface level

###Canopy cover

#Load required package
library(vegan)

#Load data
cwm.trait.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(cwm.trait.data)

#Define trait by site matrix
cwm.trait.mtx.can<-cwm.trait.data[,c(4:7,9:12,15:17,18:20,22,23)]
rownames(cwm.trait.mtx.can)=creeksample

#Run the NMDS model
cwm.trait.nmds.can<-metaMDS(cwm.trait.mtx.can, distance = 'bray',autotransform=FALSE)
cwm.trait.nmds.can

##Fit landuse and hydrological variables to NMDS
#First bring in the land use and hydrological data
land.use.hydro.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') #load in data called: land use and water part 2, this has the updated metrics from Dr. Laub
attach(land.use.hydro.data)

#Then define the land use and hydrological variable matrix
land.use.hydro.mtx.can<-data.frame(land.use.hydro.data[,c(12,11,10,14,20,24,29)],row.names=creeksample)

#Then conduct the environmental fitting analysis
env.fit.can.trait<-envfit(cwm.trait.nmds.can,land.use.hydro.mtx.can) 
env.fit.can.trait 

#Store the environmental fitting scores for later plotting
can.envfit.trait.scores<-scores(env.fit.can.trait,display="vectors")

#Permanova analysis by site
perm.result.can.trait<-adonis2(cwm.trait.mtx.can~creekname, method = 'bray')
perm.result.can.trait

##Making an ordination plot of the canopy cover NMDS with significant land use and hydrological variables 

#set up exporting as a tiff
tiff(filename="C:\\Users\\laubs\\Documents\\Figure5a_traitNMDScan.tif", width=7.8, height=5, units="in", pointsize=12, res=500)

#First define a blank plot
ordiplot(cwm.trait.nmds.can,type="none",xlim=c(-1,1),ylim=c(-0.5,0.5))

#Then add each site as an ellipse
ordiellipse(cwm.trait.nmds.can,groups=creekname,kind="ehull",draw="polygon",col="dark orange",alpha=200,show.groups="Huesta")
ordiellipse(cwm.trait.nmds.can,groups=creekname,kind="ehull",draw="polygon",col="red",alpha=200,show.groups="French")
ordiellipse(cwm.trait.nmds.can,groups=creekname,kind="ehull",draw="polygon",col="light green",alpha=200,show.groups="Salado")
ordiellipse(cwm.trait.nmds.can,groups=creekname,kind="ehull",draw="polygon",col="dark red",alpha=200,show.groups="LeonCreekTrib")
ordiellipse(cwm.trait.nmds.can,groups=creekname,kind="ehull",draw="polygon",col="dark green",alpha=200,show.groups="GovernmentCanyonTrib")
ordiellipse(cwm.trait.nmds.can,groups=creekname,kind="ehull",draw="polygon",col="forest green",alpha=200,show.groups="GovernmentCanyon")
ordiellipse(cwm.trait.nmds.can,groups=creekname,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="LeonCreek")
ordiellipse(cwm.trait.nmds.can,groups=creekname,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Maverick")

#Labels for the site ellipses
text(0.12,-0.18,labels="Gov Canyon Trib")
text(-0.25,0.18,labels="Gov Canyon")
text(0.4,0.03,labels="Salado")
text(-0.04,0.05,labels="Maverick")
text(0.18,0.24,labels="Leon")
text(0.46,0.21,labels="Huesta")
text(0.61,-0.15,labels="French")
text(-0.55,-0.06,labels="Leon Trib")

#Add the stress value
text(0.7,0.47, labels="Stress = 0.11",pos=4,font=3)

#Add a legend to the plot
legend(x=-1.1,y=0.47, legend = c('0','1','12','18','19','41','45'),col = c("dark green","forest green","light green","gold","dark orange","red","dark red"),pch= c(19,19,19,19,19,19,19),pt.bg="dark green",cex=1,pt.cex=1.25,ncol=1,horiz=FALSE,bty="n")
text(-0.825,0.47,labels="Percent Impervious")

#add the land use and hydrologcial variables as arrows with labels
arrows(x0=0,y0=0,x1=can.envfit.trait.scores[1,1], y1=can.envfit.trait.scores[1,2],lwd=2,col = 'grey0')
text(x=can.envfit.trait.scores[1,1], y=can.envfit.trait.scores[1,2],col = 'grey0', labels = 'Time',font=3,adj=c(2,0))
text(x=can.envfit.trait.scores[1,1], y=can.envfit.trait.scores[1,2],col = 'grey0', labels = 'Between Floods',font=3,adj=c(1.03,1.2))
arrows(x0=0,y0=0,x1=can.envfit.trait.scores[2,1], y1=can.envfit.trait.scores[2,2],lwd=2,col = 'grey0')
text(x=can.envfit.trait.scores[2,1], y=can.envfit.trait.scores[2,2],col = 'grey0', labels = 'Flood',font=3,adj=c(0.5,1.2))
text(x=can.envfit.trait.scores[2,1], y=can.envfit.trait.scores[2,2],col = 'grey0', labels = 'Frequency',font=3,adj=c(0.5,2.2))
arrows(x0=0,y0=0,x1=can.envfit.trait.scores[4,1], y1=can.envfit.trait.scores[4,2],lwd=2,col = 'grey0')
text(x=can.envfit.trait.scores[4,1], y=can.envfit.trait.scores[4,2],col = 'grey0', labels = 'Watershed Area',font=3,adj=c(-0.05,1))
arrows(x0=0,y0=0,x1=can.envfit.trait.scores[5,1], y1=can.envfit.trait.scores[5,2],lwd=2,col = 'grey0')
text(x=can.envfit.trait.scores[5,1], y=can.envfit.trait.scores[5,2],col = 'grey0', labels = 'Impervious Cover',adj=c(1.1,-0.5),font=3)
arrows(x0=0,y0=0,x1=can.envfit.trait.scores[6,1], y1=can.envfit.trait.scores[6,2],lwd=2,col = 'grey0')
text(x=can.envfit.trait.scores[6,1], y=can.envfit.trait.scores[6,2],col = 'grey0', labels = 'Mixed Forest',font=3,adj=c(0.2,1.2))
arrows(x0=0,y0=0,x1=can.envfit.trait.scores[7,1], y1=can.envfit.trait.scores[7,2],lwd=2,col = 'grey0')
text(x=can.envfit.trait.scores[7,1], y=can.envfit.trait.scores[7,2],col = 'grey0', labels = 'Agriculture',pos=2,font=3)

#stop graphics device
dev.off()

###Basal Area

#Load required package
library(vegan)

#Load data
cwm.trait.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(cwm.trait.data)

#Define trait by site matrix
cwm.trait.mtx.ba<-cwm.trait.data[c(1,4:13,15:18,20:41,43:45),c(24:27,29:31,35:37,38:40,42,43)]
rownames(cwm.trait.mtx.ba.hybrid.nomulti.plusnat)=creeksample[c(1,4:13,15:18,20:41,43:45)]

#New site vector for basal area and stem count analysis since some rows were excluded
creekname.bastem<-creekname[c(1,4:13,15:18,20:41,43:45)]

#Run the NMDS model
cwm.trait.nmds.ba<-metaMDS(cwm.trait.mtx.ba, distance = 'bray',autotransform=FALSE)
cwm.trait.nmds.ba

##Fit landuse and hydrological variables to NMDS
#First bring in the land use and hydrological data
land.use.hydro.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') #load in data called: land use and water part 2, this has the updated metrics from Dr. Laub
attach(land.use.hydro.data)

#Then define the land use and hydrological variable matrix
land.use.hydro.mtx.bastem<-data.frame(land.use.hydro.data[c(1,4:13,15:18,20:41,43:45),c(12,11,10,14,20,24,29)],row.names=creeksample[c(1,4:13,15:18,20:41,43:45)])

#Then conduct the environmental fitting analysis
env.fit.ba.trait<-envfit(cwm.trait.nmds.ba,land.use.hydro.mtx.bastem) 
env.fit.ba.trait

#Store the environmental fitting scores for later plotting
ba.envfit.trait.scores<-scores(env.fit.ba.trait,display="vectors")

#Permanova analysis by site
perm.result.ba.trait<-adonis2(cwm.trait.mtx.ba~creekname[c(1,4:13,15:18,20:41,43:45)], method = 'bray')
perm.result.ba.trait

##Making an ordination plot of the canopy cover NMDS with significant land use and hydrological variables 

#set up exporting as a tiff
tiff(filename="C:\\Users\\laubs\\Documents\\Figure5b_traitNMDSba.tif", width=7.8, height=5, units="in", pointsize=12, res=500)

#First define a blank plot
ordiplot(cwm.trait.nmds.ba,type="none",xlim=c(-1,1),ylim=c(-0.5,0.5))

#Then add each site as an ellipse
ordiellipse(cwm.trait.nmds.ba,groups=creekname.bastem,kind="ehull",draw="polygon",col="light green",alpha=200,show.groups="Salado")
ordiellipse(cwm.trait.nmds.ba,groups=creekname.bastem,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="LeonCreek")
ordiellipse(cwm.trait.nmds.ba,groups=creekname.bastem,kind="ehull",draw="polygon",col="red",alpha=200,show.groups="French")
ordiellipse(cwm.trait.nmds.ba,groups=creekname.bastem,kind="ehull",draw="polygon",col="dark red",alpha=200,show.groups="LeonCreekTrib")
ordiellipse(cwm.trait.nmds.ba,groups=creekname.bastem,kind="ehull",draw="polygon",col="dark orange",alpha=200,show.groups="Huesta")
ordiellipse(cwm.trait.nmds.ba,groups=creekname.bastem,kind="ehull",draw="polygon",col="dark green",alpha=200,show.groups="GovernmentCanyonTrib")
ordiellipse(cwm.trait.nmds.ba,groups=creekname.bastem,kind="ehull",draw="polygon",col="forest green",alpha=200,show.groups="GovernmentCanyon")
ordiellipse(cwm.trait.nmds.ba,groups=creekname.bastem,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Maverick")

#Labels for the site ellipses
text(0.3,0.28,labels="Gov Canyon Trib")
text(-0.5,0.0,labels="Gov Canyon")
text(0.55,-0.07,labels="Salado")
text(-0.05,0.05,labels="Maverick")
text(-0.28,0.25,labels="Leon")
text(0.15,-0.11,labels="Huesta")
text(-0.12,-0.3,labels="French")
text(-0.55,0.15,labels="Leon Trib")

#Add the stress value
text(0.7,0.45, labels="Stress = 0.11",pos=4,font=3)

#Add a legend to the plot
legend(x=-1.1,y=0.43, legend = c('0','1','12','18','19','41','45'),col = c("dark green","forest green","light green","gold","dark orange","red","dark red"),pch= c(19,19,19,19,19,19,19),pt.bg="dark green",cex=1,pt.cex=1.25,ncol=1,horiz=FALSE,bty="n")
text(-0.82,0.43,labels="Percent Impervious")

#add the land use and hydrologcial variables as arrows with labels
arrows(x0=0,y0=0,x1=ba.envfit.trait.scores[1,1], y1=ba.envfit.trait.scores[1,2],lwd=2,col = 'grey0')
text(x=ba.envfit.trait.scores[1,1], y=ba.envfit.trait.scores[1,2],col = 'grey0', labels = 'Time Between Floods',font=3,adj=c(-0.05,0))
arrows(x0=0,y0=0,x1=ba.envfit.trait.scores[6,1], y1=ba.envfit.trait.scores[6,2],lwd=2,col = 'grey0')
text(x=ba.envfit.trait.scores[6,1], y=ba.envfit.trait.scores[6,2],col = 'grey0', labels = 'Mixed Forest',font=3,adj=c(0,1))

#stop graphics device
dev.off()

###Stem Count

#Load required package
library(vegan)

#Load data
cwm.trait.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') 
attach(cwm.trait.data)

#Define trait by site matrix
cwm.trait.mtx.stem<-cwm.trait.data[c(1,4:13,15:18,20:41,43:45),c(44:47,49:51,55:57,58:60,62,63)]
rownames(cwm.trait.mtx.stem)=creeksample[c(1,4:13,15:18,20:41,43:45)]

#New site vector for basal area and stem count analysis since some rows were excluded
creekname.bastem<-creekname[c(1,4:13,15:18,20:41,43:45)]

#Run the NMDS model
cwm.trait.nmds.stem<-metaMDS(cwm.trait.mtx.stem, distance = 'bray',autotransform=FALSE)
cwm.trait.nmds.stem

##Fit landuse and hydrological variables to NMDS
#First bring in the land use and hydrological data
land.use.hydro.data<-read.csv(file.choose(), header = TRUE, fileEncoding = 'UTF-8-BOM') #load in data called: land use and water part 2, this has the updated metrics from Dr. Laub
attach(land.use.hydro.data)

#Then define the land use and hydrological variable matrix
land.use.hydro.mtx.bastem<-data.frame(land.use.hydro.data[c(1,4:13,15:18,20:41,43:45),c(12,11,10,14,20,24,29)],row.names=creeksample[c(1,4:13,15:18,20:41,43:45)])

#Then conduct the environmental fitting analysis
env.fit.stem.trait<-envfit(cwm.trait.nmds.stem,land.use.hydro.mtx.bastem)
env.fit.stem.trait

#Store the environmental fitting scores for later plotting
stem.envfit.trait.scores<-scores(env.fit.stem.trait,display="vectors")

#Permanova analysis by site
perm.result.stem.trait<-adonis2(cwm.trait.mtx.stem~creekname[c(1,4:13,15:18,20:41,43:45)], method = 'bray')
perm.result.stem.trait

##Making an ordination plot of the canopy cover NMDS with significant land use and hydrological variables 

#set up exporting as a tiff
tiff(filename="C:\\Users\\laubs\\Documents\\Figure5c_traitNMDSstem.tif", width=7.8, height=5, units="in", pointsize=12, res=500)

#First define a blank plot
ordiplot(cwm.trait.nmds.stem,type="none",ylim=c(-1,0.5))

#Then add each site as an ellipse
ordiellipse(cwm.trait.nmds.stem,groups=creekname.bastem,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="LeonCreek")
ordiellipse(cwm.trait.nmds.stem,groups=creekname.bastem,kind="ehull",draw="polygon",col="forest green",alpha=200,show.groups="GovernmentCanyon")
ordiellipse(cwm.trait.nmds.stem,groups=creekname.bastem,kind="ehull",draw="polygon",col="red",alpha=200,show.groups="French")
ordiellipse(cwm.trait.nmds.stem,groups=creekname.bastem,kind="ehull",draw="polygon",col="dark orange",alpha=200,show.groups="Huesta")
ordiellipse(cwm.trait.nmds.stem,groups=creekname.bastem,kind="ehull",draw="polygon",col="light green",alpha=200,show.groups="Salado")
ordiellipse(cwm.trait.nmds.stem,groups=creekname.bastem,kind="ehull",draw="polygon",col="dark red",alpha=200,show.groups="LeonCreekTrib")
ordiellipse(cwm.trait.nmds.stem,groups=creekname.bastem,kind="ehull",draw="polygon",col="dark green",alpha=200,show.groups="GovernmentCanyonTrib")
ordiellipse(cwm.trait.nmds.stem,groups=creekname.bastem,kind="ehull",draw="polygon",col="gold",alpha=200,show.groups="Maverick")

#Labels for the site ellipses
text(0.65,-0.2,labels="Gov Canyon Trib")
text(-1.2,0.2,labels="Gov Canyon")
text(0.2,0.1,labels="Salado")
text(-0.12,-0.1,labels="Maverick")
text(-0.17,-0.4,labels="Leon")
text(-0.15,0.3,labels="Huesta")
text(0.48,0.27,labels="French")
text(-0.5,0.22,labels="Leon Trib")

#Add the stress value
text(0.7,0.45, labels="Stress = 0.12",pos=4,font=3)

#Add a legend to the plot
legend(x=-1.95,y=0.45, legend = c('0','1','12','18','19','41','45'),col = c("dark green","forest green","light green","gold","dark orange","red","dark red"),pch= c(19,19,19,19,19,19,19),pt.bg="dark green",cex=1,pt.cex=1.25,ncol=1,horiz=FALSE,bty="n")
text(-1.55,0.46,labels="Percent Impervious")

#add the land use and hydrologcial variables as arrows with labels
arrows(x0=0,y0=0,x1=stem.envfit.trait.scores[7,1], y1=stem.envfit.trait.scores[7,2],lwd=2,col = 'grey0')
text(x=stem.envfit.trait.scores[7,1], y=stem.envfit.trait.scores[7,2],col = 'grey0', labels = 'Percent Agriculture',adj=c(1,1),font=3)

#stop graphics device
dev.off()




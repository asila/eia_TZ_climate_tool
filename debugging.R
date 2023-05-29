Thresholds<-data.table(
  Variable=hazards,
  Renamed=hazards,
  Threshold=c(haz_class[description=="Moderate"][match(gsub("_max","",hazards[1:8]),index_name)][,lower_lim]),
  Direction=rep(">",8)
)

Analysis_Vars<-Thresholds$Renamed[c(1,4,7)]

FileName<-  gsub("<","L",gsub(">","G",paste(unlist(Thresholds[,!"Renamed"]),collapse="")))
FileName2<-  gsub("<","L",gsub(">","G",paste(unlist(Thresholds[Renamed %in% Analysis_Vars,!"Renamed"]),collapse="")))


Hazards<- HazardWrapper(Thresholds=Thresholds,
                FileName=FileName,
                SaveDir=SaveDir,
                PropThreshold=0.5,
                PropTDir=">",
                hazard_dir=hazard_dir,
                Scenarios=Scenarios)

HazComb<- HazCombWrapper(Hazards=Hazards,
                 SaveDir=SaveDir,
                 Scenarios=Scenarios,
                 FileName=FileName2,
                 SelectedHaz = Analysis_Vars)

HazComb$MeanHaz$`historic-historic`$Classes
HazComb$MeanHaz$`historic-historic`$RastReclass

Scenario<-paste0(Scenarios$Scenario,"-",Scenarios$Time)[1]
Admin2<-Geographies$NAME_2
AdminLevel1<-"District"


HazXRegion<-HazXRegionWrapper(SaveDir=SaveDir,
                    FileName=FileName2,
                    SPAM=terra::rast("Data/mapspam/mapspam_crops.tif"),
                    Geographies=Geographies,
                    Cropland=Cropland,
                    TotalPop=TotalPop,
                    HazComb=HazComb)

Palette<-"turbo"

HazPalCombMean<-PalFun(PalName=Palette,
                                 N=nrow(HazComb[["MeanHaz"]][[Scenario]][["Classes"]]),
                                 Names=HazComb[["MeanHaz"]][[Scenario]][["Classes"]])


HazPalCombProp<-PalFun(PalName=Palette,
                                 N=nrow(HazComb[["PropHaz"]][[Scenario]][["Classes"]]),
                                 Names=HazComb[["PropHaz"]][[Scenario]][["Classes"]])


addGeog1<-function(){terra::plot(terra::aggregate(Geographies[Geographies$NAME_2 %in% Admin2,],by="NAME_1"),add=T,border="black",lwd=borderwidth)}
addGeog2<-function(){terra::plot(Geographies[Geographies$NAME_2 %in% Admin2,],add=T,border="black",lwd=borderwidth)}

borderwidth<-1
Plot_Vars<-Analysis_Vars
TextSize<-1


PlotHazards<-
  terra::mask(
    terra::crop(x = Hazards[[Scenario]], Geographies[Geographies$NAME_2 %in% Admin2,]),
    Geographies[Geographies$NAME_2 %in% Admin2,]
  )

terra::plot(PlotHazards[[paste0(Plot_Vars,"_mean")]],
            fun=if(AdminLevel1=="District"){addGeog2}else{addGeog1},
            cex.main = TextSize*1.2,
            col=PalFun(PalName=Palette,
                       N=50,
                       Names=1:50))

SaveFile<-paste0(SaveDir,"/Tables-",FileName2,".RData")
HazXRegion<-miceadds::load.Rdata2(SaveFile)



Thresholds<-data.table(
  Variable=hazards,
  Renamed=hazards,
  Threshold=c(haz_class[description=="Severe"][match(gsub("_max","",hazards[1:8]),index_name)][,lower_lim]),
  Direction=rep(">",8)
)

Analysis_Vars<-Thresholds$Renamed[c(1,3,4)]

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
Admin1<-unique(Geographies$NAME_2)
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

HazXRegion<-
  HazXRegionWrapper(SaveDir=SaveDir,
                    FileName=FileName2,
                    SPAM=terra::rast("Data/mapspam/mapspam_crops.tif"),
                    Cropland=Cropland,
                    TotalPop=TotalPop,
                    Geographies=Geographies,
                    HazComb=HazComb)

AdminLevel<-if(AdminLevel1=="Region"){
    "Admin1"
  }else{
    "Admin2"
  }


SubGeog<-if(AdminLevel1=="District"){
    Geographies[Geographies$NAME_2 %in% Admin2,]
  }else{
    Geographies[Geographies$NAME_1 %in% Admin1,]
  }





SxRtab<-
  SxRtabFun(Hazards=PlotHazards,
            Plot_Vars=Plot_Vars,
            ExtractBy = SubGeog_rast)

HazMeth<-"MeanHaz"

DT_Data2<-PrepTable(Data=HazXRegion,
          Method=HazMeth,
          Scenario=Scenario,
          AdminLevel=AdminLevel,
          Geographies=Geographies,
          A1=Admin1,
          A2=Admin2,
          Table="SPAM")[,Area:=round(Area/100,2)]



MakeWide<-"Yes"
MinArea<-50


DT_Data2[,Code:=paste(Admin1,Admin2,Hazard,Crop)][,N:=.N,by=Code]
DT_Data2[N>1][order(Code)]

Check<-HazXRegion[[HazMeth]][[Scenario]][[AdminLevel]][["SPAM"]]
Check[,Code:=paste(Admin2,Hazard,Crop)][,N:=.N,by=Code]
Check[N>1][order(Code)]


X<-if(MakeWide=="Yes"){
    if(AdminLevel=="Admin2"){
      dcast(DT_Data2[Area>MinArea,Area:=round(Area,3)],Admin1+Admin2+Crop~Hazard,value.var="Area")
    }else{
      dcast(DT_Data2[Area>MinArea,Area:=round(Area,3)],Admin1+Crop~Hazard,value.var="Area") 
    }
  }else{
    DT_Data2()[,Area:=round(Area,3)]
  }



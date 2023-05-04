### R code of Quantifying the paleoclimate of the Middle Jurassic Yanliao Biota from biological data##


#/////Chapter1 Insect data/////#
##/////Part1 Pre-processing/////##
###/////input insect data/////###
data_H<-read.csv("~Hydrophytic.csv")# the data of table S1
IH<-data_H[which(rowSums(data_H==0)==0),]
###/////END/////###

###/////data processing/////###
IH$K<-IH$weight^(-0.75)
S<-sum(IH$K)
IH$RNumber<-IH$K/S
IH$Rbiomass<-IH$weight*IH$RNumber
IH1<-data.frame(IH[grep("?phytophagy",IH$Feeding),])
write.csv(IH1,"~IH1.csv")#Get phytophagy insect relative biomass
###/////END/////###

##/////Part2 calculate and plot the data/////##
###/////calculate and plot/////###
{
  IH.new<-read.csv("~IH1.csv")
  class_H<-IH.new$Order
  n_H<-length(levels(class_H))
  logmean_WH<-tapply(log10(IH.new$weight),class_H,mean)
  logsd_WH<-tapply(log10(IH.new$weight),class_H,sd)
  logmean_RBH<-tapply(log10(IH.new$Rbiomass),class_H,mean)
  logsd_RBH<-tapply(log10(IH.new$Rbiomass),class_H,sd)
}#first process
{
  set.seed(1)
  logdata_WH<-list()
  for(i in 1:n_H){
    if(table(class_H)[i]>1){
      logdata_WH[[i]]<-rnorm(100000,mean=logmean_WH[i],sd=logsd_WH[i])
      names(logdata_WH)[i]<-levels(class_H)[i]
      #setwd("~") 
      #pdf(file = paste0(names(logdata_WH[i]),"_","weight", "_", ".pdf"))
      #hist(logdata_WH[[i]],main=paste("Histogram of",names(logdata_WH[i]),"weight"),
      #     breaks=30,freq=F)
      #curve(dnorm(x,mean=logmean_WH[i],sd=logsd_WH[i]), 
      #      add=TRUE,col="darkblue",lwd=3)
    }
  }
  #dev.off()
}#process and plot weight
{
  set.seed(2)
  logdata_RBH<-list()
  for(i in 1:n_H){
    if(table(class_H)[i]>1){
      logdata_RBH[[i]]<-rnorm(100000,mean=logmean_RBH[i],sd=logsd_RBH[i])
      names(logdata_RBH)[i]<-levels(class_H)[i]
      #setwd("~")
      #pdf(file = paste0(names(logdata_RBH[i]),"_","RBiomass","_", ".pdf"))
      #hist(logdata_RBH[[i]],
      #     main=paste("Histogram of",names(logdata_RBH[i]),"RBiomass"),
      #     breaks=30,freq=F)
      #curve(dnorm(x,mean=logmean_RBH[i],sd=logsd_RBH[i]), 
      #      add=TRUE,col="darkblue",lwd=3)
    }
  }
  #dev.off()
}#process and plot relative biomass
{
  a_H<-list()
  logRBI_H<-0
  for(i in 1:n_H){
    if(table(class_H)[i]>1){
      a_H[[i]]<-log10(table(class_H)[i])+logdata_RBH[[i]]
    }else{
      a_H[[i]]<-logmean_RBH[i]
    }
    logRBI_H<-log10(10^(logRBI_H)+10^(a_H[[i]]))
  }
  #setwd("~")
  #pdf(file = paste0("logRBI", "_", ".pdf"))
  #hist(logRBI_H,main=paste("Histogram of","logRBI"),breaks=35,freq=F)
  #curve(dnorm(x,mean=mean(logRBI_H),sd=sd(logRBI_H)), 
  #      add=TRUE,col="darkblue",lwd=3)  #The relative biomass distribution of phytophagous insects 
  #dev.off()
  
}#RBI of phytophagyous insects community by Monte carlo 

{
  {
    set.seed(3)
    data_ID<-runif(100,min=000,max=1700)
    logID<-rnorm(100000,mean=mean(log10(data_ID)),sd=sd(log10(data_ID)))
    #setwd("~")
    #pdf(file = paste0("logID", "_", ".pdf"))
    #hist(logID,main=paste("Histogram of","logID"),breaks=30,freq=F)
    #curve(dnorm(x,mean=mean(log10(data_ID)),sd=sd(log10(data_ID))),
    #      add=TRUE,col="darkblue",lwd=3)
    #dev.off()
  }#Total insect density estimated by Monte Carlo
  {
    set.seed(4)
    data_FR<-runif(100,min=0.051,max=0.13)
    logFR<-rnorm(100000,mean=mean(log10(data_FR)),sd=sd(log10(data_FR)))
    #setwd("~")
    #pdf(file = paste0("logFR", "_", ".pdf"))
    #hist(logFR,main=paste("Histogram of","logFR"),breaks=30,freq=F)
    #curve(dnorm(x,mean=mean(log10(data_FR)),sd=sd(log10(data_FR))), 
    #     add=TRUE,col="darkblue",lwd=3)
    #dev.off()
  }#Insect feeding rate estimated by Monte Carlo
  {
    set.seed(5)
    data_AR<-runif(100,min=1.4,max=10.3)
    logAR<-rnorm(100000,mean=mean(log10(data_AR)),sd=sd(log10(data_AR)))
    #setwd("~")
    #pdf(file = paste0("logAR", "_", ".pdf"))
    #hist(logAR,main=paste("Histogram of","logAR"),breaks=30,freq=F)
    #curve(dnorm(x,mean=mean(log10(data_AR)),sd=sd(log10(data_AR))), 
    #     add=TRUE,col="darkblue",lwd=3)
    #dev.off()
  }#Insect accumulation rate estimated by Monte Carlo
  {
    logTR<-log10(0.1)
  }#Insect translate rate  
}#parameter estimate
{
  logIB_H<-logRBI_H+logID  #Insect biomass
  logCB_H<-logIB_H-logTR  #Consumption biomass
  logNPP_H<-logCB_H-logFR  #Production biomass
  logVB_H<-logNPP_H+logAR  #vegetation biomass
}#calculate logVB of H
{
  setwd("~")
  pdf(file = paste0("logIB", "_", ".pdf"))
  hist(logIB_H,main=paste("Histogram of","logIB"),breaks=30,freq=F)
  curve(dnorm(x,mean=mean(logIB_H),sd=sd(logIB_H)), 
        add=TRUE,col="darkblue",lwd=3)
  pdf(file = paste0("logCB", "_", ".pdf"))
  hist(logCB_H,main=paste("Histogram of","logCB"),breaks=50,freq=F)
  curve(dnorm(x,mean=mean(logCB_H),sd=sd(logCB_H)), 
        add=TRUE,col="darkblue",lwd=3)
  pdf(file = paste0("logNPP", "_", ".pdf"))
  hist(logNPP_H,main=paste("Histogram of","logNPP"),breaks=160,freq=F)
  curve(dnorm(x,mean=mean(logNPP_H),sd=sd(logNPP_H)), 
        add=TRUE,col="darkblue",lwd=3)
  pdf(file = paste0("logVB", "_", ".pdf"))
  hist(logVB_H,main=paste("Histogram of","logVB"),breaks=100,freq=F)
  curve(dnorm(x,mean=mean(logVB_H),sd=sd(logVB_H)), 
        add=TRUE,col="darkblue",lwd=3)
  dev.off()
}#plot the logIB/logCB/logPB/logVB of H
###/////END/////###
################################################################




################################################################

#/////Chapter2 climate data/////#
##/////Part1 process the data/////##
{
  library(raster)
  {
    lon<-c(-180:180)
    lat<-c(-90:90)
    no<-length(lon)
    na<-length(lat)
    ro<-rep(lon,na)
    ra<-rep(lat,no)
    tra<-array(ra,dim=c(181,361))
    a_1<-t(tra)
    a_2<-array(a_1,dim=c(1,65341))
    x1<-rbind(ro,a_2)
    tude<-t(x1)
    colnames(tude)<-c("long","lat")
  }#Generate coordinates with a resolution of 1 
  {
    {
      setwd("~")#the data of climate 
      B<-list.files(path=getwd(),pattern="tif$",full.names=TRUE)
      b1<-raster(B[1])
      b2<-raster(B[2])
      b3<-raster(B[3])
      b4<-raster(B[4])
      b<-overlay(b1,b3,fun=function(x,y){(x+y)/0.5})#need costing about 25 minute
      vb<-extract(b,tude)
    }#process of biomass
    {
      setwd("~/")
      A<-list.files(path=getwd(),pattern="tif$",full.names=TRUE)
      a<-raster(A)
      va<-extract(a,tude)
    }#process of altitude
    {
      setwd("~/")
      T<-list.files(path=getwd(),pattern="tif$",full.names=TRUE)
      t<-raster(T)
      vt<-extract(t,tude)
    }#process of MAT
    {
      setwd("~/")
      P<-list.files(path=getwd(),pattern="tif$",full.names=TRUE)
      p<-raster(P)
      vp<-extract(p,tude)
    }#process of MAP
  }#process of climate
  {
    data_clim.raster<-cbind(tude,vt,vp,va,vb)
    data_clim.raster<-data_clim.raster[complete.cases(data_clim.raster),]#delete NA
    data_clim.raster<-subset(data_clim.raster,data_clim.raster[,6]>0)#delete 0 of biomass
    colnames(data_clim.raster)<-c("long","lat","MAT","MAP","Altitude","Biomass")
  }#climate cbind
  #write.csv(data_clim.raster,"~/data_3d.csv")
}#process of climate
#/////END/////##

##/////Part2/////##
{
  {
    library(ggplot2)
    library(raster)
    library(Rmisc)
    data<-read.csv("~/data_3d.csv")
    M1<-data[,4:6]
    M2<-data[,4:7]
    M2<-M2[,-3]
    #data<-as.data.frame(data_clim.raster)
    #M1<-data[,3:5]
    #M2<-data[,3:6]
    #M2<-M2[,-3]
    colnames(M1)<-c("x","y","z")
    colnames(M2)<-c("x","y","z")
  }#raw data of climate and prepare
  {
    C_H<-subset(M2,
                M2$z<=10^(mean(logVB_H)+sd(logVB_H)-3)&
                  M2$z>=10^(mean(logVB_H)-sd(logVB_H)-3))
    data_mat<-seq(min(data$MAT),max(data$MAT),(max(data$MAT)-min(data$MAT))/12)
    data_map<-seq(min(data$MAP),max(data$MAP),(max(data$MAP)-min(data$MAP))/12)
    out.clH<-NULL
    for(i in 1:nrow(C_H)){
      data.clH<-c(data_mat[which.min(abs(data_mat-C_H$x[i]))],
                  data_map[which.min(abs(data_map-C_H$y[i]))])
      out.clH<-rbind(out.clH,data.clH)
    }
    out.clH<-as.data.frame(out.clH)
    out<-aggregate(cbind(out.clH[0],num=1),out.clH,length)
    colnames(out)<-c("MAT","MAP","number")
    Cl_H<-extract(raster(MBA::mba.surf(M1, 120, 120, extend=TRUE)$xyz.est),out[,1:2])
    data_CH<-cbind(out[,1:2],Cl_H,out[,3])
    colnames(data_CH)<-c("MAT","MAP","Altitude","number")
  }#output data_CH for point on 3D
  {
    library(plot3D)
    mba.int1<- MBA::mba.surf(M1, 4, 4,extend=TRUE)$xyz.est
    mba.int2<- MBA::mba.surf(M2, 4, 4,extend=TRUE)$xyz.est
    #setwd("~")
    #pdf(file = paste0("Climate3D","-","biomass","-",".pdf"))
    p1<-persp3D(x=mba.int1[[1]],y=mba.int1[[2]],z=mba.int1[[3]],
                clab=c("Vegetation biomass","g/m2"),colkey=list(side=4,length=0.5),
                colvar=mba.int2[[3]],facets=F,curtain=T,ltheta=-10,phi=17, theta=110,
                xlab="MAT",ylab="MAP",zlab="Altitude",ticktype="detailed",bty="u",resfac=15,
                main="Relationship between climate and vegetation biomass")
    
    points(trans3d(data_CH$MAT,data_CH$MAP,data_CH$Altitude,pmat=p1),
           col="red",pch=20,cex=scale(data_CH$number))
  }#plot 3D and point
  {
    {
      V_Cl_H<-extract(raster(MBA::mba.surf(M1, 120, 120, extend=TRUE)$xyz.est),C_H[,1:2])
      V_data_CH<-cbind(C_H[,1:2],V_Cl_H)
      colnames(V_data_CH)<-c("MAT","MAP","Altitude")
      write.csv(V_data_CH,"~/V_data_CH.csv")
    }#data for vioplot
    {
      library(vioplot)
      #setwd("~")
      {
        #pdf(file = paste0("Vioplot","-","MAT in Daohugou","_", ".pdf"))
        vT<-vioplot(V_data_CH$MAT,col="red",main="MAT in Daohugou",
                    names=c("MAT of H"))
      }#Vioplot of MAT
      {
        #pdf(file = paste0("Vioplot","-","MAP in Daohugou","_", ".pdf"))
        vP<-vioplot(V_data_CH$MAP,col="red",main="MAP in Daohugou",
                    names=c("MAP of H"))
      }#Vioplot of MAP
      {
        #pdf(file = paste0("Vioplot","-","Altitude in Daohugou","_", ".pdf"))
        vA<-vioplot(V_data_CH$Altitude,col="red",main="Altitude in Daohugou",
                    names=c("Altitude of H"))
      }#Vioplot of Altitude
    }#output the vioplot of climate
  }#raster climate
  
}#process of rastering climate
#/////END/////##

##/////Part3/////##
{
  data<-read.csv("~/data_3d.csv")
  data<-data[,4:7]
  B1<-which(data$Biomass<500)
  B2<-which(data$Biomass<1000&data$Biomass>=500)
  B3<-which(data$Biomass<2000&data$Biomass>=1000)
  B4<-which(data$Biomass<3000&data$Biomass>=2000)
  B5<-which(data$Biomass<4000&data$Biomass>=3000)
  B6<-which(data$Biomass>=4000)
  B<-list(B1,B2,B3,B4,B5,B6)
  names(B)<-c("B1","B2","B3","B4","B5","B6")
  
  tol.sum<-colSums(data)
  data.f<-cbind(data[,1]/tol.sum[1],data[,2]/tol.sum[2],data[,3]/tol.sum[3])
  colnames(data.f)<-colnames(data[,1:3])
  rownames(data.f)<-data[,4]
  
  set.seed(001)
  for.nmds <- NULL
  nmds.names<-NULL
  for(i in 1:length(B)){
    j=0
    while(j<500){
      samp.row<-sample(B[[i]],500,T)
      this.data<-data.f[samp.row,]
      this.sums<-colSums(this.data)
      for.nmds<-rbind(for.nmds,this.sums)
      j<-j+1
    }
    nmds.names<-c(nmds.names,rep(names(B[i]),500))
  }
  
  library(vegan)
  set.seed(0001)
  nmds.out <- metaMDS(for.nmds)
  out<-cbind(nmds.names,nmds.out$points)
  fg<-as.data.frame(nmds.out$species)
  write.csv(out,"~/out.csv")
  write.csv(fg,"~/fg.csv")
}#output data of nmds
{
  library(ggplot2)
  library(colormap)
  scores<-read.csv("~/out.csv",header=T)
  scores <- scores[,2:ncol(scores)]
  ffg<-read.csv("~/fg.csv",header=T,row.names=1)
  
  colnames(scores) <- c("host","NMDS1","NMDS2")
  n.rep <- 500 # 100 replicates per host
  
  viridis <- c("gray",colormap(nshades=(length(unique(scores$host))-1))) # uses the colormap package
  viridis <- as.vector(t(replicate(n.rep, viridis)))
  scores <- cbind(scores, viridis)
  
  for.ellipse <- NULL
  for (j in 1:length(unique(scores$host))) {
    this.host <- as.character(unique(scores$host)[j])
    for.ellipse <- rbind(for.ellipse,
                         c(as.character(this.host),
                           max(scores$NMDS1[which(scores$host== this.host)])- min(scores$NMDS1[which(scores$host== this.host)])))
  }
  for.ellipse <- as.data.frame(for.ellipse)
  if (min(as.numeric(as.character(for.ellipse[,2]))) == 0) {
    for.ellipse <- scores[-which(scores$host==for.ellipse[1,which((as.numeric(as.character(for.ellipse[,2]))) == 0)]),]
  } else {
    for.ellipse <- scores
  }
  
  ell.col <- NULL
  for (i in 1:length(unique(for.ellipse$host))) {
    ell.col <- c(ell.col, rep(as.character(for.ellipse$viridis[1+(n.rep*(i-1))]), 52))
  }
  
  label.coord <- NULL
  for (i in 1:(nrow(scores)/n.rep)) {
    this.scores <- scores[((1+((i-1)*n.rep)):(i*n.rep)),]
    label.coord <- rbind(label.coord, c(mean(this.scores$NMDS1), mean(this.scores$NMDS2)))
  }
  colnames(label.coord) <- c("NMDS1", "NMDS2")
  label.coord <- as.data.frame(label.coord)
  label.host <- unique(scores$host)
  
  set.seed(1)
  scores <- scores[sample(nrow(scores), nrow(scores), F),]
  
  p <- ggplot() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(color = "black", size=0.5), legend.position="none") +
    #stat_ellipse(data=for.ellipse, aes(x=NMDS1, y=NMDS2, group=(host)), geom = "polygon", fill=NA, color="black", lwd=1, level=0.84, alpha=0.4) +
    #stat_ellipse(data=for.ellipse, aes(x=NMDS1, y=NMDS2, group=(host)), geom = "polygon", fill="white", color=NA, lwd=1, level=0.84, alpha=0.4) +
    stat_ellipse(data=for.ellipse, aes(x=NMDS1, y=NMDS2, group=(host)), geom = "polygon", fill=ell.col, color="black", lwd=1, level=0.84, alpha=0.2) +
    geom_point(data=scores, (aes(x=NMDS1, y=NMDS2)), color=scores$viridis, alpha=0.6) +
    #geom_jitter(data=scores, (aes(x=NMDS1, y=NMDS2)), width=0.05, height=0.05, color=scores$viridis, alpha=0.6, size=1) +
    #stat_ellipse(data=for.ellipse, aes(x=NMDS1, y=NMDS2, group=(host)), geom = "polygon", fill=NA, color=ell.col, lwd=0.5, level=0.84, alpha=0.4) +
    annotate("text", hjust=0.5, x=label.coord$NMDS1, y=label.coord$NMDS2, label= label.host, size=5 ) +
    annotate("text", hjust=0.5, x= ffg$MDS1, y= ffg$MDS2, label= rownames(ffg), size=5,colour = "red") + 
    coord_cartesian(expand=T, xlim=c(-0.7, 0.5), ylim=c(-0.2, 0.2))
}#plot nmds
##/////END/////##

##/////Part4/////##
{
  V_data_CH<-read.csv("~/V_data_CH.csv")
  t<-V_data_CH$MAT
  p<-V_data_CH$MAP
  A<-V_data_CH$Altitude
  findPeak<-function(x){
    td<-density(x,bw=(max(x)-min(x))/10)
    list(td$x[which.max(td$y)])
  }
  
  p_t1<-findPeak(t[t<10])
  P_t2<-findPeak(t[t>=10])
  P_p<-findPeak(p)
  P_a<-findPeak(A)
}#find Peak of MAT&MAP and Altitude 
{
  skewness<-function(x){
    m<-mean(x)
    s<-sd(x)
    list(mean(((x-m)/s)^3))
  }
  kurtosis<-function(x){
    m<-mean(x)
    s<-sd(x)
    list(mean(((x-m)/s)^4))
  }
  S_t1<-skewness(t[t<10])
  S_t2<-skewness(t[t>=10])
  S_p<-skewness(p)
  S_a<-skewness(A)
  
  K_t1<-skewness(t[t<10])
  K_t2<-skewness(t[t>=10])
  K_p<-skewness(p)
  K_a<-skewness(A)
}#calculate the skewness and kurtosis of MAT&MAP and Altitude
##/////END/////##



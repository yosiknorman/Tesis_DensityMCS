rm(list = ls())
setwd("~/Desktop/Studi_Yosik/least/code/")
load("../data/6_akumulasi_mcs_with_scc.rda")

st_b = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
ymd_MC = list()
H_mc = list()
len_mc = c()
HMcB = list()
M_mc = list()
lonb = list()
latb = list()


for(i in 1:6){
  ymd_MC[[i]] = substr(hasil_akumulasi[[i]][[2]],1,8)
  M_mc[[i]] = substr(hasil_akumulasi[[i]][[2]],5,6)
  H_mc[[i]]  = substr(hasil_akumulasi[[i]][[2]],9,10)
  len_mc[i] = length(substr(hasil_akumulasi[[i]][[2]],9,10))
  lonb[[i]] = hasil_akumulasi[[i]][[3]]
  latb[[i]] = hasil_akumulasi[[i]][[4]]
}

iM_mc = list()
im
for(i in 1:6){
  iM_mc[[i]] = list()
  for(j in 1:5){
    iM_mc[[i]][[j]] = which(as.integer(M_mc[[i]]) == im[j])
    
  }
  names(iM_mc[[i]]) = st_b[im]
}

lat_jason = list()
lon_jason = list()
for(i in 1:6){
  lat_jason[[i]] = list()
  lon_jason[[i]] = list()
  len_jason[[i]] = c()
  for(j in 1:5){
    lat_jason[[i]][[j]] = latb[[i]][iM_mc[[i]][[j]]]
    lon_jason[[i]][[j]] = lonb[[i]][iM_mc[[i]][[j]]]
    
  }
  names(lon_jason[[i]]) = st_b[im]
  names(lat_jason[[i]]) = st_b[im]
}
len_jason = list()
for(i in 1:6){
  len_jason[[i]] = list()
  for(j in 1:5){
    len_jason[[i]][[j]] = length(lonb[[i]][iM_mc[[i]][[j]]])
  }
  len_jason[[i]] = unlist(len_jason[[i]])
}

colors = c(rgb(0.5,0.2,0.7,alpha = 0.7),
           rgb(1,0.2,0.2,alpha = 0.7), 
           rgb(1,0.7,0.2,alpha = 0.7), 
           rgb(0.4,0.7,0.2,alpha = 0.7),
           rgb(1,0.2,0.9,alpha = 0.7),
           rgb(0.1,0.2,0.9,alpha = 0.7))

# df = data.frame(lonb,latb)


library(akima)
library(MASS)
library(ggplot2)
library(viridis)
get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
denn = get_density(lonb[[1]],latb[[1]],n = 100)
fld <- interp(x = lonb[[1]], y = latb[[1]], z = denn,duplicate = T)


# par(mar=c(0,0,0,-1))
# par(xpd=NA)
source("Filled.contour2.R")
library(maps)
nn = c("a) MCCs","b) PECSs","c) MBCCSs","d) MBECSs","e) SMBCCSs","f) SMBECSs")
fld=list()
denn = list()
for(i in 1:6){
  denn[[i]] = get_density(lonb[[i]],latb[[i]],n = 100)
  fld[[i]] <- interp(x = lonb[[i]], y = latb[[i]], z = denn[[i]],duplicate = T)
}


rnfld = list()

for(i in 1:5){
  rnfld[[i]] = c(range( fld[[i]]$z[!is.na(fld[[i]]$z)]   ),
            range( fld[[i+1]]$z[!is.na(fld[[i+1]]$z)]   ))
}

rnfld = range(unlist(rnfld))
# x11()
for(i in 1:6){
  
  png(filename = sprintf("../png/MCS%s.png",i),height = 700, width = 1200)
  my.filled.contour(x =fld[[i]]$x,y=fld[[i]]$y,(fld[[i]]$z)*10000,las=0,
                 col = colorRampPalette(c("white",colors[1:5]))(30),
                 levels = seq(rnfld[1]*10000,rnfld[2]*10000,length=27),axes = T,
                 plot.axes = {axis(1,cex.axis = 3);axis(2,cex.axis = 3);
                   grid(col="black",lwd=2);title(main = sprintf("%s",nn[i]),cex.main = 4);
                   points(lonb[[i]],latb[[i]],pch = 3,cex = 0.7);
                   map("world", fill=F, col="black", bg=NULL,
                       xlim=c(min(fld[[i]]$x),max(fld[[i]]$x)),
                       ylim=c(min(fld[[i]]$y), max(fld[[i]]$y)),
                       resolution = 0.0001,
                       add=T,interior = F,lwd=2);las=1
                       # contour(x = (fld[[i]]$x),y = (fld[[i]]$y),z = (fld[[i]]$z),nlevels = 5,add = T,lwd = 2,cex = 3)
                 })
  dev.off()
}

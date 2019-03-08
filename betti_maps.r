# setting working directory 

library(raster)
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
library(rasterVis)
library(rgdal)
library(pals)
###########################################################################
ls_tmp=list.files(pattern="tmp2m*.nc")
ls_prec=list.files(pattern="apcp*.nc")

italy_confini <- getData('GADM', country='ITA', level=0)

ls_files_tmp=sapply(ls_tmp,raster) # creo files letti e pronti al plot
ls_files_prec=sapply(ls_prec,raster) # creo files letti e pronti al plot

# ls_tmp [] vettore
# ls_files_tmp [[]] lista
# extent= 'xmin, xmax, ymin, ymax)

ita_bounds_bolam=extent(ls_files_tmp[[1]])
ita_bounds_bolam[1]=ita_bounds_bolam[1]-360
ita_bounds_bolam[2]=ita_bounds_bolam[2]-360


###########################################################################
years=paste0("Year ",2008:2017)


res_tmp=list()
res_prec=list()
res_anomtmp=list()
res_anomprec=list()

for ( i in 1:length(ls_files_tmp)) {

extent(ls_files_tmp[[i]])=ita_bounds_bolam
extent(ls_files_prec[[i]])=ita_bounds_bolam
res_tmp[[i]]=crop(ls_files_tmp[[i]],extent(italy_confini))-273.16
res_prec[[i]]=crop(ls_files_prec[[i]],extent(italy_confini))


}

res_stack_tmp=stack(res_tmp)
res_stack_prec=stack(res_prec)
climtmp <- calc(res_stack_tmp, mean)
climprec<- calc(res_stack_prec, mean)

for ( i in 1:10) {

res_anomtmp[[i]]=res_stack_tmp[[i]]-climtmp
res_anomprec[[i]]=res_stack_prec[[i]]-climprec

}


for ( i in 1:10) {

res_prec[[i]][which(getValues(res_prec[[i]])>3000)]=3000
res_anomprec[[i]][which(getValues(res_anomprec[[i]])>500)]=500
res_anomprec[[i]][which(getValues(res_anomprec[[i]])<=-500)]=-500


}

res_stack_prec=stack(res_prec)
res_stack_anomprec=stack(res_anomprec)

names(res_stack_tmp)=years
names(res_stack_prec)=years
names(res_stack_anomtmp)=years
names(res_stack_anomprec)=years


png("betti_plots_tmps.png",width = 1024, height = 768)
ThemeTmp<- rasterTheme(region = rev(brewer.pal(9,"Spectral")))
levelplot(res_stack_tmp,par.settings=ThemeTmp,main = "Temperatura media annua (°C)") + layer(sp.polygons(italy_confini, lwd=0.8, col="black"))
dev.off()

png("betti_plots_anomtmps.png",width = 1024, height = 768)
ThemeTmp<- rasterTheme(region = rev(brewer.pal(9,"Spectral")))
levelplot(res_stack_anomtmp,par.settings=ThemeTmp,main = "Anomalie temperatura media annua (°C)") + layer(sp.polygons(italy_confini, lwd=0.8, col="black"))
dev.off()

png("betti_plots_anomprec.png",width = 1024, height = 768)
ThemeTmp<- rasterTheme(region = brewer.pal(9,"Spectral"))
levelplot(res_stack_anomprec,par.settings=ThemeTmp,main = "Anomalie precipitazione media annua (mm)") + layer(sp.polygons(italy_confini, lwd=0.8, col="black"))
dev.off()

png("betti_plots_prec.png",width = 1024, height = 768)
ThemeTmp<- rasterTheme(region = brewer.pal(9,"Blues"))
levelplot(res_stack_prec,par.settings=ThemeTmp,main = "Precipitazione media annua (mm)") + layer(sp.polygons(italy_confini, lwd=0.8, col="black"))
dev.off()

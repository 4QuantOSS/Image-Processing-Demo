# basic libraries
library(shiny)
require(ggplot2)
require(plyr)
require(grid) # contains the arrow function
require(biOps)


# functions for converting images back and forth
im.to.df<-function(in.img) {
  out.im<-expand.grid(x=1:nrow(in.img),y=1:ncol(in.img))
  out.im$val<-as.vector(in.img)
  out.im
}
df.to.im<-function(in.df,val.col="val",inv=F) {
  in.vals<-in.df[[val.col]]
  if(class(in.vals[1])=="logical") in.vals<-as.integer(in.vals*255)
  if(inv) in.vals<-255-in.vals
  out.mat<-matrix(in.vals,nrow=length(unique(in.df$x)),byrow=F)
  attr(out.mat,"type")<-"grey"
  out.mat
}
ddply.cutcols<-function(...,cols=1) {
  # run standard ddply command
  cur.table<-ddply(...)
  cutlabel.fixer<-function(oVal) {
    sapply(oVal,function(x) {
      cnv<-as.character(x)
      mean(as.numeric(strsplit(substr(cnv,2,nchar(cnv)-1),",")[[1]]))
    })
  }
  cutname.fixer<-function(c.str) {
    s.str<-strsplit(c.str,"(",fixed=T)[[1]]
    t.str<-strsplit(paste(s.str[c(2:length(s.str))],collapse="("),",")[[1]]
    paste(t.str[c(1:length(t.str)-1)],collapse=",")
  }
  for(i in c(1:cols)) {
    cur.table[,i]<-cutlabel.fixer(cur.table[,i])
    names(cur.table)[i]<-cutname.fixer(names(cur.table)[i])
  }
  cur.table
}

nx<-5
ny<-5
cross.im<-expand.grid(x=c(-nx:nx)/nx*2*pi,y=c(-ny:ny)/ny*2*pi)
cross.im<-cbind(cross.im,
                val=10*(1.5*with(cross.im,abs(cos(x*y))/(abs(x*y)+(3*pi/nx)))+
                  0.5*runif(nrow(cross.im))))
bn.wid<-diff(range(cross.im$val))/10

show.img<-function(im.data) {
  ggplot(im.data,aes(x=x,y=y))+
    geom_raster(aes(fill=val))+
    scale_fill_gradient(low="black",high="white")+
    labs(fill="Intensity",color="")+
    theme_bw(20)
}
show.thresh.img<-function(im.data,thresh.val) {
  im.data$thresh<-(im.data$val>=thresh.val)
  show.img(im.data)+
    geom_tile(data=subset(im.data,thresh),aes(color="Above\nThreshold"),fill="red",alpha=0.3)
}
get.hist.comparison<-function(dataA,colorName) {
  ggplot(dataA,aes(x=val))+
    geom_density(aes(color=ctype))+
    labs(title="Image Intensity Histogram",color=colorName)+
    theme_bw(20)
}


## Basic settings

img.names<-new.env() # env is the closest thing to a pydict
img.names$Cell<-function(...) {
  load("cell.RData")
  out.img<-10-10^cell.img
  attr(out.img, "type") <- "grey"
  out.img
}
img.names$ACross<-function(...) df.to.im(cross.im)

## Filters
filter.funs<-new.env()
filter.funs$None<-function(img,size,sigma) img
filter.funs$Canny<-function(img,size,sigma) imgCanny(img,sigma)/25
filter.funs$Median<-function(img,size,sigma) imgBlockMedianFilter(img,size)

flat.fun<-function(x,op) {
  xmin<-min(x)
  xmax<-max(x)
  y<-op((x-xmin)/(xmax-xmin))
  ymin<-min(y)
  ymax<-max(y)
  (y-ymin)/(ymax-ymin)*(xmax-xmin)+xmin
}

## Gamma functions
gamma.funs<-new.env()
gamma.funs$Linear<-function(x) x
gamma.funs$Quadradic<-function(x) flat.fun(x,function(x) x*x)
gamma.funs$Cubic<-function(x) flat.fun(x,function(x) x*x*x)
gamma.funs$Log<-function(x) flat.fun(x,function(x) log(x+1e-3))
gamma.funs$Exp<-function(x) flat.fun(x,function(x) 10^x)
gamma.funs$Sqrt<-function(x) flat.fun(x,function(x) sqrt(x))
gamma.funs$CubeRoot<-function(x) flat.fun(x,function(x) x^(0.33))
gamma.funs$Sine<-function(x) flat.fun(x,function(x) sin(pi*x))

## Morphological operations
morph.funs<-new.env()
morph.funs$Erosion<-function(img,size) imgStdBinaryErosion(img,size)
morph.funs$Dilation<-function(img,size) imgStdBinaryDilation(img,size)
morph.funs$Opening<-function(img,size) imgStdBinaryOpening(img,size)
morph.funs$Closing<-function(img,size) imgStdBinaryClosing(img,size)

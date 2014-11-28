source("imgbasics.R")
shinyServer(function(input, output) {
  # Base Code
  get.array.image<-reactive({
    img.names[[input$image_name]]()
  })
  get.raw.image<-reactive({
    im.to.df(get.array.image())
  })
  # Filtering
  get.filtered.image<-reactive({
    img<-get.array.image()
    filt<-filter.funs[[input$filter_name]]
    im.to.df(filt(img,input$filter_size,input$filter_sigma/10))
  })
  output$filterPlot <- renderPlot({
    dual.image<-rbind(cbind(get.raw.image(),ftype="Unfiltered"),
                      cbind(get.filtered.image(),ftype=input$filter_name))
    print(
      show.img(dual.image)+
        facet_wrap(~ftype)
      )
  })
  output$fhistPlot <- renderPlot({
    gam.data<-rbind(
      cbind(get.filtered.image(),ctype="After"),
      cbind(get.raw.image(),ctype="Before")
    )
    print(
      get.hist.comparison(gam.data,"Filtering")
    )
  })
  
  # Gamma
  get.gamma.image<-reactive({
    im.data<-get.filtered.image()
    im.data$val<-gamma.funs[[input$gamma_name]](im.data$val)
    im.data
  })
  output$gthreshPlot <- renderPlot({
    print(show.img(get.gamma.image()))
  })

  output$ghistPlot <- renderPlot({
    gam.data<-rbind(
      cbind(get.gamma.image(),ctype="After"),
      cbind(get.filtered.image(),ctype="Before")
      )
    print(
      get.hist.comparison(gam.data,"Gamma Correction")
      )
  })

  output$gammaPlot <- renderPlot({
    xdat<-seq(0,10,length.out=100)
    xdf<-data.frame(x=xdat,y=gamma.funs[[input$gamma_name]](xdat))
    gamma.img<-get.gamma.image()
    std.img<-get.filtered.image()
    print(
      ggplot(xdf,aes(x=x,y=y))+
        geom_point(aes(color=y))+
        geom_line()+
        geom_rug(data=std.img,aes(x=val),sides="b",position="jitter",alpha=0.25)+
        geom_rug(data=gamma.img,aes(y=val),sides="l",position="jitter",alpha=0.25)+
        scale_colour_gradient(low="black",high="white")+
        xlim(0,10)+ylim(0,10)+coord_equal()+
        labs(title="Gamma Transfer Function",x="Input Intensity",y="Output Intensity")+
        theme_bw(20)
    )
  })
  
  ## Threshold
  output$threshPlot <- renderPlot({
    print(show.thresh.img(get.gamma.image(),input$threshold))
  })
  
  output$thistPlot <- renderPlot({
    gam.data<-rbind(
      cbind(get.raw.image(),ctype="Unfiltered"),
      cbind(get.filtered.image(),ctype="Filtered"),
      cbind(get.gamma.image(),ctype="Gamma Corrected")
    )
    print(
      get.hist.comparison(gam.data,"Step")+
        geom_vline(aes(color="Threshold"),xintercept=input$threshold)
    )
  })
  
  ## Morphology
  output$morphPlot <- renderPlot({
    thresh.img<-get.gamma.image()
    # this code looks a bit strange because everything is flipped around for the stdbinary operations in this package
    thresh.img$val<-(thresh.img$val<input$threshold)

    morphf<-morph.funs[[input$morph_name]]
    morph.img<-im.to.df(
      morphf(df.to.im(thresh.img,val.col="val"),input$morph_size)
      )
    dual.image<-rbind(cbind(thresh.img,ftype="Threshold Image"),
                      cbind(morph.img,ftype=input$morph_name)
                      )
    dual.image$thresh<-ifelse(dual.image$val<1,"In","Out")
    print(
      ggplot(dual.image,aes(x=x,y=y))+
        geom_raster(aes(fill=thresh))+
        #scale_fill_gradient(low="black",high="white")+
        labs(fill="Segmented",color="")+
        facet_wrap(~ftype)+
        theme_bw(20)
        
    )
  })
  

})
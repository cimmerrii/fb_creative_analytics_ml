



set.dimensions <- function(dimension.cutoff,dimensions.to.use, study.data, sample.size) {
  
  dimensions.suggested<-sample.dimension.reduction(dimension.cutoff,study.data$noise.data,sample.size)
  
  dimensions<-ifelse(dimensions.to.use=="default",
                     dimensions.suggested,
                     dimensions.to.use)
  
  
  return(dimensions)
}

sample.dimension.reduction<-function(dimension.cutoff,data,sample.size){
  tic("dimension reduction sample")
  data<-get.noise.cvt(data)
  #reduce the data to the sample size
  reduced.data<-data[sample(nrow(data), sample.size,replace=TRUE), ]
  
  #do the MCA analysis
  res.mca<-MCA(reduced.data,ncp=60,graph=TRUE)
  
  #make a dimension chart
  eig.val<-get_eigenvalue(res.mca)
  plot(eig.val[,3])
  
  #figure out the cutoff to suggest
  eig.val<-as.data.frame(eig.val) #convert to dataframe
  
  #set dimension.cutoff to either the lowest eigenvalue or the cutoff
  dimension.cutoff <- max(dimension.cutoff,
                          .01+ min(eig.val$cumulative.variance.percent))
  
  suggested.dimensions<-eig.val %>%
    dplyr::filter(cumulative.variance.percent<dimension.cutoff) %>%
    nrow()  #count how many rows are below cutoff
  
  #figure out how many levels there are
  
  
  
  #number of levels of original
  sapply(data[,sapply(data, is.factor)], nlevels)->num.levels.all
  #number of levels in the sample
  sapply(reduced.data[,sapply(reduced.data, is.factor)], nlevels)->num.levels.sample
  
  #do some housekeeping
  num.levels.all<-as.numeric(sum(num.levels.all))
  num.levels.sample<-as.numeric(sum(num.levels.sample))
  
  #print the numbers of levels
  print(paste("The entire dataset has ", num.levels.all," levels in the noise factors",sep=""))
  print(paste("The sample of ",sample.size, " rows has ", num.levels.sample," levels in the audience factors",sep=""))
  
  
  reduced.factors<-get_mca_ind(res.mca)
  reduced.factors<-as.data.frame(reduced.factors[1])
  toc()
  return(suggested.dimensions)
}


do.dimensional.reduction<-function(reducable.factors,dimensions){
  #key.data<-clean.pre.dim.reduction(all)
  
  #make the 3 dataframes- numbers, not reduce factors, and reducable factors
  #number.data<-key.data[,1:5]
  #format.data<-key.data[,20:27]
  #reducable.factors<-key.data[,6:19]
  tic("full dimension reduction")
  #do the MCA analysis
  res.mca<-MCA(reducable.factors,dimensions,graph=TRUE)
  
  #this gives the individual values of each row
  reduced.factors<-get_mca_ind(res.mca)
  reduced.factors<-as.data.frame(reduced.factors[1])
  toc()
  reduced.factors
}

clean.pre.dim.reduction<-function(all){  
  all<-factorize(all)
  
  #rearrange and get to the key columns we care about
  key.columns <- dplyr::select(all, 
                               c(
                                 purchase,
                                 spend,
                                 spendsquare,
                                 spendroot,
                                 utm.spend,
                                 #stuff to PCA
                                 vert,
                                 landing.page.name,
                                 promo,
                                 interest,
                                 behavior,
                                 demographic,
                                 lookalike,
                                 campaign.type.account,
                                 year.month,
                                 year.month.account,
                                 year.week,
                                 year.week.account,
                                 week.day,
                                 week.day.account,
                                 #this group gets held out from the PCA
                                 video.shape,
                                 video.style,
                                 video.duration,
                                 call.to.action,
                                 format,
                                 text.overlay,
                                 approach,
                                 platform
                               )
  )
  key.columns$week.day<-as.factor(key.columns$week.day)
  key.columns
}

evaluate.many.models<-function(study){
  for (i in 0:1){
    start.timer<-start.timing()
    dimensions<-max(2,100*i)
    reduced.study<-do.dimensional.reduction(study,dimensions)
    dimension.vars<-make.dimension.vars(dimensions)
    formula.maxi<-make.formula(creative.vars,dimension.vars)
    model<-make.model(reduced.study,formula.maxi)
    saveRDS(model,paste("model #",i+1,".RDS",sep=''))
    end.timing(start.timer)
  } 
  
  for (i in 1:7){
    dimensions<-100*i
    model<-readRDS(paste("model #",i+1,".RDS",sep=''))
    assign(paste("model.",i*100,sep=''),model)
  }
  model.2<-readRDS("model #1.RDS")
  model.50<-readRDS("model #2.RDS")
  model.50<-model50
  AIC(model.2,model.50,model.100,model.200,model.300,model.400,model.500)
  
  model.2.data<-model.2$coefficients
  model.50.data<-model.50$coefficients
  model.100.data<-model.100$coefficients
  model.200.data<-model.200$coefficients
  model.300.data<-model.300$coefficients
  model.400.data<-model.400$coefficients
  model.500.data<-model.500$coefficients
  #model.600.data<-model.600$coefficients
  #model.700.data<-model.700$coefficients
  list(model.2.data,model.50.data,model.100.data,model.200.data,model.300.data,model.400.data,model.500.data)->model.list
  cbmodel.data.frame<-merge(model.2.data,model.50.data)
  
  coefficients <-as.data.frame(model.2.data)
  coefficients[2]<-model.50.data[1:27]
  coefficients[3]<-model.200.data[1:27]
  coefficients[4]<-model.300.data[1:27]
  coefficients[5]<-model.400.data[1:27]
  coefficients[6]<-model.500.data[1:27]
}



run.one.client.cvt <- function(filtered.results, 
                               data.update, 
                               date.cutoff, 
                               model, 
                               video.retarg.remove,
                               video.prosp.remove,
                               prosp.all.remove,
                               retarg.all.remove,
                               client.vars,
                               target.account,
                               account.var.name
                              ){
  
  
  one.client <-list()
  
  one.client$retargeting.video <- master.cvt.run(
    data.update,
    campaign.type="retargeting",
    analysis.type="single",
    video.or.all="video",
    target.account,
    account.var.name,
    date.cutoff,
    dimensions.to.use="default",
    dimension.cutoff =99,
    remove.list=video.retarg.remove,
    model,
    model.to.use="default")
  

 
  
  return(one.client)
}


run.all.client.cvt <- function(filtered.results, data.update, date.cutoff, model) {
  

  writeLines("\nrunning retargeting.video\n")
  filtered.results$retargeting.video <- try.master.cvt.run(dimension.start = 99,
                                                           data.update,
                                                           campaign.type = "retargeting",
                                                           analysis.type = "all",
                                                           video.or.all = "video",
                                                           target.account = "",
                                                           date.cutoff,
                                                           dimensions.to.use = "default",
                                                           remove.list = list("bob"),
                                                           model,
                                                           model.to.use = "default")

  saveRDS(filtered.results,"universal.filtered.results.1.rds")

  writeLines("\nrunning prospecting.video\n")
  filtered.results$prospecting.video <- try.master.cvt.run(dimension.start = 99,
                                                        data.update,
                                                       campaign.type = "prospecting",
                                                       analysis.type = "all",
                                                       video.or.all = "video",
                                                       target.account = "",
                                                       date.cutoff,
                                                       dimensions.to.use = "default",
                                                       remove.list = list("bob"),
                                                       model,
                                                       model.to.use = "default")

  saveRDS(filtered.results,"universal.filtered.results.2.rds")

  
  writeLines("\nrunning prospecting.all\n")
  filtered.results$prospecting.all <- try.master.cvt.run(dimension.start = 90,
                                                         data.update,
                                                         campaign.type = "prospecting",
                                                         analysis.type = "all",
                                                         video.or.all = "all",
                                                         target.account = "",
                                                         date.cutoff,
                                                         dimensions.to.use = "default",
                                                         remove.list = list("bob"),
                                                         model,
                                                         model.to.use = "default")
  

  saveRDS(filtered.results,"universal.filtered.results.3.rds")

  
  writeLines("\nrunning retargeting.all\n")
  filtered.results$retargeting.all <- try.master.cvt.run(dimension.start=90,
                                                         data.update,
                                                         campaign.type = "retargeting",
                                                         analysis.type = "all",
                                                         video.or.all = "all",
                                                         target.account = "",
                                                         date.cutoff,
                                                         dimensions.to.use = "default",
                                                         remove.list = list("bob"),
                                                         model,
                                                         model.to.use = "default")

  saveRDS(filtered.results,"universal.filtered.results.4.rds")
  
  writeLines("\nrunning retention.all\n")
  filtered.results$retention.all <- try.master.cvt.run(dimension.start=99,
                                                         data.update,
                                                         campaign.type = "retention",
                                                         analysis.type = "all",
                                                         video.or.all = "all",
                                                         target.account = "",
                                                         date.cutoff,
                                                         dimensions.to.use = "default",
                                                         remove.list = list("bob"),
                                                         model,
                                                         model.to.use = "default")
  
  saveRDS(filtered.results,"universal.filtered.results.5.rds")
  writeLines("\nrunning retention.videol\n")
  
  filtered.results$retention.video <- try.master.cvt.run(dimension.start=99,
                                                       data.update,
                                                       campaign.type = "retention",
                                                       analysis.type = "all",
                                                       video.or.all = "video",
                                                       target.account = "",
                                                       date.cutoff,
                                                       dimensions.to.use = "default",
                                                       remove.list = list("bob"),
                                                       model,
                                                       model.to.use = "default")
  

  saveRDS(filtered.results,"universal.filtered.results.final.rds")


  return(filtered.results)
}

try.master.cvt.run <- function(dimension.start,
                               data.update,
                               campaign.type,
                               analysis.type,
                               video.or.all,
                               target.account,
                               date.cutoff,
                               dimensions.to.use,
                               remove.list,
                               model,
                               model.to.use){
  

  #cycle from dimension.start to 0, testing for a model that runs
  for (i in seq(from = dimension.start,to = 0, by = -11)){
    
    #set dimension cutoff to i
    dimension.cutoff <- i
    writeLines(paste("\nrunning with dimension cutoff of ", i,"% \n",sep=''))
    
    #now try to run the model with these dimensions
    one.result <-  tryCatch({
      #actually do cvt
      master.cvt.run(
        data.update,
        campaign.type,
        analysis.type,
        video.or.all,
        target.account,
        account.var.name = "",
        date.cutoff,
        dimensions.to.use,
        dimension.cutoff,
        remove.list,
        model,
        model.to.use)
      
    }, error = function(e) {  
      #if it errors, retun error
      "error"
    })
    
    
    if(one.result != "error") { #if it didnt error, be done
      writeLines(paste("\n complete with dimension cutoff of ",i,"% \n",sep=''))
      break
      }
    
    #if it did error, keep looping
  }

  return(one.result)
}




nosweeep.client.cvt <- function(filtered.results, data.update, date.cutoff, model) {
  
  
  writeLines("\nrunning retargeting.video\n")
  filtered.results$retargeting.video <- try.master.cvt.run(dimension.start = 99,
                                                           data.update,
                                                           campaign.type = "retargeting",
                                                           analysis.type = "all",
                                                           video.or.all = "video",
                                                           target.account = "",
                                                           date.cutoff,
                                                           dimensions.to.use = "default",
                                                           remove.list = list("spend:format"),
                                                           model,
                                                           model.to.use = "default")
  
  saveRDS(filtered.results,"universal.filtered.results.1.rds")
  
  writeLines("\nrunning prospecting.video\n")
  filtered.results$prospecting.video <- try.master.cvt.run(dimension.start = 99,
                                                           data.update,
                                                           campaign.type = "prospecting",
                                                           analysis.type = "all",
                                                           video.or.all = "video",
                                                           target.account = "",
                                                           date.cutoff,
                                                           dimensions.to.use = "default",
                                                           remove.list = list("bob"),
                                                           model,
                                                           model.to.use = "default")
  
  saveRDS(filtered.results,"universal.filtered.results.2.rds")
  
  
  writeLines("\nrunning prospecting.all\n")
  filtered.results$prospecting.all.nosweep <- master.cvt.run(
    data.update,
    campaign.type = "prospecting",
    analysis.type = "all",
    video.or.all = "all",
    target.account = "",
    date.cutoff,
    dimensions.to.use = 100,
    dimension.cutoff,
    remove.list = list("bob"),
    model,
    model.to.use = "default")
  
  
  saveRDS(filtered.results,"universal.filtered.results.3.rds")
  
  
  writeLines("\nrunning retargeting.all\n")
  filtered.results$retargeting.all <- try.master.cvt.run(dimension.start=99,
                                                         data.update,
                                                         campaign.type = "retargeting",
                                                         analysis.type = "all",
                                                         video.or.all = "all",
                                                         target.account = "",
                                                         date.cutoff,
                                                         dimensions.to.use = "default",
                                                         remove.list = list("bob"),
                                                         model,
                                                         model.to.use = "default")
  
  saveRDS(filtered.results,"universal.filtered.results.4.rds")
  saveRDS(filtered.results,"universal.filtered.results.final.rds")
  
  
  return(filtered.results)
}

run.all.client.cvt.fast <-function(filtered.results, data.update, date.cutoff, model) {
  campaign.type <- list()
  
  campaign.type$retargeting.video <- "retargeting"
  campaign.type$prospecting.video <- "prospecting"
  campaign.type$retargeting.all <- "retargeting"
  campaign.type$prospecting.all <- "prospecting"
  
  video.type <- list()
  video.type$retargeting.video <- "video"
  video.type$prospecting.video <- "video"
  video.type$retargeting.all <- "all"
  video.type$prospecting.all <- "all"
  
  
  filtered.results <- mapply(try.master.cvt.run, 
                             campaign.type = campaign.type,
                             video.or.all = video.type,
                             dimension.start = 11,
                             data.update = data.update,
                             analysis.type = "all",
                             video.or.all = "video",
                             target.account = "",
                             date.cutoff = date.cutoff,
                             dimensions.to.use = "default",
                             remove.list = list("spend:format"),
                             model=model,
                             model.to.use = "default")
  
  return(filtered.results)
  
}

#as of 2.16.19 the framebridge one is working and should be the basis of other one client cvt
framebridge.cvt <- function(filtered.results, data.update, target.account, date.cutoff, model, model.to.use) {
  
  client.vars <- framebridge.vars
  target.account <- "framebridge paid facebook"
  dimension.cutoff <- 99
  date.cutoff <- as.Date("2018-11-01")
  clientname <- "framebridge"
  
  filtered.results$framebridge.prospecting <- master.cvt.run(data.update,
                                                         campaign.type = "prospecting",
                                                         analysis.type = "single",
                                                         video.or.all = "all",
                                                         target.account,
                                                         account.var.name = target.account,
                                                         date.cutoff = date.cutoff,
                                                         dimensions.to.use = "default",
                                                         dimension.cutoff = dimension.cutoff,
                                                         remove.list<- list("spend:framebridge.video.length","spend:promo","spend:video.duration","spend:ad.style","spend:approach","spend:ad.content","spend:camera.angle","spend:seasonal.or.holiday.content","spend:has.person.or.part.of.person"),
                                                         model,
                                                         model.to.use)
  
  saveRDS(filtered.results,"framebridge.1.rds")
  
  
  filtered.results$framebridge.prospecting.video <- master.cvt.run(data.update,
                                                             campaign.type = "prospecting",
                                                             analysis.type = "single",
                                                             video.or.all = "video",
                                                             target.account,
                                                             account.var.name = target.account,
                                                             date.cutoff = as.Date("2010-01-01"),
                                                             dimensions.to.use = "default",
                                                             dimension.cutoff = dimension.cutoff,
                                                             remove.list<- list("spend:framebridge.asset.environment","spend:format","spend:promo","spend:video.duration","spend:ad.style","spend:approach","spend:ad.content","spend:camera.angle","spend:seasonal.or.holiday.content","spend:has.person.or.part.of.person"),
                                                             model,
                                                             model.to.use)
  
  
  saveRDS(filtered.results,"framebridge.2.rds")
  
  filtered.results$framebridge.retargeting.video <- master.cvt.run(data.update,
                                                                   campaign.type = "retargeting",
                                                                   analysis.type = "single",
                                                                   video.or.all = "video",
                                                                   target.account,
                                                                   account.var.name = target.account,
                                                                   date.cutoff = as.Date("2010-01-01"),
                                                                   dimensions.to.use = "default",
                                                                   dimension.cutoff = 50,
                                                                   remove.list<- list("spend:framebridge.asset.environment","spend:call.to.action","spend:format","spend:promo","spend:video.duration","spend:ad.style","spend:approach","spend:ad.content","spend:camera.angle","spend:seasonal.or.holiday.content","spend:has.person.or.part.of.person"),
                                                                   model,
                                                                   model.to.use)
 
  
  saveRDS(filtered.results,"framebridge.3.rds")
  
  filtered.results$framebridge.retargeting <- master.cvt.run(data.update,
                                                                   campaign.type = "retargeting",
                                                                   analysis.type = "single",
                                                                   video.or.all = "all",
                                                                   target.account,
                                                                   account.var.name = target.account,
                                                                   date.cutoff = as.Date("2010-01-01"),
                                                                   dimensions.to.use = "default",
                                                                   dimension.cutoff = dimension.cutoff,
                                                                   remove.list<- list("spend:framebridge.video.length","spend:promo","spend:video.duration","spend:ad.style","spend:approach","spend:ad.content","spend:camera.angle","spend:seasonal.or.holiday.content","spend:has.person.or.part.of.person"),
                                                                   model,
                                                                   model.to.use)
  
  saveRDS(filtered.results,"framebridge.4.rds")
  
  return(filtered.results)
  
}

run.one.client.cvt <- function(filtered.results, data.update, date.cutoff, model,target.account,remove.list,client.vars) {
  dimension.cutoff<-95


  print("running prospecting.one")
  filtered.results$prospecting.all <- master.cvt.run(data.update,
                                                     campaign.type = "prospecting",
                                                     analysis.type = "all",
                                                     video.or.all = "all",
                                                     target.account,
                                                     date.cutoff,
                                                     dimensions.to.use = "default",
                                                     remove.list,
                                                     model,
                                                     model.to.use = "default")

  saveRDS(filtered.results,"universal.filtered.results.3.rds")

  print("running retargeting.all")
  filtered.results$retargeting.all <- master.cvt.run(data.update,
                                                     campaign.type = "retargeting",
                                                     analysis.type = "all",
                                                     video.or.all = "all",
                                                     target.account = "",
                                                     date.cutoff,
                                                     dimensions.to.use = "default",
                                                     remove.list = list("bob"),
                                                     model,
                                                     model.to.use = "default")


  print("running retarg.video")
  filtered.results$retargeting.video <- master.cvt.run(data.update,
                                                       campaign.type = "retargeting",
                                                       analysis.type = "single",
                                                       video.or.all = "video",
                                                       target.account,
                                                       date.cutoff,
                                                       dimensions.to.use = "default",
                                                       remove.list = list("spend:format"),
                                                       model,
                                                       model.to.use = "default")

  saveRDS(filtered.results,"universal.filtered.results.1.rds")


  print("running prospecting.video")
  filtered.results$prospecting.video <- master.cvt.run(data.update,
                                                       campaign.type = "prospecting",
                                                       analysis.type = "all",
                                                       video.or.all = "video",
                                                       target.account = "",
                                                       date.cutoff,
                                                       dimensions.to.use = "default",
                                                       remove.list = list("spend:format"),
                                                       model,
                                                       model.to.use = "default")

  saveRDS(filtered.results,"universal.filtered.results.2.rds")


  saveRDS(filtered.results,"universal.filtered.results.4.rds")


  return(filtered.results)
}



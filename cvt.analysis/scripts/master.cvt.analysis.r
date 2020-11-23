one.client <- run.one.client.cvt(
  filtered.results, 
  data.update, 
  date.cutoff, 
  model=list(), 
  video.retarg.remove=list("spend:format"),
  video.prosp.remove=list("spend:format"),
  prosp.all.remove=list("bob"),
  retarg.all.remove=list("bob"),
  client.vars = purple.carrot.vars,
  target.account = "purple carrot",
  account.var.name="purple.carrot",
)


#Section #2 -----------------------------------------------------
#choose which of these yyou want to run (or make a new one)
filtered.results <- run.all.client.cvt(filtered.results, data.update, date.cutoff, model)
filtered.results <- bitsbox.cvt(filtered.results, data.update, target.account, date.cutoff, model, model.to.use)
filtered.results <- keto.cvt(filtered.results, data.update, target.account, date.cutoff, model, model.to.use)

#this adds in the universal results if you choose them
filtered.results <- add.filtered.results(filtered.results, add.universal.results)
#or if you have no data. you can just read it locall
#do not do both the abvoe and below just do either
filtered.results<-readRDS("universal.filtered.results.final.rds")

#SECTION #3 ONCE YOU HAVE ALL THE RESULTS SAVED THEN PLOT THEM

#merge data and graph it
all.data <- dplyr::bind_rows(filtered.results) #merges all the various items of hte filtered.results list
saveRDS(all.data,paste(target.account," all.data.rds",sep = '')) #saves alldata
make.plots(all.data,studyname) #make pdf of alldata

#if you have refreshed the master all client cvt, save it here
saveRDS(all.data,"universal.cvt.plot.data.rds")


filtered.results <- run.one.client.cvt(filtered.results, 
                   data.update, 
                   date.cutoff, 
                   model, 
                   account="purple carrot",
                   video.retarg.remove=c("bob"),
                   video.prosp.remove=c("bob"),
                   prosp.remove=c("bob"),
                   retarg.remove=c("bob")
                   )
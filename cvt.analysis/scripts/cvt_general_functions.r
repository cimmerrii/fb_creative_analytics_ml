
#source("sf_r_helper_functions.R")
#source("dimension_reduction_functions.r")
#source("glm_functions.r")
#source("super_table_import_functions.r")
#source("qc_scripts_for_cvt_cleaning.r")
#source("cvt_clean.r")
#source("cvt.cleaning.functions.r")
#source("cvt.constant.definitions.r")
#source("cvt_plotting_functions.r")
#source("cvt.meta.functions.r")
#source("find.missing.cvt.value.funcs.r")
#setwd(locations$files)

master.cvt.run <- function(data.update,
                           campaign.type,
                           analysis.type,
                           video.or.all,
                           target.account,
                           account.var.name,
                           date.cutoff,
                           dimensions.to.use,
                           dimension.cutoff,
                           remove.list,
                           model,
                           model.to.use){

  #this msection actaully does thae master model run


  #add filtered results
  #filtered.results <- add.filtered.results(filtered.results, add.universal.results)
  #updated.data <- do.data.update(data.update)  #see if we need to do a re-read or a re-clean
  saved.data <- read.saved.data() #read the data into memory

  #do a bunch of data processing
  check.inputs(campaign.type,analysis.type,video.or.all,saved.data)#check if inputs pass muster
  studyname <- paste(campaign.type, target.account, video.or.all,sep=".")#read data into memory
  study <- get.clean.study(study=saved.data[[campaign.type]], #this passes only the right campaign.type to study
                           target.account,video.or.all,date.cutoff) #this filters by client, date, etc
  study.data <- make.study.data.list(study, target.account,video.or.all)
  dimensions <- set.dimensions(dimension.cutoff,dimensions.to.use, study.data, sample.size)
  
  writeLines(paste("\n with dimension cutoff of ", dimension.cutoff,"%, model running with actual dimensions of ",dimensions,"\n",sep=''))
  
  vars.list <- make.vars.list(study.data,dimensions,analysis.type)
  study.data <- add.study.data(study.data, dimensions, study, clientname, studyname,analysis.type,account.var.name)

  #make the formulas and the models
  formula <- suppressWarnings(make.list.of.formulas(vars.list,analysis.type,remove.list,study.data))
  model <- if(analysis.type=="single"){add.client.models(study.data, formula,model,video.or.all,analysis.type)#this is single client cvt
  } else {add.universal.models(study.data, formula,model,video.or.all)}  #this is univerals cleitn cvt
  #if you get contrast errors, go and add them to remove.list
  #if the final model crashes (NaN error) consider changing dimensions to a lower number from default

  #read the model results
  print.model.results(model)

  #now clean and save the data
  result.num.to.keep <- ifelse(model.to.use=="default",length(model),model.to.use) #sets up the mdoel for saving
  results <- results.list(model,studyname,study.data,video.or.all,result.num.to.keep)  #confidence intervals

  rowcounts <- extract.rowcounts(results[[result.num.to.keep]],model[[result.num.to.keep]])

  final.results<-final.clean.results(results[[result.num.to.keep]]) #does the scaling pre graphing
  return(final.results)
}


extract.rowcounts <- function(one.result,one.model){

  data<-one.model$data

  rowcount<-data.frame()

  rownumber<-1

  for(colnum in 1:length(data)){

    #figure out how many levels the column has
    levels <- levels(data[[colnum]])
    numlevels<-length(levels)

    #if it has 0 levels, it must not be a factor, so skip this column
    if(numlevels == 0) {next}


    factornum <-1
    #now cycle through all the levels of that factor, ading one row to teh table
    for(factornum in 1:numlevels){
      rowcount[rownumber,1] <- names(data[colnum]) #insert name of variable
      rowcount[rownumber,2] <- levels[factornum] #insert name of factor
      rowcount[rownumber,3] <- sum(data[colnum] == levels[factornum]) #this counts the values that match the factor
      rownumber <- rownumber + 1
    }

  }

  rowcount$category <- rowcount[1]
  rowcount$value <- rowcount[2]
  rowcount$count <- rowcount[3]

  total <- nrow(data)
  rowcount$percent <- rowcount[3] / total

  #still need to clean up naming and clean up extra columns

  return(rowcount)
}


make.study.data.list <- function(study, target.account,video.or.all) {

  #clean up study factors
  #study<-droplevels(study, exclude = if(anyNA(levels(all))) NULL else NA)
  #study<-if(video.or.all=="video"){dplyr::filter(study,format=="video"|format==)}else{study}

  #general setup
  study.data<-read.study.data(study) #this is now an array of data at $universal.data and $noise.data

  sample.size<-min(nrow(study.data$noise.data),1000)
  #do dimenension reduction

  return(study.data)
}

read.saved.data<-function(){
  setwd(here())
  setwd('..')
  setwd('cvt.clean')
  setwd('output')
  
  prospecting<-readRDS("prospecting.rds")
  retargeting<-readRDS("retargeting.rds")
  retention<-readRDS("retention.rds")
  frequency<-readRDS("frequency.rds")
  activation<-readRDS("activation.rds")
  prospecting.video<-readRDS("prospecting.video.rds")
  retargeting.video<-readRDS("retargeting.video.rds")
  retention.video<-readRDS("retention.video.rds")
  frequency.video<-readRDS("frequency.video.rds")
  activation.video<-readRDS("activation.video.rds")
  list<-list(
    "prospecting"=prospecting,
    "retargeting"=retargeting,
    "retention"=retention,
    "frequency"=frequency,
    "activation"=activation,
    "prospecting.video"=prospecting.video,
    "retargeting.video"=retargeting.video,
    "retention.video"=retention.video,
    "frequency.video"=frequency.video,
    "activation.video"=activation.video
  )
  setwd(here("data"))
  return(list)
}


get.clean.study<-function(study,target.account,video.or.all,date.cutoff){
  study<-dplyr::filter(study, grepl(target.account,account))
  study<-dplyr::filter(study, date>=date.cutoff)
  study<-if(video.or.all=="video"){dplyr::filter(study,format=="video"|format=="slideshow")}else{study}
  study[] <- lapply(study, function(x) if(is.factor(x)) fct_infreq(x,ordered=NA) else x)
  study
}


function(vector){
  vector %>% fct_collapse(none = c("none","0","")) -> vector
  vector
}

add.filtered.results <- function(filtered.results, add.universal.results) {
  save.data<-readRDS("universal.filtered.results.final.rds")
  current.data<-filtered.results

  #make two lists, one merged, and one not merged
  merged<-c(current.data,save.data)
  no_add<-current.data
  length(no_add)<-length(merged) #make sure they have the same length

  #if add.unversal=true
  output<-list()
  output<-case_when(
    add.universal.results ~ merged,
    TRUE ~ no_add
  )


  #now set up the various strings of names
  names.save.data<-names(save.data)
  names.current.data<-names(current.data)
  names.new<-c(names.current.data,names.save.data)
  length(names.current.data)<-length(names.new)

  #now overwrite the names to correct setup
  names(output) <- case_when(
    add.universal.results ~ names.new,
    TRUE ~ names.current.data
  )

  filtered.output<- output[!sapply(output, is.null)]


  return(filtered.output)
}


make.list.of.formulas <- function(vars.list,analysis.type,remove.list,study.data) {
  formula<-list()
  formula$a.one.both<-make.formula(vars.list$one)
  formula$b.spend.both<-make.formula(vars.list$one,vars.list$spend)
  formula$c.account.uni<-make.formula(vars.list$one,vars.list$spend,vars.list$account)
  formula$d.uni.cvt.uni<-make.formula(vars.list$one,vars.list$spend,vars.list$account,vars.list$universal.cvt)
  formula$f.noise.uni<-make.formula(vars.list$one,vars.list$spend,vars.list$account,vars.list$universal.cvt,vars.list$reduced.noise)

  formula<-if(analysis.type=="single"){client.cvt.formulas(vars.list,formula,study.data)}
    else{alldata.cvt.formulas(vars.list,formula)} #add client part

  #now we remove the remove list from all the formulas
  for(item in 1:length(remove.list)){
    formula$d.uni.cvt.client<-if(analysis.type=="single"){remove.term.from.formula(formula$d.uni.cvt.client,remove.list[[item]])}else{formula$d.uni.cvt.client}
    formula$e.client.cvt.client<-if(analysis.type=="single"){remove.term.from.formula(formula$e.client.cvt.client,remove.list[[item]])}else{formula$e.client.cvt.client}
    formula$f.noise.client<-if(analysis.type=="single"){remove.term.from.formula(formula$f.noise.client,remove.list[[item]])}else{formula$f.noise.client}
    formula$g.video.client<-if(analysis.type=="single"){remove.term.from.formula(formula$g.video.client,remove.list[[item]])}else{formula$g.video.client}
    formula$h.raw.noise.client<-if(analysis.type=="single"){remove.term.from.formula(formula$h.raw.noise.client,remove.list[[item]])}else{formula$g.video.client}
    formula$d.uni.cvt.uni<-if(analysis.type=="all"){remove.term.from.formula(formula$d.uni.cvt.uni,remove.list[[item]])}
    formula$f.noise.uni<-if(analysis.type=="all"){remove.term.from.formula(formula$f.noise.uni,remove.list[[item]])}
    formula$g.video.uni<-if(analysis.type=="all"){remove.term.from.formula(formula$g.video.uni,remove.list[[item]])}
  }

  return(formula)
}

check.inputs<-function(campaign.type,analysis.type,video.or.all,saved.data){
  analysis.error<-if(analysis.type=="single"|analysis.type=="all"){NULL}else{"analysis.type error! must be 'single' or 'all'"}
  campaign.type.error<-if(is.null(nrow(saved.data[[campaign.type]]))){"Campaign type error! Campaign type does not match any available campaign type"}else{NULL}
  video.error<-if(video.or.all=="all"|video.or.all=="video"){NULL}else{"video.type error! must be 'video' or 'all'"}
  
  print("all of these should return NULL:")
  if(exists("analysis.error")){print(analysis.error)}
  if(exists("campaign.type.error")){print(campaign.type.error)}
  if(exists("video.error")){print(video.error)}
  return
}


client.cvt.formulas<-function(vars.list,formula,study.data){
  formula$d.uni.cvt.client<-make.formula(vars.list$one,vars.list$spend,vars.list$universal.cvt)
  formula$e.client.cvt.client<-make.formula(vars.list$one,vars.list$spend,vars.list$universal.cvt,vars.list$client.cvt)
  formula$f.noise.client<-make.formula(vars.list$one,vars.list$spend,vars.list$universal.cvt,vars.list$client.cvt,vars.list$reduced.noise)
  formula$g.video.client<-make.formula(vars.list$one,vars.list$spend,vars.list$universal.cvt,vars.list$client.cvt,vars.list$reduced.noise,vars.list$video.vars)
  formula$g.video.client<-remove.term.from.formula(formula$g.video.client,"spend:format")

  formula$h.raw.noise.client<-make.formula(vars.list$one,
                                           vars.list$spend,
                                           vars.list$universal.cvt,
                                           vars.list$client.cvt,
                                           make.vars(study.data$noise.data))
  return(formula)
}

alldata.cvt.formulas<-function(vars.list,formula){
  formula$g.video.uni<-make.formula(vars.list$one,vars.list$spend,vars.list$account,vars.list$universal.cvt,vars.list$reduced.noise,vars.list$video.vars)
  formula$g.video.uni<-remove.term.from.formula(formula$g.video.uni,"spend:format")
  return(formula)
}


make.vars.list <- function(study.data, dimensions,analysis.type) {
  vars.list<-list()
  vars.list$one<-var.one
  vars.list$spend<-var.spend
  vars.list$account<-var.account
  vars.list$universal.cvt<-make.vars(study.data$universal.data)
  vars.list$reduced.noise<-make.dimension.vars(dimensions)
  vars.list$video.vars<-video.vars

  #still need to do vars.list$client.cvt
  vars.list$client.cvt<-if(analysis.type=="single"){client.vars} #add client part

  return(vars.list)
}


make.vars<-function(data){
  #make an array of the column names
  headers<-colnames(data)
  #figure out how many elements the array has
  length<-as.numeric(length(headers))

  string<-NULL
  i<-1
  #do a loop and assemple the data
  for(i in 1:length){
    string<-paste(string,"spend:",headers[i]," + ",sep="")
  }
  string %>% trim.beginning() %>% trim.end() -> string
  return(string)
}


read.study.data <- function(study) {
  study.universal.data<-get.universal.cvt(study)
  study.noise.data<-get.noise.cvt(study)
  list<-list("universal.data"=study.universal.data,"noise.data"=study.noise.data)
  return(list)
}


make.model.list <- function(study.data, formula) {

  #both types of cvt
  model<-list()
  
  
  model$a.one.both<-make.model(study.data$reduced.all,formula$a.one.both)
  model$b.spend.both<-make.model(study.data$reduced.all,formula$b.spend.both)
  return(model)
}

add.universal.models<-function(study.data,formula,model,video.or.all) {

  model<-make.model.list(study.data, formula) #these are always base case models
  #universal cvt only
  model$c.account.uni<-make.model(study.data$reduced.all,formula$c.account.uni)
  model$d.uni.cvt.uni<-make.model(study.data$reduced.all,formula$d.uni.cvt.uni)
  model$f.noise.uni<-if(video.or.all!="video"){make.model(study.data$reduced.all,formula$f.noise.uni)} else {model$d.uni.cvt.uni}
  model<-if(video.or.all=="video"){add.video.model.all(model, study.data, formula)}else{model}
  return(model)
}




add.client.models<-function(study.data,formula,model,video.or.all,analysis.type) {

  model<-make.model.list(study.data, formula) #these are always base case models

  #client cvt only
  model$d.uni.cvt.client<-make.model(study.data$reduced.all,formula$d.uni.cvt.client)
  model$e.client.cvt.client<-make.model(study.data$reduced.all,formula$e.client.cvt.client)
  model$f.noise.client<-make.model(study.data$reduced.all,formula$f.noise.client)


  model$g.video.client<-if(video.or.all=="video"){make.model(study.data$reduced.all,formula$g.video.client)}

  return(model)
}

add.study.data <- function(study.data, dimensions, study, clientname, studyname,analysis.type,account.var.name) {
  #target.account <- clientname
  #add reduced noise
  study.data$reduced.noise.data<-do.dimensional.reduction(study.data$noise.data,dimensions)

  #make a bunch of intermediate datasets
  study.universal.cvt<-get.universal.cvt(study)   #format etc
  study.delivery.vars<-get.delivery.cvt(study)
  study.video.vars<-get.video.cvt(study)
  reduced.study.noise.vars<-study.data$reduced.noise.data

  #setup study.client cvt only if idoing client analysis
  study.client.cvt<-if(analysis.type=="single"){get.client.cvt(study,clientname)}

  #reassumble the data, depening on analysis type
  reduced.study<-if(analysis.type=="single"){cbind(study.delivery.vars,study.universal.cvt,study.video.vars,study.client.cvt,reduced.study.noise.vars)}else{cbind(study.delivery.vars,study.universal.cvt,study.video.vars,reduced.study.noise.vars)}

  #get video data
  video<-filter(reduced.study,format=="video")

  #write the merged new data do study data
  study.data$reduced.all<-reduced.study
  study.data$reduced.all.video<-video

  study.data$client<-if(analysis.type=="single"){dplyr::select(study,contains(clientname))} else {1}
  study.data$reduced.all<-if(analysis.type=="single"){cbind(study.data$reduced.all,study.data$client)}else{study.data$reduced.all}

  return(study.data)
}


filter.results<-function(start.result){
  #remove rows on the category kill list
  one.result<-dplyr::filter(start.result,!category %in% kill.category)

  #remove rows on the row kill list
  one.result<-dplyr::filter(one.result,!value %in% kill.value)
  one.result<-dplyr::filter(one.result,nchar(value)>0)

  #remove rows with p values under the cutoff
  one.result<-dplyr::filter(one.result,(p.value<p.value.cutoff | is.na(p.value)))

  return(one.result)
}



do.data.update<-function(data.update){

  discard<-if(data.update=="local"){"no.update"}
  else if (data.update=="clean.dont.get") {clean.but.dont.get.data()}
  else if (data.update=="refresh.one"){update.one.account(refresh.account)}
  else if (data.update=="refresh.all"){get.and.clean.data()}
  else ("error")
  return(discard)
}


final.clean.results <- function(one.result)  {

  #this is a duplicate step to performed before, b
  #but is tehre to refilter if you change the kill list without redoing confint
  one.result<-filter.results(one.result)

  #this line removes rows where their uncertainty is larger than the entire category
  one.result <- remove.rows.with.huge.errors(one.result)



  #summarize the table
  one.result.summary <- one.result %>%
    dplyr::group_by(study.subject,category) %>% dplyr::summarize(rows=n(),max.est=max(estimate), min.est=min(estimate),range=max.est-min.est)
  meta.summary <- one.result.summary %>%
    dplyr::group_by(study.subject) %>%
    dplyr::summarize(total.range=sum(range),scalar=100/total.range)

  #merge teh summary data back to the table, and then clean the table up.
  merged.results<-dplyr::left_join(one.result,one.result.summary)
  add.scalar<-dplyr::left_join(merged.results,meta.summary)
  merged.results<-dplyr::select(add.scalar,study.subject,category,value,p.value,estimate,low.value,high.value,min.est,rows,range,scalar) %>%
    dplyr::arrange(-range,-estimate)

  #now filter out rows with no range, or with rowcount =1
  merged.results<-dplyr::filter(merged.results,rows>0&range>0)

  #now make final estiamtes
  final.result <- merged.results %>%
    dplyr::mutate(adj.est=(estimate-min.est)*scalar,adj.low.value=(low.value-min.est)*scalar,adj.high.value=(high.value-min.est)*scalar) %>%
    dplyr::select(study.subject,category,value,p.value,adj.est,adj.low.value,adj.high.value)


  return(final.result)
}


remove.rows.with.huge.errors <- function(one.result) {

  one.result$keep <-"keep"

  #remove anything where its range is larger than the range of everything else in its group
  for (i in 1:nrow(one.result)){
    exam.category<-one.result$category[i]

    exam.value<-one.result$value[i]

    data.to.check<-one.result %>% dplyr::filter(value != exam.value & category == exam.category)

    max.in.data <- max(
      max(data.to.check$high.value,na.rm=TRUE),
      max(data.to.check$estimate,na.rm=TRUE))

    min.in.data <- min(
      min(data.to.check$low.value,na.rm=TRUE),
      min(data.to.check$estimate,na.rm=TRUE))

    one.result$keep[i] <-case_when(
      is.na(one.result$low.value[i]) ~one.result$keep[i],
      is.na(one.result$high.value[i]) ~one.result$keep[i],
      one.result$high.value[i] > max.in.data & one.result$low.value[i] < min.in.data ~ "kill",
      TRUE ~  one.result$keep[i]
    )

  }

  #now remove the ones we dont want
  one.result<- one.result %>% dplyr::filter(keep=="keep")
  return(one.result)
}


get.remove.list <- function(studyname) {
  list.of.lists<-list()

  list.of.lists$retargeting.framebridge<-list("spend:has.logo","spend:approach","spend:framebridge.image.type")


  return(list.of.lists[[studyname]])
}




make.dimension.vars<-function(dimensions){
  nums<-seq(1,dimensions,by=1)
  nums<-paste(nums,collapse=" + spend:coord.Dim.")
  paste("spend:coord.Dim.",nums,sep='')
}
#stuff not in here is spend:year.week, spend:account, spend:campaign.type



add.video.model.all <- function(model, study.data, formula) {
  model$g.video.uni<-make.model(study.data$reduced.all.video,formula$g.video.uni)
  return(model)
}

results.list<-function(model,studyname,study.data,video.or.all,startmodel){
  results<-list(NULL)
  cycles<-length(model)
  for (i in startmodel:cycles){
    print(paste("starting on model #",i," out of ",cycles,sep=""))
    results[[i]]<-clean.results(model[[i]],study.data$reduced.all,studyname,i,video.or.all)
    print(paste("results for model #",i," out of ",cycles," saved",sep=""))
  }
  names(results)<-names(model)
  #save models
  #save.models(model, studyname, results)
  
  print(names(results))
  saveRDS(results,paste(studyname," raw.results.rds",sep=''))
  
  return(results)
}

save.models <- function(model, studyname, results) {
  #save results
  setwd(here("data"))
  saveRDS(model,paste(studyname, "all models.rds",sep=' '))
  
  #get length of mdoel, and then save the two hightest models
  length<-length(model)
  write.csv(results[[length]],file=paste(studyname,names(model[length]),"final results.csv",sep=' '))
  write.csv(results[[length-1]],file=paste(studyname,names(model[length-1]),"final results.csv",sep=' '))
  #write.csv(results[[length-1]],file=paste(studyname,"2nd to last results.csv",sep=' '))
  return
}

results.add.columns <- function(results) {
  
  
  #add two columsn of nulls to results
  results[6]<-"null"
  results[7]<-"null"
  results[8]<-"null"
  
  colnames(results)[6]<-"category"
  colnames(results)[7]<-"value"
  colnames(results)[8]<-"raw.names"
  
  #remove :spend from all results
  results[8]<-results[1]
  results[1] <- lapply(results[1], gsub, pattern='spend:', replacement='')
  

  
  return(results)
  
}

extract.results.values <- function(num.rows, num.headers, results, headers) {
  #loop thorugh all rows, and all combinations of headers
  for (row in 1:num.rows){
    #this is where it loops through all the column names
    for (header.value in 1:num.headers){
      #adds category name in column 6
      results[row,6]<-ifelse(grepl(headers[header.value,1],results[row,1]), #if row value is a match
                             as.character(headers[header.value,1]), #make column 6 have a header value
                             as.character(results[row,6])) #otherwise leave it untouched
      
      #adds variable value in column 7
      results[row,7]<-ifelse(grepl(headers[header.value,1],results[row,1]),
                             gsub(headers[header.value,1], "", results[row,1]),
                             results[row,7])
    }
  }
  return(results)
}


results.add.zero.values <- function(num.headers, results, num.rows, headers, levels) {
  #loop through all the levels and put in zero values
  header.value<-1
  for(header.value in 1:num.headers){
    
    #put in the estimate = 0
    results[num.rows+header.value,2]<-0
    #put the category in the cataegory column
    results[num.rows+header.value,6]<-as.character(headers[header.value,1])
    #put the value in for the defaulty
    results[num.rows+header.value,7]<-ifelse(is.null(levels[[header.value]][1]),"none",levels[[header.value]][1])
  }
  
  results<-dplyr::select(results,category,value,p.value,estimate,raw.names)
  return(results)
}

clean.results<-function(one.model,data,studyname,model.cycle,video.or.all){

  results<-list()
  #remove the rows that are noise variables
  tidy(one.model) %>% filter(!grepl("coord.Dim",term)) -> tidy.model
  
  #filter by video or not
  results<-if(video.or.all=="video") {tidy.model %>% dplyr::filter(str_detect(term,"video"))}
  else {tidy.model}
  
  #make an array of the column names
  headers<-as.data.frame(colnames(data))
  headers %>% filter(!grepl("coord.Dim",colnames(data))) -> headers
  
  #figure out how many elements the array has
  num.headers<-as.numeric(nrow(headers))
  num.rows<-nrow(results[1])
  
  #this adds a bunch of columns we will need later
  results<-results.add.columns(results)
  
  #this goes and read the messy "formatpostcard" and splits it into format and postcard
  results<-extract.results.values(num.rows, num.headers, results, headers)

  #now we are going to add the "baseline rows" for the default levels
  levels<-sapply(data, levels)

  #loop through all the categorise and add zero values
  results<-results.add.zero.values(num.headers, results, num.rows, headers, levels)
  
  #now delete rows we dont want
  results<-filter.results(results)
  
  
  #add confidence intervals
  results<-add.result.confidence.intervals(results,one.model,model.cycle)
  
  #now go add another column and set it to studyname
  results[7]<-studyname
  colnames(results)[7]<-"study.subject"
  
  #remove columnsno one cares about
  results$std.error<-NULL
  results$statistic<-NULL
  
  return(results)
}

add.result.confidence.intervals<-function(results,one.model,model.cycle){
  
  results[6]<-NA
  results[7]<-NA
  
  colnames(results)[6]<-"low.value"
  colnames(results)[7]<-"high.value"
  
  num.rows<-nrow(results[1])
  incomplete<-TRUE
  confint<-NULL
  row<-1
  
  
  while(incomplete){
    confint<-NULL
    tryCatch({
      confint <- suppressMessages(confint.default(one.model,results$raw.names[row], level=.66))
    },error=function(e){cat("Model #",model.cycle," Row #",row," ERROR :",conditionMessage(e),"\n")})
    
    results[row,6]<-if(is.null(confint)){NA}else{as.numeric(confint[[1]])}
    results[row,7]<-if(is.null(confint)){NA}else{confint[[2]]}
    incomplete<-if(row<num.rows){TRUE}else{FALSE}
    incomplete<-if(is.na(results$raw.names[row+1])){FALSE}else{TRUE}
    print(paste("confint for row #", row," out of ",num.rows," finished",sep=''))
    row<-row+1
    
  }
  print(paste("confidence intervals for model #",model.cycle,sep=''))
  
  results$raw.names<-NULL
  #sort by category and impact
  results<-results[order(results$category,-results$estimate),]
  
  return(results)
}

make.formula<-function(...){
  #v1
    formula<-as.formula(paste("purchase ~",paste(..., sep="+")),env=parent.frame())
  trim.end(formula)
  formula
}

add.client.formula <- function(formula, vars.list) {
  formula$client.basic<-make.formula(vars.list$base.var,vars.list$delivery.vars,vars.list$creative.vars,vars.list$client.vars)
  formula$client.maxi<-make.formula(vars.list$base.var,vars.list$delivery.vars,vars.list$creative.vars,vars.list$client.vars,vars.list$dimension.vars)
  return(formula)
}


get.client.cvt<-function(study,clientname){
  study.client.cvt<-dplyr::select(study,contains(clientname))
  study.client.cvt<-cleanup.factors(study.client.cvt)
  return(study.client.cvt)
}

get.delivery.cvt<-function(study){
  study.delivery.vars<-dplyr::select(study,account,spend,spendsquare,spendroot,purchase)
  study.delivery.vars<-cleanup.factors(study.delivery.vars)
  study.delivery.vars
}

reduce.formula<-function(results.maxi,formula.maxi,cutoff){
  
  formula.reduced<-formula.maxi
  dim(results.maxi)[1]-1->numrows
  i<-1
  for (i in 1:numrows){
    
    #see if p value is worse than cutoff
    if(results.maxi[i,5]>cutoff){
      #setup up the string to cut out
      results.maxi[i,1]->remove
      remove<-paste("+ ",remove," ",sep="")
      if(
        #check if the thing you are going to remove is a format variable
        #if it is, skip the cycle
        grepl("coord.Dim",remove,ignore.case=TRUE)
      ){
        print(paste("removing row ",i," value of ",results.maxi[i,1],sep=''))
        #slice the formula and remove the offending variable
        formula.reduced<-remove.term.from.formula(formula.reduced,remove)
        
      }
    }
  }
  formula.reduced
}
step.based.noise.reduction<-function(data,start.model,limit,steps){
  update(start.model,.~.,data=data)->start.model
  model.step<-step(start.model,scope=c(upper=limit$upper,lower=limit$lower),direction="both",trace=10,steps=steps)
  return(model.step)
}

#this fucntion cycles thorugh a bunch of cutoff values and recoreds the AIC
find.optimal.cutoff<-function(results.maxi,formula.maxi,data){
  
  #do some setup:
  cutoff.start<-.04  
  cycles<-5
  aic<-1:cycles
  cutoff.value<-1:cycles
  i<-1
  
  #cycle through the cutoffs, recording the AIC as you go
  for (i in 1:cycles){
    print(paste("starting meta-cycle #",i,sep=""))
    #set active cutoff as a power of 2 of the base cutoff
    cutoff.active<-cutoff.start*(2^(i-1))
    #do teh reduction cycle
    model.reduced<-reduction.cycle(results.maxi,formula.maxi,cutoff.active,data)
    
    #record your results
    aic[i]<-AIC(model.reduced)
    cutoff.value[i]<-cutoff.active
  }
  
  #print the results
  print("aic values are:")
  print(aic)
  print("cutoff values are: (choose cutoff with lowest AIC)")
  print(cutoff.value)

  #return the results you saved
  results<-list(aic)
  results<-list(results,cutoff.value)
  return(results)
}





remove.term.from.formula<-function(formula,remove){

  
  #first
  split<-unlist(
    strsplit(
      as.character(formula[3]),
      split=remove
    )
  )
  #clean up the end of string number 1 and the start of string #2
  if(!is.na(split[1])){
    split[1]<-gsub("\n","",split[1],fixed = TRUE)
    split[1]<-trim.end(split[1])
  }
  if(!is.na(split[2])){
  split[2]<-trim.beginning(split[2])
  }
  #now paste it all nice together again
  formula<-make.formula(split[1],split[2])
  formula<-trim.na.from.formula(formula)
  #end of formula
  formula
}

trim.na.from.formula<-function(one.formula){
  text<-as.character(one.formula)
  text[[3]]<-gsub("\n","",text[[3]],fixed=TRUE)
  text[[3]]<-if(last.2.char(text[[3]])=="NA"){
    text.no.na<-str_sub(text[[3]], 1, str_length(text[[3]])-2)
    trim.end(text.no.na)
  } else {text[[3]]}
  formula.string<-as.formula(paste(text[[2]],text[[1]],text[[3]],sep=" "))
  return(formula.string)
}


remove.account.from.formula<-function(formula){
  
  #setup some variabels for the function
  formula.length<-length(formula)
  remove<-list()
  remove[1]<-"spend:account"
  remove[2]<-"spendsquare:account"
  remove[3]<-"spendroot:account"
  remove.length<-length(remove)
  new.formula<-list()
  new.formula<-formula
  
  #loop through all values of remove[] and all values of formula removing any matches
  
  #the first loop loops through all the items in the list formula
  for (i in 1:formula.length){
    #2nd loop loops through all the items on the 'remove list'
    for (n in 1:remove.length){
      #now we set the big part o fthe i'th iten on new.formula
      new.formula[[i]]<-if(grepl(remove[[n]],as.character(new.formula[[i]][3])))
        #check if it contians the string to remove
      {remove.term.from.text.formula(as.character(new.formula[[i]][3]),remove[[n]])
      } else {
          new.formula[[i]][3]}
          #otherwise return it unchanged
    }
  }
  
  return(new.formula)
}

remove.term.from.text.formula<-function(one.formula,remove.text){
  #first
  split<-unlist(
    strsplit(
      as.character(one.formula),
      split=remove.text
    )
  )
  #clean up the end of string number 1 and the start of string #2
  if(!is.na(split[1])){
    split[1]<-gsub("\n","",split[1],fixed = TRUE)
    split[1]<-gsub("NA","",split[1],fixed = TRUE)
    split[1]<-trim.end(split[1])
  }
  if(!is.na(split[2])){
    split[2]<-trim.beginning(split[2])
  }
  #now paste it all nice together again
  #this uses various edge cases in case one of the edges of the formula is an end
  one.formula<-make.formula(1)
  one.formula<- if(!is.na(split[1]) & !is.na(split[2])){make.formula(split[1],split[2])}
  one.formula<- if(!is.na(split[1]) & is.na(split[2])) {
    make.formula(split[1])
        } else {
        one.formula
        }
  
  one.formula<- if(is.na(split[1]) & !is.na(split[2])) {
    make.formula(split[2])
  } else {
    one.formula
  }
  
  #end of formula
  one.formula
}







print.model.inputs <- function(one.formula, all) {
  #remove unused factor levels
  all<-droplevels(all, exclude = if(anyNA(levels(all))) NULL else NA)
  
  #make list of items
  items<-strsplit(
    as.character(one.formula[3]),
    split="\\+")
  items<-items[[1]]
  items<-lapply(items,gsub,pattern=" ",replacement="")
  items<-lapply(items,gsub,pattern="spend:",replacement="")
  items<-lapply(items,gsub,pattern="\n",replacement="")
  
  
  #turn list into dataframe
  inputs<- data.frame(matrix(unlist(items), nrow=length(items), byrow=T))
  colnames(inputs)<-c("variable")
  inputs$num.values<-0
  names(inputs$num.values)<-"num.values"
  
  
  #cycle through all the items
  num.items<-nrow(inputs)
  for (i in 1:num.items){
    one.item<-as.character(inputs[i,1])
    if(one.item=="1"){next}
    col.num<-match(one.item,names(all))
    inputs$num.values[i]<-nlevels(all[[col.num]])
  }
  inputs %>% dplyr::filter(!str_detect(variable,"coord.Dim")) ->inputs
  
  #remove any terms from formula that need to be removed
  print(paste("for the formula the following levels of data exist",sep=''))
  print(inputs)
  return
}


make.model<-function(data,one.formula){
  
  #do risk management around if the formula is null
  one.formula <- if(is.null(one.formula)){
    make.formula(1)
  } else {
    one.formula
  }
  
  
  #see if we need to remove any terms where levels=1
    #make an array of all the terms we care about
  print.model.inputs(one.formula,data) 
  
  data<-pre.glm.factor.order(data)

  tic("model fitting")  
  #start_time <-Sys.time()
  model <-suppressWarnings(glm2(one.formula,
              data = data,
              family = poisson, 
              weight = spend,
              model=TRUE,
              y = TRUE
  ))
  toc()
 
  #print(paste("model complete in ",time, sep=""))
  model
}

pre.glm.factor.order<-function(data){
  data<-order.factors(data)
  
  
  
  return(data)
}

reduction.cycle<-function(results.maxi,formula.maxi,cutoff,reduced.study){
  #do a 2nd round of reductions if you want
  
  #setup some variables
  i<-1
  success<-FALSE
  formula.reduced<-formula.maxi
  results.maxi.reduced<-results.maxi
  
  #loop through dropping variables until it stops changing
  while (success==FALSE){
    
    #do some setup and housekeeping
    print(paste("starting sub-round #",i,sep=''))
    
    #now this makes the newer shorter formula
    formula.reduced.new<-reduce.formula(results.maxi.reduced,formula.reduced,cutoff)
    
    #see if we break from the loop
    if(nchar(formula.reduced.new[3])==nchar(formula.reduced[3])){
      print(cat("complete after ",i," sub-rounds\n"))
      success<-TRUE
      break
    }
    
    #and then makes an updated model off of that formula
    model.maxi.reduced <-make.model(reduced.study,formula.reduced.new)
    results.maxi.reduced<-tidy(model.maxi.reduced)
        
    #now see if we have broken out of hte loop and stopped editng
    if(nchar(formula.reduced.new[3])==nchar(formula.reduced[3]))
      {success<-TRUE
      print(cat("complete after ",i," sub-rounds\n"))
      break
      }

    #now do housekeeping for next cycle
    formula.reduced<-formula.reduced.new

    i<-i+1
  }
  model.maxi.reduced
}


find.optimal.noise.cutoff <- function(model, formula, studyname,data) {
  cutoff.choices<-find.optimal.cutoff(tidy(model$f.noise.client),formula$f.noise.client,data)
  saveRDS(cutoff.choices,paste(studyname,".cutoff.choices.rds",sep=""))
  
  print(AIC(model$f.noise.client))  #this is for comparison to the cutoff chocies
  #look at the results and choose what you wanta
  print(cutoff.choices)
  return
}

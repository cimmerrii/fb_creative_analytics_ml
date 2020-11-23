#now set up some universal variables
target.account<-""  #set to "" if you are doing multi client cvt
campaign.type<-"retargeting" #set to prospecting, retargeting, etc
analysis.type<-"all" #ONLY can be set to "single" or "all"
video.or.all<-"all" #only video or all
client.vars<-ebates.activation.vars  #ignore if doing multi client cvt
date.cutoff<-as.Date("2010-01-01") #set to "2010-01-01" if you want all data
read.data<-"local" #set to "local" or "query.all" or "query.one" or "re.clean"
target.account<-"" #ONLY used if you choose query.one in the above line
dimensions<-300
target.model<-6 #when you see the AIC model run. set this to the model # that you want




#if models throw "contrst" errors, put items inthe list below to remove them
remove.list<-list("bob")
#custom.ink.prospecting.video list("spend:custom.ink.name.brand","spend:format","spend:call.to.action","spend:emojis.in.pic.or.vid","spend:has.logo","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:has.person.or.part.of.person")
#custom.ink.prospecintg - list("spend:call.to.action","spend:emojis.in.pic.or.vid","spend:has.logo","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:has.person.or.part.of.person")
#custom.ink.retention.video  list("spend:format","spend:custom.ink.people","spend:call.to.action","spend:ad.style","spend:approach","spend:emojis.in.pic.or.vid","spend:has.logo","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image")
#custom.ink.retention list("spend:custom.ink.people","spend:call.to.action","spend:ad.style","spend:approach","spend:emojis.in.pic.or.vid","spend:has.logo","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image")
#custom.ink.retarg.video list("spend:custom.ink.name.brand","spend:format","spend:has.logo","spend:has.person.or.part.of.person","spend:call.to.action","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:ad.style","spend:emojis.in.pic.or.vid","spend:approach")
#custom.ink.retarg- list("spend:has.logo","spend:has.person.or.part.of.person","spend:call.to.action","spend:seasonal.or.holiday.content","spend:sale.or.pricing.in.image","spend:ad.style","spend:emojis.in.pic.or.vid","spend:approach")
#framebridge.retarg- list("spend:has.logo","spend:approach","spend:framebridge.image.type","spend:framebridge.audio.sound")    
#framebridge.retarg.video -list("spend:has.logo","spend:approach","spend:framebridge.image.type","spend:framebridge.audio.sound","spend:format")
#framebridge.prospecting list("spend:has.logo","spend:approach")
#fraemebridge.prospecitng video (buggy)  list("spend:has.logo","spend:approach","spend:framebridge.image.type","spend:framebridge.audio.sound","spend:format")
#universal prospecting- <-list("bob")
#ebates activation <-list("spend:has.logo")
#ebates activation.video list("spend:has.logo","spend:format")



locations <- list(scripts="C:/Users/noah/Dropbox (Social Fulcrum)/Social Fulcrum Company Dropbox/Software/Cross-Client Analytics/Multi Client CVT/Scripts",files="C:/Users/noah/Dropbox (Social Fulcrum)/Social Fulcrum Company Dropbox/Software/Cross-Client Analytics/Multi Client CVT/Data")
setwd(locations$scripts)
source("cvt_general_functions.r")


if(read.data=="local") {"do nothing"} else if (read.data=="query.all") {}


saved.data<-if(exists("saved.data")){saved.data}else{read.saved.data()}



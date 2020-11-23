setwd("C:/Users/Noah/Dropbox (Social Fulcrum)/Social Fulcrum Company Dropbox/Software/R.scripts.and.data/cvt.analysis")

#this next two lines combines install.packages and library()
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr,
               dplyr,
               tidyr,
               lme4,
               optimx,
               forcats,
               coefplot,
               broom,
               #gmodels,
               RMySQL,
               ggplot2,
               MASS,
               broom,
               factoextra,
               FactoMineR,
               Hmisc,
               profvis,
               gsubfn,
               stringr,
               rlist,
               MASS,
               here,
               tictoc,
               glm2,
               taskscheduleR)

options(max.print=99999, #print lots of stuff
        scipen=7,      #use less scientific notation)
        verbose=FALSE)



setwd(here())
setwd('..')
setwd('shared.scripts')
source("sf_r_helper_functions.R")

#load project specific functions
setwd(here("scripts"))
source("cvt_general_functions.r")
source("cvt.constant.definitions.r")
source("cvt.meta.functions.r")
source("cvt_plotting_functions.r")
source("dimension_reduction_functions.r")
source("glm_functions.r")

#now set up the workding directory
setwd(here("data"))

sink("cvt.screen.log.txt",append=TRUE,split=TRUE)

Sys.time()

#SECTION #1 SET UP USER INPUTS---------------------------

#takes a simple account name, like framebridge.  write anyting you want.
#BE SURE TO SET TO "" if you are doing multi account
target.account <- ""

#acceptable answers are: "framebridge.vars","penrose.hill.detail.vars"
#penrose.hill.vars, purple.carrot.vars, leesa.vars, ebates.vars, custom.ink.vars,
#ignore if doing multi client cvt
client.vars <- keto.vars

#what kind of analysis are you doing?
analysis.type <- "all" #ONLY can be set to "single" or "all"- triggers one client vs all client analysis
campaign.type <- "prospecting" #set to prospecting, retargeting, frequency, activation,
video.or.all <- "all" #only video or all- determines if doing a "video detailed analysis or all formats'
add.universal.results <- FALSE #this determiends whether you show just client results, or in contrast to universal results

#any filtering you want to do?
date.cutoff <- as.Date("2010-01-01") #set to "2010-01-01" if you want all data
#set this once you look at the dimension plot curve.

data.update <- "local"  #choose 'local','clean.dont.get','refresh.one','refresh.all'
#use this only if you are doing a partial refresh
#this needs to have one of the following values:
refresh.account <- "Perfect Keto"

#set this to value of results$ that you want to save
#typicall for video.or.all- "video" its <-6, and for "all" its <-5
model.to.use <- "default"  #set to default to save the highest # model, or set to a number to choose one manually
#reduce it if the model refuses to run, otherwise set it to capture ~90-100% of the variations
dimensions.to.use <- "default" #leave this to default if it works- otherwise manually set to lwoer #s if the nosie models crash


#if models throw "contrst" errors, put items inthe list below to remove them
remove.list <- list("spend:leesa.gender","spend:has.logo","spend:call.to.action")

#just run straigh through this to do more environment setup
studyname <- paste(campaign.type, target.account, video.or.all,sep=".") #set up studyanme
updated.data <- do.data.update(data.update) #see if we need to do a huge data re-do, otherwise does nothing
model<-list()

filtered.results <- run.all.client.cvt(filtered.results, data.update, date.cutoff, model)

sink()
#merge data and graph it
all.data <- ldply(filtered.results,data.frame) #merges all the various items of hte filtered.results list
saveRDS(all.data,paste(target.account," all.data.rds",sep = '')) #saves alldata
make.plots(all.data,studyname) #make pdf of alldata



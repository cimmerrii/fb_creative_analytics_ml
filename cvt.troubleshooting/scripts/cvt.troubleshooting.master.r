#SECTION #0 SET UP THE ENVIRONMENT
#Step 0- set up the environment
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
               gmodels,
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
               IndexNumR,
               forecast)

options(max.print=99999, #print lots of stuff
        scipen=7,      #use less scientific notation)
        verbose=FALSE)


#load universal scripts
setwd(here())
setwd('..')
setwd('shared.scripts')
source("sf_r_helper_functions.R")

#load project specific functions
setwd(here("scripts"))
source("find.missing.cvt.value.funcs.r")
source("investigate.outlier.funcs.r")

#now go read the "all" file you are going to need
setwd(here())
setwd('..')
setwd('cvt.clean')
setwd('output')
all<-readRDS("all.cvt.data.3.rds")

#now set up the workding directory
setwd(here("data"))



untagged.images <- return.untagged.images()


slideshow.comparison <- merge.data.w.avg(study,
  zoom.data(study %>% filter(format=="slideshow")))
  
emoji.comparison <- merge.data.w.avg(study,
    zoom.data(study%>% filter(emojis.in.pic.or.vid=="y")))


#zoom in on instagram
ig.campaign <- all %>% 
  dplyr::filter(as.character(has.insta.campaign)=="instagram") %>%
  dplyr::select(account,campaign,campaign.type,format,image.name, image.consensus, utm)

cleanup.factors(ig.campaign) -> ig.campaigns
write.csv(ig.campaign, "campaign includes instagram")

#zoom in on instagram
ig.platform <- all %>% 
  dplyr::filter(as.character(platform)=="igstories") %>%
  dplyr::filter(!grepl("ig_story",as.character(format))) %>%
  dplyr::select(account,platform,campaign.type,format,image.name, image.consensus, utm)

cleanup.factors(ig.platform) -> ig.platform
write.csv(ig.platform, "platform.igstory.weird.format.csv")

#zoom in on script video benefit:
script.video.benefit <- all %>% 
  dplyr::filter(as.character(video.type)=="script video - benefit") %>%
  dplyr::select(account,platform,campaign.type,format,image.name, image.consensus, utm,purchase) %>%
  group_by(account,image.name) %>%
  dplyr::summarise(rows=n(),num_sales=sum(purchase))

cleanup.factors(script.video.benefit) -> script.video.benefit
write.csv(script.video.benefit, "script.video.benefit.csv")

#zoom in on script video overall:
script.video<- all %>% 
  dplyr::filter(grepl("script", as.character(video.type),ignore.case=TRUE)) %>%
  dplyr::select(format, video.type, account,platform,campaign.type,format,image.name, image.consensus, utm,purchase) %>%
  group_by(video.type,account,image.name) %>%
  dplyr::summarise(rows=n(),num_sales=sum(purchase))

cleanup.factors(script.video) -> script.video
write.csv(script.video, "script.video.csv")

#zoom in on 3rd party conetnet
third.party<- all %>% 
  dplyr::filter(grepl("party", as.character(ad.style),ignore.case=TRUE)) %>%
  dplyr::select(account, format, video.type, ad.style,platform,format,image.name, image.consensus, utm,purchase) %>%
  group_by(account,image.name,ad.style) %>%
  dplyr::summarise(rows=n(),num_sales=sum(purchase))

cleanup.factors(third.party) -> third.party
write.csv(third.party, "third.party.csv")

#zoom in on instagram with unlablled type
ig.unlabelled <- all %>% 
  dplyr::filter(as.character(format)=="ig_story") %>%
  factorize() %>%
  group_by(account,image.consensus) %>%
  dplyr::summarise(rows=n(),num_sales=sum(purchase))


cleanup.factors(ig.unlabelled) -> ig.unlabelled
write.csv(ig.platform, "platform.igstory.weird.format.csv")

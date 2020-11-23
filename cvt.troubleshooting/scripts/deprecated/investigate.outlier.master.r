#SECTION #0 SET UP THE ENVIRONMENT
locations <- list(scripts="C:/Users/noah/Dropbox (Social Fulcrum)/Social Fulcrum Company Dropbox/Software/Cross-Client Analytics/Multi Client CVT/Scripts",files="C:/Users/noah/Dropbox (Social Fulcrum)/Social Fulcrum Company Dropbox/Software/Cross-Client Analytics/Multi Client CVT/Data")
setwd(locations$scripts)
source("cvt_general_functions.r")
setwd(locations$scripts)
source("investigate.outlier.funcs.r")



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

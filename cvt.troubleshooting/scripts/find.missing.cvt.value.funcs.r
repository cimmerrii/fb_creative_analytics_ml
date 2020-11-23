


#this function returns a list of images that ran in the past 30 days and have no tages
return.untagged.images <-function(){
  
  setwd(here())
  setwd('..')
  setwd('cvt.clean')
  setwd('data')

    
  data<-readRDS("supertable.post.cleaning.2.rds")
  
  setwd(here())
  
  data<-factorize(data)
  days.to.use <- 30

  today<-Sys.Date()
  start.date <- today - days.to.use

  filtered.data <- data %>%
    dplyr::filter(date > start.date) %>%
    dplyr::filter(as.character(ad.content)=="none") %>%
    dplyr::filter(spend>0) %>%
    dplyr::group_by(account,adset.image, image.consensus) %>%
    dplyr::summarize(max.date=max(date),ttl.spend = sum(spend),count.of.ad.days=n()) %>%
    ungroup()

  write.csv(filtered.data,"images.that.need.tags.csv")

  return(filtered.data)
}

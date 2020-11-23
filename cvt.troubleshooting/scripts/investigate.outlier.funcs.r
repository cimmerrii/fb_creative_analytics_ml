
#load these two functions to memory
get.avg.coca <- function(study) {
  avg.coca <- study %>%
    dplyr::select(account,campaign.type,campaign,format, video.type, image.consensus,image.name,image.url,utm, spend, purchase) %>%
    group_by(account,campaign.type) %>%
    dplyr::summarise(checkouts=sum(purchase),spend=sum(spend),avg.coca=spend/checkouts) %>%
    ungroup() %>%
    dplyr::filter(checkouts != 0) %>%
    arrange(account,campaign.type)
  return(avg.coca)
}


zoom.data<-function(data){  
  zoom.data<-data %>%
    dplyr::select(account,campaign.type,campaign,format, video.type, image.consensus,image.name,image.url,utm, spend, purchase) %>%
    group_by(account, campaign.type, image.consensus, image.name) %>%
    dplyr::summarise(checkouts=sum(purchase),spend=sum(spend),coca=spend/checkouts) %>%
    ungroup() %>%
    arrange(desc(coca))
  return(zoom.data)
}

merge.data.w.avg <- function(study, compare.data) {
  
  avg.coca<-get.avg.coca(study)
  
  merged.data<-left_join(compare.data,avg.coca, by=c("account"="account","campaign.type"="campaign.type")) %>%
    dplyr::select(-"checkouts.y",-"spend.y",-"image.name") %>%
    rename(checkouts=checkouts.x,spend=spend.x) %>%
    arrange(desc(spend))
  
  return(merged.data)
  
}
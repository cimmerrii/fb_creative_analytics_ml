
make.plots <- function(all.data,studyname) {
  
  #this removes duplicates, splits out columms, does foramt cleanup
  all.data <- general.cleanup.preplot(all.data, study.subject)
  
  #remove rows that arent to be used in teh graph
  all.data<-remove.rows.preplot(all.data)
  
  #now find the impact data and add it to all.data
  all.data <- impact.data.preplot(all.data)
  
  #now sort all.data by impact
  all.data<-arrange(all.data,desc(category.impact))
  
  #make a list of all the unique category values
  category.list <- unique(all.data$category)
  
  #make a list of all the colors to call all the projects permanently
  subject.colors <-setNames( c('blue', 'red','orange','purple'), levels(as.factor(all.data$data.source)))
  
  #set seed for jitter
  set.seed(1)
  
  #start the pdf session
  setwd(here("output"))
  pdf(paste(studyname,"plots.pdf",sep=''),onefile=TRUE)
  
  #now loop through all the unique values of category list
  for (i in seq_along(category.list)) {
    
    #prepare the data
    graph.data<-subset(all.data, all.data$category==category.list[i])
    
    #make the graph
    plot<-draw.plot(graph.data, study.subject, subject.colors, category.list, i)
    
    print(plot)
    #ggsave(paste(studyname," output of ",category.list[i],".png",sep=''))
  }
  
  #end teh pdf session
  dev.off()
  setwd(here("data"))
}

draw.plot<- function(graph.data, study.subject, subject.colors, category.list, i) {
  #initiate the plot
  #fred <- 2
  
  plot<-ggplot(
    graph.data,
    aes(x=value,
        y=adj.est,
        color=data.source)
    
  )+ #the fct_reorder sorts x location by value of Y
    
    facet_grid(campaign ~ .,scales="free")+
    
    #make the erorrbars
    geom_errorbar(aes(ymin=adj.low.value,
                      ymax=adj.high.value),
                  size=1.5,
                  width=.15,
                  position=position_jitter(w=.02,h=0)
    )+
    
    
    scale_colour_manual(values=subject.colors,drop=drop)+ #thius makes colors constant across plots
    
    #upper errorbar label
    #geom_text(aes(colour=data.source,label=smartround(adj.high.value),y=adj.high.value),vjust=-.5)+
    
    #lower errorbar label
    #geom_text(aes(colour=data.source,label=smartround(adj.low.value),y=adj.low.value),vjust=1.5)+
    
    #lcentral point label
    #geom_text(aes(colour=data.source,label=smartround(adj.est),y=adj.est,size=3),hjust=-0.5)+
    
    
  #add the labels
  labs(y="Impact on Sales",
       x=tools::toTitleCase(
         category.list[i]),
       title=paste(
         "Choices Within ",
         tools::toTitleCase(
           gsub("."," ",category.list[i],fixed=TRUE)),
         sep = '')
  )+
    
    #this makes the Y axis a bit bigger gfor padding
    scale_y_continuous(expand=c(0.1,0))+
    
    theme_light()+
    theme(plot.title=element_text(size=20,
                                  face="bold",
                                  #family="TT Times New Roman",
                                  color="tomato",
                                  hjust=0.5,
                                  lineheight=1.2),  # title
          plot.subtitle=element_text(size=15,
                                     #family="TT Times New Roman",
                                     face="bold",
                                     hjust=0.5),  # subtitle
          plot.caption=element_text(size=15),  # caption
          axis.title.x=element_text(vjust=0,
                                    size=15),  # X axis title
          axis.title.y=element_text(size=15),  # Y axis title
          axis.text.x=element_text(size=8,
                                   angle = 70,
                                   vjust=.5),  # X axis text
          axis.text.y=element_text(size=10),  # Y axis text
          legend.position="bottom",
          legend.key.size=unit(1,"cm"),
          legend.title=element_blank()
    )+
    guides(color=guide_legend(override.aes=list(alpha=1,shape=10)))
}

general.cleanup.preplot <- function(all.data, study.subject) {
  #clean up duplicates, and set max ranges
  all.data<-group_by(all.data,study.subject,category,value,adj.est)
  all.data<-summarise(all.data,
                      p.value=mean(p.value),
                      adj.low.value=max(display.min.range,mean(adj.low.value)),
                      adj.high.value=min(display.max.range,mean(adj.high.value))
  )
  
  #clean up some text replacement
  all.data<-clean.preplot.text(all.data)
  all.data<-make.text.pretty.preplot(all.data,label.wrap.width)
  
  #break out campaign & data source
  all.data<-separate(all.data,
                     study.subject,
                     into=c("campaign","data.source"),
                     sep=" ",
                     extra="merge",
                     fill="right",
                     remove=FALSE)
  
  #now fill in some data
  all.data$data.source<-ifelse(as.character(all.data$data.source)==" "|is.na(all.data$data.source),
                               "Universal",
                               all.data$data.source)
  all.data$data.source<-gsub("\"","",all.data$data.source,fixed=TRUE)
  all.data$data.source<-trim.end(all.data$data.source)
  
  #split value into value and subvalue on :
  all.data<-separate(all.data,
                     value,
                     into=c("value.toplevel","value.sublevel"),
                     sep=":",
                     extra="merge",
                     fill="left",
                     remove=FALSE)
  
  
  
  
  #set things with NA values to slightly above and below estimates
  all.data$adj.low.value<-ifelse(is.na(all.data$adj.low.value),
                                 all.data$adj.est - .1,
                                 all.data$adj.low.value
  )
  
  all.data$adj.high.value<-ifelse(is.na(all.data$adj.high.value),
                                  all.data$adj.est + .1,
                                  all.data$adj.high.value
  )
  
  return(all.data)
}

clean.preplot.text <- function(all.data) {
  #deal with the issue of format:slideshow vs video.type:slidewshow
  all.data$value<-case_when(
    all.data$value == "slideshow" & all.data$category =="format" ~ "slideshow: deets unknown",
    all.data$value == "slideshow:pic only" & all.data$category =="format" ~ "slideshow: pic only",
    all.data$value == "slideshow:text&pics" & all.data$category =="format" ~ "slideshow: text&pics",
    all.data$value == "slideshow:text only" & all.data$category =="format" ~ "slideshow: text",
    all.data$value == "slideshow" & all.data$category =="video.type" ~ "video of slideshow",
    TRUE ~ all.data$value
  )
  

  
  return(all.data)
}

make.text.pretty.preplot <-function(all.data,label.wrap.width){

  cols <- c(1:3)
  
  #clean up messy naming issues, make capitals pretty, etc
  all.data[,cols]<-lapply(all.data[,cols],function(x) gsub("."," ",x,fixed=TRUE))
  all.data[,cols]<-lapply(all.data[,cols],function(x) gsub("_"," ",x,fixed=TRUE))
  all.data[,cols]<-lapply(all.data[,cols],function(x) tools::toTitleCase(x))

  #these operations only applyt to column 1  
  all.data[1]<-lapply(all.data[1],function(x) gsub("all","",x,fixed=TRUE))
  all.data[1]<-lapply(all.data[1],function(x) gsub("video","",x,fixed=TRUE))  
  #let the long labels wrap around
  all.data$value <- str_wrap(all.data$value, width = label.wrap.width)
  
  return(all.data)
  
}

remove.rows.preplot<-function(all.data){
  #remove the rows that just dont mean anything
  rows<-nrow(all.data)
  all.data$kill.me<-"Unknown"
  
  #figure out rows that have error bars larger than all their neighbors
  for(i in 1:rows){
    #this returns all.data but minus the row you are testing
    exclude.self<-all.data[-i,] #this removes row i from all.data
    
    #select all rows that match study row
    filtered.data<-dplyr::filter(exclude.self,
                                 campaign==all.data$campaign[i],
                                 data.source==all.data$data.source[i],
                                 category==all.data$category[i])
    
    #figure out max range of study row
    max.filtered<-max(filtered.data$adj.high.value)
    min.filtered<-min(filtered.data$adj.low.value)
    
    #set kill me to true if I exceedd the whole range of my neighbors
    all.data$kill.me[i]<-ifelse(all.data$adj.low.value[i] <= min.filtered &
                                  all.data$adj.high.value[i] >= max.filtered,
                                TRUE,
                                FALSE)
  }
  
  #figure out how many rows are no "solo rows" and markt them for killing
  rows.by.category <- all.data %>%
    group_by(campaign, data.source,category) %>%
    summarise(num.rows.in.section = sum(kill.me == FALSE)) %>%
    ungroup()
  all.data<-inner_join(all.data,rows.by.category)
  
  #now remove rows that are kill.me = TRUE OR where rows = 1 or 0
  all.data<-dplyr::filter(all.data, kill.me==FALSE)
  all.data<-dplyr::filter(all.data, num.rows.in.section > 1)
  
  return(all.data)
}

impact.data.preplot <- function(all.data) {
  
  #this finds the impacts for each study area
  category.impact<-all.data %>%
    group_by(study.subject,category) %>%
    summarise(impact=max(adj.high.value)) %>%
    ungroup()
  total.impact<-category.impact %>%
    group_by(category) %>%
    summarise(category.impact=max(impact)) %>%
    ungroup()
  
  
  
  #now take the impact data, and merge it back to all.data
  all.data<-inner_join(all.data,total.impact)
  
  return(all.data)
  
}

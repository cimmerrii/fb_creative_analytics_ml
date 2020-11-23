
#this model starts at client cvt, and tries to add individual noise elements
#set things up
limit<-list(lower=formula$d.uni.cvt.uni,
            upper=formula$f.noise.uni)
start.model<-model$d.uni.cvt.uni
data<-study.data$reduced.all
step.limit<-200

#add clean data to the model
update(start.model,.~.,data=data)->start.model-> step.model -> prev.model
incomplete=TRUE
steps<-1
while(incomplete){
  step.model<-step(step.model,scope=c(upper=limit$upper,lower=limit$lower),direction="both",trace=10,steps=1)
  incomplete<-if(nchar(formula(step.model))[[3]]==nchar(formula(prev.model))[[3]]){FALSE}else{TRUE}
  incomplete<-if(steps>step.limit){FALSE}else{TRUE}
  step.model->prev.model
  steps<-steps+1
}
model$d.uni.cvt.uni.step<-step.model

#see results
lapply(model,AIC)
#save results
results<-results.list(model,studyname,study.data)
# You have to submit the probability scores, not the hard classes.

# If you are using decision trees or random forest here, probability scores can be calculated as

# score=predict(rf_model,newdata= testdata, type="prob")[,1]

# score=predict(tree_model,newdata= testdata, type=‘vector’)[,1]

# Evaluation Criterion : auc score on test data. larger auc score, better Model

# Your auc score for test data should come out to be more than 0.80
library(dplyr)

setwd("C:/Users/91878/Downloads/R files Edvancer/")
getwd()

store_train=read.csv("C:\\Users\\91878\\Downloads\\R files Edvancer\\store_train.csv", stringsAsFactors = F )
store_test=read.csv("C:\\Users\\91878\\Downloads\\R files Edvancer\\store_test.csv", stringsAsFactors = F)

  
  store_test$store=NA  
  
  store_train$data='train' #making column in tain called data
  store_test$data='test'  #making new column called data
  
  store_all=rbind(store_train,store_test)
  
  glimpse(store_all)
  
#Data Preparation##

  lapply(store_all,function(x) sum(is.na(x)))
 # In population there are 2 na 
  # In country there is 1 na
  
  store_all$population[is.na(store_all$population)] <- mean(store_all$population, na.rm = TRUE)
  
#  store_all$country[is.na(store_all$country)] <- mean(store_all$country, na.rm = TRUE)
  
  store_all$country <- ifelse(is.na(store_all$country),
                                names(which.max(table(store_all$country))),
                              store_all$country)
  glimpse(store_all)
   # Now there is no NA
  
  store_all$country=as.character(store_all$country)
  store_all$store=as.factor(store_all$store)
  store_all$storecode=substr(store_all$storecode,1,5)
  store_all$store_Type=as.factor(store_all$store_Type)
  store_all$storecode=as.factor(store_all$storecode)
  
  store_all = store_all%>%
    mutate(tot_sales=(sales0+sales1+sales2+sales3+sales4)) %>% 
    select(-sales0,-sales1,-sales2,-sales3,-sales4)
  # Create dummies #
  
  CreateDummies=function(data,var,freq_cutoff=100){
    t=table(data[,var])
    t=t[t>freq_cutoff]
    t=sort(t)
    categories=names(t)[-1]
    
    for( cat in categories){
      name=paste(var,cat,sep="_")
      name=gsub(" ","",name)
      name=gsub(",","",name)
      name=gsub("\\+","",name)
      name=gsub("-","_",name)
      name=gsub("/","_",name)
      name=gsub("\\?","Q",name)
      name=gsub("<","LT_",name)
      name=gsub(">","GT_",name)
      name=gsub("=","EQ_",name)
      
      data[,name]=as.numeric(data[,var]==cat)
    }
    
    data[,var]=NULL
    return(data)
  }

  store_all = store_all %>%
    select(-Areaname,-Id)

  store_all = CreateDummies(store_all, "state_alpha", 40)
  store_all = CreateDummies(store_all, "country", 100)
  store_all = CreateDummies(store_all, "countyname", 40)
  store_all = CreateDummies(store_all, "countytownname", 40)
  
  glimpse(store_all)

###Now we are done with preparing data , lets separate the data 

 store_train=store_all %>% filter(data=='train') %>% select(-data)
 store_test=store_all %>% filter(data=='test') %>% select(-data,-store)

  # 80:30 ratio

  set.seed(2)
  s=sample(1:nrow(store_train),0.8*nrow(store_train))
  store_train1=store_train[s,]   ##( we have R=6028 and C=86)
  store_train2=store_train[-s,]  ##(we have R=1508 and C=86)

  #Done with data prep, lets build the model now
#  Decision Tree
  library(tree)

  st.tree=tree(store ~ .-State, data=store_train1)
summary(st.tree)
st.tree
  
# Performance on validation set
  val.score <- predict(st.tree, newdata = store_train2, type = 'vector')[, 1]
  pROC::roc(store_train2$store,val.score)$auc
#Area under the curve: 0.8433
 
  st.tree.final = tree(store ~ .-State, data=store_train)
  
  test.score=predict(st.tree.final,newdata=store_test)
  test.score=predict(st.tree.final,newdata=store_test,type='vector')[,1]
  test.score
  
  write.csv(test.score,"Sanskruti_Zodape_P2_part2.csv")
  
  

  # Random Forest
  
  library(randomForest)
  library(cvTools)
  param=list(mtry=c(5,10,15,20,25,35),
             ntree=c(50,100,200,500,700),
             maxnodes=c(5,10,15,20,30,50,100),
             nodesize=c(1,2,5,10))
  subset_paras=function(full_list_para,n=10){
    all_comb=expand.grid(full_list_para)
    s=sample(1:nrow(all_comb),n)
    subset_para=all_comb[s,]
    return(subset_para)
  }
  
  mycost_auc=function(y,what){
    roccurve=pROC::roc(y,what)
    score=pROC::auc(roccurve)
    return(score)
  }
  num_trials=50
  my_params=subset_paras(param,num_trials)
  my_params
  myauc=0
  for(i in 1:num_trials){
    print(paste('starting iteration :',i))
    # uncomment the line above to keep track of progress
    params=my_params[i,]
    k=cvTuning(randomForest,store~.,
               data =store_train1,
               tuning =params,
               folds = cvFolds(nrow(store_train1), K=10, type ="random"),
               cost =mycost_auc, seed =2,
               predictArgs = list(type="prob")
    )
    score.this=k$cv[,2]
    score.this=k$cv[,2]
    if(score.this>myauc){
      print(params)
      #uncomment the line above to keep track of progress
      myauc=score.this
      print(myauc)
      #uncomment the line above to keep track of progress
      best_params=params
    }
    print('DONE')
    #uncomment the line above to keep track of progress
  }
  myauc  
  ## 0.84
  best_params

  #best_params
 # mtry ntree maxnodes nodesize
#  702   35   100       15       10
  
#Lets use these to build our final model.

  store.rf.final=randomForest(store~.,
                           mtry=best_params$mtry,
                           ntree=best_params$ntree,
                           maxnodes=best_params$maxnodes,
                           nodesize=best_params$nodesize,
                           data=store_train1
  )
  test.score=predict(store.rf.final,newdata=store_train2,type="prob")[,2]
  test.score

  fit=randomForest(store~.,data=store_train1)
  fit
  
  library(car)

  prob_pred = predict(fit,newdata = store_train2,"prob")[,2] 
  prob_pred
  
  library(pROC)

auc_score=auc(roc(store_train2$store,test.score))
  auc_score
 #Area under the curve: 0.8432
  

 # ROC-AUC Curve
  install.packages("ROCR")
  library(ROCR)
  
  ROCRPred <- prediction(predictions = test.score,store_train2$store)
  ROCRPerf <- performance(ROCRPred, measure ="tpr", x.measure ="fpr")
  plot(ROCRPerf)


  plot(ROCRPerf, colorize = TRUE)
  plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
  plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
  abline(a=0, b=1)


  auc = performance(ROCRPred, measure = "auc")
  auc = auc@y.values[[1]]
  auc
  auc = round(auc, 4)
  
  legend (.5,.4,auc, title = "AUC", cex =1)
  
  test.score_1=predict(store.rf.final,newdata=store_test)
  
  test.score_1
  
  write.csv(test.score_1,"Sanskruti_Zodape_project2_part2.csv")
  
  




























library(e1071)
library(plyr)
library(class)
library(ggplot2)
library(leaps)
library(randomForest)



#############log reg model##################
get_pred_logreg<- function(train,test)
{
  nf<- ncol(train)
  trainset<- train
  testset<- test
  query<- test[,-nf]
  names(trainset)[nf]<- "y"
  names(testset)[nf]<- "y"
  glm_model<- glm( y ~ .,data = trainset,family=binomial())
  glm<- predict(glm_model,as.data.frame(testset),type = "response")
  glm<- 1-glm
  glm.pred<- data.frame(glm,test[,nf])
  
  return ( glm.pred )
}

#############SVM model##################
get_pred_svm<- function(train,test)
{
  nf<- ncol(train)
  trainset<- train
  testset<- test
  query<- test[,-nf]
  names(trainset)[nf]<- "y"
  names(testset)[nf]<- "y"
  
  svm.model<- svm(as.factor(y) ~ ., data = as.data.frame(trainset),probability=TRUE)
  svm<- predict(svm.model,testset,probability  = TRUE)
  svm<- attr(svm,"probabilities")[,"1"]
  svm.pred<- data.frame(svm,testset[,nf])
  return ( svm.pred )
  
}
############# Naive Bayes model##################
get_pred_nb<- function(train,test)
{
  nf<- ncol(train)
  trainset<- train
  query<- test[,-nf]
  names(trainset)[nf]<- "y"
  
  nb.model<- naiveBayes(y ~ ., data = trainset)
  
  nb<- predict(nb.model, query,type = "raw")
  nb<- nb[,"1"]
  nb.pred<- data.frame(nb,test[,nf])
  return ( nb.pred )
  
}

############# KNN model ##################
get_pred_knn<- function(train,test,k)
{
  nf<- ncol(train)
  input<- train[,-nf]
  query<- test[,-nf]
  knn.model<- knn(as.data.frame(input),as.data.frame(query),cl=train[,nf],k=k,prob = TRUE,use.all = FALSE)
  knn.model1<- attr(knn.model,"prob")
  for(i in 1:length(knn.model))if(knn.model[i]=="0") knn.model1[i]<- 1-knn.model1[i]
  pred_knn<-data.frame(knn.model1,test[,nf])
  return( pred_knn )
}

############# Random Forest model##################
get_pred_randomforest <- function(train,test){
  # Get the dependent variable from the last column
  all_variables <- colnames(train)
  dependent_variable <- all_variables[length(all_variables)]
  frm <- formula(paste(dependent_variable,"~."))
  model <- randomForest(frm, data=train,ntree=30)
  pred <- predict(model, test[-length(test)], type = "prob")
  # Convert to probability of the binary == 1 class
  pred <- pred[,2]
  true <- test[,length(test)]
  result <- as.data.frame(cbind(pred,as.character(true)))
  return(result)
}

############################ k- cross validation ##########################

do_cv_class<- function(df,k,model_name)
{
  # variable declaration, model identification 
  col<- ncol(df)
  row<- nrow(df)
  name<- names(df)
  len<- nchar(model_name)
  if( model_name == "logreg") model<- get_pred_logreg
  if(model_name == "svm") model<- get_pred_svm
  if(model_name == "nb") model<- get_pred_nb
  if(substr(model_name,len-1,len) == "NN") model<- get_pred_knn
  if(model_name == "rf") model<- get_pred_randomforest
  k_list<- as.list(0)
  k_index<- as.list(0)
  
  #generate random indexes and split the indexes into k lists
  partition<- floor( row/k)
  tot<- partition*k
  set.seed(1)
  index<- sample(1:tot,tot)
  x<- seq_along(index)
  k_index<- split(index,ceiling(x/partition)) 
  
  #use index list to divide df into k lists 
  for(i in 1:k)
  {
    k_list[[i]]<- df[index[k_index[[i]]],]
    
  }
  #append left over rows to the last list 
  if(partition*k!=row)
  {
    tot1<- (partition*k)+1
    k_list[[k]]<- rbind(k_list[[k]],df[tot1:row,])
  }
  
  #create training data using k-1 folds and predict the kth fold using a predictive model
  out<- data.frame()
  for(j in 1:k)
  { 
    name<- names(df)
    train_temp<- as.data.frame(matrix(ncol=col))
    names(train_temp)<- name
    train_temp<- na.omit(train_temp)
    list_temp<- k_list[-j]
    test_temp<- as.data.frame(k_list[j])
    
    # prepare training set for each iteration
    for(p in 1:k-1)   
    {
      train_temp<- rbind(train_temp,as.data.frame(list_temp[p]))
    }
    
    #call to prediction models and binding the output of each fold
    if(substr(model_name,len-1,len) == "NN")
    {
      y<- as.integer(substr(model_name,1,len-2))
      pred_val<- model(train_temp,test_temp,y)
    }
    else
    {
      pred_val<- model(train_temp,test_temp)
    }
    
    out<- rbind(out,pred_val)
  }
  return(out)
}


############## Finding measures#####################

get_metrics<- function(df,cutoff = 0.5)
{
  tp<-0
  fp<-0
  fn<-0
  tn<-0
  
  for (i in 1:nrow(df))
  {
    true_val<- df[i,2]
    pred_val<- df[i,1]
    if(pred_val>=cutoff && true_val =="1")
      tp<-tp+1 
    if(pred_val<cutoff && true_val =="1")
      fn<- fn+1
    if(pred_val>=cutoff && true_val =="0")
      fp<- fp+1
    if(pred_val<cutoff && true_val =="0")
      tn<- tn+1
  }
  n<- tp+tn+fp+fn
  tp_rate<- tp/(tp+fn)
  fp_rate<- fp/(tn+fp)
  accuracy<- (tp+tn)/n
  recall<- tp/(tp+fn)
  
  out<- data.frame(tp_rate,fp_rate,accuracy,recall)
  colnames(out)<- c("tp_rate","fp_rate","accuracy","recall")
  return ( out )
}

###################################default class###############################################3

default_do_cv_class<- function(df,k)
{
  # variable declaration, model identification 
  col<- ncol(df)
  row<- nrow(df)
  name<- names(df)
  k_list<- as.list(0)
  k_index<- as.list(0)
  
  #generate random indexes and split the indexes into k lists
  partition<- floor( row/k)
  tot<- partition*k
  set.seed(1)
  index<- sample(1:tot,tot)
  x<- seq_along(index)
  k_index<- split(index,ceiling(x/partition)) 
  
  #use index list to divide df into k lists 
  for(i in 1:k)
  {
    k_list[[i]]<- df[index[k_index[[i]]],]
    
  }
  #append left over rows to the last list 
  if(partition*k!=row)
  {
    tot1<- (partition*k)+1
    k_list[[k]]<- rbind(k_list[[k]],df[tot1:row,])
  }
  
  #create training data using k-1 folds and predict the kth fold using training dataset
  
  out<- data.frame()
  
  for(j in 1:k)
  { 
    name<- names(df)
    train_temp<- as.data.frame(matrix(ncol=col))
    names(train_temp)<- name
    train_temp<- na.omit(train_temp)
    list_temp<- k_list[-j]
    test_temp<- as.data.frame(k_list[j])
    
    # prepare training set for each iteration
    for(p in 1:k-1)   
    {
      train_temp<- rbind(train_temp,as.data.frame(list_temp[p]))
    }
    
    #assess the training dataset to check for the majority classifier
    tcol<- ncol(train_temp)
    trow<- nrow(train_temp)
    count_of_1<- count(train_temp[,tcol])[1,"freq"]
    if(count_of_1 > trow/2) {out2<- rep(1,nrow(test_temp))} else {out2<- rep(0,nrow(test_temp))}
    
    out1<- as.data.frame(cbind(as.data.frame(out2),as.data.frame(test_temp[,tcol])))
    colnames(out1)<- c("pred","actual")
    out<- as.data.frame(rbind(out,as.data.frame(out1)))
  }
  return(out)
}







#####################################################################################################

# Example dataset is from a 1975 energy consumption survey which contains abput 45 variables.

classify_data<-  read.csv("dataset.csv",sep =",",header = TRUE)

#names<- c("TOTSQFT","Electric.Appliance.Use..Estimated.","HD65","Electric.Space.Heat.Use..Estimated.","NHSLDMEM","LPG.Water.Heat.Use..Estimated.",
          #"YEARMADE","Electric.AC.Use..Estimated.","Fuel.Oil.Space.Heat.Use..Estimated.", 
          #"TYPEHUQ","MONEYPY","Natural.Gas.Space.Heat.Use..Estimated.",
          #"X..Heated","LPG.Appliance.Use..Estimated.","LPG.Space.Heat.Use..Estimated.", 
          #"CD65","Natural.Gas.Appliance.Use..Estimated.","ATHOME","Fuel.Oil.Water.Heat.Use..Estimated.",
          #"SDESCENT","X.Attic.Heated","Electric.Freezer.Use..Estimated.",
          #"Natural.Gas.Water.Heat.Use..Estimated.","HBUSNESS","X.Garage.AC", 
          #"HHAGE","URBRUR","Electric.Dryer.Use..","Fuel.Oil.Appliance.Use..Estimated.", 
          #"Electric.Dishwasher.Use..Estimated.","HHSEX","OTHWORK","X.RHMAC", 
          #"Average.Age","X..RHU.Heating","X.Garage.Heated","Total.Consumption.BTU.")

#classify_data<- classify_data[,names]

#select the best feature
leaps<- regsubsets(Total.Consumption.BTU. ~.,data = classify_data,nvmax = 45,nbest = 1)
summary.out<- summary(leaps)
df_sum<- as.data.frame(summary.out$outmat)

#CLASSIFY high and low consumers based on the median value
classify_data$high_low<- ifelse(classify_data$Total.Consumption.BTU.> median(classify_data$Total.Consumption.BTU.),"1","0")

#Remove the total consumption from the dataset
num<- which(colnames(classify_data) == "Total.Consumption.BTU.")
classify_data<- classify_data[,-num]

# defining data frames to hold accuracy at every variable count

svm_acc<- as.data.frame(matrix(NA,2,2))
svm_acc<- na.omit(svm_acc)

log_acc<- as.data.frame(matrix(NA,2,2))
log_acc<- na.omit(log_acc)

nb_acc<- as.data.frame(matrix(NA,2,2))
nb_acc<- na.omit(nb_acc)

nn_acc<- as.data.frame(matrix(NA,2,2))
nn_acc<- na.omit(nn_acc)

def_acc<- as.data.frame(matrix(NA,35,2))


rf_acc<- as.data.frame(matrix(NA,2,2))
rf_acc<- na.omit(rf_acc)



#looping across all variables, in this case 45 and print metrics as every feature is added

for(i in 1:45)
{
  
names<- colnames(df_sum) [which(df_sum[i,]== "*")] 

df1<- classify_data[,names]
names(df1)<- names
df2<- classify_data[,"high_low"]
df2<- as.factor(df2)
df<- as.data.frame(cbind.data.frame(df1,df2),col.names = rbind(names,"high_low"))

message("variables")
print(names)

message("##############", i ," model metrics################################")
log_out<- do_cv_class(df,10,"logreg")
print(get_metrics(log_out))
l_temp<- c(get_metrics(log_out)$accuracy,i)
log_acc<- rbind(log_acc,l_temp)

message("############## RANDOM FOREST################################")
r_out<- do_cv_class(df,10,"rf")
r_out$pred<- as.character(r_out$pred)
print(get_metrics(r_out))
rf_temp<- c(get_metrics(r_out)$accuracy,i)
rf_acc<- rbind(rf_acc,rf_temp)

message( paste("************************* SVM **************************"))
svm_out<- do_cv_class(df,10,"svm")
print(get_metrics(svm_out))
s_temp<- c(get_metrics(svm_out)$accuracy,i)
svm_acc<- rbind(svm_acc,s_temp)

message( paste("************************* NB **************************"))
nb_out<- do_cv_class(df,10,"nb")
print(get_metrics(nb_out))
nb_temp<- c(get_metrics(nb_out)$accuracy,i)
nb_acc<- rbind(nb_acc,nb_temp)

message( paste("************************* KNN **************************"))
nn_out<- do_cv_class(df,10,"5NN")
print(get_metrics(nn_out))
nn_temp<- c(get_metrics(nn_out)$accuracy,i)
nn_acc<- rbind(nn_acc,nn_temp)

message( paste("************************* DEFAULT PREDICTOR **************************"))
def<- default_do_cv_class(df,10)
print(get_metrics(def))
def_temp<- c(get_metrics(def)$accuracy,i)
def_acc<- rbind(def_acc,def_temp)

}


# plot accuracy vs number of features using data collected in the above loop

colnames(nn_acc)<- c("accuracy_knn","num")
colnames(svm_acc)<- c("accuracy_svm","num")
colnames(nb_acc)<- c("accuracy_nb","num")
colnames(def_acc)<- c("accuracy_def","num")
colnames(log_acc)<- c("accuracy_log","num")
colnames(rf_acc)<- c("accuracy_rf","num")


merged<- merge(nn_acc,rf_acc)
merged<- merge(merged,nb_acc)
merged<- merge(merged,svm_acc)
merged<- merge(merged,log_acc)
merged<- merge(merged,def_acc)


p<-ggplot(merged, aes(num))+
  geom_line(aes(y=accuracy_svm,colour="SVM"))+  
  geom_line(aes(y=accuracy_nb, colour="Naive_Bayes"))+
  geom_line(aes(y=accuracy_def,colour="Default"))+
  geom_line(aes(y=accuracy_knn,colour="Knn"))+
  geom_line(aes(y=accuracy_rf,colour="Random_Forest"))+
  scale_colour_manual(name="Classifer",values = c(SVM ="red", Naive_Bayes ="green", Default="blue",Knn = "black",Random_Forest="#CC6666"))
 
  p = p + xlab("Number Of Variables")
  p = p + ylab("Accuracy")
  
  
  # Title
  p = p + ggtitle("Number Of Variables vs Accuracy")
  
  
  # Display
  print(p)



















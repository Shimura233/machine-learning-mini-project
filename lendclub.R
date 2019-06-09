
#PROJECT4
#setseed
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
set.seed(100)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Install packages
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "xgboost"
# you can put all packages you need here. 
)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#data import
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
dataset = read.csv("LoanStats_2007_to_2018Q2.csv", stringsAsFactors = F)
test_2018_3 = read.csv("LoanStats_2018Q3.csv", stringsAsFactors = F)
test_2018_4 = read.csv("LoanStats_2018Q4.csv", stringsAsFactors = F)
split_id = read.csv("Project4_test_id.csv")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#preprocess
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#remove variables
var_select = c('addr_state', 'annual_inc', 'application_type', 'dti', 'earliest_cr_line',
               'emp_length', 'emp_title', 'fico_range_high', 'fico_range_low', 'grade',
               'home_ownership', 'initial_list_status', 'installment', 'int_rate', 'id',
               'loan_amnt', 'loan_status', 'mort_acc', 'open_acc', 'pub_rec',
               'pub_rec_bankruptcies', 'purpose', 'revol_bal', 'revol_util', 'sub_grade',
               'term', 'title', 'total_acc', 'verification_status', 'zip_code')
dataset_sub = subset(dataset,select = var_select)
test_2018_3 = subset(test_2018_3,select = var_select)
test_2018_4 = subset(test_2018_4,select = var_select)

#string to float
percentage_tran = function(x){
  x = gsub("^\\s+|\\s+$", "", x)
  as.double(substr(x,1,nchar(x)-1)) 
}

test_2018_3$int_rate = sapply(test_2018_3$int_rate, percentage_tran)
test_2018_4$int_rate = sapply(test_2018_4$int_rate, percentage_tran)
test_2018_3$revol_util = sapply(test_2018_3$revol_util, percentage_tran)
test_2018_4$revol_util = sapply(test_2018_4$revol_util, percentage_tran)

#function implementing general feature transformations
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
preprocess = function(dataset){
  #response var
  #unique(dataset$loan_status) #Levels: Charged Off Default Fully Paid
  dataset$loan_status = sapply(dataset$loan_status, function(x){if (x=='Fully Paid') 0 else 1})
  
  #var selection: title associated with purpose
  dataset_sub = dataset
  
  #clear data
  #loan_amnt
  dataset_sub$loan_amnt = log(dataset_sub$loan_amnt)
  annual_inc_tran = function(x){
    if(x<=0)
      0
    else
      log(x)
  }
  dataset_sub$annual_inc = sapply(dataset_sub$annual_inc,annual_inc_tran)
  #emp_length: na as others2: maybe conbine
  dataset_sub$emp_length = as.factor(sapply(dataset_sub$emp_length, function(x){if(is.na(x))'other2'else x}))
    
  #dataset_sub$emp_title = dataset$emp_title
  tran_emp_title = function(x){
    if(grepl('manager|supervisor|officer|director|senior|president|associate',x, ignore.case = T)) 
      'manager' 
    else if (grepl('assistant',x, ignore.case = T)) 
      'assistant'
    else if (grepl('teacher|tutor|instructor',x, ignore.case = T)) 
      'teacher'
    else if (grepl('doctor|nurse|medical|therapist',x, ignore.case = T)) 
      'medical'
    else if (grepl('engineer',x, ignore.case = T)) 
      'engineer'
    else if (is.na(x))
      'other2'
    else 
      'other1'
  }
  #dataset_sub$emp_title = dataset$emp_title
  dataset_sub$emp_title = as.factor(sapply(dataset_sub$emp_title, tran_emp_title))
  dataset_sub$title = as.factor(sapply(dataset_sub$title, function(x){if(is.na(x))'other2'else x}))
  
  #pub_rec_bankruptcies: missing value as zero
  dataset_sub$pub_rec_bankruptcies = sapply(dataset_sub$pub_rec_bankruptcies, function(x){if(is.na(x)) 0 else x})
  
  #mort_acc: missing as zero
  dataset_sub$mort_acc = sapply(dataset_sub$mort_acc, function(x){if(is.na(x)) 0 else x})
  
  #revol_util: if revol_bal==0, as 0; else, as group median
  median_until = median(dataset_sub$revol_util,na.rm=T)
  
  tran_revol_util = function(until,bal){
    if(is.na(until)){
      if(bal==0){
        0}
      else{
        median_until}
    }
    else
      until
  }
  
  dataset_sub$revol_util= mapply(tran_revol_util,dataset_sub$revol_util, dataset_sub$revol_bal)
  #earliest_cr_line transform to days
  dataset_sub$earliest_cr_line = sapply(dataset_sub$earliest_cr_line,function(x){2019-as.integer(substring(x,nchar(as.character(x))-3))})
  
  #dti: replace missing value or value above 100 by group median
  median_dti = median(dataset_sub$dti,na.rm = T)
  #dataset_sub$dti = dataset$dti
  tran_dti = function(dti){
    if(is.na(dti)||dti==999){
      median_dti
    }
    else if (dti>300)
      300
    else
      dti
  }
  dataset_sub$dti = sapply(dataset_sub$dti, tran_dti)
  
  #vars to factor
  dataset_sub$title = as.factor(dataset_sub$title)
  dataset_sub$zip_code = as.factor(dataset_sub$zip_code)
  dataset_sub$term = as.factor(dataset_sub$term)
  dataset_sub$grade = as.factor(dataset_sub$grade)
  dataset_sub$sub_grade = as.factor(dataset_sub$sub_grade)
  dataset_sub$home_ownership = as.factor(dataset_sub$home_ownership)
  dataset_sub$verification_status = as.factor(dataset_sub$verification_status)
  dataset_sub$purpose = as.factor(dataset_sub$purpose)
  dataset_sub$addr_state = as.factor(dataset_sub$addr_state)
  dataset_sub$initial_list_status = as.factor(dataset_sub$initial_list_status)
  dataset_sub$application_type = as.factor(dataset_sub$application_type)
  return(dataset_sub)
}

dataset_sub = preprocess(dataset_sub)
test_2018_3 = preprocess(test_2018_3)
test_2018_4 = preprocess(test_2018_4)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#split data of 2017-2018.2Q
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
test1 = dataset_sub[dataset_sub$id %in% split_id$test1,]
test2 = dataset_sub[dataset_sub$id %in% split_id$test2,]
test3 = dataset_sub[dataset_sub$id %in% split_id$test3,]

train1 = dataset_sub[!dataset_sub$id %in% split_id$test1,]
train2 = dataset_sub[!dataset_sub$id %in% split_id$test2,]
train3 = dataset_sub[!dataset_sub$id %in% split_id$test3,]
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#MODEL 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
classifer = function(train, test){
  #input: train and test dataset(format: df)
  #output: prediction of prob
  #transform to matrix: id and lable(loan_status) removed
  train_mat = data.matrix(subset(train,select=-c(id,loan_status))) 
  test_mat = data.matrix(subset(test,select=-c(id,loan_status)))
  #boosting tree
  xgb.model= xgboost(data = train_mat, label= train$loan_status, max.depth= 2, eta= 0.2, nthread = 5, nrounds= 500, verbose = FALSE, objective = "binary:logistic")
  predict_res = predict(xgb.model, test_mat)
  return(predict_res)
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#PREDICTION AND OUTPUT
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#split1
predict1 = classifer(train1, test1)
output <- data.frame("id"=test1$id,"prob"=predict1)
write.csv(output,"mysubmission_test1.txt",row.names=FALSE)

#split2
predict2 = classifer(train2, test2)
output <- data.frame("id"=test2$id,"prob"=predict2)
write.csv(output,"mysubmission_test2.txt",row.names=FALSE)

#split3
predict3 = classifer(train3, test3)
output <- data.frame("id"=test3$id,"prob"=predict3)
write.csv(output,"mysubmission_test3.txt",row.names=FALSE)

#2018Q3
predict.Q3 = classifer(dataset_sub, test_2018_3)
output <- data.frame("id"=test_2018_3$id,"prob"=predict.Q3)
write.csv(output,"mysubmission_2018Q3.txt",row.names=FALSE)

#2018Q4
predict.Q4 = classifer(dataset_sub, test_2018_4)
output <- data.frame("id"=test_2018_4$id,"prob"=predict.Q4)
write.csv(output,"mysubmission_2018Q4.txt",row.names=FALSE)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




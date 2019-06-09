#Set Seed
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
set.seed(100)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Install packages
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "glmnet",
  "text2vec",
  "pROC",
  "MASS"# you can put all packages you need here. 
)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#import and split data
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
myvocab = scan(file = "myvocab.txt", what = character()) #vocabulary 
all = read.table("Project2_data.tsv",stringsAsFactors = F,header = T) #whole dataset
all$review = gsub('<.*?>', ' ', all$review) #simple preprocessing
splits = read.table("Project2_splits.csv", header = T) #split index
train = all[-which(all$new_id%in%splits[,s]),]
test = all[which(all$new_id%in%splits[,s]),]
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#common preprocessing for 3 models
#using text2vec, construct dtm for training data and test data based on vocabulary determined by training data
#as well as generate iterator
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  it_train = itoken(train$review,
                    preprocessor = tolower, 
                    tokenizer = word_tokenizer)
  it_test = itoken(test$review,
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer)
  vocab = create_vocabulary(it_train,ngram = c(1L,3L))
  vocab=prune_vocabulary(vocab,term_count_min=10,term_count_max=20000,doc_proportion_max=0.8,doc_proportion_min=0.001)
  vocab = vocab[vocab$term %in% myvocab, ]
  trigram_vectorizer = vocab_vectorizer(vocab)
  dtm_train = create_dtm(it_train, trigram_vectorizer)
  dtm_test = create_dtm(it_test, trigram_vectorizer)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
#Model1: Logistic Regression with l1 penalty  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  model1 = cv.glmnet(x = dtm_train, y = train$sentiment , family = 'binomial',alpha = 1, nfolds = 10)
  pre_re1 = predict(model1, dtm_test, s="lambda.1se", type = 'response')
  pre_re1=pre_re1[,1]
  names(pre_re1) <- "prob"
  output <- data.frame("new_id"=test$new_id,"prob"=pre_re1)
  write.csv(output,"mysubmission1.txt",row.names=FALSE)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
 
#Further reduce terms based on LASSO results to save training time
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  coefs = as.vector(coef(model1))[-1] #get coefficencies from LASSO
  coef_name = dtm_train@Dimnames[[2]] #get feature names
  coef_name_rm = coef_name[coefs==0]  #get zero feature names
  reduced_data = dtm_train[,!coef_name %in% coef_name_rm]
  reduced_testdata=dtm_test[,!coef_name %in% coef_name_rm]#remove zero features;also the data for model_3
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
#Model2: Discriminant Analysis(Naive Bayes)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
trainMYNB=function(x,y){
    prob=mean(y==1)
    para=apply(x,2,function(te){
      c((sum(y==0)-sum(te[y==0]==0)+0.5)/(sum(y==0)+0.5),(sum(y==1)-sum(te[y==1]==0)+0.5)/(sum(y==1)+0.5))})
    return(list(prob=prob,para=para))
  }# My function to train parameters for naive bayes classifier
predicMYNB=function(x,trained){
  prob=trained$prob
  para=trained$para
  1/(1+(1-prob)/prob*prod(((1-sign(x))*(1-para[1,])+sign(x)*para[1,])/((1-sign(x))*(1-para[2,])+sign(x)*para[2,])))
}# My funxtion to estimate posterior probability based on trained parameters
model2_data=as.matrix(reduced_data)
model2_testdata=as.matrix(reduced_testdata)
MYNB.fit=trainMYNB(model2_data,train$sentiment)
pre_re2=apply(model2_testdata,1,predicMYNB,MYNB.fit)
names(pre_re2) <- "prob"
output <- data.frame("new_id"=test$new_id,"prob"=pre_re2)
write.csv(output,"mysubmission2.txt",row.names=FALSE)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Model3: Logistics Regression with L2 penalty and reduced vocabulary
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
model3 = cv.glmnet(x = reduced_data, y = train$sentiment,family = 'binomial',alpha = 0, nfolds = 10)
pre_re3 = predict(model3, reduced_testdata, s="lambda.1se", type = 'response')
pre_re3=pre_re3[,1]
names(pre_re3) <- "prob"
output <- data.frame("new_id"=test$new_id,"prob"=pre_re3)
write.csv(output,"mysubmission3.txt",row.names=FALSE)  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



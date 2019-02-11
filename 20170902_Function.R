### ライブラリの読み込み
library(MASS)
library(car)

### アタックリスト作成用の関数 ###
get_attack_list<-function(model, data, threshold){
  ypred<-predict(object=model, newdata=data, type="response")
  return(ifelse(ypred > threshold, 1, 0))
}

### パスの設定
setwd("D:/DataMix/06.170812")

### 学習用データ読み込み
bank_marketing_data<-read.csv("homework_data/bank_marketing_train.csv")

### データ編集(変換)
  ### y を 1 or 0 に変換
bank_marketing_data$y <- ifelse(bank_marketing_data$y=="yes",1,0)

### データ編集(追加)
  ### loan と housing のいずれも no かどうか
bank_marketing_data$noloan <- ifelse((bank_marketing_data$loan=="no")*(bank_marketing_data$housing=="no"), "yes", "no")

### カテゴリー変数の変換
bank_marketing_data$job<-as.factor(bank_marketing_data$job)
bank_marketing_data$job<-factor(bank_marketing_data$job,levels=c("unknown", "admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed"))
bank_marketing_data$marital<-as.factor(bank_marketing_data$marital)
bank_marketing_data$marital<-factor(bank_marketing_data$marital,levels=c("unknown","married", "single", "divorced"))
bank_marketing_data$default<-as.factor(bank_marketing_data$default)
bank_marketing_data$default<-factor(bank_marketing_data$default,levels=c("unknown", "no", "yes"))
bank_marketing_data$housing<-as.factor(bank_marketing_data$housing)
bank_marketing_data$housing<-factor(bank_marketing_data$housing,levels=c("unknown","yes","no"))
bank_marketing_data$loan<-as.factor(bank_marketing_data$loan)
bank_marketing_data$loan<-factor(bank_marketing_data$loan,levels=c("unknown","yes","no"))
bank_marketing_data$noloan<-as.factor(bank_marketing_data$noloan)
bank_marketing_data$noloan<-factor(bank_marketing_data$noloan,levels=c("no","yes"))
bank_marketing_data$contact<-as.factor(bank_marketing_data$contact)
bank_marketing_data$contact<-factor(bank_marketing_data$contact,levels=c("telephone","cellular"))
bank_marketing_data$month<-as.factor(bank_marketing_data$month)
bank_marketing_data$month<-factor(bank_marketing_data$month,levels=c("may", "dec", "nov", "apr", "mar", "jul", "aug", "oct", "jun"))
bank_marketing_data$day_of_week<-as.factor(bank_marketing_data$day_of_week)
bank_marketing_data$day_of_week<-factor(bank_marketing_data$day_of_week,levels=c("mon", "tue", "wed", "thu", "fri"))
bank_marketing_data$poutcome<-as.factor(bank_marketing_data$poutcome)
bank_marketing_data$poutcome<-factor(bank_marketing_data$poutcome,levels=c("nonexistent", "failure", "success"))
bank_marketing_data$education<-as.factor(bank_marketing_data$education)

### 学習データとテストデータに分割
set.seed(1236)  # コードの再現性を保つためseedを固定
train_idx<-sample(c(1:dim(bank_marketing_data)[1]), size = dim(bank_marketing_data)[1]*0.7)
train<-bank_marketing_data[train_idx, ]
test<-bank_marketing_data[-train_idx, ]

### ロジスティック回帰の実行
bank_marketing_data.lr<-glm(y~job+marital+default+contact+month+day_of_week+poutcome+cons.conf.idx+nr.employed+noloan, data=train, family="binomial")

### アタックリストの作成 ###
  
  ### 対象データの読み込み
  # 課題データの読み込み
  candidates_data<-read.csv("homework_data/20170902/bank_marketing_test.csv")
  # テストデータの読み込み
  #candidates_data<-test

  ### データ編集(変換)
  ### y を 1 or 0 に変換
  candidates_data$y <- ifelse(candidates_data$y=="yes",1,0)

  ### データ編集(追加)
  ### loan と housing のいずれも no かどうか
  candidates_data$noloan <- ifelse((candidates_data$loan=="no")*(candidates_data$housing=="no"), "yes", "no")
  
  ### カテゴリー変数の変換
  candidates_data$job<-as.factor(candidates_data$job)
  candidates_data$job<-factor(candidates_data$job,levels=c("unknown", "admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed"))
  candidates_data$marital<-as.factor(candidates_data$marital)
  candidates_data$marital<-factor(candidates_data$marital,levels=c("unknown","married", "single", "divorced"))
  candidates_data$default<-as.factor(candidates_data$default)
  candidates_data$default<-factor(candidates_data$default,levels=c("unknown", "no", "yes"))
  candidates_data$housing<-as.factor(candidates_data$housing)
  candidates_data$housing<-factor(candidates_data$housing,levels=c("unknown","yes","no"))
  candidates_data$loan<-as.factor(candidates_data$loan)
  candidates_data$loan<-factor(candidates_data$loan,levels=c("unknown","yes","no"))
  candidates_data$noloan<-as.factor(candidates_data$noloan)
  candidates_data$noloan<-factor(candidates_data$noloan,levels=c("no","yes"))
  candidates_data$contact<-as.factor(candidates_data$contact)
  candidates_data$contact<-factor(candidates_data$contact,levels=c("telephone","cellular"))
  candidates_data$month<-as.factor(candidates_data$month)
  candidates_data$month<-factor(candidates_data$month,levels=c("may", "dec", "nov", "apr", "mar", "jul", "aug", "oct", "jun"))
  candidates_data$day_of_week<-as.factor(candidates_data$day_of_week)
  candidates_data$day_of_week<-factor(candidates_data$day_of_week,levels=c("mon", "tue", "wed", "thu", "fri"))
  candidates_data$poutcome<-as.factor(candidates_data$poutcome)
  candidates_data$poutcome<-factor(candidates_data$poutcome,levels=c("nonexistent", "failure", "success"))
  candidates_data$education<-as.factor(candidates_data$education)

    ### アタックリストの作成
  attack_list<-get_attack_list(model=bank_marketing_data.lr, data=candidates_data, threshold=0.196)
  #attack_list

### ummaryで各種統計値の確認
  summary(bank_marketing_data.lr)
  
### オッズ比の計算
  exp(bank_marketing_data.lr$coefficients)
  
### 期待収益の計算 ###
  ### confusion matrixを作る
  #conf_mat<-table(test$y, attack_list)
  conf_mat<-table(candidates_data$y, attack_list)
  conf_mat
  #conf_mat[1]
  #conf_mat[2]
  #conf_mat[3]
  #conf_mat[4]
  
  #期待収益 は？
  conf_mat[4] * 2000 - (conf_mat[3] + conf_mat[4]) * 500
  
  # 正解率
  accuracy<-(conf_mat[1] + conf_mat[4]) /(conf_mat[1] + conf_mat[2] + conf_mat[3] + conf_mat[4])
  accuracy
  
  # 適合率(precision)
  precision<-conf_mat[4] / (conf_mat[3] + conf_mat[4])
  precision
  
  # 再現率(Recall)
  recall<-conf_mat[4]/ (conf_mat[2] + conf_mat[4]) 
  recall
  
  # F値
  2*precision*recall/(precision+recall)



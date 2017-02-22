install.packages("xgboost")
library("xgboost")
library("dplyr")

#RMSE Function

RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

#Random Forest Cross - Validation
cross_validation<-function(data,fold)
{
  
  m<-(100/fold) 
  for(i in 1:fold)
  {
    smp_size <- floor((0.01*(100-m)) * nrow(data))
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind, ]
    test <- data[-train_ind, ]
    model <- train(SalePrice~ OverallQual+ExterQual+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+GrLivArea+GarageCars+GarageArea+YrSold+YearBuilt+KitchenQual+BsmtFinSF1+OverallCond,
                   data=train , method="rf",ntree=200 )
    predict<-predict(model,test)
    RMSE2[i] <- RMSE(predict, test$SalePrice)
  }
  return(RMSE2)
}

#Global Boosting with cross-validation 
#Including the most important variables from Random forest output

cross_validation_gbm<-function(data,fold)
{
  
  m<-(100/fold) 
  for(i in 1:fold)
  {
    smp_size <- floor((0.01*(100-m)) * nrow(data))
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind, ]
    test <- data[-train_ind, ]
    model <- gbm(SalePrice ~ OverallQual+ExterQual+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+GrLivArea+GarageCars+GarageArea+YrSold+YearBuilt+KitchenQual+BsmtFinSF1+OverallCond, data = train, distribution = "laplace",
                 shrinkage = 0.02,
                 interaction.depth = 10,
                 bag.fraction = 0.50,
                 n.minobsinnode = 20,
                 keep.data = F,
                 verbose = F,
                 n.trees = 1000)
    predict<-predict(model,test,n.trees=1000)
    RMSE2[i] <- RMSE(predict, test$SalePrice)
  }
  return(RMSE2)
}


# Burota for feature selection
set.seed(13)
bor.results <- Boruta(data,data$SalePrice,
                      maxRuns=101,
                      doTrace=0)
#Feature Selection
fix<-TentativeRoughFix(bor.results)
#Fix the tentative variables for which their importance has not yet decided
getSelectedAttributes(fix)
#get the list of selected attributes


#GBM cross-validation with important attributes from Boruta

cross_validation_gbm2<-function(data,fold)
{
  
  m<-(100/fold) 
  RMSE2<-NA
  
  for(i in 1:fold)
  {
    smp_size <- floor((0.01*(100-m)) * nrow(data))
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind, ]
    test <- data[-train_ind, ]
    model <- gbm(SalePrice ~ 
                   MSSubClass +            LotArea +        LotShape +      
                   LandContour +    Neighborhood +   BldgType +       HouseStyle +    
                   OverallQual +    OverallCond +    YearBuilt +      YearRemodAdd +  
                   Exterior1st +    Exterior2nd +    MasVnrArea +     ExterQual +     
                   Foundation +     BsmtQual +       BsmtExposure +   BsmtFinType1 +  
                   BsmtFinSF1 +     BsmtUnfSF +      TotalBsmtSF +    HeatingQC +     
                   CentralAir +     X1stFlrSF +      X2ndFlrSF +      GrLivArea +     
                   BsmtFullBath +   FullBath +       HalfBath +       BedroomAbvGr +  
                   KitchenAbvGr +   KitchenQual +    TotRmsAbvGrd +   Functional +    
                   Fireplaces +     GarageType +     GarageYrBlt +    GarageFinish +  
                   GarageCars +     GarageArea +           PavedDrive +    
                   WoodDeckSF +     OpenPorchSF +    SaleCondition,     
                 
                 data = train, distribution = "laplace",
                 shrinkage = 0.01,
                 interaction.depth = 10,
                 bag.fraction = 0.5,
                 n.minobsinnode = 10,
                 keep.data = F,
                 verbose = F,
                 n.trees = 1000)
    predict<-predict(model,test,n.trees=1000)
    RMSE2[i] <- RMSE(predict, test$SalePrice)
  }
  return(RMSE2)
}




#Cross-validation: XG Boost
#Including only the important variables from boruta feature selection
cross_validation_xgboost<-function(data,fold)
{
  
  m<-(100/fold) 
  A=NULL
  for(i in 1:fold)
  {
    

    
    #data<-data[,c("OverallQual","ExterQual","TotalBsmtSF","X1stFlrSF","X2ndFlrSF",
    #              "GrLivArea","GarageCars","GarageArea","YrSold","YearBuilt",
    #              "KitchenQual","BsmtFinSF1","OverallCond","SalePrice")]
    
    
    var_type<-data.frame(summarise_all(data,class))
    char_var<-colnames(data[which(var_type=="character"|var_type=="factor")])
    char_df<-data[,c(char_var)]
    names<-colnames(char_df)
    for(i in names)
    {
      char_df[,i]<-as.numeric(as.factor(char_df[,i]))
    }
    int_var<-colnames(data[which(var_type=="integer"|var_type=="numeric")])
    int_df<-data[,c(int_var)]
    data<-cbind(int_df,char_df)
    
    smp_size <- floor((0.01*(100-m)) * nrow(data))
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind, ]
    train1<-train
    train<- as.matrix(train, rownames.force=NA)
    train <- as(train, "sparseMatrix")
    out<-c("SalePrice","Id")
    dtrain <- xgb.DMatrix(data=train[,!colnames(train) %in% out],label=train[,c("SalePrice")])
    test <- data[-train_ind, ]
    test1<-test
    test<- as.matrix(test, rownames.force=NA)
    test <- as(test, "sparseMatrix")
    dtest <- xgb.DMatrix(data=test[,!colnames(test) %in% out],label=test[,c("SalePrice")])
    
    
    xgb_params = list(
      
      colsample_bytree = 0.4,
      subsample = 0.8,
      eta = 0.01, 
      objective = 'reg:linear',
      max_depth = 10,
      alpha = 1,
      gamma = 2,
      min_child_weight = 1
    )
    
    #xgboost parameters
    
    #Eta- Shrinkage: how much does the algorithm learn while building consequent trees
    #Gamma- How much of a loss reduction is required for further partition on a leaf node
    #Max Depth= Need to specify the max depth of the tree
    #min_child_weight- Minimum no of instances needed in each node
    #Subsample- decision tree will randomly select the specified proportion of rows as sample
    #col_sample_bytree- subsample ratio of columns while developing trees
    #Alpha- regularization term on weights
    #Base_score- initial prediction score of all instances
    #Objective- specifying the type of learner
    best_n_rounds=2000 # try more rounds
    
    #train data
    gb_dt=xgb.train(xgb_params,dtrain,nrounds = as.integer(best_n_rounds))
    
    #Predict using xgboost
    pred<-predict(gb_dt,dtest)
    
    RMSE_xgb<- RMSE(pred, test1$SalePrice)
    
    A<-c(A,RMSE_xgb)
  }
  return(A)
}



data<-data[,c("MSSubClass","MSZoning","LotFrontage","LotArea",
              "LotShape","LandSlope","Neighborhood","BldgType",
              "HouseStyle","OverallQual","OverallCond","YearBuilt",
              "YearRemodAdd","RoofStyle","Exterior1st","Exterior2nd",
              "MasVnrType","MasVnrArea","ExterQual","Foundation",
              "BsmtQual","BsmtExposure","BsmtFinType1","BsmtFinSF1",
              "BsmtUnfSF","TotalBsmtSF","HeatingQC","CentralAir",
              "X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath",
              "FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr",
              "KitchenQual","TotRmsAbvGrd","Fireplaces","GarageType",
              "GarageYrBlt","GarageFinish","GarageCars","GarageArea",
              "PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch",
              "SaleType","SaleCondition","SalePrice")]



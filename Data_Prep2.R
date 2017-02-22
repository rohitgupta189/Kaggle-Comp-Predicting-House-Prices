#Loading Caret Package
library("caret")

#Importing final train data provided by Saurav
train_final <- read.csv("C:/Users/rohit/Desktop/Spring16/Kaggle/Regression/data/train_final.csv", stringsAsFactors=FALSE)


#Importing final test data provided by Saurav
test_final <- read.csv("C:/Users/rohit/Desktop/Spring16/Kaggle/Regression/data/test_final.csv", stringsAsFactors=FALSE)


#Importing transformed train data provided by Saurav
train_trans <- read.csv("C:/Users/rohit/Desktop/Spring16/Kaggle/Regression/data/train_trans.csv", stringsAsFactors=FALSE)


#Importing transformed test data provided by Saurav
test_trans <- read.csv("C:/Users/rohit/Desktop/Spring16/Kaggle/Regression/data/test_trans.csv", stringsAsFactors=FALSE)



#Finding the variances explained by each variables
x<-nearZeroVar(train_final, saveMetrics = T)

x<-write.csv(x,"C:/Users/rohit/Desktop/Spring16/Kaggle/Regression/output/nearzerovar.csv")
#Exporting x to identify variables with near zero variance

out<- c("LandContour", "Condition2","RoofMatl",	"Heating","KitchenAbvGr",	"GarageYrBlt",
        "GarageQual","EnclosedPorch",	"X3SsnPorch",	"ScreenPorch")
  
#Removing variables with near zero variance
datasetNew <- train_final[, !colnames(train_final) %in% out]

#Considering only integer variables
var_type<-data.frame(summarise_all(datasetNew,class))
int_var<-colnames(datasetNew[which(var_type=="integer"|var_type=="numeric")])
new_data<-datasetNew[,c(int_var)]

new_data$SalePrice <- NULL
new_data$Id <- NULL


library("RED")
a<-col_missing_count(new_data)

#Running Principal component analysis
pca <- prcomp(new_data, scale. = T, center = T)

#Get eigen values
install.packages("factoextra")
library(factoextra)
eigenvalues <- get_eigenvalue(pca)
eigenvalues


#Get the variables and their correlations
pcaVar <- get_pca_var(pca)
pcaVar <- c(pcaVar)
pcaVar <- as.data.frame(pcaVar)

#Considering only those principal components for which eigen values are greater than 1
pcaVarNew <- pcaVar[, 1:9]


#Get the highly informative variables.
var <- pcaVarNew[FALSE,]
k <- 1
for(i in colnames(pcaVarNew)){
  for(j in rownames(pcaVarNew)){
    if(abs(pcaVarNew[j , i]) >= 0.5){
      var[k, i] <- j
      k <- k + 1
    }
  }
  k <- 1
}

)

#Creating interactions corresponding to important PCAs

#PCA1
train_trans$interQual <- (train_trans$OverallQual)*(train_trans$OverallCond)

#PCA2
train_trans$built <- (train_trans$YearBuilt) * (train_trans$BsmtUnfSF)

#PCA3
train_trans$Remod <- (train_trans$YearRemodAdd) * (train_trans$LotArea)

#PCA4
train_trans$Area <- (train_trans$YearBuilt) * (train_trans$MasVnrArea)


train_trans$Basement <- (train_trans$TotalBsmtSF) * (train_trans$BsmtUnfSF)

train_trans$Ground<- (train_trans$GrLivArea) * (train_trans$YearBuilt)

train_trans$Bath <- (train_trans$FullBath) * (train_trans$YearBuilt)

train_trans$AboveGround <- (train_trans$TotRmsAbvGrd) * (train_trans$BedroomAbvGr)

train_trans$livingArea <- (train_trans$TotalBsmtSF) * (train_trans$GrLivArea) 

train_trans$years <- (train_trans$YearBuilt) * (train_trans$YearRemodAdd)




#In test dataset as well

#PCA1
test_trans$interQual <- (test_trans$OverallQual)*(test_trans$OverallCond)

#PCA2
test_trans$built <- (test_trans$YearBuilt) * (test_trans$BsmtUnfSF)

#PCA3
test_trans$Remod <- (test_trans$YearRemodAdd) * (test_trans$LotArea)

#PCA4
test_trans$Area <- (test_trans$YearBuilt) * (test_trans$MasVnrArea)


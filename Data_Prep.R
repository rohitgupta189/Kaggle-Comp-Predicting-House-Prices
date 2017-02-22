library(devtools)
install_github("saushe/RED")
library(RED)
library(caret)
library(gam)

test <- read.csv("C:/Users/rohit/Desktop/Spring16/Kaggle/Regression/data/test.csv")
test_df<- test
rm(test)

train <- read.csv("C:/Users/rohit/Desktop/Spring16/Kaggle/Regression/data/train.csv", stringsAsFactors=FALSE)
hd<- train
rm(train)

##################### Train and Test Missing Value Treatment
# train ROW treatment
Row_missing<- row_missing_count(hd)
# 7 rows have more than 15% missing values
# Dedlete those rows

hd$row_missing<- apply(hd, 1, function(x){length(which(is.na(x)==T))})
hd1<- hd[-which(hd$row_missing>10), ]
hd1<- select(hd1, - row_missing)

# Merge test ad train and perform missing treatment together
test_df$SalePrice<- 1
test_df$type<- "test"
hd1$type<- "train"

Overall_data<- rbind(hd1,test_df)
# There are certain rows which have been wrongly classified numeric. Change class of those rows
Overall_data$MSSubClass<- as.character(Overall_data$MSSubClass)
Overall_data$OverallQual<- as.character(Overall_data$OverallQual)
Overall_data$OverallCond<- as.character(Overall_data$OverallCond)
Overall_data$YearBuilt<- 2016 - Overall_data$YearBuilt
Overall_data$YearRemodAdd<- 2016 - Overall_data$YearRemodAdd
Overall_data$GarageYrBlt<- 2016 - Overall_data$GarageYrBlt
Overall_data$YrSold<- 2016 - Overall_data$YrSold

# Misisng Value Summary
missing<- col_missing_count(Overall_data)

# Five variables have more than 40% missing values. I will drop them

# Lets look at the univariate to decide how we will treat missng values
univchar<-univariate_charvar(Overall_data)
univnum<- univariate_numvar(Overall_data)

################## misisng value for numeric

# for yr. garage built
length(which(Overall_data$GarageYrBlt==Overall_data$YearBuilt)) # most of the values re common so I will replace the missing with year built
Overall_data$GarageYrBlt<- ifelse(is.na(Overall_data$GarageYrBlt)==T,hd1$YearBuilt,hd1$GarageYrBlt)

Overall_data<- mv_treatment_numvar(Overall_data, col.del_cutoff = 0.4, treatment_type = mean)

################# missing values for char vars
# Univariate summary
univchar<-univariate_charvar(Overall_data)
univnum<- univariate_numvar(Overall_data)
univchar$`34_GarageType` # replace missing with unknown
univchar$`36_GarageQual` # replace missing with mode
univchar$`35_GarageFinish` # replace missing with uknown

univchar$`24_BsmtExposure`# replace with mode
univchar$`22_BsmtQual` # replace with unknown

# missing vaue treatment for categorical
Overall_data<- mv_treatment_charvar(Overall_data, default = F, col.del_cutoff = 0.4,
                                    char_var_list1 = c("GarageType", "GarageCond", "GarageFinish", "BsmtQual","MasVnrType", "BsmtFinType1", "BsmtFinType2"),
                                    char_var_list2 = c("GarageQual", "BsmtQual", "BsmtExposure", "Electrical" ))
missing_count<- col_missing_count(Overall_data)

Overall_data<- mv_treatment_charvar(Overall_data, default = F, char_var_list1 = c("BsmtCond" ))

missing_count<- col_missing_count(Overall_data)

Overall_data<- mv_treatment_charvar(Overall_data, default = F, char_var_list2 = c("MSZoning","Utilities", "Functional", "Exterior1st", "Exterior2nd", "KitchenQual", "SaleType" ))
missing_count<- col_missing_count(Overall_data)

####################Variable Transformations#############

# Treatment type
#Cutoff_0.05 <- SaleCondition, RoofMatl,RoofStyle, BldgType, HouseStyle, Condition2,Condition1,Neighborhood,LotConfig,LandSlope, Functional, Heating, Foundation, MasVnrType, Exterior2nd, Exterior1st
#Cutoff_0.06 <- PavedDrive, GarageType,LotShap
#Cutoff 0.02 <-  SaleType
#Cutoff 0.03 <- BsmtFinType2

Overall_data<- replace_charvars(Overall_data, c("SaleCondition","HeatingQC", "RoofMatl","RoofStyle", "BldgType"))

Overall_data<- replace_charvars(Overall_data, c("HouseStyle", "Condition2","Condition1"))

Overall_data<- replace_charvars(Overall_data, c("LotConfig" ))

Overall_data<- replace_charvars(Overall_data, c("Heating", "Foundation", "MasVnrType", "Exterior2nd", "Exterior1st"))

Overall_data<- replace_charvars(Overall_data, c("LandSlope","Functional", "MSZoning"))

Overall_data<- replace_charvars(Overall_data, c( "GarageType","LotShape"), cutoff = 0.06)
Overall_data<- replace_charvars(Overall_data, c("Neighborhood"), 0.01)
Overall_data<- replace_charvars(Overall_data, c("SaleType"), 0.02)
Overall_data<- replace_charvars(Overall_data,c("BsmtFinType2", "BsmtCond", "BsmtQual"), 0.03 )

Overall_data$GarageCond<- ifelse(Overall_data$GarageCond =="Ex", "Gd",
                                 ifelse(Overall_data$GarageCond== "Unknown", "Po", Overall_data$GarageCond))

Overall_data$GarageQual<- ifelse(Overall_data$GarageQual == "Ex", "Gd",Overall_data$GarageQual )

Overall_data$ExterCond<- ifelse(Overall_data$ExterCond == "Ex", "Gd", ifelse(Overall_data$ExterCond == "Po", "Fa",Overall_data$ExterCond ))
Overall_data$OverallQual<- ifelse(Overall_data$OverallQual == "2", "4", ifelse(Overall_data$OverallQual == "3", "4", ifelse(Overall_data$OverallQual == "10", "9",Overall_data$OverallQual)))

Overall_data<- replace_charvars(Overall_data,c("ExterQual"), option = "rep_max", 0.02)


################### More variable transformation on numeric ars ##################

Overall_data$LotFrontage[which(Overall_data$LotFrontage>160)]<- mean(Overall_data$LotFrontage)
Overall_data$LotArea[which(Overall_data$LotArea>40000)]<- mean(Overall_data$LotArea)
# Remove BsmtFinSF2
Overall_data<- select(Overall_data, -(BsmtFinSF2))
Overall_data<- select(Overall_data, -(LowQualFinSF))
# Chnage BsmnFUll Bath, BSmntHalfBath, Ful Bath, Half Bath as categorical
Overall_data$BsmtFullBath[which(Overall_data$BsmtFullBath==3)]<- 2
Overall_data$BsmtFullBath<- as.character(Overall_data$BsmtFullBath)

Overall_data$BsmtHalfBath[which(Overall_data$BsmtHalfBath==2)]<- 1
Overall_data$BsmtHalfBath<- as.character(Overall_data$BsmtHalfBath)

Overall_data$BedroomAbvGr[which(Overall_data$BedroomAbvGr==8)]<- 6
Overall_data$KitchenAbvGr[which(Overall_data$KitchenAbvGr==0)]<- 1
Overall_data$KitchenAbvGr[which(Overall_data$KitchenAbvGr==3)]<- 2

Overall_data$GarageYrBlt[which(Overall_data$GarageYrBlt<1905)]<- 1910
Overall_data$GarageArea[which(Overall_data$GarageArea>1200)]<- mean(Overall_data$GarageArea)

Overall_data$YrSold<- as.character(Overall_data$YrSold)

################ Remove some the variables with zero variations

Overall_data2<- select(Overall_data, - c( Electrical, Utilities, Street, GarageCond,PoolArea,MiscVal))

# Final Missing check
missing_count<- col_missing_count(Overall_data2)

#################### Separate test and trian
train1<- Overall_data2[which(Overall_data2$type=="train"),]
train1<- select(train1, -c(type))
test1<- Overall_data2[which(Overall_data2$type=="test"),]
test1<- select(test1, -c(type, SalePrice))
test1$BsmtFullBath[which(test1$BsmtFullBath== "0.43162245952463")]<- "2"
test1$OverallQual[which(test1$OverallQual== "1")]<- "4"
test1$BsmtHalfBath[which(test1$BsmtHalfBath== "0.0616603513606614")]<- "1"
test1$OverallCond[which(test1$OverallCond== "1")]<- "2"
test1$MSSubClass[ which(test1$MSSubClass=="150")]<- "20"

train1<- train1[-c(1171,519,1286),]

write.csv(train1, "train1.csv")
write.csv(test1, "test1.csv")

#VARIABLE TRANSFORMATION

#Identifying variables that are skewed
skewedVars <- NULL

for(i in names(train1)){
  if(is.numeric(train1[,i])){
    if(i != "SalePrice"){
      if(length(levels(as.factor(train1[,i]))) > 10){
        # Enters this block if variable is non-categorical
        skewVal <- skewness(train1[,i])
        print(paste(i, skewVal, sep = ": "))
        if(abs(skewVal) > 0.5){
          skewedVars <- c(skewedVars, i)
        }
      }
    }
  }
}

#For variables which have zero, we cannot simply take log transformation as log(0)=-inf
#Hence, we will be performing log(1+x) for them

log.train <- train1
log.test <- test1

for(i in skewedVars){
  if(0 %in% train1[, i]){
    log.train[,i] <- log(1+train1[,i])
    log.test[,i] <- log(1+test1[,i])
  }
  else{
    log.train[,i] <- log(train1[,i])
    log.test[,i] <- log(test1[,i])
  }
}
 

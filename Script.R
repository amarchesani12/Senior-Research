#senior Research Project

fit2 <- lm(Distance~Zone +LaunchAngle+ ExitSpeed+ Spin + Inside + High + LaunchDirection, data = SwingsNoOutliers2)
summary(fit2)

qqnorm(resid(fit2))
summary(gvlma(fit2))
#3 of 5 ASSUMPTIONS ARE SATISFIED HERE


#Get rid of Launch DIrection and Inside
fit4 <- lm(Distance~Zone+ExitSpeed+LaunchAngle+Spin+High, data = SwingsNoOutliers2)
summary(fit4)

qqnorm(resid(fit4))
summary(gvlma(fit4))



#Convert all Zone values to dummy variables. 1 if true, 0 if else. Ball is when all values are 0
SwingsNoOutliers2$Z1 <- ifelse(SwingsNoOutliers2$Zone == "1", 1 ,0)
SwingsNoOutliers2$Z2 <- ifelse(SwingsNoOutliers2$Zone == "2", 1 ,0)
SwingsNoOutliers2$Z3 <- ifelse(SwingsNoOutliers2$Zone == "3", 1 ,0)
SwingsNoOutliers2$Z4 <- ifelse(SwingsNoOutliers2$Zone == "4", 1 ,0)


fit7 <- lm(Distance~ExitSpeed+LaunchAngle+Spin+High + Z1 + Z2 + Z3 + Z4, data = SwingsNoOutliers2)
summary(fit7)

summary(gvlma(fit7))
qqnorm(resid(fit7))

plot(fit7)

#Constructing 95% confidence intervals for predicted distance

predict(fit7, data.frame(ExitSpeed = 58.2, LaunchAngle= 22, Spin = 3155, High = "No", Z1 = 0,  Z2 = 0, Z3 = 0, Z4 = 1 ), interval='confidence', level = 0.95)
#Actual result was 112 feet so it's a good prediction

predict(fit7, data.frame(ExitSpeed = 63.2, LaunchAngle= -18, Spin = 2874, High = "No", Z1 = 0,  Z2 = 0, Z3 = 1, Z4 = 0 ), interval='confidence', level = 0.95)
#This one was off, actual distance was 8 feet so it seems the model doesn't take into account launch angle as much as it should








#LOGISTIC REGRESSION
# Build a Regression model.

crs$glm <- glm(HomeRun ~ .,
               data=crs$dataset[crs$train, c(crs$input, crs$target)],
               family=binomial(link="logit"))

#CONFUSION MATRIX - LOGISTIC REGRESSION
# Obtain the response from the Linear model.
crs$pr <- as.vector(ifelse(predict(crs$glm, 
                                   type    = "response",
                                   newdata = crs$dataset[crs$test, c(crs$input, crs$target)]) > 0.5, "1", "0"))

# Generate the confusion matrix showing counts.
rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$HomeRun, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.
(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$HomeRun, crs$pr))





#DECISION TREE
# Build the Decision Tree model.
crs$rpart <- rpart(HomeRun ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

#CONFUSION MATRIX - DECISION TREE
# Obtain the response from the Decision Tree model.
crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)],
                  type="class")

# Generate the confusion matrix showing counts.
rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$HomeRun, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.
(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$HomeRun, crs$pr))






#RANDOM FOREST
crs$rf <- randomForest::randomForest(as.factor(HomeRun) ~ .,
                                     data=crs$dataset[crs$sample, c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=2,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

#CONFUSION MATRIX - RANDOM FOREST
# Obtain the response from the Random Forest model.
crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.
rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.
(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr))





#SVM - RADIAL KERNEL
crs$ksvm <- ksvm(as.factor(HomeRun) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

#CONFUSION MATRIX - SVM RADIAL KERNEL
# Obtain the response from the SVM model
crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.
rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.
(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr))






#SVM - POLYNOMIAL KERNEL (1ST DEGREE)
crs$ksvm <- ksvm(as.factor(HomeRun) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="polydot",
                 kpar=list("degree"=1),
                 prob.model=TRUE)


#CONFUSION MATRIX - SVM POLYNOMIAL KERNEL (1ST DEGREE)
# Obtain the response from the SVM model.
crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.
rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.
(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr))






#SVM - POLYNOMIAL KERNEL (2ND DEGREE)
crs$ksvm <- ksvm(as.factor(HomeRun) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="polydot",
                 kpar=list("degree"=2),
                 prob.model=TRUE)


#CONFUSION MATRIX - SVM POLYNOMIAL KERNEL (2ND DEGREE)
# Obtain the response from the SVM model.
crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.
rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.
(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr))






#SVM - LINEAR KERNEL
crs$ksvm <- ksvm(as.factor(HomeRun) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="vanilladot",
                 prob.model=TRUE)

#CONFUSION MATRIX - SVM LINEAR KERNEL
# Obtain the response from the SVM model.
crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.
rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.
(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$HomeRun, crs$pr))



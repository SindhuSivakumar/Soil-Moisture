library(caret)
library(caTools)
library(ranger)
library(Metrics)
library(randomForest)
library(CAST)
library(viridis)

#numerical_data_char_a <- numerical_data_char %>% filter(landuse1 == 1600)

numerical_data_char$soil_type <- as.factor(numerical_data_char$soil_type)
numerical_data_char$soil_texture <- as.factor(numerical_data_char$soil_texture)

# Splitting into train and test
random_sample <- createDataPartition(numerical_data_char$SM,
                                     p = 0.8, list = FALSE)

# Keeping 80% for the training set
training_data <- numerical_data_char[random_sample, ]
test_data <- numerical_data_char[-random_sample,]
write.csv(training_data, 'E:/Thesis Research/training_data.csv')
write.csv(test_data, 'E:/Thesis Research/test_data.csv')
set.seed(10)
indices <- CreateSpacetimeFolds(training_data,spacevar = "probe_id",
                                k=3)
train_control <- trainControl(method = "cv",index = indices$index, number = 3)
predictors <- c("backscatter_smooth","NDWI","topographic_wetness","landuse1", "soil_texture", "soil_type")


#model, caret train
model1 <- train(training_data[,predictors],training_data$SM, 
                method = 'rf',tuneGrid=data.frame("mtry"=2),
                trControl = train_control, importance = TRUE)
model1

#model, rf
model <- randomForest(training_data[,predictors],training_data$SM, 
                      method = 'rf',tuneGrid=data.frame("mtry"=2),
                      trControl = train_control, importance = TRUE)
model

#boxplot
p.df<- as.data.frame(p)
p_sm_lc <- cbind(test_data, p.df)
sm_landuse <- select(training_data, c("SM","landuse1"))
boxplot(Predicted~Landuse,
        data=pre_test, main="Predicted soil moisture to landuse classes",
        xlab="",
        ylab="predicted soil moisture (mm)",
        names=c("Forest","Arable land","Meadow"),
        col="grey",
        border="black"
)


test_data_f <- test_data[test_data$landuse1 == 1000,]
test_data_a <- test_data[test_data$landuse1 == 1500,]
test_data_m <- test_data[test_data$landuse1 == 1600,]


#variable importance plot
varImp(model1, scale = FALSE)
plot(varImp(model1, scale = FALSE))


r <- rmse(test_data$SM, predict(model1, test_data))
s <- 100/(max(p) - min(p))*r
p <- predict(model1, test_data)
y <- test_data[,2]
1 - sum((y-p)^2)/sum((y-mean(y))^2)

library(forestmangr)
rmse_per(y = test_data$SM,yhat = predict(model1, test_data) )
#rsquared
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

pre <- as.data.frame( predict(model1, test_data))
pre_est <- cbind(pre, test_data)
pre_test <- select(pre_est, c("predict(model1, test_data)", "SM", "landuse1"))
colnames(pre_test) <- c("Predicted", "Observed","Landuse")
plot(Observed ~ Predicted ,pre_test, xlab = "Predicted soil moisture (mm)", ylab = "Observed soil moisture (mm)")
abline(coef = c(0, 1))
fit <- lm(Observed ~ Predicted ,pre_test)
curve(predict(fit,newdata=data.frame(Predicted=x)),add=T)


#predicted to observed plot
x <- Predicted
y <- Observed
ggp <- ggplot(pre_test, aes(Predicted, Observed)) +           # Create basic ggplot
  geom_point() + labs(x = "Predicted soil moisture (mm)", y= "Observed soil moisture (mm)")

ggp 
ggp +                                      # Add regression line
  geom_smooth(method = "lm",
              formula = y ~ x) + geom_abline(slope = 1.5,
                                             intercept = -7.1, linetype = "dashed"
              )  +
  stat_regline_equation(label.y = 35, aes(label = ..rr.label..))

#LR_R = RSQUARE(test_data$SM,pre)
# predicting the target variable
merge_sp_s1_s2 <- as(merge_sp_s1_s2,"SpatRaster")
levels(merge_sp_s1_s2$landuse1) <- data.frame(ID=c(1000,1500,1600), 
                                              landuse1=c("1000","1500","1600"))
levels(merge_sp_s1_s2$soil_type) <- data.frame(ID=c(1,2,3,4), 
                                               soil_type=c("1","2","3","4"))
levels(merge_sp_s1_s2$soil_texture) <- data.frame(ID=c(10,30,40,50,60), 
                                                  soil_texture=c("10","30","40","50","60"))
prediction <- predict(merge_sp_s1_s2,model1,na.rm=TRUE)
plot(prediction, col = rev(viridis(1e3)))


#separately for each landuse
prediction_j <- predict(merge_sp_s1_s2_j, model1, na.rm=TRUE)
plot(prediction_j, col = rev(viridis(1e3)))

prediction_s <- predict(merge_sp_s1_s2_s,model1,na.rm=TRUE)
plot(prediction_s, col = rev(viridis(1e3)))

prediction_d <- predict(merge_sp_s1_s2_d,model1,na.rm=TRUE)
plot(prediction_d, col = rev(viridis(1e3)))


summary(values(prediction))

#boxplot for separate models
#forest
par(mar=c(5, 4, 4, 6))
boxplot(prediction_me,
        main="Predicted soil moisture to Meadow",
        xlab="",
        ylab="",
        # names=c("Forest"),
        col="orange",
        border="brown")
mtext("Predicted soil moisture(mm)",side=2,line=2.5)
mtext("Meadow",side=1,col="black",line=1)


#PDP plot
imp <- randomForest::importance(model)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 3))
for (i in seq_along(impvar)) {
  partialPlot(model, training_data, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(10, 25))
}
par(op)


#PDP plot
partialPlot(model1, training_data, backscatter )

library(latticeExtra)




#smooth data
training_data_s <- train_test_split(ready_data_BS_S, 0.8)[[1]]
test_data_s <- train_test_split(ready_data_BS_S, 0.8)[[2]]

set.seed(1234)
rf <- randomForest( SM ~ backscatter + NDWI + landuse1 + soil_type + soil_texture + topographic_wetness, 
                    data = training_data, 
                    ntree = 350, mtry= 5,  importance = TRUE)
rf
plot(randomForest::importance(rf))
varImpPlot(rf)
prediction <-predict(rf, test_data)

rmse(test_data$SM, predict(model1, test_data))
ffs_mode <- ffs(training_data_s[,2:9], training_data_s$SM, method="rf",minVar=3)

#hanna
set.seed(10)
indices <- CreateSpacetimeFolds(training_data,spacevar = "probe_id",k=5)
ctrl <- trainControl(method='cv', number = 5)
#'
#' #define potential predictors:
predictors <- c("backscatter","NDWI","soil_type","soil_texture","landuse1","topographic_wetness")

model <- train(training_data[,..predictors],training_data$SM,
               method="rf",tuneGrid=data.frame("mtry"=5),
               importance=TRUE,ntree=350,
               trControl=ctrl, na.rm=TRUE)
model
rmse(test_data$SM, predict(model1, test_data))
prediction <- predict(merge_sp_s1_s2,model1,na.rm=TRUE)
plot(prediction)



#testing, hyperparameter selection
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

tuneGrid <- expand.grid(.mtry = c(1: 7))
rf_mtry <- train(SM~ backscatter + NDWI + landuse1 + soil_type + soil_texture + topographic_wetness,
                 data = training_data,
                 method = "rf",
                 metric = "RMSE",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)
best_mtry <- rf_default$bestTune$mtry

#maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(20: 30)) {
  set.seed(1234)
  rf_maxnode <- train(SM~ backscatter + NDWI + landuse1 + soil_type + soil_texture + inclination + height + exposition,
                      data = training_data,
                      method = "rf",
                      metric = "RMSE",
                      #metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

#ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(SM~ backscatter + NDWI + landuse1 + soil_type + soil_texture + inclination + height + exposition,
                       data = training_data,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 30,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#end
fit_rf <- train(SM~ backscatter + NDWI + landuse1 + soil_type + soil_texture + inclination + height + exposition,
                data = training_data,
                method = "rf",
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 350,
                maxnodes = 39)
prediction <-predict(fit_rf, test_data)
rmse(test_data$SM, predict(fit_rf, test_data))
#increase margin 
par(mar = rep(2, 4))


library(dplyr)
library(stats)
library(corrplot)
library(car)
library(qpcR)

###import data
setwd(dir = "C:/Users/MSachs.MSACHS-DELL/Documents/UVA MSDS/STAT 6021/Project/")
diamond.df <- read.csv("diamonds.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
summary(diamond.df)
###lets take a look at the various levels of our categorical variables
colors <- unique(diamond.df$color)
clarity <- unique(diamond.df$clarity)
cut <- unique(diamond.df$cut)
###clarity, cut, and color are all ordinal variables in this case; not purely categorical, so recode accordingly
###update: he wants us to treat them as categorical variables
diamond.df$cut <- as.factor(diamond.df$cut)
diamond.df$clarity <- as.factor(diamond.df$clarity)
diamond.df$clarity <- dplyr::recode_factor(diamond.df$clarity,  `IF` = "FL", `FL` = "FL",
                                           `VVS2` = "VVS", `VVS1` = "VVS",
                                           `VS2` = "VS", `VS1` = "VS",
                                           `SI2` = "SI", `SI1` = "SI")
diamond.df$clarity<-relevel(diamond.df$clarity, ref = "SI")
diamond.df$color <- as.factor(diamond.df$color)
diamond.df$color <- dplyr::recode_factor(diamond.df$color, `D` = "C", `F` = "C", `E` = "C",
                                         `G` = "N1", `H` = "N1", `I` = "N2",`J` = "N2",
                                        `K` = "F")
diamond.df$color<-relevel(diamond.df$color, ref = "N2")
###view scatter plot of variables
pairs(diamond.df, lower.panel = NULL)
###view correlation of variables
res1 <- cor.mtest(diamond.df[,c(1,5)], conf.level = .95)
corrplot.mixed(cor(diamond.df[,c(1,5)]), order = "original",tl.cex = 1,tl.pos = "lt",tl.col = "black", tl.srt = 50, number.cex = 1,p.mat = res1$p, sig.level = .05
)
###create simple regression model with predictor variable of highest correlation
plot(price~carat, data = diamond.df)
###transform predictor first
diamond.df$carat2 <- exp(diamond.df$carat)
###re-plot basic relationship
plot(price~carat2, data = diamond.df)
###this doesnt help, so let's repeat with log transformation
diamond.df$carat3 <- log(diamond.df$carat)
plot(price~carat3, data = diamond.df)
###this still doesnt help! what if we re-plot with a double log?
plot(log(price)~carat3, data = diamond.df)
###look at model with this transformation, which seems much better....but first, what does box cox say?
model.s.tp <-lm(price ~ carat3, data = diamond.df)
bc1 <- boxcox(model.s.tp,lambda = seq(-1, 1, 1/10))
bc1$x[which.max(bc1$y)]
###seems that the double log is recommended per box cox
###create new price
diamond.df$price2.tp <- log(diamond.df$price)
model.s.tp1 <- lm(price2.tp ~ carat3, data = diamond.df)
plot(model.s.tp1)
###transforming the predictor helps when coupled with a transformation of the response, but what if we don't transform the x?
model.s <-lm(price ~ carat, data = diamond.df)
bc <- boxcox(model.s,lambda = seq(-1, 1, 1/10))
bc$x[which.max(bc$y)]
diamond.df$price2 <- diamond.df$price ^ 0.3131
diamond.df$price3 <- diamond.df$price ^ 0.5
diamond.df$price4 <- log(diamond.df$price)
model.s <- lm(price2 ~ carat, data = diamond.df)
model.s2 <- lm(price3 ~ carat, data = diamond.df)
model.s3 <- lm(price4 ~ carat, data = diamond.df)
plot(model.s)
plot(model.s2)
plot(model.s3)
###model 2 seems to do the best, confirm with ACF plot
acf(model.s2$residuals, main="ACF of Residuals")
###lets compare the double log model
acf(model.s.tp1$residuals, main="ACF of Residuals")
###isolated issues of collinearity
###given the collinearity is a bit better with the double log, and the actual x vs y plot is much better, we will go with the double log
summary(model.s.tp1)
###use model to predict price
pred_data = as.data.frame(diamond.df$carat3)
names(pred_data) <- "carat3"
pred_interval <- predict(model.s.tp1, newdata = pred_data, level=0.95, interval="confidence")
pred_val <- predict(model.s.tp1, newdata = pred_data, level=0.95, interval="prediction")
pred <- predict(model.s.tp1, newdata = pred_data)
###combine all predictions into dataframe and isolate observations with largest residuals
prediction.df <- cbind.data.frame(diamond.df$price2.tp,pred,c(diamond.df$price2.tp - pred), pred_interval,pred_val)
prediction.df <- prediction.df[,-c(4,7)]
names(prediction.df) <- c("Actual Transformed Price","Predicted Transformed Price","Residual","Confidence Lower Bound","Confidence Upper Bound","Prediction Lower Bound","Prediction Upper Bound")
prediction.df <- prediction.df[rev(order(abs(prediction.df$Residual),prediction.df$Residual)),]
###which observations occur outside of the prediction interval
prediction.df_outliers<- prediction.df[prediction.df$`Actual Transformed Price`<prediction.df$`Prediction Lower Bound` | prediction.df$`Actual Transformed Price`>prediction.df$`Prediction Upper Bound`,]
###what is the actual $ residual??
prediction.df_outliers$`Actual Price` <- exp(prediction.df_outliers$`Actual Transformed Price`)
prediction.df_outliers$`Predicted Price` <- exp(prediction.df_outliers$`Predicted Transformed Price`)
prediction.df_outliers$`Residual $` <- prediction.df_outliers$`Actual Price` - prediction.df_outliers$`Predicted Price`
###reorder based on $ residual
prediction.df_outliers <- prediction.df_outliers[rev(order(abs(prediction.df_outliers$`Residual $`),prediction.df_outliers$`Residual $`)),]
prediction.df_outliers$`Value Proposition` <- ifelse(prediction.df_outliers$`Residual $` > 0 , "Overvalued","Undervalued")
###if we recall our outliers that were identified in our plots, we have 728, 364, and 384
prediction.df_outliers_confirmed <- prediction.df_outliers[rownames(prediction.df_outliers) %in% c(728,364,384),]
###sure enough, these were some of the outliers with the highest residuals, and they showed up in our dataframe. the second largest outlier did not show as an outiler per cooks distance, but it did on the alternative model
original_outliers <- diamond.df[rownames(diamond.df) %in% rownames(prediction.df_outliers),]
original_outliers_subset <- diamond.df[rownames(diamond.df) %in% c(384,940,389),]
###create basic linear model using first order terms only
model <- lm(price2.tp ~ carat3 + cut + color + clarity, data = diamond.df)
summary(model)
anova(model)
###we see that every predictor variable is statistically significant; let's take a look at a model that incorporates interaction effects
model2 <- lm(price2.tp ~ carat3 + clarity + cut + color + carat3:clarity + carat3:cut + carat3:color + clarity:cut + clarity:color + cut:color, data = diamond.df)
summary(model2)
anova(model2)
###although the anova looks great, there are quite a few interactions that aren't significant; what if we drop the variables and stick with model over model2
anova(model,model2)
###statistically significant, so we can't drop the variables!
#use vif to gauge collinearity of original and expanded models
vif(model)
#vif(model2)
###could we bring in even higher order terms??
model3 <-  lm(price2.tp ~ carat3 + clarity + cut + color + carat3:clarity + carat3:cut + carat3:color + clarity:cut + clarity:color + cut:color + carat3:clarity:cut + carat3:clarity:color + carat3:cut:color + clarity:color:cut, data = diamond.df)
summary(model3)
anova(model3)
###all statistically significant! let's compare it to model2
anova(model2,model3)
###model 3 has the most explanative power, but notice that it is incremental only
#vif(model3)
PRESS(model3)
###value is 0.9735513, we didn't overfit

library(dplyr)
library(stats)
library(corrplot)
library(car)
library(qpcR)
library(ggplot2)

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
diamond.df$cut <- relevel(diamond.df$cut, ref = "Good")
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
###add box plots for categorical levels vs price
boxplot(price~color,data=diamond.df, main="Diamond Price By Color",
        xlab="Color", ylab="Price")
boxplot(price~cut,data=diamond.df, main="Diamond Price By Cut",
        xlab="Cut", ylab="Price")
boxplot(price~clarity,data=diamond.df, main="Diamond Price By Clarity",
        xlab="Clarity", ylab="Price")
###add dataframe with categorical level counts
observation_count <- aggregate(diamond.df$price,by = list(diamond.df$color),function(x){NROW(x)})
names(observation_count) <- c("Level","Count")
observation_count$Variable <- "Color"
observation_count2 <- aggregate(diamond.df$price,by = list(diamond.df$cut),function(x){NROW(x)})
names(observation_count2) <- c("Level","Count")
observation_count2$Variable <- "Cut"
observation_count3 <- aggregate(diamond.df$price,by = list(diamond.df$clarity),function(x){NROW(x)})
names(observation_count3) <- c("Level","Count")
observation_count3$Variable <- "Clarity"
observation_count <- merge(observation_count, observation_count2, by = c("Variable","Level","Count"), all = TRUE)
observation_count <- merge(observation_count, observation_count3, by = c("Variable","Level","Count"), all = TRUE)
###limited sample sizes for astor ideal cut and FL clarity
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
###create graph for prediction interval
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
original_outliers_subset <- diamond.df[rownames(diamond.df) %in% c(728,364,384),]
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
#vif(model)
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
###value is 0.9735513, we didn't overfit!!! let's repeat our outlier analysis from our updated model
###use model to predict price
pred_data2 = as.data.frame(diamond.df[,c(2:4,7)])
pred_interval2 <- predict(model3, newdata = pred_data2, level=0.95, interval="confidence")
pred_val2 <- predict(model3, newdata = pred_data2, level=0.95, interval="prediction")
pred2 <- predict(model3, newdata = pred_data2)
###create graph for prediction interval
###combine all predictions into dataframe and isolate observations with largest residuals
prediction.df2 <- cbind.data.frame(diamond.df$price2.tp,pred2,c(diamond.df$price2.tp - pred2), pred_interval2,pred_val2)
prediction.df2 <- prediction.df2[,-c(4,7)]
names(prediction.df2) <- c("Actual Transformed Price","Predicted Transformed Price","Residual","Confidence Lower Bound","Confidence Upper Bound","Prediction Lower Bound","Prediction Upper Bound")
prediction.df2 <- prediction.df2[rev(order(abs(prediction.df2$Residual),prediction.df2$Residual)),]
###which observations occur outside of the prediction interval
prediction.df_outliers2<- prediction.df2[prediction.df2$`Actual Transformed Price`<prediction.df2$`Prediction Lower Bound` | prediction.df2$`Actual Transformed Price`>prediction.df2$`Prediction Upper Bound`,]
###what is the actual $ residual??
prediction.df_outliers2$`Actual Price` <- exp(prediction.df_outliers2$`Actual Transformed Price`)
prediction.df_outliers2$`Predicted Price` <- exp(prediction.df_outliers2$`Predicted Transformed Price`)
prediction.df_outliers2$`Residual $` <- prediction.df_outliers2$`Actual Price` - prediction.df_outliers2$`Predicted Price`
###reorder based on $ residual
prediction.df_outliers2 <- prediction.df_outliers2[rev(order(abs(prediction.df_outliers2$`Residual $`),prediction.df_outliers2$`Residual $`)),]
prediction.df_outliers2$`Value Proposition` <- ifelse(prediction.df_outliers2$`Residual $` > 0 , "Overvalued","Undervalued")
###sure enough, these were some of the outliers with the highest residuals, and they showed up in our dataframe. the second largest outlier did not show as an outiler per cooks distance, but it did on the alternative model
original_outliers2 <- diamond.df[rownames(diamond.df) %in% rownames(prediction.df_outliers2),]
###lets see the prediction interval plotted
ggplot(data=diamond.df, aes(x = carat3)) + geom_point(aes(y = price2.tp)) + 
        geom_ribbon(data=cbind.data.frame(pred_val2,diamond.df), aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) 



####bring in market data
market.df <- read.csv("market diamonds.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
new_colors <- unique(market.df$color)[!(unique(market.df$color) %in% colors)]
new_cuts <- unique(market.df$cut)[!(unique(market.df$cut) %in% cut)]
new_clarity <- unique(market.df$clarity)[!(unique(market.df$clarity) %in% clarity)]
####hmmmm, no new colors, but the market has 1 new clarity level and 2 "new" cuts; the premium seems to be a replacement for astor ideal -- let's rename
market.df$cut <- gsub("Premium","Astor Ideal",market.df$cut)
###let's make sure none of our blue nile diamonds are included in our market proxy; remove extraneous variables and use anti-join
market.df <- market.df[,c(1:4,7)]
orig.di.df <- read.csv("diamonds.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
market.df2 <- anti_join(market.df,orig.di.df, by = names(market.df))
NROW(market.df) - NROW(market.df2)
###there were 17 diamonds in our market data that were from blue nile; these are excluded so as no to influence our model
###similar to our blue nile data, lets ignore whether these variables are ordinal and treat them all as categorical. need to recode and re-level
###we do have to change some base classes with the introduction of "Fair" cut and "I1", clarity
market.df <- market.df2
market.df$cut <- as.factor(market.df$cut)
market.df$cut <- relevel(market.df$cut, ref = "Fair")
market.df$clarity <- as.factor(market.df$clarity)
market.df$clarity <- dplyr::recode_factor(market.df$clarity,  `IF` = "FL", `FL` = "FL",
                                           `VVS2` = "VVS", `VVS1` = "VVS",
                                           `VS2` = "VS", `VS1` = "VS",
                                           `SI2` = "SI", `SI1` = "SI", `I1` = "I")
market.df$clarity<-relevel(market.df$clarity, ref = "I")
market.df$color <- as.factor(market.df$color)
market.df$color <- dplyr::recode_factor(market.df$color, `D` = "C", `F` = "C", `E` = "C",
                                         `G` = "N1", `H` = "N1", `I` = "N2",`J` = "N2",
                                         `K` = "F")
market.df$color<-relevel(market.df$color, ref = "N2")
###view scatter plot of variables
pairs(market.df, lower.panel = NULL)
###add box plots for categorical levels vs price
boxplot(price~color,data=market.df, main="Diamond Price By Color",
        xlab="Color", ylab="Price")
boxplot(price~cut,data=market.df, main="Diamond Price By Cut",
        xlab="Cut", ylab="Price")
boxplot(price~clarity,data=market.df, main="Diamond Price By Clarity",
        xlab="Clarity", ylab="Price")
###add dataframe with categorical level counts
m_observation_count <- aggregate(market.df$price,by = list(market.df$color),function(x){NROW(x)})
names(m_observation_count) <- c("Level","Count")
m_observation_count$Variable <- "Color"
m_observation_count2 <- aggregate(market.df$price,by = list(market.df$cut),function(x){NROW(x)})
names(m_observation_count2) <- c("Level","Count")
m_observation_count2$Variable <- "Cut"
m_observation_count3 <- aggregate(market.df$price,by = list(market.df$clarity),function(x){NROW(x)})
names(m_observation_count3) <- c("Level","Count")
m_observation_count3$Variable <- "Clarity"
m_observation_count <- merge(m_observation_count, m_observation_count2, by = c("Variable","Level","Count"), all = TRUE)
m_observation_count <- merge(m_observation_count, m_observation_count3, by = c("Variable","Level","Count"), all = TRUE)
###limited sample sizes for astor ideal cut and FL clarity
###view correlation of variables
res1 <- cor.mtest(market.df[,c(1,5)], conf.level = .95)
corrplot.mixed(cor(market.df[,c(1,5)]), order = "original",tl.cex = 1,tl.pos = "lt",tl.col = "black", tl.srt = 50, number.cex = 1,p.mat = res1$p, sig.level = .05
)
###the correlation between carat and price is even higher with the market data
###create simple regression model with predictor variable of highest correlation
plot(price~carat, data = market.df)
###this doesn't seem all that linear; can we improve it?
###transform predictor first
market.df$carat2 <- exp(market.df$carat)
###re-plot basic relationship
plot(price~carat2, data = market.df)
###this doesnt help, so let's repeat with log transformation
market.df$carat3 <- log(market.df$carat)
plot(price~carat3, data = market.df)
###this still doesnt help! what if we re-plot with a double log?
plot(log(price)~carat3, data = market.df)
###looks like this works again! lets use this for modeling
###first, lets verify what box cox says
model.m.tp <-lm(price ~ carat3, data = market.df)
bc1 <- boxcox(model.m.tp,lambda = seq(-1, 1, 1/10))
bc1$x[which.max(bc1$y)]
###seems that the double log is recommended per box cox, it's basically 0
###create new price
market.df$price2.tp <- log(market.df$price)
model.m.tp1 <- lm(price2.tp ~ carat3, data = market.df)
plot(model.m.tp1)
###note the residual plot isn't great at the tail end of fitted values, but its the best i can get
###lets see the acf plot
acf(model.m.tp1$residuals, main="ACF of Residuals")
###yikes!!! this model will not work at all, that acf plot is horrible
model.m.tp1 <- lm(price2.tp ~ carat2, data = market.df)
plot(model.m.tp1)
###this residual plot is not satisfactory
###what if we transform the predictor with the square root
market.df$carat4 <- market.df$carat ^ 0.5
model.m.tp1 <- lm(price2.tp ~ carat4, data = market.df)
plot(model.m.tp1)
###still no good. what if we square x?
market.df$carat5 <- market.df$carat ^ 2
model.m.tp1 <- lm(price2.tp ~ carat5, data = market.df)
plot(model.m.tp1)
###we will try one last transformation!
market.df$carat6 <- 1/market.df$carat
model.m.tp1 <- lm(price2.tp ~ carat6, data = market.df)
plot(model.m.tp1)
###clearly none of these x transformations work with a log of the response. we need to re-evaluate
plot(price~carat, data = market.df)
plot(price~carat2, data = market.df)
plot(price~carat3, data = market.df)
plot(price~carat4, data = market.df)
plot(price~carat5, data = market.df)
plot(price~carat6, data = market.df)
###none are great linear candidates right off, would a response variable transformation help any of these? we need to try for carat2, carat4, carat5, and carat6
model.m.tp <-lm(price ~ carat2, data = market.df)
bc1 <- boxcox(model.m.tp,lambda = seq(-1, 1, 1/10))
bc1$x[which.max(bc1$y)]
###for carat 2, it's pretty close to log. and we know that doesnt work. BC value is 0.17
model.m.tp <-lm(price ~ carat4, data = market.df)
bc1 <- boxcox(model.m.tp,lambda = seq(-1, 1, 1/10))
bc1$x[which.max(bc1$y)]
###for carat 4, it's also pretty close to log and we know that doesnt work. BC is 0.15
model.m.tp <-lm(price ~ carat5, data = market.df)
bc1 <- boxcox(model.m.tp,lambda = seq(-1, 1, 1/10))
bc1$x[which.max(bc1$y)]
###carat 5 is promising! it is closer to square root, which we haven't tried. BC is 0.3737
model.m.tp <-lm(price ~ carat6, data = market.df)
bc1 <- boxcox(model.m.tp,lambda = seq(-1, 1, 1/10))
bc1$x[which.max(bc1$y)]
###carat 6 is similarly promising. BC is -0.35. lets try transforming the response against carat 5 and carat 6
###carat5 first
market.df$price.sqt <- market.df$price ^ 0.5
market.df$price.sqt2 <- market.df$price ^ 0.37
model.m.tp <-lm(price.sqt ~ carat5, data = market.df)
plot(model.m.tp)
###not great. carat 5 for the true box cox
model.m.tp <-lm(price.sqt2 ~ carat5, data = market.df)
plot(model.m.tp)
###struck out. lets try carat 6
market.df$price.sqt.n <- market.df$price ^ -0.5
market.df$price.sqt2.n <- market.df$price ^ -0.35
model.m.tp <-lm(price.sqt.n ~ carat6, data = market.df)
plot(model.m.tp)
###not too bad!! worth looking at the acf plot. lets see the true box cox too
model.m.tp <-lm(price.sqt2.n ~ carat6, data = market.df)
plot(model.m.tp)
###pretty similar. lets use the inverse of the square root since it is a bit more intuitive
model.m.tp <-lm(price.sqt.n ~ carat6, data = market.df)
plot(model.m.tp)
acf(model.m.tp$residuals, main="ACF of Residuals")
####nooooooo, the acf is no good. lets try with box cox exact value
model.m.tp <-lm(price.sqt2.n ~ carat6, data = market.df)
plot(model.m.tp)
acf(model.m.tp$residuals, main="ACF of Residuals")
###back to the drawing board...this is bummer. there is no transformation i can make it to cement a linear relationship between the two....but what if i introduce categorical variables?
###this section is for testing only; iterative process
model.m.tp <-lm(price ~ carat3 + cut + clarity + color, data = market.df)
bc1 <- boxcox(model.m.tp,lambda = seq(-1, 1, 1/10))
bc1$x[which.max(bc1$y)]
model.m.tp.1 <-lm(price2.tp ~ carat3 + cut + clarity + color, data = market.df)
plot(model.m.tp.1)
model.m.tp.1 <-lm(price2.tp ~ carat3, data = market.df[-c(16282,27627,27412,25996,23641),])
plot(model.m.tp.1)
acf(model.m.tp.1$residuals, main="ACF of Residuals")
###does the order of my dataset impact acf?
model.m.tp.1 <-lm(price2.tp ~ carat3, data = market.df[order(market.df$carat3),])
plot(model.m.tp.1)
acf(model.m.tp.1$residuals, main="ACF of Residuals")
###interesting...it seems like the order of the data set has a significant impact on the acf plot! let's try to randomize the order
model.m.tp.1 <-lm(price2.tp ~ carat3, data = market.df[sample(NROW(market.df)),])
plot(model.m.tp.1)
acf(model.m.tp.1$residuals, main="ACF of Residuals")
summary(model.m.tp.1)
###notice the slope here of 1.68....since this model uses the same transformations, does the slope of our blue nile simple regression model differ significantly from the slope in our market model? what about the intercept of 8.45
###H0: confidence interval for slope of original model fits the slope of our market model 
###HA: confidence interval for slope of original model doesn't fit the slope of our market model
orig_slope <- model.s.tp1$coefficients["carat3"]
orig_st_error <- coef(summary(model.s.tp1))[, "Std. Error"][2]
slope_lower <- orig_slope - (qt(0.975,NROW(diamond.df)-2) * orig_st_error)
slope_upper <- orig_slope + (qt(0.975,NROW(diamond.df)-2) * orig_st_error)
crit_t <- (orig_slope - model.m.tp.1$coefficients["carat3"])/coef(summary(model.s.tp1))[, "Std. Error"][2]
p_val <-  2*(1 - pt(crit_t, NROW(diamond.df)-2))
model.m.tp.1$coefficients["carat3"] >= slope_lower & model.m.tp1$coefficients["carat3"] <= slope_upper
###blue nile charges more for increase in carats than the market as whole. test whether the y-intercepts are different; meaning are the base prices for blue nile lower or higher than market?
###HO: y-intercept of the blue nile model = y-intercept of the market model
###HA: y-intercept of the blue nile model != y-intercept of the market model
orig_int <- model.s.tp1$coefficients[1]
mark_int <- model.m.tp.1$coefficients[1]
crit_t2 <- (orig_int - mark_int)/coef(summary(model.s.tp1))[, "Std. Error"][1]
p_val2 <-  2*(1 - pt(crit_t2, NROW(diamond.df)-2))
###statistically significant! we reject the null. blue nile charges a premium for their brand
###it worked!!!! and notice how much stronger this simple regression is than the simple regression with blue nile. lets add the categorical variables in
model.m.tp.2 <-lm(price2.tp ~ carat3 + cut + clarity + color, data = market.df[sample(NROW(market.df)),])
summary(model.m.tp.2)
anova(model.m.tp.2)
###we see that every predictor variable is statistically significant; let's take a look at a model that incorporates interaction effects
model2m <- lm(price2.tp ~ carat3 + cut + clarity + color + carat3:clarity + carat3:cut + carat3:color + clarity:cut + clarity:color + cut:color, data = market.df[sample(NROW(market.df)),])
summary(model2m)
anova(model2m)
###although the anova looks great, there are quite a few interactions that aren't significant; what if we drop the variables and stick with model over model2
anova(model.m.tp.2,model2m)
model2m <- lm(price2.tp ~ carat3 + cut + clarity + color + carat3:clarity + carat3:cut + clarity:cut + clarity:color + cut:color, data = market.df[sample(NROW(market.df)),])
summary(model2m)
###statistically significant, so we can't drop all the variables! we can however, drop the carat3:color interaction
#use vif to gauge collinearity of original and expanded models
#vif(model)
#vif(model2)
###could we bring in even higher order terms?? be sure to applythe term drop for carat3:color interactions
model3m <-  lm(price2.tp ~ carat3 + cut + clarity + color + carat3:clarity + carat3:cut  + clarity:cut + clarity:color + cut:color + carat3:clarity:cut  + clarity:color:cut, data = market.df[sample(NROW(market.df)),])
summary(model3m)
anova(model3m)
###all statistically significant! let's compare it to model2
anova(model2m,model3m)
###statistically significant....which has the highest adjusted r2? model3...barely. notice that it isnt quite as predictive as the blue nile model
###does it overfit?


###lets take our trained model3 and run it on the blue nile data
pred_data3 = as.data.frame(diamond.df[,c(2:4,7)])
pred_interval3 <- predict(model3m, newdata = pred_data3, level=0.95, interval="confidence")
pred_val3 <- predict(model3m, newdata = pred_data3, level=0.95, interval="prediction")
pred3 <- predict(model3m, newdata = pred_data3)
###create graph for prediction interval
###combine all predictions into dataframe and isolate observations with largest residuals
prediction.df3 <- cbind.data.frame(diamond.df$price2.tp,pred2,c(diamond.df$price2.tp - pred3), pred_interval3,pred_val3)
prediction.df3 <- prediction.df3[,-c(4,7)]
names(prediction.df3) <- c("Actual Transformed Price","Predicted Transformed Price","Residual","Confidence Lower Bound","Confidence Upper Bound","Prediction Lower Bound","Prediction Upper Bound")
prediction.df3 <- prediction.df3[rev(order(abs(prediction.df3$Residual),prediction.df3$Residual)),]
###which observations occur outside of the prediction interval
prediction.df_outliers3<- prediction.df3[prediction.df3$`Actual Transformed Price`<prediction.df3$`Prediction Lower Bound` | prediction.df3$`Actual Transformed Price`>prediction.df3$`Prediction Upper Bound`,]
###what is the actual $ residual??
prediction.df_outliers3$`Actual Price` <- exp(prediction.df_outliers3$`Actual Transformed Price`)
prediction.df_outliers3$`Predicted Price` <- exp(prediction.df_outliers3$`Predicted Transformed Price`)
prediction.df_outliers3$`Residual $` <- prediction.df_outliers3$`Actual Price` - prediction.df_outliers3$`Predicted Price`
###reorder based on $ residual
prediction.df_outliers3 <- prediction.df_outliers3[rev(order(abs(prediction.df_outliers3$`Residual $`),prediction.df_outliers3$`Residual $`)),]
prediction.df_outliers3$`Value Proposition` <- ifelse(prediction.df_outliers3$`Residual $` > 0 , "Overvalued","Undervalued")
###sure enough, these were some of the outliers with the highest residuals, and they showed up in our dataframe. the second largest outlier did not show as an outiler per cooks distance, but it did on the alternative model
original_outliers3 <- diamond.df[rownames(diamond.df) %in% rownames(prediction.df_outliers3),]
###what is the outlier % at blue nile?
NROW(prediction.df_outliers3)/NROW(diamond.df)
###19% of diamonds are over or undervalued relative to the market!!!
###what percentage are over vs undervalued for our outliers?
NROW(prediction.df_outliers3[prediction.df_outliers3$`Value Proposition` == "Overvalued",])/NROW(diamond.df)
NROW(prediction.df_outliers3[prediction.df_outliers3$`Value Proposition` == "Undervalued",])/NROW(diamond.df)
###what about the percentage regarding all predictions?
###Overvalue
NROW(prediction.df3[prediction.df3$Residual >0,])/NROW(diamond.df)
###Undervalue
NROW(prediction.df3[prediction.df3$Residual < 0,])/NROW(diamond.df)
###interesting....basically, blue nile is great for finding undervalued cheap diamonds, but they charge a huge premium on the higher end diamonds
###lets see the prediction interval plotted
ggplot(data=diamond.df, aes(x = carat3)) + geom_point(aes(y = price2.tp)) + 
        geom_ribbon(data=cbind.data.frame(pred_val3,diamond.df), aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) 
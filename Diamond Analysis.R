library(dplyr)
library(stats)
library(corrplot)
library(car)

###import data
setwd(dir = "C:/Users/MSachs.MSACHS-DELL/Documents/UVA MSDS/STAT 6021/Project/")
diamond.df <- read.csv("diamonds.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA"))
summary(diamond.df)
###lets take a look at the various levels of our categorical variables
colors <- unique(diamond.df$color)
clarity <- unique(diamond.df$clarity)
cut <- unique(diamond.df$cut)
###clarity, cut, and color are all ordinal variables in this case; not purely categorical, so recode accordingly
diamond.df$cut <- dplyr::recode(diamond.df$cut,Good = 1, `Very Good` = 2, Ideal = 3, `Astor Ideal` = 4)
diamond.df$clarity <- dplyr::recode(diamond.df$clarity, SI2 = 1, SI1 = 2, VS2 = 3, VS1 = 4, VVS2 = 5, VVS1 = 6, IF = 7, FL = 8)
diamond.df$color <- dplyr::recode(diamond.df$color, K = 1, J = 2, I = 3, H = 4, G = 5, F = 6, E = 7, D = 8)
###view scatter plot of variables
pairs(diamond.df, lower.panel = NULL)
###view correlation of variables
res1 <- cor.mtest(diamond.df, conf.level = .95)
corrplot.mixed(cor(diamond.df), order = "original",tl.cex = 1,tl.pos = "lt",tl.col = "black", tl.srt = 50, number.cex = 1,p.mat = res1$p, sig.level = .05
)
###create simple regression model with predictor variable of highest correlation
model.s <-lm(price ~ carat, data = diamond.df)
bc <- boxcox(model.s,lambda = seq(-1, 1, 1/10))
bc$x[which.max(bc$y)]
diamond.df$price3 <- diamond.df$price ^ 0.3131
diamond.df$price2 <- diamond.df$price ^ 0.5
diamond.df$pric4 <- log(diamond.df$price)
model.s.r <- lm(price4 ~ carat, data = diamond.df)
model.s.r2 <- lm(price2 ~ carat, data = diamond.df)
model.s.r3 <- lm(price3 ~ carat, data = diamond.df)
acf(model.s.r2$residuals, main="ACF of Residuals")
predict.lm(model, newdata, level=0.95, interval="confidence")
###create basic linear model
model <- lm(price ~ ., data = diamond.df)
summary(model)
###we see that every predictor variable is statistically significant; let's take a look at a model that incorporates interaction effects
model2 <- lm(price ~ carat + clarity + cut + color + carat:clarity + carat:cut + carat:color + clarity:cut + clarity:color + cut:color, data = diamond.df)
summary(model2)
###given that "carat" has a positive correlation, and the intuitive nature of carat driving price, the fact this model has a negative coefficient for the carat variable
###would indicate collinearity with the interaction terms (or others)
#use vif to gauge collinearity of original and expanded models
vif(model)
vif(model2)
###is a 0.1 increase in explanative power of the model worth the multicollinearity? what if we bring in higher order terms? 
model3 <- lm(price ~ carat + clarity + cut + color + carat:clarity + carat:cut + carat:color + carat:clarity:cut + cut:color, data = diamond.df)
summary(model3)
vif(model3)

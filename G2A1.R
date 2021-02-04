#### Genomics 2 assignment 1
#Set up
library(ggplot2)
data(mtcars)

#### 1) Linear models
#Do lm for no interaction and for interactin
no_interaction <- lm(mpg ~ hp + am, data = mtcars)
with_interaction <- lm(mpg ~ hp*am, data = mtcars)
confint(no_interaction, level = 0.9)
confint(with_interaction, level = 0.9)

#Plot predicted vs actual for no interaction and interaction
plot(predict(no_interaction), mtcars$mpg, xlab="predicted",ylab="actual")
abline(a=0,b=1)

plot(predict(with_interaction), mtcars$mpg, xlab="predicted",ylab="actual")
abline(a=0,b=1)

#Make a plot of the residuals
plot(density(with_interaction$residuals))
plot(density(no_interaction$residuals))

#Lms with and without interaction
design.matrix <- model.matrix( ~ hp + am, data = mtcars)
no_interaction <- lm(mpg ~ hp + am, data = mtcars)
lm(mtcars$mpg ~ design.matrix) #This seems to be the same as above
with_interaction <- lm(mpg ~ hp*am, data = mtcars)

#Confidence intervals
CI.no.interaction <- predict(no_interaction, mtcars, interval = "confidence", level=0.90)
CI.interaction <- predict(with_interaction, mtcars, interval = "confidence", level=0.90)

#Summary of them and plot them
summary(with_interaction)
summary(no_interaction)
plot(mpg ~ hp + am, data = mtcars, col=2)
abline(no_interaction, col=3, lwd=2)

par(mfrow = c(2,2))
plot(mpg ~ am * hp, data = mtcars, col=2)
abline(with_interaction, col=3, lwd=2)

ggplot(data = mtcars, aes(x = mpg, y = hp, col = as.factor(am))) + geom_point(shape = 'square') +
  theme_minimal() + geom_abline(intercept = no_interaction$coefficients[1], slope = no_interaction$coefficients[2]) +
  ylim(c(0, 400)) + geom_point(aes(x = no_interaction$fitted.values, y = mtcars$hp))


ggplot(data = mtcars, aes(x = mpg, y = hp, col = as.factor(am))) + geom_point(shape = 'square') +
  theme_minimal() + geom_abline(intercept = with_interaction$coefficients[1], slope = with_interaction$coefficients[2]) +
  ylim(c(0, 400)) #+ geom_point(aes(x = no_interaction$fitted.values, y = mtcars$hp))

#Plot to show observed vs predicted for horsepower and transmission for no interaction - line of fit probably needs to be equation in form mpg = a + b*hp + c*am
predict_no_interact <- predict(no_interaction, mtcars)
ggplot(data = mtcars, aes(y = mpg, x = hp, shape = as.factor(am))) + geom_point(col = 'red') +
  theme_minimal() + geom_abline(intercept = no_interaction$coefficients[1], slope = no_interaction$coefficients[2]) +
  geom_point(aes(x = hp, y = predict_no_interact, shape = as.factor(am))) + 
  labs(title = 'Predicted vs actual mpg for model without interaction')

#Plot to show observed vs predicted for horsepower and transmission for interaction
predict_with_interact <- predict(with_interaction, mtcars)
ggplot(data = mtcars, aes(y = mpg, x = hp, shape = as.factor(am))) + geom_point(col = 'red') +
  theme_minimal() + geom_abline(intercept = with_interaction$coefficients[1], slope = with_interaction$coefficients[2]) +
  geom_point(aes(x = hp, y = predict_with_interact, shape = as.factor(am))) + 
  labs(title = 'Predicted vs actual mpg for model with interaction')


#root mean square error
sqrt(mean((p - newdf$mpg)^2, na.rm=TRUE))

#Plot residuals
par(mfrow=c(2,1))
# plot model object
plot(no_interaction, which =1:2)

par(mfrow=c(2,1))
# plot model object
plot(with_interaction, which =1:2)
plot(density(with_interaction$residuals))
plot(density(no_interaction$residuals))

#Question 2
data("iris")
new.data <- iris
new.data$versicolor <- ifelse(new.data$Species == 'versicolor', T, F)
regression <- glm(versicolor ~ Petal.Width + Sepal.Width + Sepal.Length + Petal.Length, 
                  data = new.data, family= binomial(link = logit))

ggplot(regression$qr) + geom_point()

#Question 3
X1 <- seq(0, 1, length.out = 100)
X2 <- runif(100)


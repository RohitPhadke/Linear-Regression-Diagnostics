library(data.table)
library(ggplot2)
advert = copy(Ch4_marketing)
setDT(advert)
class(advert)
str(advert)
advert [ ,.(summary(google_adwords), summary(facebook), summary(twitter)) ]

# Converting neumerical into categorical data
advert[ ,pop_density := factor(cut(marketing_total,3), labels = c('Low', 'Medium', 'High'), ordered = T)]
table(advert$pop_density)

table (advert$pop_density )
advert$pop_density
View(advert)

ggplot (advert, aes(x = pop_density, y = marketing_total)) + geom_boxplot () + theme_bw () + labs( y="marketing_total ",x ="pop_density", title ="Distribution")

g = advert [ , google_adwords ]
f = advert [ , facebook ]
tw = advert [ , twitter ]
nv =c(g ,f , tw )
nv
nc =c( rep('g',NROW ( g ) ) ,rep ('f',NROW (f) ) ,rep ('tw',NROW (tw) ) )
#Creating table here with columns as defined
advert2 = data.table ( nv, nc )
advert2

ggplot( advert2 , aes ( x = nc , y = nv ) ) + geom_boxplot ()

#We can check which data could be considered for investment also by using pairs() function

pairs(advert)



head(advert)



#Simple Linear Regression

?lm
?lm
model1 =lm(revenues ~ marketing_total, data = advert)
model1

ggplot ( advert , aes ( x = revenues , y = marketing_total ) ) + geom_point (color ='purple') + geom_smooth ( method = "lm")

#Checking residuals from model1
str (model1)
model1$residuals

summary(model1)

#Normality
#creating table for plotting graph with residuals, only one column is required as z score will be calculated by ggplot
resdf = data.table ('res'= model1$residuals )

#Table with just residual column
resdf

#Plotting graph to check Normality with histogram
ggplot ( resdf , aes ( x = res ) ) + geom_histogram ( bins =10 , fill
                                                        ='purple',  color ='black')

#Z scores mapped with residuals using qq plot
ggplot ( resdf, aes ( sample = res ) ) + stat_qq (color ='blue') + stat_qq_line()

#Equal variance
#Extracting predicted values from model1 one and saving in pred column in resdf table

resdf [ ,pred := model1$fitted.values ]

#Checking for predicted values
resdf$pred

#create a scatter-plot with the residuals on the y-axis and the predicted values on the x-axis.

ggplot( resdf , aes ( x = pred , y = res ) ) + geom_point ( color ='purple') + geom_smooth ( method ='lm')

#########################################################################################################
plot(residuals(model1) ~ advert$revenues)
abline(h=0)
########################################################################################################
plot(fitted(model1),sqrt(abs(residuals(model1))), xlab="Fitted",ylab= expression(sqrt(hat(epsilon))))
########################################################################################################
#to check time series in residuals
n <- length(residuals(model1))
plot(tail(residuals(model1),n-1) ~ head(residuals(model1),n-1), xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col=grey(0.75))

summary(lm(tail(residuals(model1),n-1) ~ head(residuals(model1),n-1) -1))
###################################################################################################################

summary (advert $ marketing_total)
fivenum(advert $ marketing_total)

#SLR model
#predict the revenues if you want to spend $460,000 in marketing

advert [ marketing_total >430 , marketing_total ] #Fisrt taking out values if any exist near 460

# Since we do not have any so we predict

#Creating a table newrv and put marketing_total values greater then 460 in a column
newrev = data.table ( marketing_total = seq (460 ,470 ,5) )

newrev

#We can use the predict.lm() function to estimate based on other inputs
predict.lm( model1, newrev, interval = 'predict')

#If you wanted a 99% interval, then you would use the parameter:

predict.lm( model1, newrev, level =.99, interval = 'predict')


#Confidence intervals
#assume that we use 30% of the data to analyze the rest of the data:
set.seed (4510)

#cREATING a sample out of advert table
#The .N is a strange command in data.table() that replaces the function NROW(). 
liladvert = advert [ sample (.N,.3*.N ) ] #choose only 30% of them or only 51 rows

#Testing the new model liladvert with 30% of the data for lineraity
samp_model = lm( revenues ~ marketing_total , data = liladvert )
samp_model
#Result shows that revenue(y) increases by 60 (b1, since after .060) for every 1000 increase in marketing total 


#Confidence Intervals - sample() and .N Functions

#The sample() function takes random samples from a data container. Hereis a simple example using a vector:

vv =5:15
sample ( vv,5)


#If you are doing a particular simulation and you would like to see the same results simulated, that is when you use the set.seed() feature EVERYTIME.
set.seed (7)
sample ( vv ,5)

#This shows that if you take 100 different random samples from the advert table, 95% of them will estimate the slope of marketing total(x) between .053 and .0667.
confint (samp_model )

#Transforming Data

x =1:10
y =c (1 ,1.41 ,1.73 ,2 ,2.24 ,2.45 ,2.65 ,2.83 ,3 ,3.16)
fit = lm( y~x )
sampdt = data.table (x , y )

#Checking for Linearity
ggplot ( sampdt , aes ( x =x , y = y ) ) + geom_point ( color = 'purple') + geom_smooth ( method = "lm") + labs ( title =" Linearity ?")

#Extracting residuals
sampdt [ , res := fit $ residuals ]

#Checking for Normality
ggplot ( sampdt , aes ( x = res ) ) + geom_histogram ( bins =10 ,fill ='blue', color ='white') + labs ( title = " Normality?")

#Extracting Fitted values that is predicted values
sampdt [ , pred := fit $ fitted.values ]

#Checking for Equal Variance

ggplot ( sampdt , aes ( x = pred , y = res ) ) + geom_point ( color ='purple') + geom_smooth ( method = 'lm') + labs ( title ="Equal Variance ?")


y2 = y^2
fit2 =lm( y2~x)
sampdt2 = data.table (x , y2 )
sampdt2 [ , res := fit2 $ residuals ]
sampdt2 [ , pred := fit2 $ fitted.values ]

ggplot ( sampdt , aes ( x = res) ) + geom_histogram ( bins =10 ,
                                                       fill ='blue', color ='white') + labs (title = "Normality?")

ggplot ( sampdt , aes ( sample = res) ) + stat_qq ( color =" blue ") + stat_qq_line() +labs ( title = " Normality ?")

ggplot ( sampdt2, aes ( x = pred , y = res ) ) + geom_point ( color ='purple') + geom_smooth ( method = 'lm') + labs ( title =" Equal Variance ?")


############################################################################################################################################
###Refining data for Simple Linear Regression
## Checking L.I.N.E assumptions for revenue & google_adwords
x = advert$google_adwords  # independent
y = advert$revenues  # dependent 

fit = lm(advert$revenues~advert$google_adwords)
sampdt = data.table(x,y)

# Check for Linearity
ggplot(sampdt, aes(x= x, y = y)) + geom_point(color = 'blue') + geom_smooth(method = "lm")+ labs(title="Linearity?")

# Check for Normality
## histogram 
sampdt[,res:=fit$residuals]
ggplot(sampdt ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + labs(title= 'Normality?')

## QQ-Plot
ggplot(sampdt ,aes(sample=res)) + stat_qq(color='blue')+ stat_qq_line()

# Check for Equal Varience
sampdt[,pred:=fit$fitted.values]
ggplot(sampdt ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm') + labs(title='Equal  Variance?')


##data transformation for google_adwords
x = log(advert$google_adwords)  # independent
y = advert$revenues  # dependent 

fit = lm(y~x)
sampdt = data.table(x,y)

# Check for Linearity
ggplot(sampdt, aes(x= x, y = y)) + geom_point(color = 'blue') + geom_smooth(method = "lm")+ labs(title="Linearity?")

# Check for Normality
## histogram 
sampdt[,res:=fit$residuals]
ggplot(sampdt ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + labs(title= 'Normality?')

## QQ-Plot
ggplot(sampdt ,aes(sample=res)) + stat_qq(color='blue')+ stat_qq_line()

# Check for Equal Varience
sampdt[,pred:=fit$fitted.values]
ggplot(sampdt ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm') + labs(title='Equal  Variance?')

##here, transformation works to make our model a perfect fit for Regression
####################################################################################################################################################

## Checking L.I.N.E assumptions for revenue & facebook
x = advert$facebook  # independent
y = advert$revenues  # dependent 

fit = lm(advert$revenues~advert$facebook)
sampdt = data.table(x,y)

# Check for Linearity
ggplot(sampdt, aes(x= x, y = y)) + geom_point(color = 'blue') + geom_smooth(method = "lm")+ labs(title="Linearity?")

# Check for Normality
## histogram 
sampdt[,res:=fit$residuals]
ggplot(sampdt ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + labs(title= 'Normality?')

## QQ-Plot
ggplot(sampdt ,aes(sample=res)) + stat_qq(color='blue')+ stat_qq_line()

# Check for Equal Varience
sampdt[,pred:=fit$fitted.values]
ggplot(sampdt ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm') + labs(title='Equal  Variance?')


##data transformation for facebook
x = sqrt(advert$facebook)  # independent
y = advert$revenues  # dependent 

fit = lm(y~x)
sampdt = data.table(x,y)

# Check for Linearity
ggplot(sampdt, aes(x= x, y = y)) + geom_point(color = 'blue') + geom_smooth(method = "lm")+ labs(title="Linearity?")

# Check for Normality
## histogram 
sampdt[,res:=fit$residuals]
ggplot(sampdt ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + labs(title= 'Normality?')

## QQ-Plot
ggplot(sampdt ,aes(sample=res)) + stat_qq(color='blue')+ stat_qq_line()

# Check for Equal Varience
sampdt[,pred:=fit$fitted.values]
ggplot(sampdt ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm') + labs(title='Equal  Variance?')

##here, we can see that sqrt transformation makes our data more refine
############################################################################################################################################

## Checking L.I.N.E assumptions for revenue & twitter
x = advert$twitter  # independent
y = advert$revenues  # dependent 

fit = lm(advert$revenues~advert$twitter)
sampdt = data.table(x,y)

# Check for Linearity
ggplot(sampdt, aes(x= x, y = y)) + geom_point(color = 'blue') + geom_smooth(method = "lm")+ labs(title="Linearity?")

# Check for Normality
## histogram 
sampdt[,res:=fit$residuals]
ggplot(sampdt ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + labs(title= 'Normality?')

## QQ-Plot
ggplot(sampdt ,aes(sample=res)) + stat_qq(color='blue')+ stat_qq_line()

# Check for Equal Varience
sampdt[,pred:=fit$fitted.values]
ggplot(sampdt ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm') + labs(title='Equal  Variance?')


##data transformation for twitter
x = log(advert$twitter)  # independent
y = advert$revenues  # dependent 

fit = lm(y~x)
sampdt = data.table(x,y)

# Check for Linearity
ggplot(sampdt, aes(x= x, y = y)) + geom_point(color = 'blue') + geom_smooth(method = "lm")+ labs(title="Linearity?")

# Check for Normality
## histogram 
sampdt[,res:=fit$residuals]
ggplot(sampdt ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white') + labs(title= 'Normality?')

## QQ-Plot
ggplot(sampdt ,aes(sample=res)) + stat_qq(color='blue')+ stat_qq_line()

# Check for Equal Varience
sampdt[,pred:=fit$fitted.values]
ggplot(sampdt ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm') + labs(title='Equal  Variance?')

## here, transformation is not helping us as the transform data are not adding any value to original data
###################################################################################################################################################

#Multiple regression for all the three so we get three slopes

#Checking for Collinearity
library(faraway)

model2 = lm( revenues ~ google_adwords + facebook + twitter , data = advert )
model2
vif(model2)

#Since values for all three are less then five, so we can conclude that collinearity problem for the respective dataset. Low values of VIF
#also indicates that estimates we delevop based on the model will be more stable

#Gives all result summaries of the results of various model fitting functions
summary ( model2 )

#Low
#p-values indicate that the variable helps explain the model better than a
#high p-value. Which variable can be considered unnecessary?

#probability quantile function of the F distribution
qf (.95 , df1 =3 , df2 =168)

#The adjusted R-squared value shows how much of the variation in the
#dependent variable, is explained by the variation in the independent
#variables

#The 3 predictors also generate their own p-values via the t-statistic. Low
#p-values indicate that the variable helps explain the model better than a
#high p-value

########################################################################################################
#to check time series in residuals for model2
m <- length(residuals(model2))
plot(tail(residuals(model2),m-1) ~ head(residuals(model2),m-1), xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col=grey(0.75))

summary(lm(tail(residuals(model2),m-1) ~ head(residuals(model2),m-1) -1))
###################################################################################################################



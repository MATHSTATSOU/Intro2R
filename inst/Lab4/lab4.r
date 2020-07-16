
# #This will be a different path if in the lab or at home
# dird="E:\\OneDrive\\MATH4753\\DATAxls\\" # 2017
# 
# #my function to read data 
# myread=function(csv){
#   fl=paste(dird,csv,sep="")
#   read.table(fl,header=TRUE,sep=",")
# }
# #EASY WAY TO READ IN FILES

#spruce.df=myread("SPRUCE.csv")#MS pg478

spruce.df = read.csv("SPRUCE.csv")

#with(spruce.df, dput(list(D=BHDiameter,H=Height), 
                     #file="spruce.dat"))
# Or use 
#spruce.df=read.table(file.choose(),header=TRUE,sep=",")

#get wd
getwd()

#Top six lines
tail(spruce.df)

#Plot the points
windows()

plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
ylim=c(0,max(Height)),xlim=c(0,max(BHDiameter)), 
main="Spruce height prediction",data=spruce.df)


library(ggplot2)
windows()
g = ggplot(spruce.df,mapping = aes(x = BHDiameter, y = Height)) + 
  geom_point()
print(g)

g = g+ geom_smooth(formula = y~ log(x), method = "lm", col = "steelblue")
g = g + geom_smooth(formula = y ~ x, method = "lm", col = "Black")
g = g + geom_smooth(formula = y~ x+ I(x^2), method ="lm", col = "Red")
print(g)

g = g + geom_smooth(formula = y~ poly(x,3), method ="lm", col = "green3")
g



#load s20x library and make lowess smoother
library(s20x)

trendscatter(Height~BHDiameter,f=0.5,data=spruce.df)
# Now make the linear model
spruce.lm=lm(Height~BHDiameter,data=spruce.df)
summary(spruce.lm)
#residuals  created from the linear model object
height.res=residuals(spruce.lm)

#fitted values made from the linear model object
height.fit=fitted(spruce.lm)

windows()
#Make the plot using the plot function 
plot(height.fit,height.res)

# Put a lowess smoother through res vs fitted
trendscatter( height.fit,height.res)

# Quick way to make a residual plot
plot(spruce.lm, which =1)

# Two plots testing normality
windows()
normcheck(spruce.lm,shapiro.wilk = TRUE)


## Quadratic object using the linear model
quad.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)
summary(quad.lm)
add1(spruce.lm,.~.+I(BHDiameter^2))
anova(spruce.lm)
anova(quad.lm)
anova(spruce.lm,quad.lm)
cubic.lm=lm(Height~BHDiameter + I(BHDiameter^2)+I(BHDiameter^3),data=spruce.df)
anova(cubic.lm)
add1(quad.lm,.~.+I(BHDiameter^3))
#add to the scatter plot
windows()
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
ylim=c(0,max(Height)),xlim=c(0,max(BHDiameter)), 
main="Spruce height prediction",data=spruce.df)

coef(quad.lm)
names(quad.lm)
quad.lm$coef[2]

myplot=function(x){
 0.86089580 +1.46959217*x  -0.02745726*x^2
 }
 
 #Or more general method
myplot=function(x){
 quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
 } 
 
curve(myplot, lwd=2, col="steelblue",add=TRUE)
 
 
plot(quad.lm, which=1)

plot(spruce.lm,which=1)
normcheck(quad.lm,shapiro.wilk = TRUE)


summary(quad.lm)

predict(quad.lm, data.frame(BHDiameter=c(3,6,8)))


ciReg(quad.lm)



predict(quad.lm, data.frame(BHDiameter=c(15,18,20)))

anova(spruce.lm,quad.lm)

data = 15:24
predict20x(quad.lm,data.frame(BHDiameter = data, `I(BhDiameter)^2`=data^2))

anova(quad.lm)
anova(spruce.lm)

height.qfit=fitted(quad.lm)

RSS=with(spruce.df, sum((Height-height.qfit)^2))
RSS
MSS = with(spruce.df, sum((height.qfit-mean(Height))^2))
MSS

TSS = with(spruce.df, sum((Height-mean(Height))^2))
TSS


MSS/TSS


cooks20x(quad.lm)


#Now remove the 24th datum and reanalyze data

quad2.lm=lm(Height~BHDiameter + I(BHDiameter^2) , data=spruce.df[-24,])
summary(quad2.lm)
summary(quad.lm)


###############################################################################

#some other code you might need
#The following code plots residuals
windows()
#Plot the data
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
ylim=c(0,max(Height)),xlim=c(0,max(BHDiameter)), 
main="Spruce height prediction",data=spruce.df)

#Make a quadratic model
quad.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)

# Find the coefficients
coef(quad.lm)

#Make a function that produces heights for inputs "x"
myplot=function(x){
 0.86089580 +1.46959217*x  -0.02745726*x^2
 }

# add the quadratic to the points 
curve(myplot, lwd=2, col="steelblue",add=TRUE)

#Place segments (residuals) on the plot (except for the 3 largest cooks distances. 18, 21, 24)
with(spruce.df[-c(18,21,24),],segments(BHDiameter, Height, BHDiameter, height.qfit[-c(18,21,24)]) )
with(spruce.df[c(18,21,24),],segments(BHDiameter, Height, BHDiameter, height.qfit[c(18,21,24)], col="Red", lwd=3) )
with( spruce.df, arrows(5,Height[24], BHDiameter[24], Height[24],lwd=2,col="Blue"))
with(spruce.df,text(2,Height[24], paste("Highest Cook's","\n", "distance",sep=" ")))
with(spruce.df, text(BHDiameter,Height, 1:36,cex=0.5,pos=4))
 #########################################################################


layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))

#Lets look at where the plots will go
layout.show(4)

#Plot the data
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
main="Spruce height prediction",data=spruce.df)
# add the line
abline(spruce.lm)


#make a new plot
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
main="Spruce height prediction",data=spruce.df)

abline(spruce.lm)

#make yhat the estimates of E[Height | BHDiameter]
yhat=with(spruce.df,predict(spruce.lm,data.frame(BHDiameter)))
yhat=fitted(spruce.lm)
# Draw in segments making the residuals (regression errors)
with(spruce.df,{
segments(BHDiameter,Height,BHDiameter,yhat)
})

RSS=with(spruce.df,sum((Height-yhat)^2))

RSS

#make a new plot
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
main="Spruce height prediction",data=spruce.df)

#make nieve model
with(spruce.df, abline(h=mean(Height)))
abline(spruce.lm)

#make the explained errors (explained by the model)
with(spruce.df, segments(BHDiameter,mean(Height),BHDiameter,yhat,col="Red"))
MSS=with(spruce.df,sum((yhat-mean(Height))^2))
MSS

# Total  error
#make a new plot
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
main="Spruce height prediction",data=spruce.df)

with(spruce.df,abline(h=mean(Height)))
with(spruce.df, segments(BHDiameter,Height,BHDiameter,mean(Height),col="Green"))
TSS=with(spruce.df,sum((Height-mean(Height))^2))
TSS
RSS + MSS
MSS/TSS

summary(spruce.lm)

#obtain coefft values
coef(spruce.lm)

#Calculate new y values given x
predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))

anova(spruce.lm)

spruce2.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)
summary(spruce2.lm)


### More on the problem
windows()
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)

yhatt=with(spruce.df,fitted(spruce2.lm))
with(spruce.df,plot(BHDiameter,yhatt,col="Red")
)

sum(residuals(spruce2.lm)^2)
plot(yhatt~BHDiameter,data=spruce.df,type="p")
summary(spruce2.lm)
anova(spruce2.lm)
anova(spruce.lm,spruce2.lm)
MSS
RSS

## piecewise linear model in R
## Model y = b0 + b1x + b2(x-xk)*(x>xk)
## You will need to change the code appropriately
sp2.df=within(spruce.df, X<-(BHDiameter-20)*(BHDiameter>20)) # this makes a new variable and places it within the same df
sp2.df

lmp=lm(Height~BHDiameter + X,data=sp2.df)
tmp=summary(lmp)
names(tmp)
myf = function(x,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0)
}
plot(spruce.df,main="Piecewise regression")
myf(0, coef=tmp$coefficients[,"Estimate"])
curve(myf(x,coef=tmp$coefficients[,"Estimate"] ),add=TRUE, lwd=2,col="Blue")
abline(v=18)
text(18,16,paste("R sq.=",round(tmp$r.squared,4) ))

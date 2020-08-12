### Function
mylsq=function(x,y){
ssxx=sum((x-mean(x))^2 )
ssxy=sum() ## fill in the missing portion
b1hat=ssxy/ssxx
b0hat=   ##  fill in the missing portion
return(list(b0hat=b0hat,b1hat= )) #fill in the missing portion
}

x=1:20;set.seed(29);y=4+6*x + rnorm(20,0,5)
plot(x,y)


mypred=function(x,b0,b1){
ym=b0+ ## fill in the gap
ym
}


mysq=function(x,y){
n=length(x) # or y
ssxx=sum((x-mean(x))^2 )
ssxy=sum() ## fill in the missing portion
b1hat=ssxy/ssxx
b0hat=   ##  fill 
yhat=b0hat+  ##  fill 
ssr=sum((y-yhat)^2)
sq= ##  fill 
return(list(ssr=ssr,sq=sq))
}


#This will be a different path if in the lab or at home
dird="C:\\Users\\HyDRO-Lab\\Google Drive\\MATH4753\\Dataxls\\"

#my function to read data 
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
#EASY WAY TO READ IN FILES


#10.32 page 464
seed=myread("SEEDGERM.csv")
seed
layout(matrix(1:2,nr=1,nc=2))
plot(CHANGE~TEMP,data=seed,col="Blue",pch=19,main="Change Vs Temp")
with(seed,text(TEMP,CHANGE,1:7,pos=1))
seed.lm=with(seed,lm(CHANGE~TEMP))
abline(seed.lm)
summary(seed.lm)
names(seed.lm)
coef=seed.lm$coef
coef

# 5th point unusual
plot(CHANGE~TEMP,data=seed[-5,],col="Blue",pch=19,main="Change Vs Temp with 5th point removed")
with(seed,text(TEMP,CHANGE,1:7,pos=1))
seed.lm2=with(seed[-5,],lm(CHANGE~TEMP))
abline(seed.lm2)
summary(seed.lm2)
names(seed.lm2)
coef=seed.lm2$coef
coef








oj.df=myread("OJUICE.csv")#MS pg478

# Or use 
#oj.df=read.table(file.choose(),header=TRUE,sep=",")

#get wd
getwd()

#Top six lines
head(oj.df)

#Plot the points

plot(SweetIndex~Pectin,bg="Blue",pch=21,cex=1.2,
ylim=c(0,max(SweetIndex)),xlim=c(0,max(Pectin)), 
main="Sweet Index",data=oj.df)

plot(SweetIndex~Pectin,bg="Blue",pch=21,cex=1.2, 
main="Sweet Index",data=oj.df)

# Calculate slope by hand
## beta1
ssyy=with( oj.df,sum((SweetIndex-mean(SweetIndex))^2))
ssyy
#load s20x library and make lowess smoother
library(s20x)
layout(matrix(1:3,nr=3,nc=1))
trendscatter(Height~BHDiameter,f=0.5,data=spruce.df)
trendscatter(Height~BHDiameter,f=0.6,data=spruce.df)
trendscatter(Height~BHDiameter,f=0.7,data=spruce.df)
# Now make the linear model
spruce.lm=lm(Height~BHDiameter,data=spruce.df)

#add to the scatter plot
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
ylim=c(0,max(Height)),xlim=c(0,max(BHDiameter)), 
main="Spruce height prediction",data=spruce.df)

abline(spruce.lm)

#layout
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

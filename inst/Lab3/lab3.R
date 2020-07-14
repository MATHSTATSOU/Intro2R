#This will be a different path if in the lab or at home
dird="\\Users\\HyDRO-Lab\\Desktop\\MATH4753\\DATAxls\\"

#my function to read data 
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
#EASY WAY TO READ IN FILES
fin.df=myread("FINTUBES.csv")
spruce.df=myread("SPRUCE.csv")#MS pg478

# Or use 
fin.df=read.table(file.choose(),header=TRUE,sep=",")

#Top six lines
head(fin.df)

#initial plot, compare to get perspective ranges and intercepts
with(fin.df,  {
layout(matrix(1:2,nr=2))
plot(HEAT~RATIO,bg="Blue",pch=21)
plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,max(HEAT)),xlim=c(0,3))
 }
)

#Using a new package
#Download and install ggplot2
library(ggplot2)
g=ggplot(fin.df, aes(x=RATIO,y=HEAT,colour=RATIO))
g=g+geom_point() + geom_line()+ geom_smooth(method="lm")
g+ggtitle("HEAT Vs RATIO")


#new plotting window
windows()
plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,max(HEAT)),xlim=c(0,3), data=fin.df)

#Load library
#make a new plot
library(s20x)
trendscatter(HEAT~RATIO,f=0.7, data=fin.df)

#layout
layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))

#Lets look at where the plots will go
layout.show(4)

#Plot the data
with(fin.df, 
plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,1.1*max(HEAT)),xlim=c(0,1.1*max(RATIO)))
)

mtext("Dr Stewart's plot",side=3)


# make a linear model
ht.lm=with(fin.df, lm(HEAT~RATIO))

#plot a least squares regression line
abline(ht.lm)

#make a new plot
with(fin.df, 
plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,1.1*max(HEAT)),xlim=c(0,1.1*max(RATIO)))
)

#make yhat the estimates of E[HEAT | RATIO]
yhat=with(fin.df,predict(ht.lm,data.frame(RATIO)))
#OR you could use -- (yhat values the predicted values for all the RATIO values )
yhat=fitted(ht.lm)

# Draw in segments making the residuals (regression deviations)
with(fin.df,{
segments(RATIO,HEAT,RATIO,yhat)
})
abline(ht.lm)

#residual sum of squares
RSS=with(fin.df,sum((HEAT-yhat)^2))

RSS

#make a new plot
with(fin.df, 
plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,1.1*max(HEAT)),xlim=c(0,1.1*max(RATIO)))
)

#make nieve model
with(fin.df, abline(h=mean(HEAT)))
abline(ht.lm)

#make the explained deviations (explained by the model)
with(fin.df, segments(RATIO,mean(HEAT),RATIO,yhat,col="Red"))
MSS=with(fin.df,sum((yhat-mean(HEAT))^2))
MSS

# Total  error
#make a new plot
with(fin.df, 
plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,1.1*max(HEAT)),xlim=c(0,1.1*max(RATIO)))
)
with(fin.df,abline(h=mean(HEAT)))
with(fin.df, segments(RATIO,HEAT,RATIO,mean(HEAT),col="Green"))
TSS=with(fin.df,sum((HEAT-mean(HEAT))^2))
TSS
RSS + MSS

#Can you find this number in the summary info?
MSS/TSS

summary(ht.lm)
#intercept =    0.2134
#Slope =     2.4264
# Equation HEAT=0.2134 +2.4264*RATIO
#Get coeffts
coef(ht.lm)

#Predict new HEAT values for RATIO values
predict(ht.lm, data.frame(RATIO=c(2,2.3,2.5)))

# Regression analysis of variance table
#Can you find some of the ss in here?
anova(ht.lm)

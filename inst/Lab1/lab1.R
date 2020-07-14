#Task 1
getwd()

#Task 2
# file.choose() should not be used in R markdown.
#Best to use a file name example read.csv("DDT.csv")
## How do we read data files into R?
## Most of the data we will use will come from a collection of excel files zipped 
## together and made available on CANVAS
# Read in the data from DDT
# header is true because of variable names on the top of each column

#ddt=read.table(file.choose(),sep=",",header=TRUE) 

ddt = read.csv("DDT.csv")

# First six lines
head(ddt)

#What class is the object ddt?
class(ddt)

#what are the variables in the data frame?
names(ddt)

# Summarize the variables
summary(ddt)

# Manual entry
x=scan()


#Use the c "combine" operator to make vectors
y=c(1,2,3,4,5)


# manipulate the data frame
#Find the fish with weight more than 800gms
with(ddt, ddt[WEIGHT>800,])

#place the output in an object
b800=with(ddt, ddt[WEIGHT>800,])

#Summarize it
summary(b800)

# Use combination boolean expressions to subset
with(ddt, ddt[WEIGHT>800 & SPECIES=="LMBASS",])


#ordering data frames
with(ddt,ddt[order(WEIGHT),])


#More complex orderings
with(ddt,ddt[order(SPECIES,WEIGHT),])

with(ddt,ddt[order(SPECIES,DDT),])

#Using dplyr
library(dplyr)
library(ggplot2)
# notice how the infix %>% operator works ( the pipe )

ddt %>% filter(WEIGHT > 200, LENGTH > 30, DDT < 300) %>% mutate(AREA = LENGTH*WEIGHT) %>% ggplot(aes(x=AREA,y=DDT,fill=RIVER)) + geom_boxplot() + ggtitle("How to use dplyr and ggplot")


## Making vectors
v=1:10
v
2*v
2*v^2
sqrt(v)

#make  matrix
#nr=nu. of rows, byrow=TRUE, fill by row
mat=matrix(1:10,nr=5,nc=2,byrow=TRUE)
mat

#Adding names to columns and rows
#rep = repeat, repeat "r" 5 times
colnames(mat)=c("c1","c2")
rownames(mat)=paste(rep("r",5),1:5,sep="")

#entering the name of the object releases its contents
mat


#Use tables to summarize and plot categorical variables
sp=with(ddt,table(SPECIES))
sp
barplot(sp)
pie(sp,col=1:4)


#2-D tables
spriv=with(ddt,table(SPECIES,RIVER))
spriv
barplot(spriv)

#Add some options to make the graph more readable
barplot(spriv,beside=TRUE,col=1:3)

#2-D tables with reversing the variables
rivsp=with(ddt,table(RIVER,SPECIES))
rivsp

#Row and column sums
rowSums(rivsp)
colSums(rivsp)

#Proportion of the column totals
rivsp/colSums(rivsp)

#
barplot(rivsp,col=rainbow(4))

#Embellish the graphic using colors and a legend
barplot(rivsp,beside=TRUE,col=rainbow(4))
with(ddt,legend(1,80, leg=levels(RIVER),bty="n",fill=rainbow(4),cex=0.8))

#scatter plots
with(ddt,plot(WEIGHT~LENGTH))

#Use the data option
plot(WEIGHT~LENGTH,data=ddt)

#embellish the plot with species using color using 'ifelse(test,yes,no)'
fishcol=with(ddt,ifelse(SPECIES=="CCATFISH","Red",
                        ifelse(SPECIES=="SMBUFFALO","Blue","Green")))

#River col
rivcol=with(ddt, ifelse(RIVER=="FCM","Red",
                        ifelse(RIVER=="LCM","Blue",
                               ifelse(RIVER=="SCM","Green","Black"))))
head(fishcol)
plot(WEIGHT~LENGTH,data=ddt,col=fishcol)

#Add river text
text(WEIGHT~LENGTH,RIVER,pos=2,cex=0.5,data=ddt)

#The above plot is messy -- can we do better? Use coplots
coplot(WEIGHT~LENGTH|RIVER,data=ddt,col=fishcol)

#How to interpret the coplot (color here is redundant)
coplot(WEIGHT~LENGTH|RIVER,data=ddt,col=rivcol)

#Given the combinations of all levels of SPECIES and RIVER = RIVER*SPECIES
coplot(WEIGHT~LENGTH|RIVER*SPECIES,data=ddt,col=fishcol)

#boxplots (Notice where the quant. and Qual. variables are placed)
boxplot(WEIGHT~SPECIES,col="Red",data=ddt)

#boxplots of LENGTH by RIVER
boxplot(LENGTH~RIVER,col="Green",data=ddt)


#boxplots of each quantitative variable
#Notice the use of layout()
layout(matrix(c(1,2,3),nr=1,nc=3))# 1 row 3 cols
layout.show(3)
with(ddt,boxplot(LENGTH,ylab="LENGTH",col="Blue",notch=TRUE))
with(ddt,boxplot(WEIGHT,ylab="WEIGHT",col="Green",notch=TRUE))
with(ddt,boxplot(MILE,ylab="MILE",col="Red",notch=TRUE))





# How to use a pre-made function
# highlight and send the complete function to R
scatterhist = function(x, y, xlab="", ylab=""){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x,y)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}

with(ddt, scatterhist(LENGTH,WEIGHT, xlab="LENGTH"))

#Make a histogram
with(ddt, hist(DDT))


# data.table

library(data.table)




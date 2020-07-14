# This is the directory structure to my data
#This will be different on the lab computer and on your home computer
dird="C:\\Users\\stew9983\\OneDrive - University of Oklahoma\\DATAxls\\"

#my function to read data 
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
mpg.df=myread("EPAGAS.csv")

#or just use in an R script NOT in RMD!!!!

#I.df=read.table(file.choose(),header=TRUE,sep=",")

# in RMD use 

I.df = myread("IRONORE.csv")

#Put the IRONORE vatriable into vector I
I=I.df$IRON

#Standardize(transform) into z values
z=(I-mean(I))/sd(I)

#class
class(z)



# Find the z values greater than 3 in size
z[abs(z)>3]

# Find the I values corresponding to these z values
I[abs(z)>3]

# Find the values of z that are possible outliers
z[abs(z)>=2 & abs(z)<=3]

#Find the values of I which are possible outliers
 I[abs(z)>=2 & abs(z)<=3]
 
 

# Plot the outliers in red and the possible outliers in blue
mycol = ifelse(abs(z)>3, "Red",
        ifelse(abs(z)>=2 &abs(z)<=3,"Blue", "Black"))  

#Plot points with different plotting shapes        
mypch = ifelse(abs(z)>3, 20,
        ifelse(abs(z)>=2 &abs(z)<=3,21, 22))

#Use an installed library 
# You will need to install the "lattice" package -- unless using the Lab computers
#In R go to Packages   -- select "install packages" choose a mirror and then the "lattice" package
# Now issue the following commands
library(lattice)
dotplot(I,col=mycol)

## Chebyshev
## The proportion of data within 3 standard deviations of the mean
## At least 1-1/9 of the data = 8/9
8/9

## What is actually the case?
length(I[abs(z)<3])/length(I)

## Within 2 standard deviations of the mean
## At least 1-1/4=3/4
3/4

## What is actually the case?
length(I[abs(z)<2])/length(I)

## Can you see that this is the same as
length(I[mean(I)-2*sd(I) < I & I < mean(I)+ 2*sd(I) ])/length(I)


## Make z values

z=(x-mean(x))/sd(x)
















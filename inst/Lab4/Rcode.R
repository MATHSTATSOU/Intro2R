

myf = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}

rsq = function(xk,data){ # data=spruce.df
  df=within(data, X<-(BHDiameter-xk)*(BHDiameter>xk))  
  lmp=lm(Height ~ BHDiameter + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}

rsqdash = function(xk,h,data) {
 (rsq((xk+h/2),data)-rsq((xk-h/2),data))/h
}


myf2 = function(x,xk1,xk2,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk1)*(x-xk1>0)+ coef[4]*(x-xk2)*(x-xk2>0)
}

coeff = function(xk1,xk2,data){ # data=spruce.df
  df=within(data, {
            X1<-(BHDiameter-xk1)*(BHDiameter>xk1) 
            X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
            ) 
  lmp=lm(Height ~ BHDiameter + X1 + X2, data=df)
  coef(lmp)
}


rsq2 = function(xk1,xk2){ # data=spruce.df
  df=within(spruce.df, {
  X1<-(BHDiameter-xk1)*(BHDiameter>xk1) 
  X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
  )
  lmp=lm(Height ~ BHDiameter + X1 +X2, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}

dataxk = function(n,d){
  df = matrix(NA,nr=1,nc=3)
  #x=seq(11.3477387-1,18.8251256+1, length=n)
  x1=seq(11.3477387-1,11.3477387+1,length=n)
  x2=seq(18.8251256-1,18.8251256+1,length=n)
  for(xk1 in x1){
    
    for(xk2 in x2){
      df = rbind(df,c(xk1,xk2,rsq2(xk1,xk2)))
    }
  }
  colnames(df)=c("xk1","xk2","Rsq")
  invisible(df[-1,])
}


# dataxk = function(n,d){
# df = matrix(NA,nr=1,nc=3)
# x=seq(11.3477387-1,18.8251256+1, length=n)
# x1=seq(11.3477387-1,11.3477387+1,length=n)
# x2=seq(18.8251256-1,18.8251256+1,length=n)
# for(xk1 in x[1:(n-1)]){
#   
#   for(xk2 in x[x>=xk1]){
#     df = rbind(df,c(xk1,xk2,rsq2(xk1,xk2)))
# }
# }
# colnames(df)=c("xk1","xk2","Rsq")
# invisible(df[-1,])
# }


myplot3d = function(xk, data=spruce.df){
  
    df=within(data, X<-(BHDiameter-xk)*(BHDiameter>xk))  
    lmp=lm(Height ~ BHDiameter + X, data=df)
    rgl::plot3d(lmp,data=df)
  }
  
  
  
#myplot3d(17)


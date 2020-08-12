n=30;x=1:n;set.seed(24);y=10 -4*x +rnorm(n,0,6) # Artificial data
y.lm=lm(y~x)
summary(y.lm)
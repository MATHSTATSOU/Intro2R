#Lab 13
set.seed(13); x=rnorm(40,mean=15,sd=5)
set.seed(20); y=rnorm(35,mean=10,sd=4)

var.test(x,y)
t.test(x,y,alt="greater")

set.seed(20); y=rnorm(35,mean=##,sd=4)
t.test(x,y,alt="greater")


###### Task 3

set.seed(50); x=rnorm(30,mean=50, sd=10)
set.seed(40); y=rnorm(40,mean=55, sd=20)

var.test(x,y)
var.test(y,x)

var.test(x,y,alt="less")
var.test(y,x,alt="greater")

ci1=var.test(x,y)$conf.int
ci2= var.test(y,x)$conf.int
1/ci1 ;ci2
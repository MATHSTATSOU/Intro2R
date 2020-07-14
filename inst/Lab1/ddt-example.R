# Read in the ddt data

ddt = read.csv("ddt.csv")

# Check
head(ddt,20)

library(ggplot2)
#install.packages("ggplot2")


# ggplot uses layers

g = ggplot(ddt, aes(x = LENGTH, y = WEIGHT )) + 
  geom_point(aes( shape = SPECIES))

print(g)

# add some clusters

gt = g +  stat_ellipse(aes(color = SPECIES), type = "t", level=0.95)
gt

gtp = g + stat_ellipse(aes(fill=SPECIES),geom="polygon", alpha = 0.6,level=0.95)

gtp

# need some more packages
library(ggpubr)
library(ggpmisc)

formula <- y~x + I(x^2) + I(x^3)
g  = g + geom_smooth(method = "lm", formula = formula,aes(col = SPECIES))
g

lbyr <- c(12,13,14)*100
g = g+ stat_cor(aes(color = SPECIES), label.y = lbyr)+
  stat_poly_eq(
    aes(color = SPECIES, label = ..eq.label..),
    formula = formula, label.y = c(21,22,23)*100, parse = TRUE) + xlim(0,60)

# or enter `g`

g

# add some labels

g = g + labs(subtitle="Weight Vs Length", 
             y="Weight", 
             x="Length", 
             title="Scatterplot", 
             caption = "Source: DDT.csv")
g

# Use facets

g  = g + facet_wrap(~SPECIES)
g


# Use themes to control other aspects of the plot

g = g + theme(legend.position = "bottom")
g

g = g+theme(axis.text.x = element_text(angle=65, vjust=0.6))
g


## We can make fancy boxplots

 b = ggplot(ddt, aes(x = RIVER, y = LENGTH))
 b = b + geom_boxplot(aes(fill= SPECIES)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot", 
       subtitle="Length Vs River",
       caption="Source: DDT.csv",
       x="River",
       y="Length")

b

# add facets

b = b + facet_wrap(~SPECIES)
b

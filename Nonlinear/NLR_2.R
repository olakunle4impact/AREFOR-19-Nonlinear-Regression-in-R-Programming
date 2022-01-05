#https://www.r-bloggers.com/first-steps-with-non-linear-regression-in-r/

setwd('D:/personal/R_tutorials/Regression/data')
#######################################
#EXAMPLE 1 : Michaelis-Menten equation#
#######################################
#simulate some data
reaction_df <- read.table('reaction_df.csv', sep=",", header=T)
plot(v~s, data=reaction_df)
#for simple models nls find good starting values for the parameters even if it throw a warning
m<-nls(v~vmax*s/(k+s), data=reaction_df, trace=T,
       start=list(vmax=1,k=1))
#get some estimation of goodness of fit
cor(reaction_df$s,predict(m))

#plot
plot(v~s, data=reaction_df)
lines(reaction_df$s,predict(m),lty=2,col="red",lwd=3)

#Problem with the NLS method, finding the right starting values
#If you set starting values out of range, it will return you the wrong optimals, or it will simply fail

#######################################
#EXAMPLE 2#
#######################################
#simulate some data, this without a priori knowledge of the parameter value
y<-runif(1,5,15)*exp(-runif(1,0.01,0.05)*x)+rnorm(51,0,0.5)
#visually estimate some starting parameter values
plot(x,y)
#from this graph set approximate starting values
a_start<-8 #param a is the y value when x=0
b_start<-2*log(2)/a_start #b is the decay rate
#model
m<-nls(y~a*exp(-b*x),start=list(a=a_start,b=b_start))
#get some estimation of goodness of fit
cor(y,predict(m))

lines(x,predict(m),col="red",lty=2,lwd=3)

#################SELF STARTING FUNCTION###########
#https://bio.libretexts.org/Bookshelves/Introductory_and_General_Biology/Book%3A_General_Biology_(Boundless)/45%3A_Population_and_Community_Ecology/45.2%3A_Environmental_Limits_to_Population_Growth/45.2B%3A_Logistic_Population_Growth#:~:text=A%20graph%20of%20this%20equation,population%20growth%20than%20exponential%20growth.&text=In%20logistic%20growth%2C%20population%20expansion,in%20an%20S%2Dshaped%20curve.
#https://rdrr.io/r/stats/SSlogis.html
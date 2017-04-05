###############################################################################
# Dependencies: data.table
# Created: Sat 26 Apr 2014 11:3900:26 AM IST
###############################################################################

### Learning the parameters of Gompertz curve
### Gompertz curve has the following equation: alpha x exp (-beta x exp(-gamma x time))

#setwd("F:/Data/initiatives/exploration")
#feed$gtin <- as.factor(feed$gtin)
feed<-iniData[[1]]
feed1<-feed1[!is.na(feed1$value),]
feed1$variable<-as.character(feed1$variable)
presentDate<<-tail(feed1[feed1$variable=="Initiative",]$actDate,1)
print(presentDate)
feed1A<-feed1[!(feed1$variable=="Initiative"),]
feed.split <- split(feed1A, feed1A$variable)

### initializing parameters [MANUAL INPUTs]
priors <- vector()
priors[1] <- 4000
point1 <- 3
point2 <- 7


# initialize variables
extra.weight <- 10

prior.fn <- function(variable,actDate,store,extra.weight,priors){
    actDate <- as.Date(actDate, "%Y-%m-%d")
    actDates <- seq.Date(min(actDate[complete.cases(store)]),
                         min(actDate[complete.cases(store)]) + 89, by ="1 day")
    store <-  store[complete.cases(store)]
    day <- length(store)
    predictMark <- c(rep("actual",day),rep("predicted",90-day))
    variable = rep(variable, 90)
    
    print(length(store))
    if(length(store)>=7)
    {

      #Inactive stores with on hand quantity
      
      currentInactive<-unique(
        feed[
          feed$product==variable[1]&
          feed$salesDate==tail(actDate,1)&
          feed$quantity==0&
          feed$amount==0&
          feed$onHand!=0,]$store)
      
      activated<-unique(
        feed[
          feed$product==variable[1]&
          feed$quantity!=0&
          feed$amount!=0,]$store)
      
      if(length(currentInactive)>0)
      {
        inactiveStores<-data.frame(
          cbind(
            as.character(variable[1]),
            currentInactive[!is.na(match(currentInactive,activated))],
            1))
        SKU.onHandStores<<-rbind(SKU.onHandStores,inactiveStores)
      }    
      
      
  #### functions
      g <<- function (y, alpha) {
          log(log(alpha/y))
      }
  
      priors[3] <-  (g(store[point1], priors[1]) - g(store[point2], priors[1])) / (point2 - point1)
      priors[2] <- exp(g(store[point2], priors[1]) + priors[3] * point2)
  
      gompertz <<- function(t, params) {
          params[1] * exp (-params[2] * exp(-params[3] * t))
      }
  
      error <<- function(factors) {
          component1 = sum(weight.day * (gompertz(x.day, factors) - y.day)^2)
          weight1 = 1/sum(weight.day)
          component2 = (factors[1] - priors[1])^2
          lambda <- slope / (day)^2 / store[day]
          return (component1 * weight1 + lambda * component2)
      }
  
      y.day <- store
      x.day <- 1:day
      extra.points <- day
  
      weight.day <- c(rep(1,length(x.day)-extra.points), exp(1), exp(2), exp(3), exp(4),exp(5),exp(6), exp(7), exp(8))
  
      if(length(currentInactive)>0)
      {
        lower.limit <- c(tail(store,1)+nrow(inactiveStores), 0.7 *priors[2], 0.7 * priors[3])            
              
      }
      else
      {
        lower.limit <- c(max(max(y.day), 0.7 * priors[1]), 0.7 *priors[2], 0.7 * priors[3])  
      }
      
      upper.limit <- c(min(priors[1], 1.5 * priors[1]), 1.5 *priors[2], 1.5 * priors[3])
  
      slope = y.day[max(x.day)] - y.day[max(x.day) -1]
  
      model <- optim(par = priors, fn = error, method = "L-BFGS-B", lower = lower.limit, upper = upper.limit)
  
      priors.final <- model$par
  
      fit <- gompertz(1:90,priors.final)
  
      actual = c(store,fit[day+1:90])
      actual = actual[complete.cases(actual)]
  
      return(list(actDates = actDates, fit = actual, predictMark = predictMark, variable = variable))
    }
    else
    {
      actual = c (store,rep(tail(store,1),90-day))
      return(list(actDates = actDates, fit = actual, predictMark = predictMark, variable = variable))
    }

}

SKU.onHandStores<<-NA
feed2 <- lapply(feed.split,FUN = function(x) prior.fn(x$variable,x$actDate,x$value,extra.weight,priors))
SKU.onHandStores<-SKU.onHandStores[2:nrow(SKU.onHandStores),]
names(SKU.onHandStores)<-c('product','stores','onHand')
SKU.onHandStores$product<-as.character(SKU.onHandStores$product)
rm(feed1A,feed.split)

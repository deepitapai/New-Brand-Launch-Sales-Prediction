###########################################################################
# Purpose: The code to compute calloboraitve filtering results.
# Dependencies: None
# Data: Similarity Matrix, Store Acivation Probability
# Created: Sun 27 Apr 2014 10:05:26 AM IST
###########################################################################

# enviornment setup
# rm(list = ls())
require(reshape)
require(zoo)
require(data.table)


# data manipulation
data.aggregation<- aggregate(x= as.numeric(surrogateData[[1]]$quantity), by=list(surrogateData[[1]]$store, surrogateData[[1]]$product), FUN="sum")
#colnames(data.aggregation)<- c("product", "store","quantity")
colnames(data.aggregation)<- c("store","product","quantity")
data.pivot <- cast(data.aggregation, store ~ product, sum)

distance.matrix <- as.matrix(dist(data.pivot, method = "euclidean"))


# gain feed
ini.feed<-iniData[[1]][!(iniData[[1]]$quantity==0&iniData[[1]]$amount==0),]
ini.split <- split(ini.feed,ini.feed$product)

# user defined functions

roll.mean <-  function(quantity){
    r.avg <- ifelse((length(quantity)-7)<1,mean(quantity,na.rm = TRUE),try(mean(quantity[(length(quantity)-7):length(quantity)])))
    final <- ifelse(is(r.avg,"try-error"),0,r.avg)
    return(final)
             }


avg.salesfn <- function(x){

    #y <- split(x,x$store)
    #feed.mean <- lapply(y,function(x) roll.mean(x$quantity))
    feed.mean <- aggregate(x$quantity, by = list(product = x$product,store = x$store), roll.mean)
    #out.mean <- do.call(c,feed.mean)

return(feed.mean)
}

# average sales for each store per gtin
avg.sales <- lapply(ini.split,avg.salesfn)

# all stores
all.stores <- unique(surrogateData[[1]]$store)

start.sales <- function(avg.sales,all.stores){

    if(length(avg.sales$store)>100){

        test.stores <- setdiff(all.stores, avg.sales$store)

        yy <- as.data.frame(avg.sales$x)

        ss <- (distance.matrix[rownames(distance.matrix)%in%test.stores,avg.sales$store])
        str(ss)
        finalresult <- list()
        for (i in 1:nrow(ss))
        {
            final <- NULL
            train <- cbind(as.numeric(t(ss[i,])),yy)
            colnames(train)<- c("sim", "sales")
            flag <- train[order(train[,1]), ]
            sim<- flag[1:10,1]
            sales<- flag[1:10,2]
            salespredict <- as.numeric(t(sim) %*% sales)/sum(sim)
            store = rownames(ss)[i]
            product = avg.sales$product[i]
            final <- as.data.frame(list(product = product, store = store , salespredict = salespredict))
            finalresult = rbind(finalresult, final)
        }

    }else finalresult <- NULL
return(finalresult)
}

system.time(startSales <- lapply(avg.sales,function(x) start.sales(x,all.stores)))

startSales<-ldply(startSales)

#function to fill the start sales for predicted stores 
predict.store.sales<-function(store_df,flag){
  if(flag=='predicted')
  {
    store_df$quantity<-as.numeric(store_df$quantity)
    store_df$product<-as.character(store_df$product)
    product.store<-c(as.character(store_df$product[1]),as.character(store_df$store[1]))
    start.sales<-subset(startSales,
                        as.character(startSales$product)==product.store[1]&
                        as.character(startSales$store)==product.store[2])$salespredict
    
    if(length(start.sales)>0)
    {
      store_df$quantity<-as.numeric(store_df$quantity)
      store_df$quantity[1]<-start.sales
    }
    else
    {
      store_df$quantity[1]<-NA
    }
    
  }
  return(list(store_df=store_df,flag=flag))
}


for(i in 1:length(feed3))
{
  feed3[[i]]<- lapply(feed3[[i]],FUN=function(x)predict.store.sales(x$store_df,x$flag))
}

rm(ini.feed,data.aggregation,data.pivot,distance.matrix,ini.split,avg.sales,all.stores)



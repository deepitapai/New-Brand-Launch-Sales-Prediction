###############################################################################
# Purpose: The code to identify stores which are going to activated
# Dependencies: zoo and plyr
# Created: Sat 13 Jun 2014 
###############################################################################

require(plyr)
require(zoo)

#Arranging stores in descending order of the Femcare SKUs sold
surrogateData[[1]]$quantity<-as.numeric(surrogateData[[1]]$quantity)
data.pi<-ddply(surrogateData[[1]],~store,summarise,SKUs=length(unique(product)),Sales=sum(quantity))
data.pi<-data.pi[
  order(-data.pi$SKUs,-data.pi$Sales),]
colnames(data.pi)<-c('stores','SKUs','sales')

SKU.fn<-function(actDates,fit,predictMark,product)
{
    
    #Stores activated so far
  gtin<-as.character(head(product,1))  
  feed.gtin<-feed[
      feed$product==gtin&
      feed$quantity!=0&
      feed$amount!=0,]


    stores.activated<-data.frame(
      cbind(
        ddply(feed.gtin,~store,summarise,active_date=min(salesDate)),
        'A'))
    colnames(stores.activated)<-c("stores","active_date","activated")
    stores.activated<-stores.activated[
      order(stores.activated$active_date),]
    
    #Merging the two data frames to identify the inactive stores
    result<-merge(data.pi,stores.activated, 
                  by.data.pi='stores', 
                  by.stores.activated='stores',
                  all=T)
    result<-merge(result,
                  SKU.onHandStores[as.character(SKU.onHandStores$product)==gtin,],
                  by.x='stores',
                  by.y='stores',
                  all=T)
    result$onHand<-as.numeric(result$onHand)
    result$onHand[is.na(result$onHand)]<-0    
    result<-result[
      order(-result$onHand,-result$SKUs,-result$sales),]
    
    
    #Building the data structure and filling the actual sales value for each store
    feed.gtin.store<-split(feed.gtin,feed.gtin$store)
    
    feed3<-lapply(feed.gtin.store,FUN=function(x)store.fn(x$product,x$store,x$amount,x$quantity,x$salesDate))
    
                
    #Identifying inactive stores
    toactivate<-subset(result,!is.finite(activated))
    total.days<-length(predictMark)
    actual.days<-length(subset(predictMark,predictMark=="actual"))
    stores.predict<-fit[actual.days:total.days]
    predict.days<-actual.days+1
    stores.actDates<-actDates[predict.days:total.days]
    stores.eachday<-round(diff(stores.predict,lag=1,differences=1),digits=0)
        
    for(i in 1:length(stores.eachday))
    {
      if(stores.eachday[i]<=0)
      {
        next
      }
      else
      {        
        predicted.stores<-data.frame(cbind(head(toactivate$stores,stores.eachday[i]),stores.actDates[i],gtin))
        names(predicted.stores)<-c('store','actDate','product')
        predicted.gtin.store<-split(predicted.stores,predicted.stores$store)
        temp<-lapply(predicted.gtin.store,FUN=function(x)predicted.store.fn(as.character(x$product),as.character(x$store),x$actDate))
        feed3<-c(feed3,temp)
        toactivate<-toactivate[-1:-stores.eachday[i],]
        
      }
    }
    return(feed3)
}


store.fn<-function(product,store,amount,quantity,salesDate){
  
  feed.gtin.store<-data.frame(product,store,amount,quantity,salesDate)
  feed.gtin.store$salesDate1<-as.numeric(feed.gtin.store$salesDate)
  feed.gtin.store<-feed.gtin.store[
      order(feed.gtin.store$salesDate1),]
    
  df<-data.frame(cbind(seq(head(feed.gtin.store$salesDate,1),head(feed.gtin.store$salesDate,1)+89,by="1 day"),0))
  names(df)<-c('salesDate','quantity')
  
    
  df<-merge(feed.gtin.store,df, by.x='salesDate1', by.y='salesDate',all=T)
  df$salesDate<-df$salesDate1
  df$salesDate<-as.Date(df$salesDate)
  df$product<-product[1]
  df$store<-store[1]
  drops<-c('amount','salesDate1','quantity.y')
  df<-df[,!(names(df)%in%drops)]
  names(df)<-c('product','store','quantity','salesDate')
  
  return(list(store_df=df,flag='activated'))

}

predicted.store.fn<-function(product,store,actDate)
{
  actDate<-as.Date(as.numeric(as.character(actDate)))
  df<-data.frame(cbind(product,store,NA,seq(head(actDate,1),head(actDate,1)+89,by="1 day")))
  names(df)<-c('product','store','quantity','salesDate')
  df$salesDate<-as.Date(as.numeric(as.character(df$salesDate)))
  return(list(store_df=df,flag='predicted'))
}

# remove.na.fn<-function(x){
#   ls<-x[!is.na(x)]
#   return(ls)
# }


feed3<-lapply(feed2,FUN=function(x)SKU.fn(x$actDates,x$fit,x$predictMark,x$variable))
rm(data.pi,feed)

# feed3<-lapply(feed3,FUN=function(x)remove.na.fn(x))






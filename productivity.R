###############################################################################
# Purpose: Sales Productivity for each SKU and store combination is calculated
# Dependencies: data table
# Created: Sat 18 Jun 2014 
###############################################################################

## Productivity funciton to predict the future trend of sales for each SKU and Store combination
productivity.fn<-function(store_df,flag)
{
  
  if(flag=='activated')
  {
    current<-which(store_df$salesDate==presentDate)+1
    if(current>=7)
    {
      store_df$quantity[current:90]<-round(mean(tail(store_df$quantity[!is.na(store_df$quantity)],7)),digits=0)
    }
  }
  else
  {
    store_df$quantity<-as.numeric(store_df$quantity)
    store_df$product<-as.character(store_df$product)
    store_df$store<-as.character(store_df$store)
    store_df$quantity<-round(head(store_df$quantity,1),digits=0)
  }
 
  return(list(store_df=store_df,flag=flag))
}

#Calling the productivity function for each store and SKU combination
for(i in 1:length(feed3))
{
  feed3[[i]]<- lapply(feed3[[i]],FUN=function(x)productivity.fn(x$store_df,x$flag))

}

#Function to convert the lists into dataframes
list.to.df.fn<-function(store_df,flag)
{
  df<-data.frame(matrix(unlist(store_df),nrow=nrow(store_df)))
  names(df)<-names(store_df)
  df$flag<-flag
  return(df)
}

#Combining the list of dataframes into a single dataframe

df_final<-NA
for(i in 1:length(feed3))
{
  temp<-ldply(lapply(feed3[[i]],FUN=function(x)list.to.df.fn(x$store_df,x$flag)))
  df_final<-rbind(df_final,temp)
}
rm(temp)
df_final<-df_final[2:nrow(df_final),]
df_final$salesDate<-as.Date(as.numeric(as.character(df_final$salesDate)))





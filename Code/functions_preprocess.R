age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(
    to_lt$mon < from_lt$mon |
      (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
    age - 1,
    age
  )
}

preprocess1 <- function(order) {
  #remove data where delivery_date is ? as they never seem to be returned (for whatever reason)
  #order <- order[order$delivery_date!="?",]
  #Converting dates
  for (chrVar in c("order_date", "delivery_date", "user_reg_date", "user_dob")) {
    order[, chrVar] <- as.Date(order[[chrVar]])
  }
  
  
  #replace 1990-12-31 with NA because this date seems default
  #order$delivery_date[order$delivery_date=="1990-12-31"]<-NA
  
  #Subtract delivery date from order date so we can see "delivery time" which might be important for prediction.
  order$delivery.time<-order$delivery_date - order$order_date
  #take care of weird delivery dates in the past (which for some reason also appear in the test data)
  #order$delivery.time[order$delivery.time<0]<- -500
  #order$delivery.time[is.na(order$delivery.time)] <- -1000
  order$delivery.time_factor[is.na(order$delivery.time)]<-"neverReturned"
  order$delivery.time_factor[order$delivery.time<0]<-"rarelyReturned"
  order$delivery.time_factor[order$delivery.time>=0]<-"returnedHalfTheTime"
  order$delivery.time_factor<- as.factor(order$delivery.time_factor)
  str(order$delivery.time)
  
  #User date of birth
  #I assume that someone, who is below 18 years old, can't order something. However, I should consider default date of birth like '1900-11-19 " as NA too. There are stil some years seem as default but mostly 1900-11-19.l
  
  order$user_dob[order$user_dob=="1900-11-19"]<-NA
  
  order$legal.age<-age(order$user_dob,order$order_date)
  order$legal.age[order$legal.age<18]<-NA
  
  order$order_month<-as.factor(strftime(order$order_date, "%m"))
  order$order_date<-NULL   
  order$delivery_date<-NULL
  order$user_dob<-NULL
  order$order_item_id <- NULL
  order$item_size <- NULL
  
  t<-table(order$item_color)
  order$item_color<-as.character(order$item_color)
  order$item_color[t[order$item_color]<1000]<-"Other"
  order$item_color<-as.factor(order$item_color)
  
  # title: everything that isn't Mrs is other
  order$user_title<-as.character(order$user_title)
  order$user_title[order$user_title!='Mrs']<-'Other'
  order$user_title<-as.factor(order$user_title)
  
  #TODO: remove columns we won't use
  return(order)
}

preprocess2_params <- function(order) {
  params<-NULL
  params$legal.age.median <- median(na.omit(order$legal.age))
  params$delivery.time.median <- median(na.omit(order$delivery.time))
  return(params)
  
  
}

preprocess2 <- function(order,params) {
  order$legal.age[is.na(order$legal.age)] <- params$legal.age.median
  order$delivery.time[is.na(order$delivery.time)] <- params$delivery.time.median
  return(order)
}
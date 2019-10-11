
sale_org <- read.csv("sale.csv",stringsAsFactors=FALSE)
revenue_org <- read.csv("revenue.csv",stringsAsFactors = FALSE)
date_index <-read.csv("date_indexing.csv",stringsAsFactors = FALSE)


#Sorting sale and revenue in ascending order of the model column
sale_org = sale_org[order(sale_org$Model),]
revenue_org = revenue_org[order(revenue_org$Model),]
date_index = date_index[order(date_index$index),]



#changing to date format in date_index
date_index$date <- as.Date.character(date_index$date,"%d-%m-%Y")


#contains the rows with sum of the cells greater than 2000
sub_sale <- sale_org[(sale_org$Sales....2000 == TRUE),]
sub_revenue <- revenue_org[(revenue_org$Sales....2000 == TRUE),]


#-----------------Take comment of to check the sale and revenue files having row.sum > 2000
#write.csv(sub_sale,file = "sale_2000.csv",row.names=FALSE)
#write.csv(sub_revenue,file = "revenue_2000.csv",row.names=FALSE)


#creating price per sale data frame
price_df <- sub_revenue
sub_sale_df <- sub_sale
sub_revenue_df <- sub_revenue
print("the logical(sale>2000) and Value(sum of that row values) column will be dropped from
      the subsetted sale and revenue data frame ")


#Drop the last column(value and logical for (>2000))
price_df[,ncol(price_df)] <- NULL
sub_revenue_df[,ncol(sub_revenue_df)] <- NULL
sub_sale_df[,ncol(sub_sale_df)] <- NULL
sub_sale_df[,ncol(sub_sale_df)] <- NULL

#calculating the price Data frame
for(i in 1:nrow(price_df)){
  for(j in 3:ncol(price_df)){
    if(is.na(price_df[i,j])){
      price_df[i,j] = NA
      next
    }
    if(price_df[i,j] <= 0){
      price_df[i,j] = NA
      next
    }
    price_df[i,j] = sub_revenue_df[i,j]/sub_sale_df[i,j]
  }
}

#CSV gen for revenue/sale
write.csv(price_df,file = "price.csv",row.names=FALSE)

#categorize whether a cell high, medium and low
category_df <- price_df
for(i in 1:nrow(category_df)){
  for(j in 3 :ncol(category_df)){
   if(is.na(price_df[i,j])){
     category_df[i,j] = NA
     next
   }
   if(price_df[i,j] > 699){
     category_df[i,j] = "HIGH"
     next
   }
    if(price_df[i,j] > 299 && price_df[i,j] <= 699){
      category_df[i,j] = "MEDIUM"
      next
    }
    if(price_df[i,j] <= 299){
      category_df[i,j] = "LOW"
      next
    }
  }
}

#CSV gen for categorizing(high,medium and low) based on the values in price file
write.csv(category_df,file = "category.csv",row.names=FALSE)

#for flagging all the months weekending to FY2013 start
temp = 1
date_index["flag_sale"] <- NA 
date_index$flag_sale[1] <- 1
for(i in 2:nrow(date_index)){
  if(date_index$month[i] != date_index$month[i-1]){
    temp = temp + 1
  }
  date_index$flag_sale[i] <- temp
}
#(optional) DATE_index_FLAG CSV
write.csv(date_index,file="date_index_flag.csv",row.names=FALSE)

#flag the month-year combo >2000
sale_flag <- sale_org[(sale_org$Sales....2000 == TRUE),]

#for getting the index value of the FY2013 Start
start_point <- date_index[date_index$date==as.Date.character("06-07-2013","%d-%m-%Y"),1]
end_point <- date_index$index[nrow(date_index)]
start_sum <- which(colnames(sale_flag) == date_index$name[start_point])
end_sum <- 0
to_break <- date_index$flag_sale[nrow(date_index)]

#for handling the negative values in sale (-ve and 0 values will be changed to NA)
sale_flag[is.na(sale_flag)] <- 0
c_count_1 <- ncol(sale_flag) - 2
sale_flag[,c(3:c_count_1)][sale_flag[, c(3:c_count_1)] < 0] <- 0


#to append the 2 column for each month-year combo and fill in the values
#1st column for the combo have the sum and 2nd column for the combo tells true or false(>2000)
for(i in start_point:nrow(date_index)){
  
  if(date_index$flag_sale[i] == date_index$flag_sale[i+1]){
    next
  }
  
  nam_col <- paste(as.character(date_index$year[i]),date_index$month[i],sep = "-")
  sale_flag[nam_col] <- NA
  nam_col_2 <- paste(nam_col,"T/F",sep="-")
  sale_flag[nam_col_2] <- NA
  end_sum <- which(colnames(sale_flag) == date_index$name[i])
  
  #summing of the values
  for(m in 1:nrow(sale_flag)){
    sale_flag[m,nam_col] =  rowSums(sale_flag[m,c(start_sum:end_sum)])
  }
  
  #check >2000 TRUE/FALSE
  for(n in 1:nrow(sale_flag)){
    if(is.na(sale_flag[n,nam_col])){
      sale_flag[n,nam_col_2] = FALSE
      next
    }
    if(sale_flag[n,nam_col] >= 2000){
      sale_flag[n,nam_col_2] = TRUE
    }else{
      sale_flag[n,nam_col_2] = FALSE
    }
  }
  if(date_index$flag_sale[i+1] == to_break){
    break
  }
}
write.csv(sale_flag,file="sale_flag.csv",row.names=FALSE)

#====================================================
#------------------monthy categorization file creation
month_category <- category_df[,c(1,2)]
#-------------------7 Week calculation starts here
start_point <- date_index[date_index$date==as.Date.character("06-07-2013","%d-%m-%Y"),1]
end_point <- date_index$index[nrow(date_index)]
start_sum <- 0
end_sum <- 0
to_break <- date_index$flag_sale[nrow(date_index)]


for(i in start_point:nrow(date_index)){
  #getting to the end(weekending) of the month
  if(date_index$flag_sale[i] == date_index$flag_sale[i+1]){
    next
  }
  start_sum <- which(colnames(category_df) == date_index$name[i])
  end_sum <- start_sum - 6
  nam_col <- paste(as.character(date_index$year[i]),date_index$month[i],sep = "-")
  month_category[nam_col] <- NA
  
  for(m in 1:nrow(category_df)){
    high = 0
    medium = 0
    low = 0
    for(n in start_sum:end_sum){
      if(is.na(category_df[m,n])){
        next
      }
      if(category_df[m,n] == "LOW"){
        low = low + 1
        next
      }
      if(category_df[m,n] == "MEDIUM"){
        medium = medium + 1
        next
      }
      if(category_df[m,n] == "HIGH"){
        high = high + 1
        next
      }
    }
    if(high == 0 && medium == 0 && low == 0){
      lmh = NA
      month_category[m,nam_col] <- NA
      next
    }
    if(high == medium && medium == low){
      lmh = "HIGH"
      month_category[m,nam_col] <- "HIGH"
      next
    }
    if(high >= medium){
      if(high >= low){
        lmh = "HIGH"
        month_category[m,nam_col] <- "HIGH"
      }else{
        lmh = "LOW"
        month_category[m,nam_col] <- "LOW"
      }
    }else{
      if(medium >= low){
        lmh = "MEDIUM"
        month_category[m,nam_col] <- "MEDIUM"
      }else{
        lmh = "LOW"
        month_category[m,nam_col] <- "LOW"
      }
    }
  }
  if(date_index$flag_sale[i+1] == to_break){
    
    start_sum <- which(colnames(category_df) == date_index$name[nrow(date_index)])
    end_sum <- start_sum - 6
    nam_col <- paste(as.character(date_index$year[nrow(date_index)]),date_index$month[nrow(date_index)],sep = "-")
    month_category[nam_col] <- NA
    
    for(m in 1:nrow(category_df)){
      high = 0
      medium = 0
      low = 0
      for(n in start_sum:end_sum){
        if(is.na(category_df[m,n])){
          next
        }
        if(category_df[m,n] == "LOW"){
          low = low + 1
          next
        }
        if(category_df[m,n] == "MEDIUM"){
          medium = medium + 1
          next
        }
        if(category_df[m,n] == "HIGH"){
          high = high + 1
          next
        }
      }
      if(high == 0 && medium == 0 && low == 0){
        lmh = NA
        month_category[m,nam_col] <- NA
        next
      }
      if(high == medium && medium == low){
        lmh = "HIGH"
        month_category[m,nam_col] <- "HIGH"
        next
      }
      if(high >= medium){
        if(high >= low){
          lmh = "HIGH"
          month_category[m,nam_col] <- "HIGH"
        }else{
          lmh = "LOW"
          month_category[m,nam_col] <- "LOW"
        }
      }else{
        if(medium >= low){
          lmh = "MEDIUM"
          month_category[m,nam_col] <- "MEDIUM"
        }else{
          lmh = "LOW"
          month_category[m,nam_col] <- "LOW"
        }
      }
    }
    break
  }
}
#---------------------------------------------monthly categorization CSV file
write.csv(month_category,file="month_category.csv",row.names=FALSE)

pos <- which(colnames(sale_flag) == "Sales....2000")
colnames(sale_flag)[pos] = paste(nam_col,"T/F",sep = "-")

#release the comment to see the column name(for debugging purpose)
#names(sale_flag)

#for validating the high,medium and low comparing with corresponding flag in the sale_flag
valid_month_category <- month_category
to_break <- date_index$flag_sale[nrow(date_index)]

for(i in 1:nrow(date_index)){
  if(date_index$flag_sale[i] == date_index$flag_sale[i+1]){
    next
  }
  nam_col = paste(as.character(date_index$year[i+1]),date_index$month[i+1],sep = "-")
  nam_col_2 = paste(nam_col,"T/F",sep = "-")
  for(m in 1:nrow(month_category)){
    if(sale_flag[m,nam_col_2] == FALSE){
      valid_month_category[m,nam_col] = NA
    }
  }
  if(date_index$flag_sale[i+1] == to_break){
    break
  }
}
write.csv(valid_month_category,file="valid_month_category.csv",row.names=FALSE)

final_output <- date_index
final_output["High"] <- 0
final_output["Medium"] <- 0
final_output["LOW"] <- 0

#for SALE high,medium and low weekly calculation 
#write.csv optional 
write.csv(sub_sale_df,file="sub_sale.csv",row.names=FALSE)

#converting the month validation to weekly validation
valid_weekly <- sub_sale_df
c_count_2 <- ncol(valid_weekly)
valid_weekly[,c(3:c_count_2)] <- NA

end_point <- date_index$index[nrow(date_index)]

for(r in 1:nrow(date_index)){
  if(date_index$flag_sale[r] == date_index$flag_sale[r+1]){
    next
  }
  #start_sum <- which(colnames(sale_flag) == date_index$name[r])
  #nam_col <- paste(as.character(date_index$year[i]),date_index$month[i],sep = "-")
  nam_col <- paste(as.character(date_index$year[r+1]),date_index$month[r+1],sep = "-")
  start_sum <- which(colnames(valid_weekly) == date_index$name[r+1])
  find_end <- r+1
  for(f in find_end:nrow(date_index)){
    if(date_index$flag_sale[f] == to_break){
       found_end <- nrow(date_index)
    }else{
    if(date_index$flag_sale[f] == date_index$flag_sale[f+1]){
      next
    }
    found_end <- f
    }
  }
  found_end <- found_end + 2
  for(i in 1:nrow(valid_month_category)){
    for(j in start_sum:found_end){
     
      if(is.na(valid_month_category[i,nam_col])){
       valid_weekly[i,j] = NA
       next
     }
      if(valid_month_category[i,nam_col] == "LOW"){
        valid_weekly[i,j] = "LOW"
        next
      }
      if(valid_month_category[i,nam_col] == "MEDIUM"){
        valid_weekly[i,j] = "MEDIUM"
        next
      }
      if(valid_month_category[i,nam_col] == "HIGH"){
        valid_weekly[i,j] = "HIGH"
        next
      }
    }
  }
  if(date_index$flag_sale[r+1] == to_break){
    break
  }
}

write.csv(valid_weekly,file="valid_weekly.csv",row.names=FALSE)

sub_sale_df[is.na(sub_sale_df)] <- 0
c_count_3 <- ncol(sub_sale_df)
sub_sale_df[,c(3:c_count_3)][sub_sale_df[, c(3:c_count_3)] < 0] <- 0

#final high,medium and low calculation for sale
for(i in 1:nrow(final_output)){
  for(j in 1:nrow(sub_sale_df)){
    
    if(is.na(valid_weekly[j,final_output$name[i]])){
      next
    }
    if(valid_weekly[j,final_output$name[i]] == "LOW"){
      final_output$LOW[i] = final_output$LOW[i] + sub_sale_df[j,final_output$name[i]]
      next
    }
    if(valid_weekly[j,final_output$name[i]] == "MEDIUM"){
      final_output$Medium[i] = final_output$Medium[i] + sub_sale_df[j,final_output$name[i]]
      next
    }
    if(valid_weekly[j,final_output$name[i]] == "HIGH"){
      final_output$High[i] = final_output$High[i] + sub_sale_df[j,final_output$name[i]]
      next
    }
  }
}

write.csv(final_output,file="output_file.csv",row.names=FALSE)

#final high,medium and low calculation for revenue
sub_revenue_df[is.na(sub_revenue_df)] <- 0
c_count_4 <- ncol(sub_revenue_df)
sub_revenue_df[,c(3:c_count_4)][sub_revenue_df[,c(3:c_count_4)] < 0] <- 0

final_output_revenue <- date_index
final_output_revenue["High"] <- 0
final_output_revenue["Medium"] <- 0
final_output_revenue["LOW"] <- 0

for(i in 1:nrow(final_output_revenue)){
  for(j in 1:nrow(sub_revenue_df)){
    
    if(is.na(valid_weekly[j,final_output_revenue$name[i]])){
      next
    }
    if(valid_weekly[j,final_output_revenue$name[i]] == "LOW"){
      final_output_revenue$LOW[i] = final_output_revenue$LOW[i] + sub_revenue_df[j,final_output_revenue$name[i]]
      next
    }
    if(valid_weekly[j,final_output_revenue$name[i]] == "MEDIUM"){
      final_output_revenue$Medium[i] = final_output_revenue$Medium[i] + sub_revenue_df[j,final_output_revenue$name[i]]
      next
    }
    if(valid_weekly[j,final_output_revenue$name[i]] == "HIGH"){
      final_output_revenue$High[i] = final_output_revenue$High[i] + sub_revenue_df[j,final_output_revenue$name[i]]
      next
    }
  }
}

write.csv(final_output_revenue,file="output_file_revenue.csv",row.names=FALSE)
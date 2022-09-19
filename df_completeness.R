df_completeness <- function(df_comp){
  
  #This count the empty rows in each column and stores as a data frame
  df_cness <- as.data.frame(apply(is.na(df_comp), 2, sum))
  
  #Assign names to the columns of the count  
  df_cness <- setNames(df_cness, "Empty_rows")
  
  #count rows in a column for data frame
  Rows_number <- nrow(df_mpx)
  
  #Add count for number of rows to data columns
  df_cness$Rowcount <- replicate(nrow(df_cness), Rows_number)
  
  
  #Find completeness
  df_cness$Complete <- percent(1 - ((df_cness$Empty_rows/ df_cness$Rowcount)), 0.01)
  
  print(df_cness)
}
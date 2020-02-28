#How to add 16 days to each column, stopping at a max

next_pass <- next_pass %>% as.Date() 

len <- 1:length(next_pass)
updating_table <- NULL

for (r in len){
  x <- seq.Date(next_pass[r], to = end_date, by = 16)
  updating_table <- rbind(updating_table, x)

}





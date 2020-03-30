library("lubridate")
library("tidyverse")
csv_data <- read.csv(file = 'Data/In/CSV_Lookup_Sheet.csv')

#Creates a lookup table with dates stored in the format of numeric days elapsed since 1970 - R's default for vectors
lt <- data.frame(matrix(ncol=4,nrow=233, dimnames=list(NULL, c("Path", "Cycle_Start", "Overpass", "Cycle"))))

index <-   1

#R stores dates since Jan 1, 1970
for(r in csv_data$Paths.){
  
  x <- scan(text = r, sep = ',')
  cycle <- csv_data$Cycle.Day.[index]
  cycle_start <- '01/13/2020'
  overpass <-  mdy(cycle_start) + cycle
  
  for(path in x){
    lt$Cycle[path] <- cycle
    lt$Cycle_Start[path] <- cycle_start
    lt$Path[path] <- path
    lt$Overpass[path] <- overpass
    
    
  }
  
  index <-  index+1
  
}

write.table(lt, "LookupTable.txt", sep="\t")




setwd('~/OneDrive - Johns Hopkins/Documents/Notre Dame/Semester 1/Introduction to Biocomputing/Rproject2021/')

install.packages("data.table")
library(data.table)

convert_to_csv <- function(directory) {
  
  setwd(directory)
  
  for (file in list.files()) {
    
    # read data from a screening on a particular day that is presented in a .txt file
    data <- read.table(file,header = TRUE, sep='',stringsAsFactors = FALSE)
    
    # extract filename from filename.txt and rename to filename.csv
    # Example: screen_120.txt becomes screen_120.csv
    tt <- (file)
    filename <- regmatches(tt, regexpr("screen_[0-9].*[0-9]", tt))
    filename_csv <- paste(filename,".csv",sep="")
    
    # convert file from .txt to .csv
    convert_to_csv <- write.table(data,filename_csv,row.names=FALSE,col.names=TRUE, sep=",")
    
  } 
  
}



mergeDataFiles <- function(dir){
  
  setwd(dir)
  
  for (file in list.files()) {
    
    check.country <- regmatches(file, regexpr("country", file))
    check.country=="country"
    
    if (isTRUE(check.country=="country")){
      
      country <- substr(file,8,nchar(file))
      path <- paste(dir,file,sep="/")
      
      for (screens in list.files(path=path,pattern=".csv")) {
        
        setwd(path)
        
        data <- read.csv(screens, header = TRUE, sep=',',stringsAsFactors = FALSE)
  
        data$country <- country
        data$dayofYear <- regmatches(screens, regexpr("[0-9].*[0-9]", screens))
        
        
        filename <- regmatches(screens, regexpr("screen_[0-9].*[0-9]", screens))
        filename_updated_csv <- paste(filename,"_updated.csv",sep="")
        
        
        screen_udpated <- write.table(data,filename_updated_csv,row.names=FALSE,col.names=TRUE, sep=",")
        
      }
      
      files_to_merge <- list.files(path=path,pattern="updated.csv")
      allFiles <- lapply(files_to_merge,fread,sep=",")
      bindedFiles <- rbindlist(allFiles)
      filename_country <- paste(country,"allData.csv",sep="")
      mergedFiles <- write.csv(bindedFiles,filename_country,row.names=FALSE)
      
      file.rename(from = file.path(path, filename_country), 
                   to = file.path(dir, filename_country) )
    } 
    
    else {
      next
    }
  }
  
  setwd(dir)
  files_to_merge_1 <- list.files(pattern=".csv")
  allFiles_1 <- lapply(files_to_merge_1,fread,sep=",")
  bindedFiles_1 <- rbindlist(allFiles_1,fill=TRUE)
  unlink(files_to_merge_1)
  mergedFiles_1 <- write.csv(bindedFiles_1,"allData.csv",row.names=FALSE)
  
}






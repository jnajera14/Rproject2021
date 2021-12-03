install.packages("ggplot2")
install.packages("cowplot")
install.packages("data.table")
install.packages("dplyr")

library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)

#==================================================================================================================================================================================================================================================#

convert2csv <- function(dir){
  
  # Function Description: Converts .txt files to .csv files
  
  # Inputs:
  #   dir: directory containing .txt files

  # Outputs:
  #   returns .csv files with the same name as .txt files
  
  setwd(dir)                                                              # Sets working directory to dir
  listfile <- list.files(pattern = ".txt")                                # Creates a list of .txt file names
  
  for (file in listfile){                                                            # Loops through the elements in the list
    data <- read.table(file,header = TRUE,sep=' ',stringsAsFactors = FALSE)
    filename_csv <- sub(".txt",".csv",file)
    # Creates the filename "screen_###.csv"
    write.table(data,filename_csv,row.names=FALSE,col.names=TRUE, sep=",")           # Reads each .txt file and writes it to .csv file
  }
  
  print('Files have been written to .csv file in specified directory')
  setwd("../")
}

#==================================================================================================================================================================================================================================================#

mergeDataFiles <- function(dir,NA_option=1){
  
  # Function Description: merge all screen data from each country with added columns 
  # specifying the country name and the day at which the screen was taken
  
  # Inputs:
  #   dir: directory containing directories for each country with screen data
  #   NA_option: defaults to a value of 1 which omits NAs in compiled data
  #              if NA_option=2, NAs will be included in compiled data
  
  # Outputs:
  #   returns merged data from all countries in specified directory 
  
  setwd(dir)

  for (folder in list.files()) {
    
    # look at name of files (or folder) in directory, and determine if it has "country" in it
    # assumes naming convention is applied if other countries are added to directory (e.g. countryZ, countryXYZ)
    check.country <- regmatches(folder, regexpr("country", folder))
    check.country == "country"
    
    if (isTRUE(check.country=="country")){
      
      # extract name of country from the folder containing the screen data for that country
      country <- substr(folder,8,nchar(folder))
      # set up a path to the country of interest containing the screen data
      path <- paste(dir,folder,sep="/")
      
      for (screens in list.files(path=path,pattern=".csv")) {
        
        # set working directory using path established earlier 
        setwd(path)
        
        data <- read.csv(screens, header = TRUE, sep=',',stringsAsFactors = FALSE)
        
        # create new columns which indicate the country name and day of year at which screen data was recorded
        data$country <- country
        data$dayofYear <- regmatches(screens, regexpr("[0-9].*[0-9]", screens))
        
        # rename .csv screen file containing updated columns 
        # keeps original .csv screen files untouched when saving 
        filename <- regmatches(screens, regexpr("screen_[0-9].*[0-9]", screens))
        filename_updated_csv <- paste(filename,"_updated.csv",sep="")
        
        # create updated .csv screen file
        screen_udpated <- write.table(data,filename_updated_csv,row.names=FALSE,col.names=TRUE, sep=",")
        
      }
      
      # merge all updated .csv screen files for a particular into one .csv file
      # naming convention: for countryX, merged .csv file will be called XallData.csv
      files_to_merge <- list.files(path=path,pattern="updated.csv")
      allFiles <- lapply(files_to_merge,fread,sep=",")
      bindedFiles <- rbindlist(allFiles)
      filename_country <- paste(country,"allData.csv",sep="")
      unlink(files_to_merge) # deletes all updated .csv screen files
      mergedFiles <- write.csv(bindedFiles,filename_country,row.names=FALSE)
      
      # move merged file to original input directory from the directory of a particular country
      file.rename(from = file.path(path, filename_country), 
                   to = file.path(dir, filename_country) )
    } 
    
    else {
      next
    }
  }
  
  # set working directory back to original input directory
  setwd(dir)
  
  # once all merged .csv files for each country (i.e. XallData.csv, YallData.csv, etc.) is in original 
  # directory, this will merge all of those files together into allData.csv
  files_to_merge_1 <- list.files(pattern=".csv")
  allFiles_1 <- lapply(files_to_merge_1,fread,sep=",")
  bindedFiles_1 <- rbindlist(allFiles_1,fill=TRUE)
  unlink(files_to_merge_1) # delete individual merged files for each country
  mergedFiles_1 <- write.csv(bindedFiles_1,"allData.csv",row.names=FALSE)
  
  # Check for NA values, according to option
  if (NA_option == 1){
    data <- na.omit(data)
  } else if(NA_option == 2){
    count <- sum(is.na(data))
    if (count != 0){
      print('Warning: your data includes NA values. Set NA_option to 1 to remove')
    }
  }
  
}

#==================================================================================================================================================================================================================================================#

dataSummary <- function(dir,filename){
  
  # Function Description: Converts txt files to csv files
  
  # Inputs:
  #   dir: directory containing combined csv file
  #   filename: name of the combined csv file
  
  # Outputs:
  #   prints to console summary stats
  
  setwd(dir)
  
  data <- read.csv(filename, header = TRUE)
  
  # Total number of screens
  numscreen <- nrow(data)
  print('The number of screens that were run is ')
  print(numscreen)
  
  # Calculating positivity rate. A patient is considered positive if they have at least one marker
  positive <- rowSums(data[,3:12])
  positives <- as.logical(positive)
  posrate <- sum(positives)/length(positives) * 100
  print('The percent of patients that were infected is ')
  print(posrate)
  data <- cbind(data, positive, positives)
  
  # Counting male vs female positive patients and plotting per country
  male_count <- sum(data$positives[data$gender == 'male'])
  female_count <- sum(data$positives[data$gender == 'female'])
  print('The total number of male patients is ')
  print(male_count)
  print('The total number of female patients is ')
  print(female_count)
  
  plot1 <- ggplot(data, aes(x = positive, color = gender, fill = gender)) +
    geom_bar(position = 'dodge') +
    scale_x_continuous(name = 'Number of markers', expand = c(0,0), breaks = seq(0, 6, 1)) + 
    scale_y_continuous(name = 'Number of patients', expand = c(0,0)) + 
    labs(title = 'Distribution of number of markers across gender')+
    theme_classic()
  
  #Calculating the age distribution stats for patients and plotting per country
  age_mean <- mean(data$age[data$positives == TRUE])
  age_stdev <- sd(data$age[data$positives == TRUE])
  print('The average age of patients is ')
  print(age_mean)
  print('The standard deviation of the age of patients is ')
  print(age_stdev)
  
  plot2 <- ggplot(data, aes(x = age, color = country, fill = country)) + 
    geom_histogram(binwidth = 5, position = 'identity', alpha = 0.3) + 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = 'Age Distribution Plot') + 
    theme_classic()
  
  final_plot <- plot_grid(plot1, plot2,
            labels = c("a", "b"),
            rel_widths = c(1, 0.85),
            ncol = 2,
            nrow = 1)
  
  print(final_plot)
  
  ggsave("Summary Plot.jpg",plot=final_plot,width=12,height=8,dpi=500)
}




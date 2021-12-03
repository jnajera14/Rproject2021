# set to your own working directory t0 project folder 
n <- "~/OneDrive - Johns Hopkins/Documents/Notre Dame/Semester 1/Introduction to Biocomputing/Rproject2021/"
setwd(n)
source(paste(n,"supportingFunctions.R",sep='/'))
convert2csv(paste(n,"countryY/",sep='/')) # if there is more than one country whose screen data is in .txt format, copy and paste this line and specify country directory 
                                          # Example: for countryZ, convert2csv(paste(n,"countryZ/",sep='/'))
mergeDataFiles(n,NA_option=1)
dataSummary(n,"allData.csv")

# install.packages("ggplot2")
# install.packages("cowplot")
# install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(cowplot)

# Plotting cases per country

data <- read.table("allData.csv", header = TRUE, sep=',',stringsAsFactors = FALSE)

# create new column indicating if a patient is infected (1) or not (0)
data$infected <- as.numeric(as.logical(rowSums(data[,3:12])))
  
# create empty data frame to be appended later on
df1 <- data.frame()
  
for (Country in unique(data$country)) {
    
  # create empty vectors which will be filled in with the screening day and the total number 
  # infected for that screen day
  day <- c()
  totalInfected <- c()
  
  # isolate set of data corresponding to a particular country
  df <- data[data$country==Country,]
  uniqueDay <- unique(df$dayofYear)
  
  for (i in 1:length(uniqueDay)){
    
    # build vectors for total number of infected people in a country for a specific day
    inf <- df[df$dayofYear==uniqueDay[i],]
    totalInfected <- append(totalInfected,sum(inf$infected))
    day <- append(day,uniqueDay[i])
    
  }
  
  # create data frame summarizing total infected on a particular day for a particular country
  df2 <- data.frame(day,totalInfected)
  df2$country <- Country
  
  # combine data frames to include total infected for every day and every country
  df1 <- rbind(df1,df2)
  
}

fig1 <- ggplot(df1,aes(x=day,y=totalInfected,color=country))+
  geom_point() +
  theme_classic() +
  xlab("Day") +
  ylab("Total Infected Cases")

# calculate the marker distribution in the population for each country
ha <- aggregate(data[,3:12], by=list(country=data$country), FUN=sum)  # sum the marker column for each country
hatable <- data.frame(ha[,-1], row.names=ha[,1])                      # change the sum results to a data frame
resulttable <- t(hatable)                                             # Rotating the data frame for easier plotting

# create the distribution dataframe for two country (X and Y)
FINAL <- data.frame(Marker=row.names(resulttable),
                   countryX= resulttable[,1],
                   countryY = resulttable[,2])

fig2 <- ggplot(FINAL, aes(x="", y=countryX, fill=Marker))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)

fig3 <- ggplot(FINAL, aes(x="", y=countryY, fill=Marker))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)

plot <- plot_grid(fig1, fig2, fig3,
          labels = c("a", "b", "c","d"),
          rel_widths = c(1, 0.85),
          ncol = 3,
          nrow = 1)

plot
ggsave("Analysis Plot.jpg",plot=plot,width=15,height=8,dpi=500)


### Answers to Questions ###

print("PROBLEM 1")

print("Based on figure a, it is likely that the disease outbreak began in Country X given that, initially, Country Y has no infections whereas Country X does. Specifically, at day 140 it can be seen that positive cases start to show up in Country Y likely due to spreading from Country X")

print("PROBLEM 2")

print("Based on figures b and c, it likely that if Country Y were to develop a vaccine for the disease, it would not work for citizens of Country X. This is because the marker distribution across patients is different for each country. In particular, the most prevalent markers which show up in Country X are markers 01-05, while in Country Y it's markers 06-10.")

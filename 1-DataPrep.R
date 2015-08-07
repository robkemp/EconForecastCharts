#######################################################################
#                     Data Prep                                       
#######################################################################

library(readxl)
library(dplyr)
library(tidyr)

# Reads in the Excel File (I had to convert it to an xlsx because it wasn't reading as an Excel)
v14=read_excel("CNTYDETv14.xlsx")

# Pipe of functions that parses the data
j14=v14%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j14, "totalJobs_v14.csv")

#Removes the objects from the environment
rm(j14,v14)
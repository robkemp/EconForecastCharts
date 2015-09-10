#######################################################################
#                     Data Prep                                       
#######################################################################

library(readxl)
library(dplyr)
library(tidyr)

# Reads in the Excel File (I had to convert it to an xlsx because it wasn't reading as an Excel)
v13=read_excel("CNTYDET_2015.xlsx", sheet=3)
v14=read_excel("CNTYDET_2015.xlsx", sheet=2)
v15=read_excel("CNTYDET_2015.xlsx", sheet=1)
v15a=read_excel("CNTYDET_2015.xlsx", sheet=4)

rv13=read_excel("REGDET_13v_14v_2015.xlsx", sheet=1)
rv14=read_excel("REGDET_13v_14v_2015.xlsx", sheet=2)
rv15=read_excel("REGDET_13v_14v_2015.xlsx", sheet=3)
rv15a=read_excel("REGDET_13v_14v_2015.xlsx", sheet=4)


# Pipe of functions that parses the data
j13=v13%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j13, "totalJobs_v13.csv", row.names = FALSE)


# Pipe of functions that parses the data
j14=v14%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j14, "totalJobs_v14.csv", row.names = FALSE)


# Pipe of functions that parses the data
j15=v15%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j15, "totalJobs_v15.csv", row.names = FALSE)

# Pipe of functions that parses the data
j15a=v15a%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j15a, "totalJobs_v15a.csv", row.names = FALSE)

# Pipe of functions that parses the data
p15=v15a%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("ADJPOPC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(p15, "totalPop_v15.csv", row.names = FALSE)

# Pipe of functions that parses the data
l15=v15a%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("LFRESC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=6 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(l15, "totalLabor_v15.csv", row.names = FALSE)

# Pipe of functions that parses the data
rj13=rv13%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(rj13, "totalJobsReg_v13.csv", row.names = FALSE)



# Pipe of functions that parses the data
rj14=rv14[,-1]%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(rj14, "totalJobsReg_v14.csv", row.names = FALSE)


# Pipe of functions that parses the data
rj15=rv15[,-1]%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns
#Writes out the parsed data to a csv
write.csv(rj15, "totalJobsReg_v15.csv", row.names = FALSE)

# Pipe of functions that parses the data
rj15a=rv15a%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns
#Writes out the parsed data to a csv
write.csv(rj15a, "totalJobsReg_v15a.csv", row.names = FALSE)

rp15=rv15[,-1]%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("ADJPOPR", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(rp15, "totalPopReg_v15.csv", row.names = FALSE)

rl15=rv15[,-1]%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("LFRESR", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=6 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(rl15, "totalLaborReg_v15.csv", row.names = FALSE)


#Removes the objects from the environment
rm(j14,v14, j15, v15, j13, v13)
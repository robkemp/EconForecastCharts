library(shiny)
library(dplyr)
library(readr)




jobsForecast=function(fips){ #This line defines the start of the function and what arguments are used by putting them in the parentheses
  
  #this section loads all of the libraries of functions that I need to make the graph or get the data working
  require(readr, quietly=TRUE) #read in csv with good defaults
  require(ggplot2, quietly=TRUE) #the graphing package to make the chart
  require(grid, quietly=TRUE) #a required package to use the codemog theme I created
  require(scales, quietly=TRUE) #helps you not alter underlying data, but convert 2000 to 2,000 for charting and display
  require(codemog, quietly=TRUE) #the SDO package with all my helper functions
  require(dplyr, quietly=TRUE) #the workhorse for data manipulation and also imports the pipe "%>%" function
  
  
  #This whole pipe is assigned to the object called 'd13'
  d13=read_csv("totalJobs_v13.csv")%>% #Loads in the resaped data for Vintage 14
    rename(totalJobs=value)%>%#renames the column value to totalJobs, a better descriptor
    filter(countyfips==fips)%>%#this selects rows that meet the criteria, fips gets passed from the function call
    mutate(data="Vintage 2013")#adds a column with just the text "Vintage 2014" to act as a label for the line in the chart
  
  #This whole pipe is assigned to the object called 'd14'
  d14=read_csv("totalJobs_v14.csv")%>% #Loads in the resaped data for Vintage 14
    rename(totalJobs=value)%>%#renames the column value to totalJobs, a better descriptor
    filter(countyfips==fips)%>%#this selects rows that meet the criteria, fips gets passed from the function call
    mutate(data="Vintage 2014")#adds a column with just the text "Vintage 2014" to act as a label for the line in the chart
  
  ##Fake data to test using same process as above
  d15=read_csv("totalJobs_v15.csv")%>%
    rename(totalJobs=value)%>%
    filter(countyfips==fips)%>%
    mutate(data="Vintage 2015")
  
  d15a=read_csv("totalJobs_v15a.csv")%>%
    rename(totalJobs=value)%>%
    filter(countyfips==fips)%>%
    mutate(data="Vintage 2015 AF")
  
  d=bind_rows(d13,d14, d15, d15a) #stacks the vintage 14 and other data sets together
  
  #This whole pipe is assigned to the object called 'p'
  p=d%>% #This argument passes the data we just made to the following graphing call
    ggplot(aes(x=year, y=totalJobs, color=data))+ #This call establishes the axes and variable that groups the data
    geom_line( size=1.15)+ #This line actually adds the data to the plot (via lines in this case)
    scale_y_continuous(label=comma)+ #This line formats the values on the y-axis to have commas
    scale_color_manual(values=c(rgb(0,149,58,max=255),rgb(31,74,126, max=255), rgb(191,32,38,max=255), rgb(216,199,34,max=255)), #this line tells R what colors to make the lines
                       name="Version")+ #This line sets the legend title
    theme_codemog()+ #This line adds the formatting theme I developed
    theme(panel.grid.minor = element_line(colour = rgb(210, 210, 210, max = 255), size=12*.05))+
    labs(x="Year", y="Total Jobs", title=paste("SDO Jobs Forecast for County Number", fips, "by Version")) #This line creates labels including a dynamic title with the county number in it.
  
  
  return(p) #this call tells the function what objects to print or store in R and which to delete after it runs
}



jobsForecastReg=function(fips){ #This line defines the start of the function and what arguments are used by putting them in the parentheses
  
  #this section loads all of the libraries of functions that I need to make the graph or get the data working
  require(readr, quietly=TRUE) #read in csv with good defaults
  require(ggplot2, quietly=TRUE) #the graphing package to make the chart
  require(grid, quietly=TRUE) #a required package to use the codemog theme I created
  require(scales, quietly=TRUE) #helps you not alter underlying data, but convert 2000 to 2,000 for charting and display
  require(codemog, quietly=TRUE) #the SDO package with all my helper functions
  require(dplyr, quietly=TRUE) #the workhorse for data manipulation and also imports the pipe "%>%" function
  
  #This whole pipe is assigned to the object called 'd13'
  d13=read_csv("totalJobsReg_v13.csv")%>% #Loads in the resaped data for Vintage 14
    rename(totalJobs=value)%>%#renames the column value to totalJobs, a better descriptor
    filter(regionnumber==fips)%>%#this selects rows that meet the criteria, fips gets passed from the function call
    mutate(data="Vintage 2013")#adds a column with just the text "Vintage 2014" to act as a label for the line in the chart
  
  #This whole pipe is assigned to the object called 'd14'
  d14=read_csv("totalJobsReg_v14.csv")%>% #Loads in the resaped data for Vintage 14
    rename(totalJobs=value)%>%#renames the column value to totalJobs, a better descriptor
    filter(regionnumber==fips)%>%#this selects rows that meet the criteria, fips gets passed from the function call
    mutate(data="Vintage 2014")#adds a column with just the text "Vintage 2014" to act as a label for the line in the chart
  
  ##Fake data to test using same process as above
  d15=read_csv("totalJobsReg_v15.csv")%>%
    rename(totalJobs=value)%>%
    filter(regionnumber==fips)%>%
    mutate(data="Vintage 2015")
  
  d15a=read_csv("totalJobsReg_v15a.csv")%>%
    rename(totalJobs=value)%>%
    filter(regionnumber==fips)%>%
    mutate(data="Vintage 2015 AF")
  
  
  d=bind_rows(d13, d14, d15, d15a) #stacks the vintage 14 and other data sets together
  
  #This whole pipe is assigned to the object called 'p'
  p=d%>% #This argument passes the data we just made to the following graphing call
    ggplot(aes(x=year, y=totalJobs, color=data))+ #This call establishes the axes and variable that groups the data
    geom_line( size=1.15)+ #This line actually adds the data to the plot (via lines in this case)
    scale_y_continuous(label=comma)+ #This line formats the values on the y-axis to have commas
    scale_color_manual(values=c(rgb(0,149,58,max=255),rgb(31,74,126, max=255), rgb(191,32,38,max=255), rgb(216,199,34,max=255)), #this line tells R what colors to make the lines
                       name="Version")+ #This line sets the legend title
    theme_codemog()+ #This line adds the formatting theme I developed
    theme(panel.grid.minor = element_line(colour = rgb(210, 210, 210, max = 255), size=12*.05))+
    labs(x="Year", y="Total Jobs", title=paste("Region Number", fips)) #This line creates labels including a dynamic title with the county number in it.
  
  
  return(p) #this call tells the function what objects to print or store in R and which to delete after it runs
}

popForecast=function(fips){ #This line defines the start of the function and what arguments are used by putting them in the parentheses
  
  #this section loads all of the libraries of functions that I need to make the graph or get the data working
  require(readr, quietly=TRUE) #read in csv with good defaults
  require(ggplot2, quietly=TRUE) #the graphing package to make the chart
  require(grid, quietly=TRUE) #a required package to use the codemog theme I created
  require(scales, quietly=TRUE) #helps you not alter underlying data, but convert 2000 to 2,000 for charting and display
  require(codemog, quietly=TRUE) #the SDO package with all my helper functions
  require(dplyr, quietly=TRUE) #the workhorse for data manipulation and also imports the pipe "%>%" function
  
  p15=read_csv("totalPop_v15.csv")%>%
    rename(total=value)%>%
    filter(countyfips==fips)%>%
    mutate(data="Population")
  
  l15=read_csv("totalLabor_v15.csv")%>%
    rename(total=value)%>%
    filter(countyfips==fips)%>%
    mutate(data="Labor Force")
  
  d=bind_rows(p15, l15) #stacks the vintage 14 and other data sets together
  
  #This whole pipe is assigned to the object called 'p'
  p=d%>% #This argument passes the data we just made to the following graphing call
    ggplot(aes(x=year, y=total, color=data))+ #This call establishes the axes and variable that groups the data
    geom_line( size=1.15)+ #This line actually adds the data to the plot (via lines in this case)
    scale_y_continuous(label=comma)+ #This line formats the values on the y-axis to have commas
    scale_color_manual(values=c(rgb(67,0,152, max=255), rgb(239,117,33, max=255)), #this line tells R what colors to make the lines
                       name="Variable")+ #This line sets the legend title
    theme_codemog()+ #This line adds the formatting theme I developed
    theme(panel.grid.minor = element_line(colour = rgb(210, 210, 210, max = 255), size=12*.05))+
    labs(x="Year", y="Total", title=paste("County Number", fips)) #This line creates labels including a dynamic title with the county number in it.
  
  
  return(p) #this call tells the function what objects to print or store in R and which to delete after it runs
}

growth=function(fips){ #This line defines the start of the function and what arguments are used by putting them in the parentheses
  
  #this section loads all of the libraries of functions that I need to make the graph or get the data working
  require(readr, quietly=TRUE) #read in csv with good defaults
  require(ggplot2, quietly=TRUE) #the graphing package to make the chart
  require(grid, quietly=TRUE) #a required package to use the codemog theme I created
  require(scales, quietly=TRUE) #helps you not alter underlying data, but convert 2000 to 2,000 for charting and display
  require(codemog, quietly=TRUE) #the SDO package with all my helper functions
  require(car, quietly=TRUE) #tpackage with a nice recode function
  require(dplyr, quietly=TRUE) #the workhorse for data manipulation and also imports the pipe "%>%" function
  yrs=c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040)
  
  d15=read_csv("totalJobs_v15.csv")%>%
    rename(total=value)%>%
    filter(countyfips==fips)%>%
    mutate(data="Vintage 2015")
  
  d15a=read_csv("totalJobs_v15a.csv")%>%
    rename(total=value)%>%
    filter(countyfips==fips)%>%
    mutate(data="Vintage 2015 AF")
  
  p15=read_csv("totalPop_v15.csv")%>%
    rename(total=value)%>%
    filter(countyfips==fips)%>%
    mutate(data="Population")
  
  l15=read_csv("totalLabor_v15.csv")%>%
    rename(total=value)%>%
    filter(countyfips==fips)%>%
    mutate(data="Labor Force")
  
  d=bind_rows(p15, l15, d15, d15a)%>% #stacks the vintage 14 and other data sets together
    filter(year %in% yrs)%>%
    group_by(data)%>%
    mutate(Change=total-lag(total),
           GrowthRate=ann_gr(lag(total),total, year-lag(year)))
  #This whole pipe is assigned to the object called 'p'
  p=d%>% #This argument passes the data we just made to the following graphing call
    ggplot(aes(x=year, y=round(GrowthRate,2), color=data))+ #This call establishes the axes and variable that groups the data
    geom_line( size=1.15)+ #This line actually adds the data to the plot (via lines in this case)
    #     scale_y_continuous(label=)+ #This line formats the values on the y-axis to have commas
    scale_color_manual(values=c(rgb(67,0,152, max=255), rgb(239,117,33, max=255),rgb(191,32,38,max=255), rgb(216,199,34,max=255)), #this line tells R what colors to make the lines
                       name="Variable")+ #This line sets the legend title
    theme_codemog()+ #This line adds the formatting theme I developed
    theme(panel.grid.minor = element_line(colour = rgb(210, 210, 210, max = 255), size=12*.05))+
    labs(x="Year", y="Annualized Growth Rate", title=paste("Growth Rate for County Number", fips)) #This line creates labels including a dynamic title with the county number in it.
  
  
  return(p) #this call tells the function what objects to print or store in R and which to delete after it runs
}

growthReg=function(fips){ #This line defines the start of the function and what arguments are used by putting them in the parentheses
  
  #this section loads all of the libraries of functions that I need to make the graph or get the data working
  require(readr, quietly=TRUE) #read in csv with good defaults
  require(readxl, quietly=TRUE)
  require(ggplot2, quietly=TRUE) #the graphing package to make the chart
  require(grid, quietly=TRUE) #a required package to use the codemog theme I created
  require(scales, quietly=TRUE) #helps you not alter underlying data, but convert 2000 to 2,000 for charting and display
  require(codemog, quietly=TRUE) #the SDO package with all my helper functions
  require(car, quietly=TRUE) #tpackage with a nice recode function
  require(dplyr, quietly=TRUE) #the workhorse for data manipulation and also imports the pipe "%>%" function
  yrs=c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040)
  
  d15=read_csv("totalJobsReg_v15.csv")%>%
    rename(total=value)%>%
    filter(regionnumber==fips)%>%
    mutate(data="Vintage 2015")
  
  d15a=read_csv("totalJobsReg_v15a.csv")%>%
    rename(total=value)%>%
    filter(regionnumber==fips)%>%
    mutate(data="Vintage 2015 AF")
  
  
  c=read_excel("FIPSandRegion.xls")%>%
    rename(regionnum=PMRegion)%>%
    mutate(countyfips=as.numeric(Fips))%>%
    filter(regionnum==fips)%>%
    select(countyfips, regionnum)
  
  p15=read_csv("totalPop_v15.csv")%>%
    rename(total=value)%>%
    #     filter(countyfips==fips)%>%
    mutate(data="Population")
  
  l15=read_csv("totalLabor_v15.csv")%>%
    rename(total=value)%>%
    #     filter(countyfips==fips)%>%
    mutate(data="Labor Force")
  
  d=bind_rows(p15, l15)
    #stacks the vintage 14 and other data sets together
  d=inner_join(d, c)%>%
    group_by(year,regionnum,data)%>%
    summarize(total=sum(total))
  
  d=bind_rows(d, d15, d15a)%>% #stacks the vintage 14 and other data sets together
    filter(year %in% yrs)%>%
    group_by(data)%>%
    mutate(Change=total-lag(total),
           GrowthRate=ann_gr(lag(total),total, year-lag(year)))
  #This whole pipe is assigned to the object called 'p'
  p=d%>% #This argument passes the data we just made to the following graphing call
    ggplot(aes(x=year, y=round(GrowthRate,2), color=data))+ #This call establishes the axes and variable that groups the data
    geom_line( size=1.15)+ #This line actually adds the data to the plot (via lines in this case)
    #     scale_y_continuous(label=)+ #This line formats the values on the y-axis to have commas
    scale_color_manual(values=c(rgb(67,0,152, max=255), rgb(239,117,33, max=255),rgb(191,32,38,max=255), rgb(216,199,34,max=255)), #this line tells R what colors to make the lines
                       name="Variable")+ #This line sets the legend title
    theme_codemog()+ #This line adds the formatting theme I developed
    theme(panel.grid.minor = element_line(colour = rgb(210, 210, 210, max = 255), size=12*.05))+
    labs(x="Year", y="Annualized Growth Rate", title=paste("Growth Rate for Region Number", fips)) #This line creates labels including a dynamic title with the county number in it.
  
  
  return(p) #this call tells the function what objects to print or store in R and which to delete after it runs
}

popForecastReg=function(fips){ #This line defines the start of the function and what arguments are used by putting them in the parentheses
  
  #this section loads all of the libraries of functions that I need to make the graph or get the data working
  require(readr, quietly=TRUE) #read in csv with good defaults
  require(ggplot2, quietly=TRUE) #the graphing package to make the chart
  require(grid, quietly=TRUE) #a required package to use the codemog theme I created
  require(scales, quietly=TRUE) #helps you not alter underlying data, but convert 2000 to 2,000 for charting and display
  require(codemog, quietly=TRUE) #the SDO package with all my helper functions
  require(dplyr, quietly=TRUE) #the workhorse for data manipulation and also imports the pipe "%>%" function
  
  c=read_excel("FIPSandRegion.xls")%>%
    rename(regionnum=PMRegion)%>%
    mutate(countyfips=as.numeric(Fips))%>%
    filter(regionnum==fips)%>%
    select(countyfips, regionnum)
  
  p15=read_csv("totalPop_v15.csv")%>%
    rename(total=value)%>%
    #     filter(countyfips==fips)%>%
    mutate(data="Population")
  
  l15=read_csv("totalLabor_v15.csv")%>%
    rename(total=value)%>%
    #     filter(countyfips==fips)%>%
    mutate(data="Labor Force")
  
  d=bind_rows(p15, l15) #stacks the vintage 14 and other data sets together
  d=inner_join(d, c)%>%
    group_by(year,regionnum,data)%>%
    summarize(total=sum(total))
  
  #This whole pipe is assigned to the object called 'p'
  p=d%>% #This argument passes the data we just made to the following graphing call
    ggplot(aes(x=year, y=total, color=data))+ #This call establishes the axes and variable that groups the data
    geom_line( size=1.15)+ #This line actually adds the data to the plot (via lines in this case)
    scale_y_continuous(label=comma)+ #This line formats the values on the y-axis to have commas
    scale_color_manual(values=c(rgb(67,0,152, max=255), rgb(239,117,33, max=255)), #this line tells R what colors to make the lines
                       name="Variable")+ #This line sets the legend title
    theme_codemog()+ #This line adds the formatting theme I developed
    theme(panel.grid.minor = element_line(colour = rgb(210, 210, 210, max = 255), size=12*.05))+
    labs(x="Year", y="Total", title=paste("Region Number", fips)) #This line creates labels including a dynamic title with the county number in it.
  
  
  return(p) #this call tells the function what objects to print or store in R and which to delete after it runs
}

c=read_excel("FIPSandRegion.xls")%>%
  rename(regionnum=PMRegion, county=Name)%>%
  mutate(countyfips=as.numeric(Fips))%>%
  filter(countyfips!=1 , countyfips!=0 , countyfips!=5 ,countyfips!=13 ,countyfips!=14 ,countyfips!=31 ,countyfips!=35 ,countyfips!=59)


shinyServer(function(input,output){
  cnty=reactive({as.numeric(c%>%filter(county==input$county)%>%select(countyfips))})
  output$plot=renderPlot({jobsForecast(cnty())})
  output$Rplot=renderPlot({jobsForecastReg(input$Rnum)})
  output$plotPop=renderPlot({popForecast(cnty())})
  output$RplotPop=renderPlot({popForecastReg(input$Rnum)})
  output$plotG=renderPlot({growth(cnty())})
  output$RplotG=renderPlot({growthReg(input$Rnum)})
#   output$tableG=renderDataTable({growth(cnty())$data%>%select(year, data, GrowthRate)})
#   output$RtableG=renderDataTable({growthReg(cnty())$data%>%select(year, data, GrowthRate)})
  
})
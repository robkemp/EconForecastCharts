library(shiny)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(car)
library(dplyr)
library(readr)


c=read_excel("FIPSandRegion.xls")%>%
  rename(regionnum=PMRegion)%>%
  mutate(countyfips=as.numeric(Fips))%>%
  filter(countyfips!=1 , countyfips!=0 , countyfips!=5 ,countyfips!=13 ,countyfips!=14 ,countyfips!=31 ,countyfips!=35 ,countyfips!=59)

r=read_csv("totalJobsReg_v14.csv")%>%
  filter(year==2013)%>%
  select(regionnumber)

dashboardPage(
  dashboardHeader(title= "SDO Forecast Review"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Region", tabName = "region", icon = icon("signal", lib = "glyphicon")),
      menuItem("County", tabName = "county", icon = icon("signal", lib = "glyphicon"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "region",
              
              fluidRow(box(selectInput("Rnum","Region Number:",
                                       choices=r$regionnum)))
              ,
              fluidRow(box(plotOutput("Rplot", height = 250)),
                       box(plotOutput("RplotPop", height = 250)))
      )),
    tabItems(
      tabItem(tabName = "county",
              
              fluidRow(box(selectInput("county","County:",
                                       choices=unique(c$Name))))
              ,
              fluidRow(box(plotOutput("plot", height = 250)),
                       box(plotOutput("plotPop", height = 250)))
      )
    )
  )
)


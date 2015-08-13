library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(readr)

c=read_csv("totalJobs_v14.csv")%>%
  filter(year==2013)%>%
  select(countyfips)
r=read_csv("totalJobsReg_v14.csv")%>%
  filter(year==2013)%>%
  select(regionnumber)

dashboardPage(
  dashboardHeader(title= "Estimate Review"),
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
              fluidRow(box(plotOutput("Rplot", height = 250)))
      )),
    tabItems(
      tabItem(tabName = "county",
              
              fluidRow(box(selectInput("num","County Number:",
                                       choices=c$countyfips)))
              ,
              fluidRow(box(plotOutput("plot", height = 250)))
      )
    )
    )
  )


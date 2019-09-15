##################################################################
## Name: John Fields
## Class: IST707 - Dr. Ami Gates
## Assignment: Homework #2
## Date: 18 Jul 2019
##################################################################
##------------------------------------------------------------------
# PROCESS & TRANSFORM DATA 
##------------------------------------------------------------------
##install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

## Set the working directory to the path where the code and datafile are located
setwd("/Users/johnfields/Library/Mobile Documents/com~apple~CloudDocs/Syracuse/IST707/Homework/Week 2/")

## Read in .csv data
filename="datastoryteller.csv"
MyStoryData <- read.csv(filename, header = TRUE, na.strings = "NA")

## Look at the data as a data frame
## I changed Section from int to factor but received errors in some 
## of the plots and changed it back to int
MyStoryData
str(MyStoryData)
#View(MyStoryData)

## Read in data again after spaces removed, column names changed
filename="datastorytellerFIXED.csv"
MyStoryData <- read.csv(filename, header = TRUE, na.strings = "NA")

## Look at the data as a data frame
(head(MyStoryData))

## Check for missing values
Total <-sum(is.na(MyStoryData))
cat("The number of missing values in StoryTeller data is ", Total )

## Check each numerical variable to see that it is >=0 
for(varname in names(MyStoryData)){
## Only check numeric variables
  if(sapply(MyStoryData[varname], is.numeric)){
    cat("\n", varname, " is numeric\n")
    ## Get median
    (Themedian <- sapply(MyStoryData[varname],FUN=median))
    ##print(Themedian)
    ## check/replace if the values are <=0 
    MyStoryData[varname] <- replace(MyStoryData[varname], MyStoryData[varname] < 0, Themedian)
  }
  
}

(MyStoryData)

## Use table to explore the data
(table(MyStoryData$School))

## Use loop to create all the tables at once
for(i in 1:ncol(MyStoryData)){
  print(table(MyStoryData[i]))
}

(colnames(MyStoryData))
(head(MyStoryData))
(MyStoryData)

## Which variables contain information?  All except Section (identifier) 
## and VeryAhead (no completions or bad data)
## Does the Section? The E school likley has bad data since this school 
## has 1 section with over 100 students

## The table shows us that we have 5 schools but only 2 have much data 
## (School A & B)
## This is important because these larger schools can shift the data 
## since they have greater number of students

## Are there outliers or odd values?
## School E has 1 section with 116 students

## The structure (types) of the data
(str(MyStoryData))

## Summary of the data - mean median, sums
summary(MyStoryData)

library(plyr)

## The following will sum all rows for each "School" and per variable in the data
## Save this new aggregated result as a DF
SumBySchoolDF <- ddply(MyStoryData, "School", numcolwise(sum))
(SumBySchoolDF)

## Total the number of students for A - E and sum the columns for each row

(SumBySchoolDF)

SumOfStudents <- rowSums(SumBySchoolDF[,c("Average", "Behind", 
                                          "MoreBehind","VeryBehind","Completed")])

##Create new dataframes for visualizations and calculations
##Calculate the number of students expected to complete (Complete & Average) and 
##behind (Behind, More Behind, Very Behind)
(SumOfBehind <- rowSums(SumBySchoolDF[,c("Behind","MoreBehind","VeryBehind")]))
(SumOfCompletedAverage <- rowSums(SumBySchoolDF[,c("Average","Completed")]))
(SumByBehindCompleteDF <- data.frame(SumBySchoolDF$School,SumOfBehind,SumOfCompletedAverage))
(SumByBehindCompleteDF$Total <- SumOfBehind+SumOfCompletedAverage)
(SumByBehindCompleteDF$PercentCompleteBySchool <- (SumByBehindCompleteDF$SumOfCompletedAverage/SumByBehindCompleteDF$Total))
(TotalBehindBySchool <- (SumBySchoolDF$Behind+SumBySchoolDF$MoreBehind+SumBySchoolDF$VeryBehind))
(TotalBehind <- sum(TotalBehindBySchool))
(TotalCompleteAverageBySchool <- (SumBySchoolDF$Completed+SumBySchoolDF$Average))
(TotalCompleteAverage <- sum(TotalCompleteAverageBySchool))
(CompleteBehind <- data.frame(Group=c("Total Behind", "Total Complete/Average"),Students = c(TotalBehind,TotalCompleteAverage)))
(TotalStudents <- sum(CompleteBehind[1,2]+CompleteBehind[2,2]))

##------------------------------------------------------------------
# VISUALIZATIONS
##------------------------------------------------------------------

##Plot a comparison of the number of sections by school size
BaseGraph <- ggplot(MyStoryData)
(SchoolSizebySection<-BaseGraph + geom_bar(aes(SchoolSize, fill = SchoolSize)) + ylab("Number of Sections"))

##Plot percentage of Complete & Average / Total Students
ggplot(SumByBehindCompleteDF, aes(x=SumBySchoolDF.School,y=PercentCompleteBySchool,fill=Total))+
geom_bar(width=1,stat="identity") + ylab("Percent Expected to Complete") + xlab("School")

##Plot students per section by school and # of students
StudentsPerSection <- data.frame(SumBySchoolDF$School,SumOfStudents/SumBySchoolDF$Section)
ggplot(StudentsPerSection, aes(x=SumBySchoolDF.School,y=SumOfStudents.SumBySchoolDF.Section,fill=SumOfStudents))+
  geom_bar(width=1,stat="identity") + ylab("Students Per Section") + xlab("School")

##Determine the total number of students completed vs behind

##Visually compare the total likely to complete (Complete+Average) 
##and not likely to complete (behind, more behind, very behind)
##The bar and pie graph code is based on examples from sthda.com
(bp <- ggplot(CompleteBehind, aes(x="",y=Students,fill=Group))+
  geom_bar(width=1,stat="identity"))

(pie <- bp + coord_polar("y", start=0))

## Use visual EDA - boxplots
## What does this tell us? There is variability from school to school and 
##section to section in the completion rates
ggplot(stack(MyStoryData), aes(x = ind, y = values, color=ind)) +
  geom_boxplot()

## School A Boxplot
MyStoryData$School =="A"

JustSchoolA<-subset(MyStoryData, School == "A" )
(JustSchoolA)
(str(JustSchoolA))

## Change Section to a factor
JustSchoolA$Section<-as.factor(JustSchoolA$Section)

ggplot(JustSchoolA, aes(x = Section, y = Completed, color=Section)) +
  geom_boxplot() + ggtitle("School A Boxplot") + ylab("# Students Completed")

## School B Boxplot
MyStoryData$School =="B"

JustSchoolB<-subset(MyStoryData, School == "B" )
(JustSchoolB)
(str(JustSchoolB))

## Change Section to a factor
JustSchoolB$Section<-as.factor(JustSchoolB$Section)

ggplot(JustSchoolB, aes(x = Section, y = Completed, color=Section)) +
  geom_boxplot() + ggtitle("School B Boxplot") + ylab("# Students Completed")

## School C Boxplot
MyStoryData$School =="C"

JustSchoolC<-subset(MyStoryData, School == "C" )
(JustSchoolC)
(str(JustSchoolC))

## Change Section to a factor
JustSchoolC$Section<-as.factor(JustSchoolC$Section)

ggplot(JustSchoolC, aes(x = Section, y = Completed, color=Section)) +
  geom_boxplot() + ggtitle("School C Boxplot") + ylab("# Students Completed")

## School D Boxplot
MyStoryData$School =="D"

JustSchoolD<-subset(MyStoryData, School == "D" )
(JustSchoolD)
(str(JustSchoolD))

## Change Section to a factor
JustSchoolD$Section<-as.factor(JustSchoolD$Section)

ggplot(JustSchoolD, aes(x = Section, y = Completed, color=Section)) +
  geom_boxplot() + ggtitle("School D Boxplot") + ylab("# Students Completed")

## School E Boxplot
MyStoryData$School =="E"

JustSchoolE<-subset(MyStoryData, School == "E" )
(JustSchoolE)
(str(JustSchoolE))

## Change Section to a factor
JustSchoolE$Section<-as.factor(JustSchoolE$Section)

ggplot(JustSchoolE, aes(x = Section, y = Completed, color=Section)) +
  geom_boxplot() + ggtitle("School E Boxplot") + ylab("# Students Completed")

## The overall analysis goal is to understand the completion rate of students

# There was an error so changed to SumBySchoolDF$School from StudentsSumPerSchool$School 
# Original code TotalPerSchool <- data.frame("School" = StudentsSumPerSchool$School, 
#"Total" = SumOfStudents)
TotalPerSchool <- data.frame("School" = SumBySchoolDF$School, 
                             "Total" = SumOfStudents)

(TotalPerSchool)
plot(TotalPerSchool$School,TotalPerSchool$Total,xlab="School",ylab="Total Students")

## This plot shows the % completion rate by school
ggplot(SumBySchoolDF, aes(x=School,y=Completed/TotalPerSchool$Total,fill=SumOfStudents))+
  geom_bar(width=1,stat="identity") + ylab("Percent Complete") + xlab("School")

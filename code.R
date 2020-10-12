library(plyr)

getwd()

setwd("D://project//big data//project//project")

data <- read.csv("dataSet.csv")

first_backup <- data

second_backup = first_backup

#max of 2 ca

second_backup$max1 <- pmax(first_backup$ca1,first_backup$ca2)

second_backup$max2 <- pmax(second_backup$max1,first_backup$ca3)

second_backup$ca1=NULL

second_backup$ca2=NULL

second_backup$ca3=NULL

second_backup$Mjob=NULL

second_backup$Fjob=NULL

# aggregating data of same row

newData <- ddply(second_backup, .(Student.Name,Registeration.Number), summarize,
          
           max1=paste(max1,collapse=","), 
          
           max2=paste(max2,collapse=",") ,
           
            mte= paste(mte,collapse=","),
           
            ete=paste(ete,collapse=","),
           
            Coursetype = paste(Coursetype,collapse = ","))

Course = newData$Coursetype

REGNO = newData$Registeration.Number

# dataset for cgpa calculation
second_backup$Registeration.Number=NULL

second_backup$Coursetype=NULL

CGPA = aggregate(. ~ Student.Name, second_backup, sum)

CGPA$Coursetype = Course[1:225]

CGPA$REGNO = REGNO[1:225]

#CGPA calculation subject wise

FINAL = (second_backup$mte * (20/40) + second_backup$ete * (50/70) + second_backup$att * (5/100) + (second_backup$max1 + second_backup$max2) * (25/60)) / 10

# Adding cgpa in our dataset

second_backup$CGPA = FINAL  

#adding courseType

second_backup$course = first_backup$Coursetype

# creating dataset for focus group students

focus_group = second_backup[second_backup$CGPA >= 6 & second_backup$course=="CR",]

# counting number of students in focus group

numberOfStudentsInFocusGroup = nrow(focus_group)

# finding toppers data and CGPA

topper = focus_group[focus_group$CGPA == max(focus_group$CGPA), ]

print(topper)

#changing registration number(integer) into character for finding 1st,2nd,3rd,4rth year and passed out students

CGPA$Regnum = as.character(REGNO[1:225])

# Dataset for different year students

passedOut = CGPA[substr(CGPA$Regnum,3,3)=='2'|substr(CGPA$Regnum,3,3)=='3',]

firstYear = CGPA[substr(CGPA$Regnum,3,3)=='7',]

secondYear = CGPA[substr(CGPA$Regnum,3,3)=='6',]

thirdYear = CGPA[substr(CGPA$Regnum,3,3)=='5',]

fourthYear = CGPA[substr(CGPA$Regnum,3,3)=='4',]


#calculating number of students in 1st,2nd,3rd,4rth year and passed out

numberOfPassedOut =  nrow(passedOut)

numberOfFirstYear =  nrow(firstYear)

numberOfSecondYear = nrow(secondYear)

numberOfThirdYear = nrow(thirdYear)

numberOfFourthOut = nrow(fourthYear)

#changing factor variable into character for aggregating data according

# to studentNames and courseType

second_backup$Student.Name = as.character(second_backup$Student.Name)

second_backup$regno = first_backup$Registeration.Number


# Aggregating data according to name,regno,course... 

studCourse = aggregate(.~Student.Name+regno+course , second_backup,sum)

# Finding best students in different courseTypes

best_In_CR = studCourse[studCourse$CGPA == max((studCourse[studCourse$course=="CR",])$CGPA),]

best_In_SP = studCourse[studCourse$CGPA == max((studCourse[studCourse$course=="SP",])$CGPA),]

best_In_OM = studCourse[studCourse$CGPA == max((studCourse[studCourse$course=="OM",])$CGPA),]

best_In_OE = studCourse[studCourse$CGPA == max((studCourse[studCourse$course=="OE",])$CGPA),]

best_In_PE = studCourse[studCourse$CGPA == max((studCourse[studCourse$course=="PE",])$CGPA),]


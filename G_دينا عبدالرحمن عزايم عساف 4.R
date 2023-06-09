# Reading and Viewing  of dataset:-

setwd("F:/د.حمادة/عملى/labs")
getwd()
howell<-read.csv("G4_howell.csv",na.strings = c(""))
View(howell)


#<------------------------------------ Re-coding Columns:-
#1) Re-coding the sex column values(F<- Female , M<-Male):

howell$new_sex[howell$sex=='M']='Male'
howell$new_sex[howell$sex=='F']='Female'
View(howell)


howell$new_sex2[howell$sex=='M']=1
howell$new_sex2[howell$sex=='F']=0
View(howell)

#-----------------------------------------

#2) make some conditions in age column:

howell$ageCats[howell$age > 50 ]='Older'
howell$ageCats[howell$age < 50 & howell$age > 10]='Young'
howell$ageCats[howell$age < 10 & howell$age > 3]='Child'
View(howell)

#-----------------------------------------

#3)making slicing in data:
#get first/last N rows:

head(howell, 10)
tail(howell, 10)
View(howell)

#-----------------------------------------

#4)Remove some columns from dataset:

newhowell1=head(howell[ ,- c(2,3) ])
newhowell1

#-----------------------------------------

#5)Get a specific column.

age_info <- head(howell[ , 2])
age_info

#-----------------------------------------

#6) delete the col height:

howell$Overweight<-NULL
View(howell)

#-----------------------------------------

#7) make some conditions in weight column:

howell$weightCats[howell$weight > 45 ]='heavy'
howell$weightCats[howell$weight < 9 & howell$weight < 30]='small'
howell$weightCats[howell$weight > 30& howell$weight <45 ]='semi heavy'
View(howell)

#-----------------------------------------

#8) make some conditions in weight column:

howell$weightCats[howell$weight > x ]='heavy'
howell$weightCats[howell$weight < x]='small'
View(howell)

#-----------------------------------------

#9)filter the sex col

filter1 <- howell[howell$sex =="M", ]
filter1
View(howell)

#-----------------------------------------

#10) do some operations (mean, median) in col

h_mean<-mean(howell$height)
h_mean

h_med<-median(howell$height)
h_med
#-----------------------------------------






#<-------------------------- Dealing with missing values:-
#11) Find all rows contain NAs:

howell[!complete.cases(howell) , ]

#-----------------------------------------

#12) To known about the  types of variables:

str(howell)
summary(howell)

#-----------------------------------------







#<--------------------------Data Cleaning:-
#13) Remove text (kg) from Weight column:

howell$weight<-gsub("kg","",howell$weight)
View(howell)
howell$weight

#-----------------------------------------

#14)Replace each NA in weight according to the median of
#weight to all males for males and weight to all females for females

howell$weight<-as.numeric(howell$weight)
class(howell$weight)
View(howell)

median(howell$weight)
median((howell[ ,"weight"]),na.rm = T )
famele_med <- median(howell[ howell$sex =="F", "weight"], na.rm = T)
howell[is.na(howell$weight) & howell$sex =="F", "weight"] <-famele_med

median((howell[ ,"weight"]),na.rm = T )
male_med <- median(howell[ howell$sex =="M", "weight"], na.rm = T)
howell[is.na(howell$weight) & howell$sex =="M", "weight"] <-male_med
View(howell)

#-----------------------------------------

#15)using if-else in height col:
x<-mean(howell$height)
howell$new_height<-as.factor(ifelse(howell$height>x,"heightest","smallest"))

View(howell)

#-----------------------------------------






#<----------------------------------- Making visualization:-

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)


#16)display the effect of the Age (co_relation) using plot,name the drawing image

plot1<-ggplot(howell , aes(x=sex  , y= age))
plot1 + geom_point() + ggtitle("The co_relation between the Sex and Age")

#-----------------------------------------

#17)display the effect of height on foot_length colored by the groups of age range using scatter plot
plot2<-ggplot(howell , aes(new_sex2 , height))
plot2 + geom_point(aes(color=age)) +stat_smooth(se=FALSE)  

#-----------------------------------------

#18)Show the distribution of age using histogram,name the plot image and rename the x,y
plot3<-ggplot(howell , aes(age))
plot3 + geom_histogram(binwidth = 6)
plot3 + geom_histogram(fill = "black")+ ggtitle("The Distribution of Age")
+labs(x="Age" , y="Frequency")

#-----------------------------------------

#19)Show the distribution of Height using histogram ,name of plot
plot4<-ggplot(howell , aes(height))
plot4 + geom_histogram(binwidth = 8)
plot4 + geom_histogram(fill = "red")+ ggtitle("Child's Height distribution") 

#-----------------------------------------

#20)Show the distribution of Height using histogram ,name of plot
plot5<-ggplot(howell , aes(x=sex, fill=age))
plot5 +geom_bar()+theme_light()+labs(y="human count",title = "Sex of people")

#rm(list = ls())
library(data.table)
library(dplyr) 
library(ggplot2)
library(rcompanion)
library(mixtools)

Boston_2017 <- read.csv("C:/Users/Salem/Downloads/employee-earnings-report-2017.csv") #Dataset for the salaries of Boston from the 2017 year
View(Boston_2017) #Output for the Boston 2017 salary
Boston_2016 <- read.csv("C:/Users/Salem/Downloads/employee-earnings-report-2016.csv")#Dataset for the salaries of Boston from the 2016 year
View(Boston_2016)#Output for the Boston 2016 salary
Boston_2012 <- read.csv("C:/Users/Salem/Downloads/employee-earnings-report-2012.csv")#Dataset for the salaries of Boston from the 2012 year
View(Boston_2012)#Output for the Boston 2012 salary
Boston_2011 <- read.csv("C:/Users/Salem/Downloads/employee-earnings-report-2011.csv")#Dataset for the salaries of Boston from the 2011 year
View(Boston_2011)#Output for the Boston 2011 salary

colnames(Boston_2017) #Column names in the Boston 2017 salary
colnames(Boston_2016) <- colnames(Boston_2017) #Column names in the Boston 2016 salary equaled to the column names in the 2017 salary
colnames(Boston_2012) <- colnames(Boston_2017)#Column names in the Boston 2012 salary equaled to the column names in the 2017 salary
colnames(Boston_2011) <- colnames(Boston_2017)#Column names in the Boston 2011 salary equaled to the column names in the 2017 salary

Boston_2017$year <- 2017 #Initialized to the year of 2017
Boston_2016$year <- 2016  #Initialized to the year of 2016
Boston_2012$year <- 2012  #Initialized to the year of 2012
Boston_2011$year <- 2011  #Initialized to the year of 2011

datacombination <- rbind(Boston_2017, Boston_2016, Boston_2012, Boston_2011) #Combining all of the datasets and storing it into datacombination vector
head(datacombination) #Output of the vector
##rm(Boston_2017, Boston_2016, Boston_2012, Boston_2011) #Removed all of these four datasets
colnames(datacombination) <- tolower(colnames(datacombination)) #Column names in this vectors are being lowercased

datacombination =
  datacombination%>%
  select(year, name, department.name, title, regular, total.earnings, overtime)%>% #Focusing on these column names from the dataset
  mutate(regular = as.character(regular), #Making the regular variable a number
         regular = gsub("\\$","", regular), #Replacing the dollar sign with blank space
         regular = gsub(",","", regular),#Replacing the comma with a blank space
         regular = as.numeric(regular), #Changed regular to a number
         total.earnings2 = as.character(total.earnings), 
         total.earnings2 = gsub("\\$","", total.earnings2), 
         total.earnings2 = gsub(",","", total.earnings2),
         total.earnings2 = as.numeric(total.earnings2),
         overtime = as.character(overtime),
         overtime = gsub("\\$","", overtime), 
         overtime = gsub(",","", overtime),
         overtime = as.numeric(overtime))

datacombination2 <- arrange(datacombination, name, year) #The name and year column are arranged
head(datacombination2) #Output displayed
datacombination2$id = as.numeric(as.factor(datacombination2$name)) #Assigning each of the employees name with an id

salaries = lm(data = datacombination2, regular~year) #linear model showing the relationship between the variables, regular and year
summary(salaries) #Shows a summary of the linear model
confint(salaries)#Got the confidence interval

ggplot(datacombination2) +
  geom_histogram(aes(x = regular)) + #Plots the histograms for the four years showing how many people made a certain amount of money
  facet_wrap("year")

sample.id <- sample(x = 1:32000, 300)
ggplot(subset(datacombination2, id %in% sample.id)) + #Shows the salaries thoroughout the years for each of the 300 employees
  geom_line(aes(y = regular, x = year, group = id))

ggplot(datacombination2) +
  geom_histogram(aes(x = overtime)) + #Plots the histograms for the four years showing how many people made a certain amount of overtime pay
  facet_wrap("year")

sample.id <- sample(x = 1:32000, 300)
ggplot(subset(datacombination2, id %in% sample.id)) + #Shows the overtime pay thoroughout the years for each of the 300 employees
  geom_line(aes(y = overtime, x = year, group = id))

salaries2 = lm(data = datacombination2, overtime~year) #linear model showing the relationship between the variables, overtime and year
summary(salaries2) #Shows a summary of the linear model
confint(salaries2) #Got the confidence interval

datacombination2%>%
  group_by(id)%>%
  mutate(overtimepct = sum(overtime)/sum(regular))%>% #Getting the ratio of overtime:regular
  ungroup()%>%
  dcast(id + overtimepct~year, value.var = "regular", fun.aggregate = mean)%>%
  mutate(change = `2017` - `2011`)->salarychange #Taking the difference of the salary of 2011 and the salary of 2017 for the employees
head(salarychange)
ggplot(salarychange) +
  geom_histogram(aes(x = change)) # Histogram showing the amount of people who had a specific change of salary

mean(salarychange$change/6, na.rm = TRUE) #Averages for the increases yearly throughout six years
table(salarychange$change > 0)%>%prop.table #Table showing the difference of salaries in 2011 and 2017 for the employees
boxplot(salarychange$change) #Box and whiskers plot for the change in salary

datacombination2%>%
  group_by(title)%>%
  summarize(m = mean(regular, na.rm = F),s = sd(regular, na.rm = F),n = n())%>% #
  arrange(desc(m),desc(n)) #Prints out a table showing the title of the job positions and their average salaries in decreasing order

plot(data = salarychange,change~overtimepct, xlim = c(0,2)) #Plots overtimepct and change
model1 = lm(data = salarychange, change~overtimepct) #Shows a linear model of the relationship between the variables of change and overtimepct         
abline(model1,col = "red") #red line gets draw across to show relationship between two variables         
summary(model1) #Summary of this model

## Rename variables as needed
names(Boston_2011)[c(4,11)] = c("Regular2011","Total2011")
names(Boston_2017) = names(Boston_2011)
names(Boston_2017)[c(4,11)] = c("Regular2017","Total2017")

## Convert some variables to numeric
cleanup = function(x)
{
  x = as.character(x)
  x = gsub("$","",x,fixed=TRUE)
  x = gsub(",","",x,fixed=TRUE)
  suppressWarnings(as.numeric(x))
}
Boston_2011[,"Regular2011"] <- cleanup(Boston_2011[,"Regular2011"])
Boston_2011[,"Total2011"] <- cleanup(Boston_2011[,"Total2011"])
Boston_2017[,"Regular2017"] <- cleanup(Boston_2017[,"Regular2017"])
Boston_2017[,"Total2017"] <- cleanup(Boston_2017[,"Total2017"])

## Merge the data frames
Boston2011_17 <- merge(Boston_2011,Boston_2017, by=c("NAME","DEPARTMENT.NAME"))
                         
## Attach the data frame
attach(Boston2011_17)
d = na.omit(Regular2017 - Regular2011)
d
n = length(d)
n
alpha = .01
dbr = mean(d)
dbr
s = sd(d)
s
z = dbr/sqrt(s^2/n)
z
zc = qnorm(1-alpha/2)
zc
abs(z) > zc
hist(d, breaks= 30, col="lightblue", xlab = "Salary change", ylab = "Number of employees")
## Add vertical line x = 0 to show separation 
## between people whose salaries increases and 
## those whose salaries decreased 
abline(v=0, lwd=1.5, lty=2) 

## Probability modeling
sort(table(Boston_2017[,"TITLE"]),decreasing=T)[1:10]
y <- Boston_2017[Boston_2017$TITLE == "Teacher", "Regular2017"]#, !is.na("Regular2017")]
hist(y, freq=FALSE,col="lightblue",main="Distribution of Salaries\n For Teachers (2017)",breaks=40, ylim=c(0,3.2e-5))
x <- read.csv("C:/Users/SALEM/Downloads/employee-earnings-report-2017.csv")
x <- x$'TOTAL.EARNINGS'
x <- gsub("$", "",x,fixed = TRUE)
x <- gsub(",","",x,fixed = TRUE) 
x <-  as.numeric(x)
test <- normalmixEM(x,k=3)
test
muhat= c(2e4,7e4,1e5)
sighat = c(1e4,1e4,1e3)
lamhat = c(.12,.42,.46)
f = function(x) lamhat[1] * dnorm(x,muhat[1],sighat[1]) + 
  lamhat[2] * dnorm(x,muhat[2],sighat[2]) + 
  lamhat[3] * dnorm(x,muhat[3], sighat[3])

curve(f, add = TRUE, col="red", n)







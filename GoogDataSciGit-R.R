##To github: 
#edit_r_environ()
#GitHub_Pat = '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'
library(usethis)
use_git_config(user.name = "Damien Augustin", user.email = "idamiena@gmail.com")

#to block a full run...
.f = function()  {


#install.packages('palmerpenguins')
library('palmerpenguins')

#install.packages('ggplot2')
library('ggplot2')

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) + geom_point()

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) + geom_point(aes(color=species))

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) + geom_point(aes(shape=species,color=species)) + facet_wrap(~species)

ggplot(data=penguins,aes(x=flipper_length_mm,y=body_mass_g)) + geom_point(aes(shape=species,color=species)) + facet_wrap(~species) + 
  labs(title="Palmer Penguins: Body Mass vs. Flipper Length")


#View Data
head('palmerpenguins')
rownames(penguins)
colnames(penguins)
View(penguins)
penguins[42,]  #show row 42  or the value name in ["Adelie",]
penguins["Adelie",] 
penguins[29:34,]
str(penguins)

## Now R
library(palmerpenguins)
data(package = "palmerpenguins")
summary(penguins)
View(penguins)

## the tidyverse is a collection of packages in R with a common design philosophy for data manipulation, exploration, and visualization. 
install.packages('tidyverse')
library(tidyverse)
library(lubridate)
#########################################################

install.packages()
library("tidyverse")

######variables
first_variable <- "A String Variable"
second_variable <- 12.5
##################

###Vecotors
vec_1 <- c(12,420,69.69,2,101,42)
vec_1
#######

####Pipe
data("ToothGrowth")
View("ToothGrowth")
install.packages('dplry')
##piping into a function - The argument you want to pass the LHS to is represented by a placeholder "."
colnames(penguins)
penguins  %>%    
  lm (body_mass_g ~ flipper_length_mm, data =.)
##    lm (body_mass_g ~ flipper_length_mm, data = penguins)

###stores dataframe as flitered_tg from ToothGrowh df, where dose equals 0.5)
filtered_tg <- filter(ToothGrowth,dose==0.5)
View(filtered_tg)

###sorts (dataframe,field)
arrange(filtered_tg,len)

#see a functions arguments & help
# args(function)
args(round)
?round()


#nested function...next we will pipe it!
arrange(filter(ToothGrowth,dose==0.5),len)

filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose==0.5) %>% 
  arrange(len)

View(filtered_toothgrowth)

filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose==0.5) %>%
  group_by(supp) %>% 
  summarize(mean_len = mean(len,na.rm = T),.group="drop")

View(filtered_toothgrowth)
#######

install.packages("tidyverse")
library(ggplot2)
data("diamonds")
View(diamonds)
head(diamonds)  ###view 6 rows
str(diamonds)   ###see dataframe structure
colnames(diamonds)  ### see col names
library(tidyverse)
mutate(diamonds, carat_2=carat*100) ### add fields probably a bunch of other shit

##Create a data frame
names<-c("Damien","Jordan","Sara","Melvin")  ##vector
age<-c(40,25,19,20)
people <- data.frame(names,age)  ##creates dataframe people
glimpse(people)
##now a tibble
as_tibble(diamonds)

##  parse a column (rate) into two with a delimiter "/"
table3 %>% 
  separate(col = rate, into = c("cases", "population"), 
           sep = "/", convert = TRUE)
## combine columns (century and year) into new "year" column
table5 %>% 
  unite(col = "year", century, year, sep = "")

## joins
band_members %>% 
  left_join(band_instruments, by = "name")
band_members %>% 
  inner_join(band_instruments, by = "name")
band_members %>% 
  full_join(band_instruments, by = "name")
###"search" join (filtering join) reduce the left table to what matches in the right, import no data from the right
band_members %>% 
  semi_join(band_instruments, by = "name")
  #anti_join to find rows is the left, not matching the right
band_members %>% 
  anti_join(band_instruments, by = "name")

###Vectors and Lists
###Lists are a recursive type of vector, but they don't smash into all functions nicely
pluck("x")
typeof(x)  #returns the atomic type : double, integer logical, character, complex, and raw


###Data Cleaning tasks 3 packages
install.packages("here")
install.packages("skimr")
install.packages("janitor")
library("here")
library("skimr")
library("janitor")

skim_without_charts(penguins)
glimpse(penguins)

penguins %>% 
#  select(species)
  select(-species)  #or without

penguins %>%     #janitor package  rename column names
  rename(island_new=island) %>% 
  rename_with(tolower) %>% 
  clean_names()  #numbers characters and underscores

####Organize and filter out data
## arrage()  groupby()    filter()
penguins2 <- penguins %>% 
  arrange(-bill_length_mm)
View(penguins)
penguins %>% group_by(island) %>% drop_na() %>% summarize(mean_bill_length_mm = mean(bill_length_mm))

penguins %>% group_by(island) %>% drop_na() %>% summarize(mean_bill_length_mm = max(bill_length_mm))

penguins %>% group_by(species, island) %>% drop_na() %>% summarize(max_bill = max(bill_length_mm), mean_bill = mean(bill_length_mm))

penguins %>% filter(species =="Adelie") %>% drop_na()


###manually create a data frame.  3 lists, then a assemble into a data frame
id <- c(1:5)
full_name <- c("mike johnson", "Damien Augustin", "Robert Sledge", "dArren Jessie", "Ben Folds")
job_title <- c("Producer", "Data Analyst", "Bassist", "Drummer", "Lead Singer")
employees <- data.frame(id,full_name,job_title)
View(employees)
#####Fixing these full_names to PROPER is perhaps for another time or Python...
##Separate column (split column) and unite columns (combine columns)
separate(df, colname, into = c('first_name','last_name'), sep=' ')
unite(df,'newcolname',col1,col2, sep=' ')


as.tibble(data)
path_to_file("penguins_raw.csv")    #base R path to file  get file path
penguins <- read.csv(path_to_file("penguins.csv"))

#####rm all objects in R environment delete all objects in global environtment
rm(list = ls(all.names = TRUE))


#Anscombe's quartet exercise
install.packages('Tmisc')
library(Tmisc)
data(quartet)
View(quartet)
library(tidyverse) 

quartet %>% 
    group_by(set) %>% 
    summarize(mean(x),sd(x),mean(y),sd(y),cor(x,y))

#Using R functions to check for Bias
#This is a good way to compare actual results to predicted results... shows the average adjustment to the prediction to bring it in line
install.packages("SimDesign")
library(SimDesign)

actual_t <- c(68, 70, 72, 71,67,70)
pred_t_low <- c(67,69,71,70, 66,59)
pred_t_high <- c(69,71,73,72,68,81)

bias(actual_t, pred_t_low)  #positive number, eg the necessary fudge factor
bias(actual_t, pred_t_high) #neg num.


##################################################################################################################
#ggplot2: data visualizations
#ploty - wide rang of solutions #lattice #RGL #Dygraphs #Leaflet #Highcharter #Patchwork #gganimate #ggridges
library(palmerpenguins)
library(tidyverse)
penguins <- read.csv("penguins.csv")

#x,y related data plots
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_point() + geom_smooth()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_bin_2d()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_density_2d_filled()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_line()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_tile()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_smooth()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_polygon()

ggplot(data = penguins) +
    geom_point(mapping = aes(x  =flipper_length_mm, y = body_mass_g),color="red")

ggplot(data = penguins) +
  geom_point(mapping = aes(x  =flipper_length_mm, y = body_mass_g, color=species, shape = species , size = species)) 

ggplot(data = penguins) +
  geom_point(mapping = aes(x  =flipper_length_mm, y = body_mass_g, color=species, shape = species , alpha = species)) 

ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g, linetype=species, color=species)) +  geom_smooth()
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g, linetype=species, color=species)) +  geom_smooth() +
    labs(title = "Palmer Penguins", x = "Flipper Length (mm)", y = "Body Mass (g)")

ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g, linetype=species, color=species)) +  geom_point() 
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g, linetype=species, color=species)) +  geom_jitter() 



##################################################################################################################
##################################################################################################################
##################################################################################################################

#Now lets try a time series project from youtube to energy in germany

en_data <- read.csv("opsd_germany_daily.txt", header = TRUE,row.names = "Date")
en_data[c("2015-06-30","2015-07-04"),]
summary(en_data)

#now lets bring it in without using read_date as row names
en_data2 <- read.csv("opsd_germany_daily.txt", header = TRUE)
#see attributes of Date column
str(en_data2)
str(en_data2$Date)

#rename key col
names(en_data2)[1] <- 'read_date'

#notice it is in the char format?  why not mutate?
library(tidyverse)


en_data2 <-
  en_data2 %>% mutate(read_date = as.Date(read_date))
str(en_data2$read_date)

#lets create a Year Column
en_data2 <-
  en_data2 %>% mutate(Year =  as.numeric(format(read_date,"%Y")))  #Yes %Y should be uppercase like Ymd
tail(en_data2)
en_data2 <-
  en_data2 %>% mutate(month =  as.numeric(format(read_date,"%m")))  #m is lowercase
#fix capital Y in Year to lower case!
en_data2<- 
  en_data2 %>% rename_with(tolower,.cols="Year")  

#Add a column of a datetime component another way: 1) strip the dataframe's column into a list
german_dates <- c(en_data2$read_date)
#format the list for the day
day <- as.numeric(format(german_dates,'%d'))
# use cbind to append the list "day" back onto df
en_data2 <- cbind(en_data2,day)           #column bind  - cbind()
head(en_data2)

#now how about we try to roll this into a function SplitDateIntoYmdCols
#lets also use Roxygen to make it autocomplete callable and documented.
# https://www.youtube.com/watch?v=zvLcEndh0yM

SplitDateIntoYmdCols <- function(df,DateCol) {
  #Arg Datecol from the dataframe df used to append year month date columns
  #eg "2022-01-05"  --> Cbind(c(Year = 2022, month = 01, day = 05))
  
  df <-
    df %>% mutate(DateCol2xyz = as.Date(df[, DateCol]))   #Datecol2xyz is a hardcode column, no way
                                                        #it seems to pass the argument and abstract it
  df <-
    df %>% mutate(Year =  as.numeric(format(DateCol2xyz,"%Y")))
  df <-
    df %>% mutate(month =  as.numeric(format(DateCol2xyz,"%m")))
  df <-
    df %>% mutate(day =  as.numeric(format(DateCol2xyz,"%d")))
 
  #drop column DateCol2xyz (hardcoded in function by the first mutate, mutate not a good function)
  df <- subset(df, select = -c(DateCol2xyz))
                                                    }
#Test the function I just wrote
en_data3 <- SplitDateIntoYmdCols(en_data2,"read_date")
str(en_data3)

##################################################################################################
####Trying to pass variables into functions is not easy in R seem like it may vary across packages
var1 <- "read_date"
library(dplyr)
varName <- as.name(var1)
enquo_varName <- enquo(varName)

en_data3 <-
  en_data2 %>% mutate(newfieldname = as.Date(!!varName))  #don't think this one works either
                                                          #even with !!enquo_varName
#or
df <-
  df %>% mutate(DateCol2xyz = as.Date(df[, DateCol]))
#Then there is this "accross" method on stack overflow which i didn't seem to get to work but
#might come in handy so other strange place
my_summarise2 <- function(df, group_by_var) {
  df %>% group_by(across({{ group_by_var }})) %>% 
    summarise(mpg = mean(mpg))
}
################################################################################################

#Create a new record (insert record) as a data.frame (leaving some columns out) and see what happens when we append it to en_data
Year = c(2022); month = c(06); day = c(30);
data.frame(Year,month,day) -> xyz
en_data4 <- en_data3
en_data4 <- rbind(en_data4,xyz)  #baaaad "numbers of columns of arguments do not match!"

#or for 1 record, just insert a blank record and submit a compressed list insert record
str(en_data4)
en_data4[nrow(en_data4) +1,] = c(NA,NA,NA,NA,NA,2022,07,04)  #It won't accept: . or NULL
tail(en_data4)
#it might not like NA is first column
#Remove a record using negative compression -c: df[-c(row_index_1, row_index_2),]
#don't know row index number?  hows about an abstract reference with nrow?  (nrow counts number of rows)
en_data5 <- en_data4[-c(nrow(en_data4)),]    #don't forget that last comma befor the matrix bracket close!
tail(en_data5)

#Remove last 3 records!!!!  delete last records  last number of records delete range of records
en_data6 <- en_data5[-c((nrow(en_data5) -2):nrow(en_data5)),]
tail(en_data6)





rm(df_outtt)
rm(xyz,Year,month,day)
rm(en_data4)
rm(df2,df_as_date,en_data3,en_data_norow,en_data2)




}




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
library('palmerpenguins')
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

##################################################################################################################
##################################################################################################################
##################################################################################################################

#Now lets try 
















}


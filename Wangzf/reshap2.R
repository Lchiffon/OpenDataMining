#---------------------------------
read.table()        
read.csv()        
read.csv2()
read.delim()        
read.delim(sep="|") 
read.delim2()
read.fwf()          
#---------------------------------

#####Reshap
library(reshape2)
setwd("F:/Rworkd/DW/Intro-to-R/Day 2/data")
raw1 <- read.csv("pew.csv", header = T, sep=",")
head(raw1)
raw1 <- read.csv("pew.csv", check.names = F)
head(raw1)


###1.Values in column names
#----------------------------------------------------
melt()
melt(data, ..., na.rm = FALSE, value.name = "value")
#----------------------------------------------------
tidy <- melt(raw1, id = "religion")
head(tidy)
names(tidy) <- c("religion", "income", "n")
head(tidy)

tidy <- melt(raw1, id = "religion", variable.name ="income",
             value.name ="n")
head(tidy)


###2.Variable names in cells
raw2 <- read.delim("weather.txt", check.names = F, na.strings = ".")
head(raw2)

raw2 <- melt(raw2, id = c("year", "month", "element"), 
             variable.name = "day", na.rm = TRUE)
head(raw2)
raw2 <- raw2[, c("year", "month", "day", "element", "value")]
head(raw2)

tidy <- dcast(raw2, year + month + day ~ element, value.var = "value")
head(tidy)

#--------------------------------------------------------------
titanic2 <- read.csv("titanic2.csv", stringsAsFactors = FALSE)
head(titanic2)

tidy <- melt(titanic2, id = c("class", "age", "fate"), variable.name = "gender")
head(tidy)
tidy <- dcast(tidy, class + age + gender ~ fate, value.var = "value")
head(tidy)
titanic2$rate <- with(tidy, round(survived / (survived + perished), 2))
head(tidy)


###3.Data split across many files
cbind()
rbind()


###4.Saving data
read.csv()
write.csv()       

write.csv(tidy, file = "F:/importance/tidy.csv",
          row.names = FALSE)

readRDS()
saveRDS()       

saveRDS(tidy, "F:/importance/tidy.rds")
tidy2 <- readRDS("F:/importance/tidy.rds")
head(tidy2)

write.csv(tidy, file = bzfile("F:/importance/tidy.csv.bz2"), row.names = FALSE)                      #ѹ??????
tidy3 <- read.csv("F:/importance/tidy.csv.bz2")  


#------------------------------------------------------------------
write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "/n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
write.table(iris, "F:/iris.txt", sep = ',', quote = T, na = 'NA')
library(xlsx)
write.xlsx()
library(foreign)
write.foreign(df, datafile, codefile,
              package = c("SPSS", "Stata", "SAS"), ...)
#------------------------------------------------------------------


#####Transforming Data
library(ggplot2)
options(stringsAsFactors = FALSE)
setwd("F:/Rworkd/DW/Intro-to-R/Day 2/data")
bnames <- read.csv("bnames.csv.bz2")
head(bnames)
births <- read.csv("births.csv")
head(births)

#---------------------------------------------------------------------
garrett <- bnames[bnames$name == "Garrett", ]
qplot(year, prop, data = garrett, geom = "line")

michael <- bnames[bnames$name == "Michael", ]
qplot(year, prop, data = michael, geom = "line")
qplot(year, prop, data = michael, geom = "point")
qplot(year, prop, data = michael, geom = "line", color = sex)

michaels <- bnames[bnames$name == "Michael" | bnames$name == "Michelle", ]
qplot(year, prop, data = michaels, geom = "line", 
      color = interaction(sex, name))
#------------------------------------------------------------------------

####dplyr-----------------------------------------------------
library(dplyr)
bnames <- tbl_df(bnames)
births <- tbl_df(births)
class(bnames)

##filter   keep rows by criteria----------------------------
df <- data.frame(color = c("blue", "black", "blue", "blue", "black"), 
                 value = 1:5)
df
tbl <- tbl_df(df)
tbl

filter(tbl, color == "blue")
filter(df, value %in% c(1, 4))
#----------------------------------------------------
A <- filter(bnames, name == "Garrett")
B <- filter(bnames, year %% 100 == 0 & sex == "girl")

dim(filter(bnames, prop > 0.01 & year > 2000))

garrett <- filter(bnames, name == "Garrett")
garrett$soundex[1]
filter(bnames, soundex == "G630")

filter(bnames, sex == "girl" & (year == 1900 | year== 2000))
dim(filter(bnames, year > 2000 & prop > 0.01))
#------------------------------------------------------

##select     pick columns by name------------------------------
select(tbl, color)
select(tbl, -color)
#----------------------------------------------
a1 <- select(bnames, soundex)
a2 <- select(bnames, -c(year, name, prop, sex))
a3 <- select(bnames, (-)starts_with("sound"))
a4 <- select(bnames, (-)ends_with("dex"))
a5 <- select(bnames, (-)contains("sound"))
a6 <- select(bnames, (-)matches(""))
a7 <- select(bnames, one_of("soundex"))
a8 <- select(bnames, everything())
a9 <- select(x, : )
a10 <- select(x, num_range("", n:m))

a <- select(bnames, ends_with("ex"))

##arrange           reorder rows------------------------------

df1 <- data.frame(color = c(4, 1, 5, 3, 2), value = 1:5)
df1

tbl1 <- tbl_df(df1)
tbl1

arrange(tbl1, color)
arrange(tbl1, desc(color))

arrange(bnames, desc(prop))
garrett <- filter(bnames, name == "Garrett")
arrange(garrett, desc(prop))


##mutate         add new variables-----------------------------------------------
mutate(tbl, double = value * 2)
mutate(tbl, double = value * 2, quadruple = double * 2)

##summarise        reduce variables to values------------------------------------------
summarise(tbl, total = sum(value))
summarise(tbl, total = sum(value), avg = mean(value))


###Joining data sets----------------------------------------------------------------------


x <- data.frame(
  name = c("John", "Paul", "George", "Ringo", "Stuart", "Pete"),
  instrument = c("guitar", "bass", "guitar", "drums", "bass", "drums"))
x
y <- data.frame(
  name = c("John", "Paul", "George", "Ringo", "Brian"),
  band = c("TRUE", "TRUE", "TRUE", "TRUE", "FALSE"))
y
left_join(x, y, by = "name")
inner_join(x, y, by = "name")
semi_join(x, y, by = "name")
anti_join(x, y, by = "name")
#------------------------------------------------------------------------
bnames2 <- left_join(bnames, births, by = c("year", "sex"))
bnames2
bnames2 <- mutate(bnames2, n = prop * births)
bnames2
bnames2 <- mutate(bnames2, n = round(prop * births))
bnames2

###Group wise operations
garrett <- filter(bnames2, name == "Garrett")
garrett
sum(garrett$n)
summarise(garrett, total = sum(n))

##group_by-----------------------------------
df <- data.frame(
  color = c("blue", "black", "blue", "blue", "black"),
  value = 1:5)
df
tbl <- tbl_df(df)
tbl

summarise(tbl, total = sum(value))

group_by(tbl, color) %>%
  summarise(total = sum(value))

group_by(bnames2, name) %>%
  summarise(total = sum(n))
#---------------------------
group_by(bnames2, name, sex)
#deng jia
group_by(bnames2, name) %>%
  group_by(sex)
#---------------------------
group_by(bnames2, name, sex) %>%
  summarise(total = sum(n))

#remove group specifications
group_by(bnames2, name, sex) %>%
  ungroup()






##summary functions
min(x)          #
median(x)       #
mean(x)         #
quantile(x)     #
max(x)          #
fivenum(x)      #Tukey Five-Number Summaries

n(x)            #The number of observations in the current group.
                #This function is implemented special for each data
                #source and can only be used from within summarise, mutate and filter
sum(x)          #

sum(x > 10)     #
mean(x > 10)    #

var(x)          #
sd(x)           #
IQR(x)          #
mad(x)          #Median Absolute Deviation
range(x)        #


p <- group_by(bnames2, soundex) %>%
       summarise(total = sum(n))
p
arrange(p, desc(total))
j500 <- filter(bnames, soundex == "J500")
unique(j500$name)

group_by(bnames2, year, sex) %>%
  summarise(births = sum(n))

group_by(bnames2, year, sex) %>%
  mutate(rank = rank(desc(prop)))
browseVignettes(package = "dplyr")

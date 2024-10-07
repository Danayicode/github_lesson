
#Problem 1 ------

#03..10.2024

#Danayi Okbasillassie

#s242355

#Problem 2 -----

library(reader)
library(tidyverse)


# Skeleton file for Assignment 1 in BAN400


# Step 1: Read the entire data file into memory using the readLines()-function.
# URL or local file path (you can replace the url with your local file path)
raw_file <- readLines("https://www.sao.ru/lv/lvgdb/article/suites_dw_Table1.txt")

#set 2: identify the number L, that is, the first line that starts with 
#("--"). One way to dothis is to first extract the first two characters of 
# each line. we can do that using the substr() -function:
head(data, 10)


substr(x = raw_file, start = 1, stop = 2)

#step 3. with this as a starting point, we can locate L as follows. the final
# 'win' perhaps an overkill, but it will ensure that we locate the *first* 
#line that starts with ("--")

L <- (substr(raw_file, 1, 2) == "--") %>%  which %>% min 

#step 4: it would be nice to save the varible names in a seperate file  
cat(raw_file[1:(L-2)], sep = "\n", file  = "galaxy_variable.txt")

#step 5: we extract the variable name is a vector unlist and
#apply str_trim to remove leading and trailing white spaces
variable_names <- str_split(raw_file[L-1], "\\|") %>% unlist() %>% str_trim()

#step 6: extract the actual data frame by making an intermediate csv-file
comma_seperated_values <- 
  raw_file[(L+1):length(raw_file)] %>% 
  gsub("\\|",",", .) %>% 
  gsub(" ", "", .)

comma_seperated_values_with_names <-
  c(paste(variable_names, collapse = ","),
    comma_seperated_values)
cat(comma_seperated_values_with_names, sep =  "\n", file = "galaxies.csv")
galaxies <- read.csv("galaxies.csv")
#Problem 3 -----

#visualizaation showing the apperent incompletness of the data. We plot the
#size of the objects (measured by their mass) against their distance from us.
#it looks like all of the smallest objects are very close to us. unless there is 
# aparticular reason for why our closest neighbourhood should contact a lot of
#small objects that are not present anywhere else, it is natural to think that 
#we have not been able to detect objects of this size that are furher away
galaxies %>% 
  ggplot(aes(x = D, y = log_lk)) +
  geom_point()

#We could also count the number of objects for which each galaxy is the main
#body of influence, and plot that number as a function of the dinstance to us

#if we do som research on wikipedia or the like, we might find out the there
#are three mayor galaxies in the local group to which the milky way belongs:
#The anaromeda galaxy (codea as messieru31, much larger than the milkyway),
#The milkyway itself, and the triangulum galaxy, which is a bit smaller than
#the milkyway and lies a bit further out than Andromeda, but pretty much along 
#the same line of sight from us (coded as MESSIES033). Let us code these
#objects seperatly as well as we can identify them in the plots.

with_number_bodies <-
  galaxies %>% 
  group_by(md) %>% 
  summarise(number_of_bodies = n()) %>% 
  rename(name = md) %>% 
  full_join(galaxies) %>% 
  mutate(local_group = recode(.$name,
                              milkyway = "Milky Way",
                              MESSIER031 = "Andromeda",
                              MESSIER033 = "Triangulum",
                              .default = "other"))
#it is a bit irritating that we are missing the diameter of the milky way.
#googling, again, suggests that this is a known number, approximately 30kpc
with_number_bodies[with_number_bodies$name == "Milkyway", "a_26"] <-30

#we can then plot the number of bodies that each has under its main influence,
#plot it against its distance from us, color the dots so thatt we can identify 
#the three main galaxies in the local group, and also let the size of the dots 
#reopresent the diameter of the objects

with_number_bodies %>% 
  ggplot(aes(x = D, y = number_of_bodies, colour = local_group, size = a_26))+
  geom_point()+
  xlab("Distance from us (Mpc)")+
  ylab("Number of bodies under its main influence")+
  labs(colour = "Galaxy", size = "Diameter(kpc)")+
  theme_minimal()

#interesting plot, but it is not so clear anymore that we are missing something.

#problem 4------

#we use the same techniques as in problem 1 to read the file
raw_file <- readLines("https://www.sao.ru/lv/lvgdb/article/UCNG_Table4.txt")
L <- (substr(raw_file, 1, 2) == "--") %>%  which %>% min 
variable_names <- str_split(raw_file[L-1], "\\|") %>% unlist() %>% str_trim()
comma_seperated_values <- 
  raw_file[(L+1):length(raw_file)] %>% 
  gsub("\\|",",", .) %>% 
  gsub(" ", "", .)
comma_seperated_values_with_names <-
  c(paste(variable_names, collapse = ","),
    comma_seperated_values)

cat(comma_seperated_values_with_names, sep =  "\n", file = "speeds.csv")

speeds <- read_csv("speeds.csv")

#join the galaxies with the velocity data

Velocity <- full_join(with_number_bodies, speeds)
#make a plot of the velocity against the distance from us, and we clearly see a
#linear relationship. WE can also identify the Andromeda and the triangulum by
#gravitationaly pull dominates the expansion of space at such "small"
#distances
Velocity %>% 
  ggplot(aes(x=D, y = cz, colour = local_group))+
  geom_point()
#we can estimate the value of Hubbles constant using linear regression without
#intercepts
lm(cz ~ D - 1, data = Velocity) %>% summary()

#The value of the estimated regression constant is 79(km/s)/mpc, which, at 
#least is in the c\vicinity of the probably get a better estimate if we
#include galaxies that are much further away. For which the gravitational forces
#are less prominent


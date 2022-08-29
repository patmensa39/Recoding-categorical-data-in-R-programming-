#Recoding categorical data ####
### make sure to install all the required packages 
pacman::p_load(magrittr, pacman, rio, tidyverse, tidyr)

### importing the data 
data <- import("MobileOS_US.xlsx") %>% as_tibble() %>% print()
view(data)
glimpse(data)
attach(data)
### we have to define "MobileOS" as a factor
data %<>% mutate(MobileOS = as.factor(MobileOS)) %>% print()
glimpse(data)

### In January 2010 we realized most of the operating systems were fading out 
### so lets check them out by selecting the 2010 and converting them to a whole number
data %<>% mutate(OS_2010 = `2010-01` * 100) %>% select(MobileOS, OS_2010) %>% print()

### we will like to check out for oultliers by using a boxplot 
data %>% select(OS_2010) %>% 
  boxplot(main = "Boxplot of Operating Sytems in Jan 2010", horizontal = TRUE)

view(data)
glimpse(data)

### converting them to rows but the trick is that you will need to convert the 
### data to intergers first 
data %<>% mutate(OS_2010 = as.integer(OS_2010)) 
glimpse(data)
data %<>% uncount(OS_2010) %>% print()

### we need to check thier frequencies again 
data %>% select(MobileOS) %>% pull() %>% fct_count()


### Releveling the variables manually 
### Example making nintendo comes first 
data %<>% mutate(MobileOS = fct_relevel(MobileOS, "Nintendo"))

### checking the levels 
data %>% pull(MobileOS) %>% levels()

### creating a bar chart to see if it really worked
data %>% ggplot(mapping = aes(x = MobileOS)) + geom_bar()

### checking the levels 
data  %>% pull(MobileOS) %>% levels()

### creating a bar chart to see if it really worked
data %>% ggplot(mapping = aes(x = MobileOS)) + geom_bar()

### collapsing some of the variables (unknown and others)
data %<>% 
  mutate(MobileOS = fct_collapse(
    .$MobileOS, unknown_other = c("Unknown", "Other")
  )
)

### checking the levels 
data %>% pull(MobileOS) %>% levels()
view(data)

### creating a bar chart to see if it really worked
data %>% ggplot(mapping = aes(x = MobileOS)) + geom_bar()

### collapsing everything but not the top 3 
data %<>% mutate(
  lump_MobileOS = fct_lump(MobileOS, n = 3)
)
attach(data)
### checking the levels 
data %>% pull(lump_MobileOS) %>% levels()

### creating a bar chart to see if it really worked
data %>% ggplot(mapping = aes(x = lump_MobileOS)) + geom_bar()

### Lumping levels with fewer than 100 counts 
data %<>% mutate(min_MobileOS = fct_lump_min(MobileOS, min = 100))

### checking the levels 
data %>% pull(min_MobileOS) %>% levels()
### creating a bar chart to see if it really worked
data %>% ggplot(mapping = aes(x = min_MobileOS)) + geom_bar()

### keeping only specified catergory 
### that is comparing ios to all others 
data %<>% mutate(MobileOS_iOS = fct_other(MobileOS, keep = "iOS"))

### checking the levels 
data %>% pull(MobileOS_iOS) %>% levels()

### creating a bar chart to see if it really worked
data %>% ggplot(mapping = aes(x = MobileOS_iOS)) + geom_bar()

### Removing levels (thats removing others and unknown)
data %<>% filter (!MobileOS == "Other" | MobileOS == "unknown_other")

### checking the levels 
data %>% pull(MobileOS) %>% levels() 

### creating a bar chart again to see if it really worked
data %>% ggplot(mapping = aes(x = MobileOS)) + geom_bar()

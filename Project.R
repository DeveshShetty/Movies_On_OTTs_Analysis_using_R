install.packages("tidyverse")
install.packages("ggplot")
install.packages("plotly")
install.packages('tidytext')
install.packages('caret')
install.packages('caTools')
install.packages('randomForest')

library("tidyverse")
library("plotly")
library('tidytext')
library('caret')
library('caTools')
library('randomForest')

dp11 = read.csv("E:\\ikeee\\A_Rtut Important\\D+\\MoviesOnStreamingPlatforms_updated.csv", stringsAsFactors = FALSE)
glimpse(dp11)
summary(dp11)

dp1= dp11[!duplicated(dp11)]
#Splitting dataset according to platforms

Netflix <-  dp1 %>% 
  select(ID, Title, Year, Age, IMDb,Rotten.Tomatoes,Type,Directors,Genres,Country,Language,Runtime) %>%
  filter(dp1$Netflix == 1 ) %>%
  drop_na()
Hulu <-  dp1 %>% 
  select(ID, Title, Year, Age, IMDb,Rotten.Tomatoes,Type,Directors,Genres,Country,Language,Runtime) %>%
  filter(dp1$Hulu == 1 ) %>%
  drop_na()
Amazon <-  dp1 %>% 
  select(ID, Title, Year, Age, IMDb,Rotten.Tomatoes,Type,Directors,Genres,Country,Language,Runtime) %>%
  filter(dp1$Prime.Video == 1 ) %>%
  drop_na()
Disney <-  dp1 %>% 
  select(ID, Title, Year, Age, IMDb,Rotten.Tomatoes,Type,Directors,Genres,Country,Language,Runtime) %>%
  filter(dp1$Disney. == 1 ) %>%
  drop_na()

#movie duration distribution across platforms
Net <- Netflix %>% count(Title)
ggplot(data = Netflix ,aes(x=nrow(Title)))+geom_histogram(fill='red',col='black')+ggtitle("Netflix Title")
ggplot(data = Hulu ,aes(x=Title))+geom_histogram(fill='palegreen4',col='black')+ggtitle("Hule Title")
ggplot(data = Amazon ,aes(x=Title))+geom_histogram(fill='yellow',col='black')+ggtitle("Amazon Title")
ggplot(data = Disney ,aes(x=Title))+geom_histogram(fill='Blue',col='black')+ggtitle("Disney Title")

#movie duration distribution across platforms
ggplot(data = Netflix ,aes(x=Runtime))+geom_histogram(fill='red',col='black')+ggtitle("Netflix Runtime")
ggplot(data = Hulu ,aes(x=Runtime))+geom_histogram(fill='palegreen4',col='black')+ggtitle("Hule Runtime")
ggplot(data = Amazon ,aes(x=Runtime))+geom_histogram(fill='yellow',col='black')+ggtitle("Amazon Runtime")
ggplot(data = Disney ,aes(x=Runtime))+geom_histogram(fill='Blue',col='black')+ggtitle("Disney Runtime")

# Number of tites by each country

dp_country = filter(dp1, nchar(Country)>0)
dp_country %>%
  filter(!str_detect(Country,',')) %>%
  group_by(Country) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot() + geom_col(aes(y = reorder(Country,n), x = n)) +
  geom_label(aes(y = reorder(Country,n), x = n, label = n)) +
  labs(title = 'Approx. Number of Titles of each Country') +
  theme_minimal()

# IMDB Rating of movie 
dp1 %>%
  filter(Year !=2021 && Year !='') %>%
  group_by(IMDb,Year) %>%
  count() %>%
  ggplot()+geom_line(aes(x=Year,y=n)) +
  labs(title = 'Trend of Titles every Year') +
  theme_minimal()

dp_country %>%
  filter(Year !=2021) %>%
  group_by(IMDb,Title) %>%
  count() %>%
  ggplot(aes(x=IMDb,y=Year,group=Year,colour=Year))+geom_bar() +
  labs(title = 'Trend of Titles every Year') +
  theme_minimal()

dp_country %>%
       filter(Netflix==1) %>%
       group_by(IMDb,Year) %>%
       count() %>%
       ggplot(aes(x=IMDb,y=Year))+geom_point() +
       labs(title = 'IMDB Trend of every Year on Netflix') +
  theme_minimal()

#Age Distribution Of Movies
dp1 %>% filter(Age !='') %>% count(Age, sort = T) %>%
  mutate(prop = paste0(round(n / sum(n) * 100, 0), "%")) %>%
  ggplot(aes(x = "", y = prop, fill = Age)) +
  geom_bar(
    stat = "identity",
    width = 1,
    color = "steelblue",
    size = 1
  )+
  coord_polar("y", start = 0) +
  geom_text(
    aes(y = prop, label = prop),
    position = position_stack(vjust = 0.5),
    size = 6,
    col = "white",
    fontface = "bold"
  ) +
  scale_fill_manual (values = c('#e41a1c', '#377eb8','#6ffc76','#a765e0','#f547b2','#22d487')) +
  theme_void() +
  labs(
    title = "Age Distribution Of Movies Across All OTT Platforms",
     subtitle = "Pie Plot, Age Distribution across Movies",
    fill = ""
  )

# Number of titles based on themes / Genre of Titles

dp1 %>% filter(Genres!='') %>% 
  select(Genres) %>%
  mutate(Genres = str_split(Genres,',')) %>%
  unnest(Genres) %>%
  mutate(Genres= trimws(Genres, which = c("left")))%>%
  group_by(Genres) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(30) %>%
  ggplot() + geom_col(aes(y = reorder(Genres,n), x = n)) +
  labs(title = 'Genres of Movies',
       x = 'Movie Titles',
       y = 'Genres') +
  theme_minimal()

# Directors associated with movies
install.packages("RColorBrewer")
library(RColorBrewer)
library(tidyverse)
# Discrete color
dp1_dir <- dp1 %>% filter(Directors!='') %>%
  select(Directors) %>%
  mutate(Directors = str_split(Directors,',')) %>%
  unnest(Directors) %>%
  mutate(Directors= trimws(Directors, which = c("left")))%>%
  group_by(Directors) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(30) %>%
  ggplot() + geom_col(aes(y = reorder(Directors,n), x = n)) +
  labs(title = 'No of Movies',
       x = 'Movie Titles',
       y = 'Directors') +
  theme_minimal()
dp1_dir

#age

dp2_ml = dp1 %>% 
  select(Runtime ,Genres , Year,IMDb,Age) %>%
  mutate(Age_i = parse_number(Age))%>%
  drop_na()  
  

#create training and validation data from given data
#dp2_ml %>% filter(Runtime!=NA & Age_i!= NA & Age_i!= 'all' & Year!=NA & IMDb!=NA) %>% select(Runtime ,Genres , Year,IMDb,Age_i ) #%>%
  #mutate(Runtime = parse_number(Runtime))
dp2_ml %>% filter(is.na(Runtime)==T & is.na(Age_i)== T & Age_i!= 'all' & is.na(Year)==T & is.na(IMDb)==T) %>% select(Runtime ,Genres , Year,IMDb,Age_i )

dp3_ml = dp2_ml[!duplicated(dp2_ml)]
################################################

set.seed(88)
split <- sample.split(dp2_ml$IMDb, SplitRatio = 0.70)

#get training and test data
train <- subset(dp2_ml, split == TRUE)
test <- subset(dp2_ml, split == FALSE)

train = train %>%
  mutate(Genres = strsplit(as.character(Genres), ",")) %>% 
  unnest(Genres) %>%
  mutate(Genres = trimws(Genres, which = c("left"))) %>%
  mutate(Genres = tolower(Genres))

test = test %>%
  mutate(Genres = strsplit(as.character(Genres), ",")) %>% 
  unnest(Genres) %>%
  mutate(Genres = trimws(Genres, which = c("left"))) %>%
  mutate(Genres = tolower(Genres))

#dummify data
dmy <- dummyVars("~ .", data= train)
train <- data.frame(predict(dmy, newdata = train)) 

dmy <- dummyVars("~ .", data= test)
test <- data.frame(predict(dmy, newdata = test)) 

#Multi Linear regression
model <- lm(IMDb ~., data=train)
predicted_value <- predict(model, newdata = test)
range(predicted_value)
multi_linear = as.data.frame(cbind(Actual = test$IMDb, Predicted = predicted_value))
error =  (multi_linear$Actual - multi_linear$Predicted)
multi_linear = as.data.frame(cbind(multi_linear,error))
rmse = sqrt(mean((error)^2))
head(multi_linear)
print(rmse)

#Confusion Matrix
table(test$IMDb,predicted_value > 5.0)

#Random Forest
model <- randomForest(IMDb ~., data=train)
predicted_value <- predict(model, newdata = test)
random_forest = as.data.frame(cbind(Actual = test$IMDb , Predicted = predicted_value))
error =  (random_forest$Actual - random_forest$Predicted)
random_forest = as.data.frame(cbind(random_forest,error))
rmse = sqrt(mean((error)^2))
head(random_forest)
print(rmse)


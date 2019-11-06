library(tidyverse)
library(tidyr)
library(dplyr)

#Install packages for grabbing the googlesheet
install.packages("googlesheets")
library(googlesheets)

#Authenticate so data don't have to be public
gs_auth()

#Read in Googlesheet
sheet <-gs_url("https://docs.google.com/spreadsheets/d/1tY_BJFutyEop4QCkHQH_FGUAcB7MXoWSY28DjJnm1t4/edit#gid=996942651")
df<-gs_read(sheet)

names(df)

#Clean data so that each coin toss pair is in a separate row and each partner's coin toss is in a separate column with value 'heads' (1) or 'tails' (0)
tidy_coins<-df %>% 
  gather(Toss, Result, Coin_1: Coin_10) %>% 
  separate(Result, c("Partner_1", "Partner_2"), sep = ",") %>% 
  dplyr::mutate(Partner_1 =str_sub(Partner_1,-5,-1)) %>% 
  dplyr::mutate(Partner_2 =str_sub(Partner_2,-5,-1)) %>% 
  select(-Timestamp) %>% 
  dplyr::mutate(
    P_1 = ifelse(Partner_1=="heads", 1, 0),
    P_2 = ifelse(Partner_2=="heads", 1, 0),
    head_count = P_1 + P_2
  )

tidy_coins


#Data analysis part 1: Association between heads and tails in full data
tidy_coins %>% group_by(Partner_1, Partner_2) %>% summarize(n=n())
all_data<-glm(P_1~P_2, data=tidy_coins, family=binomial(link="logit"))
exp(coefficients(all_data)[2])


#Data analysis part 2: Subset to rows with at least one head, then look at association between heads and tails
tidy_coins_subset <- subset(tidy_coins, head_count !=0)

some_data <- glm(P_1 ~P_2, data= tidy_coins_subset, family=binomial(link="logit"))
exp(coefficients(some_data)[2])



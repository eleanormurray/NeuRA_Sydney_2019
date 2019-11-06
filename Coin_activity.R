#Install packages for grabbing the googlesheet

if(!require(here)) { install.packages("here"); require(here)}
set_here()
if(!require(gmodels)){ install.packages("gmodels");require(gmodels)}
if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)}
if(!require(tidyr)){install.packages("tidyr"); require(tidyr)}
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(janitor)){install.packages("janitor"); require(janitor)}
if(!require(googlesheets)){install.packages("googlesheets"); require(googlesheets)}



#Authenticate so data don't have to be public
#gs_auth()

#Read in Googlesheet
sheet <-gs_url("https://docs.google.com/spreadsheets/d/1tY_BJFutyEop4QCkHQH_FGUAcB7MXoWSY28DjJnm1t4/edit#gid=996942651")
df<-gs_read(sheet)

names(df)
df<-clean_names(df)
names(df)

#Clean data so that each coin toss pair is in a separate row and each partner's coin toss is in a separate column with value 'heads' (1) or 'tails' (0)
tidy_coins<-df %>% 
  gather(Toss, Result, coin_1: coin_10) %>% 
  separate(Result, c("partner_1", "partner_2"), sep = ",") %>% 
  dplyr::mutate(partner_1 =str_sub(partner_1,-5,-1)) %>% 
  dplyr::mutate(partner_2 =str_sub(partner_2,-5,-1)) %>% 
  select(-timestamp) %>% 
  dplyr::mutate(
    p_1 = ifelse(partner_1=="heads", 1, 0),
    p_2 = ifelse(partner_2=="heads", 1, 0),
    head_count = p_1 + p_2
  )

tidy_coins


#Data analysis part 1: Association between heads and tails in full data
tidy_coins %>% group_by(partner_1, partner_2) %>% summarize(n=n())
CrossTable(tidy_coins$p_1, tidy_coins$p_2, chisq = T)

all_data<-glm(p_1~p_2, data=tidy_coins, family=binomial(link="logit"))
exp(coefficients(all_data)[2])


#Data analysis part 2: Subset to rows with at least one head, then look at association between heads and tails
tidy_coins_subset <- subset(tidy_coins, head_count !=0)
CrossTable(tidy_coins_subset$p_1, tidy_coins_subset$p_2, chisq = T)

some_data <- glm(p_1 ~p_2, data= tidy_coins_subset, family=binomial(link="logit"))
exp(coefficients(some_data)[2])



#### Preamble ####
# Purpose: Prediction of result of Polls to establish the opinions of TORONTO residents on various topics covered by a City by-law
# Author: HaoCheng Xu, Jing Li
# Data: Feb 27, 2022
# Contact: jingle.li@mail.utoronto.ca
# Pre-requisites: 
# - Need to have downloaded the data and saved it to inputs/data

#### Workspace setup ####

library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(caTools)

data <- read.csv("C:/Users/TS-42/Desktop/sta304/paper_2/Polls Data.csv")

Variable <- c("X_id","ADDRESS","APPLICATION_FOR","BALLOTS_BLANK","BALLOTS_CAST",
              "BALLOTS_DISTRIBUTED","BALLOTS_IN_FAVOUR","BALLOTS_NEEDED_TO_PROCEED",
              "BALLOTS_NEEDED_TO_PROCEED_LBL","BALLOTS_OPPOSED","BALLOTS_RECEIVED_BY_VOTERS","BALLOTS_RETURNED_TO_SENDER","BALLOTS_SPOILED","CLOSE_DATE","DECLARATIONS_ADDED","FINAL_VOTER_COUNT","MORATORIUM_DATE","OPEN_DATE","PASS_RATE","PASS_RATE_LABEL","POLL_CD","POLL_ID",
              "POLL_RESULT","POTENTIAL_VOTERS","RESPONSE_RATE_MET")
Description <- c("Unique row identifier for Open Data database","Street address of the application","Type of application","Number of ballots received with no mark to identify in favour or opposed","Number of ballots returned","Number of ballots distributed","Number of ballots received and marked favour","The number of ballots needed to proceed based on percentage of return","Percentage of returned ballots needed to consider poll valid","Number of ballots received and marked opposed","Number of ballots returned to City Clerk's Office", "Number of ballot returned by Canada Post as not delivered","Number of ballots received and were not clearly marked as either in favour or opposed","Date the poll has been closed","Number of individuals added to the poll list after the poll has open","Number of total voters on the final poll list","The date to which this poll can be conducted again","Date the poll is open to public","Number of returned ballots needed for a positive poll result","Percentage of returned ballots needed to determine poll result","Poll Identification Number (Public)","Poll Application Number","Final result of poll","Number of people residing within poll boundary range","	
The number of ballots returned has met the required response rate")
des <- data.frame(Variable, Description)


kable(des, booktabs = TRUE,caption = "Description of ths Dataset") %>%
  kable_styling(font_size = 8)

data_new<-within(data, rm(X_id, ADDRESS,BALLOTS_NEEDED_TO_PROCEED_LBL,CLOSE_DATE,
                          MORATORIUM_DATE,OPEN_DATE,PASS_RATE_LABEL,POLL_CD,POLL_ID))
data_1<-na.omit(data_new)
data_1<-na.omit(data_new)
head(data_1)

summary(data_1)
data_2<- data_1%>%filter(RESPONSE_RATE_MET == "Yes")
str(data_2$POLL_RESULT)

data_2$APPLICATION_FOR<-as.factor(data_2$APPLICATION_FOR)
data_2$POLL_RESULT<-as.factor(data_2$POLL_RESULT)
names(data_2)
str(data_2)

levels(data_2$APPLICATION_FOR)
bench1<- 0 + 1.5*IQR(data_2$BALLOTS_BLANK)
data_2$BALLOTS_BLANK[data_2$BALLOTS_BLANK>bench1] <- bench1
summary(data_2$BALLOTS_BLANK)

bench2<- 52 + 1.5*IQR(data_2$BALLOTS_CAST)
data_2$BALLOTS_CAST[data_2$BALLOTS_CAST>bench2] <- bench2
summary(data_2$BALLOTS_CAST)

bench3<- 111 + 1.5*IQR(data_2$BALLOTS_DISTRIBUTED)
data_2$BALLOTS_DISTRIBUTED[data_2$BALLOTS_DISTRIBUTED>bench3] <- bench3
summary(data_2$BALLOTS_DISTRIBUTED)

bench4<- 40 + 1.5*IQR(data_2$BALLOTS_IN_FAVOUR)
data_2$BALLOTS_IN_FAVOUR[data_2$BALLOTS_IN_FAVOUR>bench4] <- bench4
summary(data_2$BALLOTS_IN_FAVOUR)

bench5<- 29 + 1.5*IQR(data_2$BALLOTS_NEEDED_TO_PROCEED)
data_2$BALLOTS_NEEDED_TO_PROCEED[data_2$BALLOTS_NEEDED_TO_PROCEED>bench5] <- bench5
summary(data_2$BALLOTS_NEEDED_TO_PROCEED)

bench6<- 11 + 1.5*IQR(data_2$BALLOTS_OPPOSED)
data_2$BALLOTS_OPPOSED[data_2$BALLOTS_OPPOSED>bench6] <- bench6
summary(data_2$BALLOTS_OPPOSED)

bench7<- 106 + 1.5*IQR(data_2$BALLOTS_RECEIVED_BY_VOTERS)
data_2$BALLOTS_RECEIVED_BY_VOTERS[data_2$BALLOTS_RECEIVED_BY_VOTERS>bench7] <- bench7
summary(data_2$BALLOTS_RECEIVED_BY_VOTERS)

bench8<- 6 + 1.5*IQR(data_2$BALLOTS_RETURNED_TO_SENDER)
data_2$BALLOTS_RETURNED_TO_SENDER[data_2$BALLOTS_RETURNED_TO_SENDER>bench8] <- bench8
summary(data_2$BALLOTS_RETURNED_TO_SENDER)

bench9<- 5 + 1.5*IQR(data_2$BALLOTS_SPOILED)
data_2$BALLOTS_SPOILED[data_2$BALLOTS_SPOILED>bench9] <- bench9
summary(data_2$BALLOTS_SPOILED)

bench10<- 0 + 1.5*IQR(data_2$DECLARATIONS_ADDED)
data_2$DECLARATIONS_ADDED[data_2$DECLARATIONS_ADDED>bench10] <- bench10
summary(data_2$DECLARATIONS_ADDED)

bench11<- 111 + 1.5*IQR(data_2$FINAL_VOTER_COUNT)
data_2$FINAL_VOTER_COUNT[data_2$FINAL_VOTER_COUNT>bench11] <- bench11
summary(data_2$FINAL_VOTER_COUNT)


bench12<- 28 + 1.5*IQR(data_2$PASS_RATE)
data_2$PASS_RATE[data_2$PASS_RATE>bench12] <- bench12
summary(data_2$PASS_RATE)


bench13<- 144 + 1.5*IQR(data_2$POTENTIAL_VOTERS)
data_2$POTENTIAL_VOTERS[data_2$POTENTIAL_VOTERS>bench13] <- bench13
summary(data_2$POTENTIAL_VOTERS)

data_2<- data_1%>%filter(RESPONSE_RATE_MET == "Yes")
data_2$POLL_RESULT<-ifelse(data_2$POLL_RESULT=="In Favour",1,0)
library(caTools)
set.seed(88)
split <- sample.split(data_2$POLL_RESULT, SplitRatio = 0.75)

#get training and test data
train <- subset(data_2, split == TRUE)
test <- subset(data_2, split == FALSE)

kable(summary(train[1:8]), format="latex", booktabs=TRUE, caption = "Summary Statistics of variables in Dataset") %>% 
  kable_styling(latex_options="scale_down")

kable(summary(data_2[9:16]), format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options="scale_down")

bar <- data.frame(
  group = c("In Favoured", "Opposed"),
  value = c(500,116)
)


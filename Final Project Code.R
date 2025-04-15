# Final Project Data
# load the package I need 
library(readr)
library(tidyverse)
library(stargazer)
library(data.table)
library(fixest)

setwd("/Users/lorewu/Desktop/Final Project") # set my directory 
recipients <- read.csv("dime_recipients_1979_2024.csv")

# filter and select out the key variables I am exploring
recipinets_select <- recipients %>%
  filter(recipient.type == "cand", !is.na(cand.gender)) %>% # explore candidate and filter out if their gender is missing
  select(cycle, # control V -- time effect
         state, # control V -- state effect
         cand.gender, # Independent V -- key treatment 
         party, # control V
         seat, # control V
         ico.status, # control V
         recipient.cfscore, # control V
         contributor.cfscore, # control V
         total.receipts, # dependent V
         total.indiv.contribs,# dependent V
         total.contribs.from.candidate, # dependent V
         total.pac.contribs, # dependent V
         total.unitemized, # dependent V
         num.givers # dependent V
         )

#recode the variables
recipinets_recode <- recipinets_select %>%
  mutate(
   female_candidate = if_else(cand.gender == "F", 1, 0), # create female gender variable that indicates as 1, other wise is 0
   party_aff = case_when(party == "100" ~ "Dem",
                         party == "200" ~ "Rep",
                         TRUE ~ "Other"), 
   status = case_when(ico.status == "I" ~ "Incumbent",
                      ico.status == "C" ~ "Challenger",
                      ico.status == "O" ~ "Open"), 
    )

#filter election type -- I want to focus on both house and senate election
recipinets_federal <- recipinets_recode %>%
  filter(seat %in% c("federal:house", "federal:senate"))

# create the ideological gap between donor and recipient 
recipinets_clean <- recipinets_federal %>%
  mutate(ideo_gap = abs(recipient.cfscore - contributor.cfscore))

# save the cleaned dataframe 
write_csv(recipinets_clean, "recipinets_clean")

# for better visualization 
summary_better <- recipinets_clean %>%
  select(
    cycle,
    recipient.cfscore,
    contributor.cfscore,
    total.receipts,
    total.indiv.contribs,
    total.pac.contribs,
    total.contribs.from.candidate,
    num.givers,
    female_candidate,
    ideo_gap) %>%
  rename(
    Year = cycle,
    `Candidate CFscore` = recipient.cfscore,
    `Donor CFscore` = contributor.cfscore,
    `Total Receipts` = total.receipts,
    `Individual Contributions` = total.indiv.contribs,
    `PAC Contributions` = total.pac.contribs,
    `Total Self-contributions` = total.contribs.from.candidate,
    `Number of Donors` = num.givers,
    `Female Candidate` = female_candidate,
    `Ideological Gap` = ideo_gap
  )

stargazer(summary_better, type = "html", title="Table 1: Summary Statistics", out="table1.htm", digits = 1)


# first stage: linear regression between total recipient and baseline control:
model_1 <- feols(total.receipts ~ female_candidate + recipient.cfscore + contributor.cfscore 
              + party_aff + status, recipinets_clean)

# second stage: interaction_effect 
model_2 <- feols(total.receipts ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                 + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                 + recipient.cfscore + contributor.cfscore + party_aff + status, recipinets_clean)

# third stage: adding time and state fixed effect 
model_3 <- feols(total.receipts ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                 + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                 + recipient.cfscore + contributor.cfscore + party_aff + status | cycle + state, recipinets_clean)

# estab the table and create latex file
etable(model_1, model_2, model_3,
       tex = TRUE,
       file = "table2_receipts.tex",
       title = "Table 2: Gender Effect on Total Receipts",
       dict = c(
         female_candidate = "Female Candidate",
         `female_candidate::ideo_gap` = "Female × Ideo Gap",
         `female_candidate::party_aff` = "Female × Party",
         `female_candidate::recipient.cfscore` = "Female × CanScore",
         `female_candidate::contributor.cfscore` = "Female × DonScore",
         recipient.cfscore = "Candidate CFscore",
         contributor.cfscore = "Donor CFscore",
         party_aff = "Party",
         status = "Incumbency",
         cycle = "Year FE",
         state = "State FE"
       ))

# apply similar steps toward other interest outcome

# total.indiv.contribs
model_in_1 <- feols(total.indiv.contribs ~ female_candidate + recipient.cfscore + contributor.cfscore 
                 + party_aff + status, recipinets_clean)
model_in_2 <- feols(total.indiv.contribs ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                    + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                    + recipient.cfscore + contributor.cfscore + party_aff + status, recipinets_clean)
model_in_3 <- feols(total.indiv.contribs ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                    + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                    + recipient.cfscore + contributor.cfscore + party_aff + status | cycle + state, recipinets_clean)

etable(model_in_1, model_in_2, model_in_3,
       tex = TRUE,
       file = "table3_receipts.tex",
       title = "Table 3: Gender Effect on Total Individual Receipts",
       dict = c(
         female_candidate = "Female Candidate",
         `female_candidate::ideo_gap` = "Female × Ideo Gap",
         `female_candidate::party_aff` = "Female × Party",
         `female_candidate::recipient.cfscore` = "Female × CanScore",
         `female_candidate::contributor.cfscore` = "Female × DonScore",
         recipient.cfscore = "Candidate CFscore",
         contributor.cfscore = "Donor CFscore",
         party_aff = "Party",
         status = "Incumbency",
         cycle = "Year FE",
         state = "State FE"
       ))

# total.pac.contribs

model_pac_1 <- feols(total.pac.contribs ~ female_candidate + recipient.cfscore + contributor.cfscore 
                     + party_aff + status, recipinets_clean)
model_pac_2 <- feols(total.pac.contribs ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                     + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                     + recipient.cfscore + contributor.cfscore + party_aff + status, recipinets_clean)
model_pac_3 <- feols(total.pac.contribs ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                     + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                     + recipient.cfscore + contributor.cfscore + party_aff + status | cycle + state, recipinets_clean)
etable(model_pac_1, model_pac_2, model_pac_3,
       tex = TRUE,
       file = "table4_receipts.tex",
       title = "Table 4: Gender Effect on Total PAC Receipts",
       dict = c(
         female_candidate = "Female Candidate",
         `female_candidate::ideo_gap` = "Female × Ideo Gap",
         `female_candidate::party_aff` = "Female × Party",
         `female_candidate::recipient.cfscore` = "Female × CanScore",
         `female_candidate::contributor.cfscore` = "Female × DonScore",
         recipient.cfscore = "Candidate CFscore",
         contributor.cfscore = "Donor CFscore",
         party_aff = "Party",
         status = "Incumbency",
         cycle = "Year FE",
         state = "State FE"
       ))


# num.givers
model_giver_1 <- feols(num.givers ~ female_candidate + recipient.cfscore + contributor.cfscore 
                     + party_aff + status, recipinets_clean)
model_giver_2 <- feols(num.givers ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                       + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                       + recipient.cfscore + contributor.cfscore + party_aff + status, recipinets_clean)
model_giver_3 <- feols(num.givers ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                       + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                       + recipient.cfscore + contributor.cfscore + party_aff + status | cycle + state, recipinets_clean)
etable(model_giver_1, model_giver_2, model_giver_3,
       tex = TRUE,
       file = "table5_receipts.tex",
       title = "Table 5: Gender Effect on Number of Donors",
       dict = c(
         female_candidate = "Female Candidate",
         `female_candidate::ideo_gap` = "Female × Ideo Gap",
         `female_candidate::party_aff` = "Female × Party",
         `female_candidate::recipient.cfscore` = "Female × CanScore",
         `female_candidate::contributor.cfscore` = "Female × DonScore",
         recipient.cfscore = "Candidate CFscore",
         contributor.cfscore = "Donor CFscore",
         party_aff = "Party",
         status = "Incumbency",
         cycle = "Year FE",
         state = "State FE"
       ))


# total.contribs.from.candidate
model_self_1 <- feols(total.contribs.from.candidate ~ female_candidate + recipient.cfscore + contributor.cfscore 
                       + party_aff + status, recipinets_clean)
model_self_2 <- feols(total.contribs.from.candidate ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                       + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                       + recipient.cfscore + contributor.cfscore + party_aff + status, recipinets_clean)
model_self_3 <- feols(total.contribs.from.candidate ~ female_candidate + female_candidate*ideo_gap + female_candidate*party_aff 
                       + female_candidate*recipient.cfscore + female_candidate*contributor.cfscore
                       + recipient.cfscore + contributor.cfscore + party_aff + status | cycle + state, recipinets_clean)
etable(model_self_1, model_self_2, model_self_3,
       tex = TRUE,
       file = "table7_receipts.tex",
       title = "Table 7: Gender Effect on Self-Contribution",
       dict = c(
         female_candidate = "Female Candidate",
         `female_candidate::ideo_gap` = "Female × Ideo Gap",
         `female_candidate::party_aff` = "Female × Party",
         `female_candidate::recipient.cfscore` = "Female × CanScore",
         `female_candidate::contributor.cfscore` = "Female × DonScore",
         recipient.cfscore = "Candidate CFscore",
         contributor.cfscore = "Donor CFscore",
         party_aff = "Party",
         status = "Incumbency",
         cycle = "Year FE",
         state = "State FE"
       ))

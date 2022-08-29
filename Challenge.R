# App update --------------
library(rsconnect)
rm("challenge/database.rdata")
rsconnect::deployApp("challenge")

# Initial Drop box connection ----
library(rdrop2)
token <- drop_auth()
saveRDS(token, "droptoken.rds")

# Database corrections -----------
library(tidyverse)
library(rdrop2)

filePath <- "database21.rdata"
filePathNew <- "database21.rdata"
token <- readRDS("challenge/droptoken.rds")

drop_download(filePath, overwrite = T, dtoken = token)
load(file = filePath)
# db2 <- db
# db21 <- db21 %>% select(-c("Clem")) # remove a name
# db[db$Date >= ymd("2020-12-01"), -1] <- 0 # Set December to 0
# db2$Total <- db2[,-1] %>% rowSums # make total
# db21 <- db21[, c(1, 2, 3, 4, 5, 8, 9, 10, 16)]
db21 <- db21 %>% select(-c(`NewNameTest`))
# db21[db21$Date == "20210409" %>% ymd(), "Virg "] <- 0
save(db21, file = filePathNew)
drop_upload(filePathNew, dtoken = token)

# Remove NA names of the month - !! change Month dates !!
filePath <- "database21.rdata"
drop_download(filePath, overwrite = T, dtoken = token)
load(file = filePath)
NA_names <- db[db$Date >= dmy("01.11.2020") & db$Date <= dmy("30.11.2020"), -1] %>% summarise_all(sum) %>% 
  select_if(function(col) col == 0) %>% colnames
db <- db %>% select(-NA_names) # remove NA names
save(db, file = filePath)
drop_upload(filePath, dtoken = token)

# Initial reset ----
db21 <- tibble(Date = c("2021-01-01" %>% ymd, "2021-01-01" %>% ymd + seq(0:363)), 
             `Ann'So` = 0,
             `Bérénice` = 0,
             `Bru` = 0,
             `Céline` = 0,
             `Etienne` = 0,
             Felipe = 0,
             Greg = 0,
             `J-V` = 0,
             `Karen Mc` = 0,
             Lucky = 0,
             Maxie = 0,
             `Mélanie` = 0,
             `Pauline V.` = 0,
             Prioschka = 0,
             Rose = 0,
             Sarah = 0,
             `Sarah Salvi` = 0,
             Virg = 0,
)
colnames(db21)[6] <- "Etienne \U0001f60a"
save(db21, file = "challenge/database21.rdata")


# 2022

filePath <- "database22.rdata"
filePathNew <- "database22.rdata"
token <- readRDS("droptoken.rds")

drop_download(filePath, overwrite = T, dtoken = token)
load(file = filePath)

db22 <- tibble(Date = c("2022-01-01" %>% ymd, "2022-01-01" %>% ymd + seq(0:363)), 
               `Anne-Sophie` = 0,
               `André` = 0,
               `Angelo` = 0,
               `Darryn` = 0,
               Fabrice = 0,
               `Fred` = 0,
               `Gurhan` = 0,
               Ingrid = 0,
               `Jean-Vincent` = 0,
               `Joao` = 0,
               `Julien` = 0,
               Louis = 0,
               Pamela = 0,
               Pauline = 0,
               Peter = 0,
               `Pierre-Nicolas` = 0,
               Robert = 0,
)

setwd("~/R Scripts/monthly-challenge/challenge")
save(db22, file = "database22.rdata")
drop_upload("database22.rdata", dtoken = token)

rm(db)
load(file = "challenge/database.rdata")


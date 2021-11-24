library(dplyr)
library(ggplot2)
library("readxl")
library(tidyr)
library(stringr)
library(readxl)
library(gdata)
library(xlsx)
library("writexl")
library(gtsummary)
library(openxlsx)

data <- read_csv("speech_data_genders.csv",header=TRUE, stringsAsFactors = TRUE)

levels(data_gendered$genders)
data_gendered <- droplevels(data_gendered)
data_gendered$person <- as.character(data_gendered$person)
data_gendered <- subset(data_gendered, select = -X)
names(data_gendered) <- c("Name", "Year", "Session_spoken", "Sentence_spoken", "Word_spoken", "Gender")

write.csv(data_gendered,"data_gendered.csv",row.names = FALSE)
MP_speech <- read.csv("data_gendered.csv",header=TRUE, stringsAsFactors = FALSE)


#CLEAN MP DATA
list <- c("ADINA","SÖZCÜSÜ","BAKANI","BAŞKAN")
MP_name_clean <- MP_speech[grep(paste(list,collapse="|"), MP_speech$FULL_NAME), ]
add_mp_speech_new <- MP_name_clean[c(1,2,3),]
MP_name_clean<- MP_name_clean[-c(1,2,3),]
MP_name_clean$word <- word(MP_name_clean$FULL_NAME,c(-1))
MP_name_clean$word2 <- word(MP_name_clean$FULL_NAME,c(-2))
MP_name_clean$word3 <- word(MP_name_clean$FULL_NAME,c(-3))
unique(MP_name_clean$word3)

list_erase <- c("BAŞKAN","BAŞKANI","BAŞKANI","BAKAN","BAKANI","YARDIMCISI","SÖZCÜSÜ", "KALKINMA","VEKELİ","BAŞKANVEKİLİ",
                "BAŞKAN","VEKİLİ","BAŞKANVELİ","ADINA","İYİ","CUMHURBAŞKANI","ADINA")
MP_name_clean$word3[MP_name_clean$word3 %in% list_erase] <-NA

MP_name_clean$word4 <- paste(MP_name_clean$word3,MP_name_clean$word2,MP_name_clean$word)
MP_name_clean$word4 <-gsub("NA ","",MP_name_clean$word4)
MP_name_clean <- subset(MP_name_clean, select=c(2,3,4,5,6,10))

MP_name_clean <- MP_name_clean[-grep("BAŞKAN", MP_name_clean$word4), ]
colnames(MP_name_clean)[6] <- "FULL_NAME"

MP_speech_new <-MP_speech[!grepl(paste(list,collapse="|"), MP_speech$FULL_NAME), ]
MP_speech_new <- rbind(add_mp_speech_new,MP_speech_new)

MP_data_clean <- rbind(MP_name_clean, MP_speech_new)
write.csv(MP_data_clean,"MP_data_clean.csv", row.names = FALSE)
x <- read.csv("MP_data_clean.csv")

####HERE CHECK GENDERS IN PYTHON- ADD GENDER &CLEAN AGAIN

c <- read.csv("cleaned_data_genders.csv", stringsAsFactors = FALSE)
write_xlsx(c,"cleaned_data_genders.xlsx")
c$Gender==c$genders
c <- c[,-c(1,6)]
colnames(c)[6] <- "Gender"
c[c$FULL_NAME=="DENİZ BAYKAL", "Gender"] = "male"

write.csv(c,"MP_data_final.csv", row.names = FALSE)
MP_data <- read.csv("MP_data_final.csv")
write_xlsx(MP_data,"MP_data_final.xlsx")


#####2007 ELECTION
#Introduce Elections
##2007
election_2007 = read_xlsx("2007-SECIM-EDITED.xlsx",col_names = FALSE)
election_2007 <- na.omit(election_2007)#Erase Party Names from Rows to Clean Data
election_2007 <- election_2007[,c(1,2)]#erase MESLEK
colnames(election_2007) <- c("X1","X2")
unique(election_2007$X2)
election_2007 <- election_2007[election_2007$X2!="ÖĞRENİM",]

election_2007$X1 <- str_squish(election_2007$X1)
election_2007$sıra <- word(election_2007$X1,c(1))
election_2007$isim1 <- word(election_2007$X1,c(2))
election_2007$isim2 <- word(election_2007$X1,c(3))
election_2007$isim3 <- word(election_2007$X1,c(4))
unique(election_2007$isim3)


election_2007[is.na(election_2007)] <- "" #replace NA with blank

election_2007$FULL_NAME <- paste(election_2007$isim1,election_2007$isim2,election_2007$isim3)
election_2007$FULL_NAME <- trimws(election_2007$FULL_NAME, "r") #erase whitespace at the end of strings

election_2007 <- election_2007[,c(2,3,7)]
colnames(election_2007) <- c("EGITIM","SIRA","FULL_NAME")

#UNITE
MP_data_2007 <- MP_data %>%
  filter(Year==2007|Year==2008|Year==2009|Year==2010)

data_united <- merge(election_2007, MP_data_2007, by="FULL_NAME", all.y = TRUE)
data_united <- data_united[rowSums(is.na(data_united)) == 0,]#because pthers are not MPs, they are invited to talk in the parliament

write.csv(data_united, "MP_SPEECH_ADAY_2007.csv",row.names = FALSE)
write.csv(election_2007,"adaylar_2007_final.csv",row.names = FALSE)

###### ELECTION 2011

election_2011 = read_xlsx("2011-seçim-edited.xlsx", col_names = FALSE)
election_2011 <- na.omit(election_2011) #Erase Party Names from Rows to Clean Data
colnames(election_2011) <- c("X1")

election_2011$X1 <- str_remove_all(election_2011$X1,"GAZETECİ| POLİS| EKONOMİST| DOKTOR|
                                   NOTER| MEMURU|AVUKAT| AVUKAT|EMEKLİ| İŞÇİ| NOTER| MEMURU| KAYMAKAM|BAĞIMSIZ")

election_2011$X1 <- str_squish(election_2011$X1)
election_2011$sıra <- word(election_2011$X1,c(1))
election_2011$EGITIM <- word(election_2011$X1,c(-1))
election_2011$isim1 <- word(election_2011$X1,c(2))
election_2011$isim2 <- word(election_2011$X1,c(3))
election_2011$isim3 <- word(election_2011$X1,c(4))
unique(election_2011$EGITIM)


election_2011$isim3 <- na_if(election_2011$isim3, "YÜKSEK")
election_2011$isim3 <- na_if(election_2011$isim3, "ORTA")
election_2011$isim3 <- na_if(election_2011$isim3, "İLK")

election_2011[is.na(election_2011)] <- "" #replace NA with blank

election_2011$FULL_NAME <- paste(election_2011$isim1,election_2011$isim2,election_2011$isim3)
election_2011$FULL_NAME <- trimws(election_2011$FULL_NAME, "r") #erase whitespace at the end of strings


election_2011 <- subset(election_2011, EGITIM!="ÖĞRENİM")
election_2011 <- subset(election_2011, EGITIM!="PARTİ")
election_2011 <- subset(election_2011, EGITIM!="PARTİSİ")
election_2011 <- subset(election_2011, EGITIM!="MESLEĞİ")
unique(election_2011$EGITIM)

election_2011 <- election_2011 %>% mutate_all(na_if,"")#replace all blanks are N
election_2011 <- na.omit(election_2011)


election_2011 <- election_2011[,c(2,3,7)]
election_2011[rowSums(is.na(election_2011))> 0,] #ALL empty rows
election_2011 <- na.omit(election_2011)
colnames(election_2011) <- c("SIRA","EGITIM","FULL_NAME")
election_2011 <- election_2011[,c(2,1,3)]

MP_data_2011 <- MP_data %>%
  filter(Year==2011|Year==2012|Year==2013|Year==2014)

data_united_2011 <- merge(election_2011, MP_data_2011, by="FULL_NAME", all.y = TRUE)
data_united_2011 <- data_united_2011[rowSums(is.na(data_united_2011)) == 0,]#because pthers are not MPs, they are invited to talk in the parliament

write.csv(data_united, "MP_SPEECH_ADAY_2011.csv",row.names = FALSE)
write.csv(election_2011,"adaylar_2011_final.csv",row.names = FALSE)


######2015 ELECTION

election_2015_2 = read.xlsx(xlsxFile = "2015.2-SEÇİM-EDİTED.xlsx", colNames = TRUE)
which(is.na(election_2015_2))

election_2015_2$FULL_NAME <- paste(election_2015_2$Adı,election_2015_2$Soyadı)

election_2015_2%>%
  filter(election_2018$EGITIM=="/"| "EMEKLİ")

unique(election_2011$EGITIM)
election_2015_2 <- election_2015_2[,c(3,4,5)]
colnames(election_2015_2) <- c("EGITIM","SIRA","FULL_NAME")


MP_data_2015_2 <- MP_data %>%
  filter(Year==2015|Year==2016|Year==2017|Year==2018)

data_united_2015_2 <- merge(election_2015_2, MP_data_2015_2, by="FULL_NAME", all.y = TRUE)
data_united_2015_2 <- data_united_2015_2[rowSums(is.na(data_united_2015_2)) == 0,]#because pthers are not MPs, they are invited to talk in the parliament

write.csv(data_united, "MP_SPEECH_ADAY_2015_2.csv",row.names = FALSE)
write.csv(election_2015_2,"adaylar_2015_2_final.csv",row.names = FALSE)

#2018 election

election_2018 = read.xlsx(xlsxFile = "2018-seçim-edited.xlsx", colNames = FALSE)

election_2018$X1 <- str_squish(election_2018$X1)
election_2018$X1 <- str_remove_all(election_2018$X1,"BAĞIMSIZ|EMEKLİ")

election_2018$EGITIM <- word(election_2018$X1,c(-1))
unique(election_2018$EGITIM)


election_2018 <- subset(election_2018, EGITIM!="PARTİSİ")
election_2018 <- subset(election_2018, EGITIM!="PARTİ")
election_2018 <- subset(election_2018, EGITIM!="EMEKLİ")
election_2018 <- subset(election_2018, EGITIM!="/")


DF_ADD <- data.frame(X1=c("2 ASAF KAYAOĞLU YÜKSEK","2 LÜTFİ DİLEK İLK"),EGITIM=c("ORTA","İLK")) #miswritten rows erased, and added.
election_2018 <- rbind(election_2018, DF_ADD)

election_2018$sıra <- word(election_2018$X1,c(1))
election_2018$isim1 <- word(election_2018$X1,c(2))
election_2018$isim2 <- word(election_2018$X1,c(3))
election_2018$isim3 <- word(election_2018$X1,c(4))

election_2018 <- election_2018 %>% mutate_all(na_if,"  ")
election_2018 <- na.omit(election_2018) #Erase Party Names from Rows to Clean Data

election_2018$isim3 <- na_if(election_2018$isim3, "YÜKSEK")
election_2018$isim3 <- na_if(election_2018$isim3, "ORTA")
election_2018$isim3 <- na_if(election_2018$isim3, "İLK")

election_2018[is.na(election_2018)] <- "" #replace NA with blank

election_2018$FULL_NAME <- paste(election_2018$isim1,election_2018$isim2,election_2018$isim3)
election_2011$FULL_NAME <- trimws(election_2011$FULL_NAME, "r") #erase whitespace at the end of strings

election_2018 <- election_2018[,c(2,3,7)]
colnames(election_2018) <- c("EGITIM","SIRA","FULL_NAME")

####2018 seçilenler
elected_2018 = read.xlsx(xlsxFile = "MP-2018-BÜYÜK HARF.xlsx", colNames = FALSE)
my_list <- list(elected_2018$X1)
my_list <-unlist(my_list)

MP_data_2018 <- MP_data[MP_data$FULL_NAME %in% my_list,]

MP_data_2018 <- MP_data %>%
  filter(FULL_NAME %in% my_list)

data_united_2018_seçilenler <- merge(election_2018, MP_data_2018, by="FULL_NAME", all.y = TRUE)
data_united_2018_seçilenler <- data_united_2018_seçilenler[rowSums(is.na(data_united_2018_seçilenler)) == 0,]#because pthers are not MPs, they are invited to talk in the parliament

write.csv(data_united, "MP-2018.csv",row.names = FALSE)
write.csv(election_2018,"adaylar_2018_final.csv",row.names = FALSE)

all_adaylar <- combine(election_2007,election_2011,election_2015_2,election_2018)
rownames(all_adaylar) <- 1:nrow(all_adaylar)
unique(all_adaylar$EGITIM)
write.csv(all_adaylar,"ALL_ADAYLAR.csv",row.names = FALSE)

All_speech <- combine(MP_data_2007,MP_data_2011,MP_data_2015_2)
write.csv(All_speech,"ALL_SPEECH.csv",row.names = FALSE)


MP <- read.csv("milletvekilleri.csv", header=TRUE, stringsAsFactors = FALSE)
MP <- MP[MP$Dönem!="21",]
MP <- MP[MP$Dönem!="22",]
MP <- MP[MP$Dönem!="20",]
MP <- MP[MP$Dönem!="25",]
unique(MP$Dönem)
MP$Dönem <- as.factor(MP$Dönem)
levels(MP$Dönem) <- list(elected_2007="23",elected_2011="24",elected_2015="26",elected_2018="27")
MP <- MP[,c(1,3)]

write.csv(all_elected,"(ALL_ELECTED.csv")

#####
all_elected = read.csv("ALL_ELECTED.csv")
all_elected <- all_elected[,c(2,3)]
all_elected$NAME <- tolower(all_elected$FULL_NAME)
all_elected$NAME <- toupper(all_elected$NAME)
all_elected$Dönem <- as.factor(all_elected$Dönem)
levels(all_elected$Dönem) <- list(election_2007="election_2007",election_2011="election_2011",
                                  election_2015_2="election_2015",election_2018="election_2018")

all_adaylar <- read.csv("ALL_ADAYLAR.csv")
all_adaylar$NAME <- tolower(all_adaylar$FULL_NAME)
all_adaylar$NAME <- toupper(all_adaylar$NAME)
all_adaylar$Dönem <- as.factor(all_adaylar$Dönem)
levels(all_adaylar$Dönem) <- list(election_2007="election_2007",election_2011="election_2011",
                                  election_2015_2="election_2015",election_2018="election_2018")


colnames(all_adaylar) <- c("EGITIM","SIRA","FULL_NAME","Dönem")
colnames(all_elected) <- c("FULL_NAME","Dönem")

all_adaylar$ELECTED <- NA

all_adaylar$Dönem <- as.character(all_adaylar$Dönem)
all_elected$Dönem <- as.character(all_elected$Dönem)

all_elected$NAME <- trimws(all_elected$NAME, "r") 
all_elected$NAME <- trimws(all_elected$NAME, "l") 
all_adaylar$NAME <- trimws(all_adaylar$NAME, "r") 
all_adaylar$NAME <- trimws(all_adaylar$NAME, "l")
all_elected$NAME <- str_squish(all_elected$NAME)
all_adaylar$NAME <- str_squish(all_adaylar$NAME)


all_adaylar$ELECTED <- ifelse(is.na(match(paste0(all_adaylar$NAME,all_adaylar$Dönem), 
                                paste0(all_elected$NAME,all_elected$Dönem))),"No", "Yes")

x <- all_adaylar[all_adaylar$ELECTED=="Yes",]

write.csv(all_adaylar,"ALL_ADAYLAR_SEÇİLDİMİ.csv", row.names = FALSE)
write.csv(all_elected,"ALL_ELECTED.csv", row.names = FALSE)

all_adaylar <- all_adaylar[,c(3,5,4,2,6,1)]

all_data$NAME <- tolower(all_data$FULL_NAME)
all_data$NAME <- toupper(all_data$NAME)

all_data$NAME <- trimws(all_data$NAME, "r") 
all_data$NAME <- trimws(all_data$NAME, "l") 
all_data$NAME <- str_squish(all_data$NAME)
all_data$NAME <- str_squish(all_data$NAME)

write.csv(all_elected,"ALL_SPEECH_FOR_ELECTED.csv", row.names = FALSE)

#####UNITE ALL DATA

e2007 <- read.csv("MP_SPEECH_ADAY_2007.csv", stringsAsFactors = FALSE)
e2011 <- read.csv("MP_SPEECH_ADAY_2011.csv", stringsAsFactors = FALSE)
e2015_2 <- read.csv("MP_SPEECH_ADAY_2015_2.csv", stringsAsFactors = FALSE)
e2018 <- read.csv("MP_SPEECH_ADAY_2018.csv", stringsAsFactors = FALSE)

all_data <- rbind(e2007,e2011,e2015_2)

####### SIRA DEĞİŞİMİ BUL

levels(all_adaylar$Dönem)
all_adaylar$Dönem <- factor(all_adaylar$Dönem,levels=c("election_2007","election_2011",
                            "election_2015_2","election_2018"), ordered = TRUE)
write_xlsx(all_adaylar,"ALL_ADAYLAR_son.xlsx")

install.packages("data.table")
library(data.table)
x <- setDT(all_adaylar)[order(Dönem),Diff := -(shift(SIRA, type= 'lead') - SIRA) ,
         by = NAME] #BİRDAHAKİ ADAY OLDUĞU ZAMAN NE KADAR DEĞİŞMİŞ YANİ MESELA 2011SIRASI-2007SIRASI FOR ALL ADAYLAR

ALL_SPEECH <- read.csv("ALL_SPEECH.csv")
ALL_SPEECH$Dönem <- NA

ALL_SPEECH$source <- as.factor(ALL_SPEECH$source)
levels(ALL_SPEECH$source) <-list(election_2007="MP_data_2007",election_2011="MP_data_2011",election_2015_2="MP_data_2015_2")
colnames(ALL_SPEECH)[7] <-"Dönem"

ALL_SPEECH$NAME <- tolower(ALL_SPEECH$FULL_NAME)
ALL_SPEECH$NAME <- toupper(ALL_SPEECH$FULL_NAME)
ALL_SPEECH$NAME <- trimws(ALL_SPEECH$NAME, "r") 
ALL_SPEECH$NAME <- trimws(ALL_SPEECH$NAME, "l") 
ALL_SPEECH$NAME <- str_squish(ALL_SPEECH$NAME)

ALL_SPEECH$merger <- paste0(ALL_SPEECH$NAME,ALL_SPEECH$Dönem)
x$merger <- paste0(x$NAME,x$Dönem)

ALL <- merge(ALL_SPEECH,x, by="merger", all=TRUE)

ALL_small <- ALL[,c(9,8,2,3,4,5,7,11,12,13,14,15,16)]

write.csv(ALL,"ALL.csv",row.names = FALSE)
write_xlsx(ALL_small,"ALL_with_erased_columns.xlsx",col_names = TRUE)
write_xlsx(all_adaylar,"ALL_ADAYLAR_ÖMER.xlsx",col_names = TRUE)


ALL_ADAYLAR_ = read_xlsx("ALL_ADAYLAR_ÖMER_SON.xlsx",col_names = TRUE)
ALL_ADAYLAR_$ADI <- word(ALL_ADAYLAR_$FULL_NAME,c(1))
write.csv(ALL_ADAYLAR_,"ALL_ADAYLAR_ÖMER_SON.csv",row.names = FALSE)

b <- read.csv("ALL_ADAYLAR_ÖMER_with_genders.csv")
b <- b[,c(2,3,4,5,6,7,8,9,11)]
b$EGITIM <- factor(b$EGITIM, levels=c("İLK","ORTA","YÜKSEK"))

b <- b %>% 
  mutate(ELECTED = recode(ELECTED, 
                    "Yes" = "1", 
                    "No" = "0"))
b$ELECTED <- factor(b$elected, levels=c(0,1))

b <- b %>% 
  mutate(genders = recode(genders, 
                          "female" = "1", 
                          "male" = "0"))
b$genders <- factor(b$genders, levels=c(0,1))
unique(b$Dönem)
b$EGITIM <- factor(b$EGITIM, levels=c("İLK","ORTA","YÜKSEK"))

b <- b %>%
 mutate(Dönem= replace(Dönem, Dönem=="election_2008"|Dönem=="election_2009"|Dönem=="election_2010", "election_2007"))
 
b$Dönem <- factor(b$Dönem, levels=c("election_2007","election_2011","election_2015_2","election_2018"))

İller_MV = read_xlsx("illerdekiMV.xlsx",col_names = FALSE)

colnames(İller_MV) <- c("CITY","İller_MV")

b <- b%>%
  right_join(İller_MV, by = 'CITY')

b$MV_sayısı <- ifelse(b$SIRA>(b$İller_MV/3),0,1)

colnames(b)[10] <-"İl_MV_Sayısı"

colnames(b)[11] <-"VULNERABILITY" #1 if vulnerable, 0 if not: vulnerability divided bu the MP numbers in the city divided by 3

b <- b %>%
  mutate(genders=replace(genders, FULL_NAME=="BERZİYA HACIMURATOĞLU"|FULL_NAME=="BEHZATİ UÇAR"|
                           FULL_NAME=="5 BARIŞ HACI"|
                           FULL_NAME=="DERKAY TAN"|
                           FULL_NAME=="ZİHNİGÜR DERELİ"| FULL_NAME=="DENİZ BAYKAL",0))
b <- b %>%
  mutate(genders=replace(genders, FULL_NAME=="REVİYYE ŞAHİDE AYGÜL"|FULL_NAME=="PARAŞİN GÜMÜŞ"|
                           FULL_NAME=="GÜLGÛN BUDAK"|FULL_NAME=="IŞIKGÜN AKFIRAT"|
                           FULL_NAME=="ASPORÇA MELİS KESER"|
                           FULL_NAME=="ZİHNİGÜR DERELİ"| FULL_NAME=="KÖNA HAZAR ÖZSOY",1))

levels(b$genders)
b$genders <- as.character(b$genders)

write.csv(b,"ALL_ADAYLAR_30_11.csv",row.names = FALSE)
  
adaylar <- read.csv("ALL_ADAYLAR_30_11.csv")
summary(adaylar)

#######
install.packages("gtsummary")
library(gtsummary)
library(tidyverse)

data_descibe <- adaylar %>%
  select(Gender,`Education Level`, Magnitude, Vulnerability)
tbl_summary(data_descibe)

colnames(adaylar) <- c("FULL_NAME","Name","Legislative Term", "List Position", "Elected", "Education Level","Change in List Position", "Constituency", "Gender", "Magnitude", "Vulnerability")

data_descibe$Vulnerability <- as.character(data_descibe$Vulnerability)
data_descibe$Gender <- as.factor(data_descibe$Gender)
data_descibe$Vulnerability <- as.factor(data_descibe$Vulnerability)

data_descibe$`Education Level` <- as.factor(data_descibe$`Education Level`)
levels(data_descibe$`Education Level`) <- c("Primary School","Secondary School","Post-Secondary School")

data_descibe$`Legislative Term` <- as.factor(data_descibe$`Legislative Term`)
levels(data_descibe$`Legislative Term`) <- c("2007 Election","2011 Election", "2015 Election", "2018 Election")

data_descibe <- adaylar %>%
  select(Gender,`Education Level`, Magnitude, Vulnerability,`Legislative Term`)



####
speech <- read.csv("ALL_SPEECH.csv")
ALL <- read.csv("ALL_with_erased_columns.csv")

speech <- speech %>%
  mutate(Gender=replace(Gender, Name=="BERZİYA HACIMURATOĞLU"|Name=="BEHZATİ UÇAR"|
                          Name=="5 BARIŞ HACI"|
                          Name=="DERKAY TAN"|
                          Name=="ZİHNİGÜR DERELİ"| Name=="DENİZ BAYKAL","male"))
speech <- speech %>%
  mutate(Gender=replace(Gender, Name=="REVİYYE ŞAHİDE AYGÜL"|Name=="PARAŞİN GÜMÜŞ"|
                          Name=="GÜLGÛN BUDAK"|Name=="IŞIKGÜN AKFIRAT"|
                          Name=="ASPORÇA MELİS KESER"|
                          Name=="ZİHNİGÜR DERELİ"| Name=="KÖNA HAZAR ÖZSOY","female"))
unique(speech$Gender)
speech[speech$Gender=="0",]
speech$Name[389] <- "NURSU MEMECAN"
speech$Name[c(4531,4532,4533,4534)] <- "SENA KALELİ"
speech$Name[8229] <- "SELİN DOĞAN"
speech$Gender[c(389,4531,4532,4533,4534,8229)] <- "female"
speech$Gender <- as.factor(speech$Gender)
levels(speech$Gender) <- c(1,0)

colnames(speech) <- c("Year","Sessions with speech","Sentence Spoken", "Word Spoken","Name", "Gender", "Legislative Term")

percent <- function(x, digits = 1, format = "f") {
  paste0(formatC(x/5, format = format, digits = digits), "%")
}

speech$`Sessions with speech (percent)` <- percent(speech$`Sessions with speech`)
percent_vec = paste(1:100, "%", sep = "")
speech$`Sessions with speech (percent)` <- as.numeric(sub("%", "", speech$`Sessions with speech (percent)`))

speech$`Sessions with speech (percent)` <- as.integer(speech$'Sessions with speech (percent)')

speech$`Legislative Term` <- as.factor(speech$`Legislative Term`)
levels(speech$`Legislative Term`) <- c("2007-2011","2011-2015", "2015-2018")


data_descibe2 <- data_united2 %>%
  select(Gender,'Sessions with speech', 'Sessions with speech (percent)','Word Spoken')
tbl_summary(data_descibe2)

colnames(data_descibe2) <- c('Sessions with speech', 'Sessions with speech (%)','Word Spoken', 'Gender')

tbl_summary(
 data_descibe2, 
  by = Gender,  # split table by group
  missing = "no" # don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

data_descibe2$Gender <- as.factor(data_descibe2$Gender)
levels(data_descibe2$Gender) <- c("Male","Female")

data_descibe$Magnitude <- as.factor(data_descibe$Magnitude)
xtable(summarize(data_descibe, type = "factor"))
print(a)
hender_table<- xtable(a)

##########################


adaylar$`Legislative Term` <- as.factor(adaylar$`Legislative Term`)

adaylar_elected <- subset(adaylar,adaylar$Elected==1)
levels(speech$`Legislative Term`) <- c("election_2007","election_2011","election_2015_2")

speech$Name <- tolower(speech$Name)
speech$Name <- toupper(speech$Name)

a$merger <- paste0(a$Name,a$`Legislative.Term`)
adaylar_elected$merger <- paste0(adaylar_elected$Name,adaylar_elected$`Legislative Term`)

which(a$Legislative.Term=="election_201")


write_xlsx(speech,"speech.xlsx")
a <- read.xlsx("speech.xlsx")
a <- subset(a,Year!=2018)
a$Name <- trimws(a$Name, "l")

x2 <- merge(x2, adaylar, by="merger", all.x = TRUE)
data_united_2011 <- data_united_2011[rowSums(is.na(data_united_2011)) == 0,]

which(is.na(data_united2))
write_xlsx(a,"speech.xlsx")
a <- read_xlsx("speech.xlsx")

data_united2 <- data_united2[is.na(data_united2$Name.y)==FALSE,]

s <- subset(data_united2,is.na(data_united2$Name.y)==TRUE)
View(which(is.na(data_united2$Name.y)==TRUE))


adaylar$Elected[17806] <- 1

data_united2[is.na(data_united2)] <- 0

data_united2$aynı <- paste(data_united2$Name.y,data_united2$Year)
write_xlsx(data_united2,"data_united2.xlsx")


final_data<- data_united2[,c(11,12,7,3,9,4,5,13,16,15,19,20)]
colnames(final_data) <- c("Name","Legislative_term","Female","Sessions_with_speech",
                          "Sessions_with_speech(%)","Sentence_spoken","Word_spoken",
                          "List_position","Change_in_list_position","Education_level",
                          "Magnitude","Vulnerability")
write.csv(final_data,"final_data_03Nov.csv",row.names = FALSE)
final_data$Legislative_term <- as.factor(final_data$Legislative_term)
levels(final_data$Legislative_term) <- c("2015_Election","2007_Election","2011_Election","2015_Election","2018_Election")
final_data$Legislative_term <- factor(final_data$Legislative_term, levels=c("2007_Election","2011_Election","2015_Election","2018_Election"))
final_data$Education_level <- as.factor(final_data$Education_level)
levels(final_data$Education_level)
final_data$Female <- as.factor(final_data$Female)



x <- final_data[,c(2,3,4,5,6,7,8,9,10,11,12)]
pairs(x)

final_data2 <- final_data
final_data2$merge <- paste(final_data$Name,final_data$Legislative_term)

final_data2 <- data_united2[,c(1,3,4,5)]
final_data2 <- aggregate(.~merger,data=final_data2,FUN=sum)

data_united2$aynı <- paste0(data_united2$'Name.y',data_united2$Year)
a <- data_united2[duplicated(data_united2$aynı),]

x$merge <- paste(a$Name,a$`Legislative.Term`)

final_data2 <- final_data2[,-c(1,2,4,5,6,7)]
final_data2$merger <- paste(final_data2$Name,final_data2$Legislative_term)

final_data2 <- final_data2[-c(1212,920,1213),]
adaylar$Name[23919] <- "BARIŞ HACI"

adaylar$merger <- paste0(adaylar$Name,adaylar$'Legislative Term')
write_xlsx(final_data2,"final_data2.xlsx")

final_data3 <- merge(final_data2,adaylar,by="merger", all.x=TRUE)

final_data3$ADI <- word(final_data3$Name,c(1))
write.csv(final_data3,"final_data3.csv")

final_data3[which(is.na(final_data3$Name))==0,]

which(is.na(final_data3$Name))
final_data3$Name[23] <- "ABDULLAH DOĞRU"

x_merged <- merge(x, final_data2, by="merge", all.y= TRUE)
x_merged <- x_merged %>% distinct()

model1 <- lm(data=x_merged,Change_in_list_position~Sessions_with_speech+Female)
summary(model1)

write_xlsx(x_merged,"x_merged.xlsx")
x_merged <- read.xlsx("x_merged.xlsx")

model3 <- lm(data=x_merged,Change_in_list_position~List_position+Sessions_with_speech+Female+Magnitude+Vulnerability+
               Education_level+Female*Vulnerability+Sessions_with_speech*Female)
summary(model3)

x_merged$Female <- as.factor(x_merged$Female)

model1 <- lm(formula = Change_in_list_position ~ Sessions_with_speech + List_position+
     Word_spoken + Female + Magnitude + Vulnerability + Education_level + 
     Vulnerability*Female + Sessions_with_speech * Female, data = final_data)
summary(model1)


model1 <- lm(formula = Change_in_list_position ~ List_position + Sessions_with_speech + 
               Word_spoken + Female + Magnitude + Vulnerability + Education_level + 
               Vulnerability*Female + Sessions_with_speech * Female, data = final_data)


x <- read.csv("final_data3_genders.csv")
x <- x %>%
  mutate(genders=replace(genders, Name=="BERZIYA HACIMURATOĞLU"|Name=="BEHZATI UÇAR"|
                          Name=="BARIŞ HACI"|
                          Name=="DERKAY TAN"|
                          Name=="ZIHNIGÜR DERELI"| Name=="DENIZ BAYKAL","0"))
x <- x %>%
  mutate(genders=replace(genders, Name=="REVIYYE ŞAHIDE AYGÜL"|Name=="PARAŞIN GÜMÜŞ"|
                          Name=="IŞIKGÜN AKFIRAT"|
                          Name=="ASPORÇA MELIS KESER"|
                          Name=="ZIHNIGÜR DERELI"| Name=="KÖNA HAZAR ÖZSOY","1"))

x <- x[,c(8,20,9,12,10,13,16,17,4,5,6)]

colnames(x) <- c("Name","Female","Legislative_term","Education_level","List_position",
                 "Change_in_list_position","Magnitude","Vulnerability",
                 "Sessions_with_speech","Sentence_spoken","Word_spoken")
x$Female <- as.factor(x$Female)
x$Vulnerability <- as.factor(x$Vulnerability)
x$Education_level <- as.factor(x$Education_level)
x$Legislative_term <- as.factor(x$Legislative_term)
levels(x$Legislative_term) <- c("2007_Election","2011_Election","2015_Election","2018_Election")
x <- x[-c(23),]
x$Female[c(382,383,384,393,1073,1074,1075)] <- "1"

write.csv(x,"FINAL_X.csv")
write_xlsx(x,"FINAL_X.xlsx")

x <- read.xlsx("FINAL_X.xlsx")






##########
x2 <- x[,x$Legislative_term!="2018_Election"]


model7 <- lm(formula = Change_in_list_position ~Sessions_with_speech+ Word_spoken+
               Female + Magnitude+Education_level*Female+Sessions_with_speech*Female+
               List_position*Female,data = a)
unique(a$Female)
summary(model7)
levels(a$Education_level) <- as.factor(a$Education_level)

x3 <- x[,c(2,3,4,5,6,7,8,9,10,11)]

x2 <- x
ggpairs(a)


x2[is.na(x2$Sessions_with_speech)] <- 0 #replace NA with blank
x2[is.na(x2$Sentence_spoken)] <- 0

x3 <- x2[-c(which(is.na(x2$Change_in_list_position))),]

which(is.na(x3))
x3[!complete.cases(x3),]

x3 <- subset(x2,Sessions_with_speech<300)

a$Female <- as.factor(a$Female)

######BOXPLOT

a_boxplot <- a[a$Legislative_term !="2018_Election",]
colnames(a_boxplot)[2] <- "Gender"
levels(a_boxplot$Gender) <- c("Male", "Female")
a_boxplot$Legislative_term <- as.factor(a_boxplot$Legislative_term)
levels(a_boxplot$Legislative_term) <- c("23rd Legislature", "24th Legislature", "26th Legislature")
a_boxplot$Legislative_term <- as.character(a_boxplot$Legislative_term)
unique(a_boxplot$Legislative_term)

ggplot(a_boxplot, aes(x=Legislative_term, y=Sessions_with_speech, fill=Gender)) + 
  geom_boxplot(outlier.alpha = 0.1)+ scale_x_discrete(name = "Legislative Term") +
  scale_y_continuous(name = "Sessions with Speech", limits=c(0,200))+
  theme_bw()+
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Paired")

##############

a%>%
  filter(Legislative_term=="2018_Election")%>%
  replace(Sessions_with_speech,NA)
a$Sessions_with_speech[which(a$Legislative_term =="2018_Election")] <- NA
a$Word_spoken[which(a$Legislative_term =="2018_Election")] <- NA
a$Sentence_spoken[which(a$Legislative_term =="2018_Election")] <- NA

write_xlsx(x3,"FINAL_X3.xlsx")

a <- read_xlsx("FINAL_X3.xlsx")

#######REGRESSION MODELS

install.packages("stargazer")
library(stargazer)

stargazer(a2)
summary(a)

a2 <- a2[,-c(2)]

a2$Higher_Education <- ifelse(a2$Education_level=="YÜKSEK", 1, 0)
levels(a$Education_level) <- c("Primary Education","Secondary Education","Post-Secondary Education")
a$Vulnerability <- as.factor(a$Vulnerability)

model1 <- lm(formula = Change_in_list_position ~ Sessions_with_speech + 
               Magnitude + Vulnerability+ Education_level + List_position, data = a)
model2 <- lm(formula = Change_in_list_position ~ Female + Magnitude + 
               Vulnerability+ Education_level + List_position, data = a)
model3 <- lm(formula = Change_in_list_position ~ Sessions_with_speech+ Female + Magnitude + 
               Vulnerability+ Education_level + List_position, data = a)
model4 <- lm(formula = Change_in_list_position ~ Sessions_with_speech + 
               +      Word_spoken + Female + Magnitude + Education_level * Female + 
               +      Sessions_with_speech * Female + List_position * Female, data = a)



stargazer(model1, model2, model3, model4,title="Results", align=TRUE)

partiler <- read.csv("milletvekilleri.csv")
partiler <- subset(partiler, Dönem>=23)
partiler <- subset(partiler, Dönem!=25)
partiler$İsim <- toupper(partiler$İsim)
partiler$Name <- partiler$İsim
partiler <- partiler[,c(2,4)]

write_xlsx(partiler, "partiler.xlsx")
write_xlsx(a, "a.xlsx")
partiler <- distinct(partiler)
View(a2[which(is.na(a2$Female)),])
partiler <- distinct(partiler$Name)
partiler <- distinct(partiler,Name, .keep_all= TRUE)
a2 <- left_join(a,partiler,by="Name")
View(a2[which(is.na(a2$Parti)),])
write_xlsx(a2, "a2.xlsx")

#######
a3 <- read.xlsx("a2.xlsx", rowNames = FALSE)

a3$Education_level <- as.factor(a3$Education_level)
a3$Female <- as.factor(a3$Female)
levels(a3$Education_level) <- c("Primary Education","Secondary Education","Post-Secondary Education")
a3$Vulnerability <- as.factor(a3$Vulnerability)
a3$Government_Party <- ifelse(a3$Parti=="AK Parti",1,0)
a3$Government_Party <- as.factor(a3$Government_Party)
a3$Legislative_term <- as.factor(a3$Legislative_term)
a3$Parti <- as.factor(a3$Parti)
subset(a3,a3$Name=="DURDU MEHMET KASTAL")
a3$Female[c(526,527)] <- 0

a3 <- a3%>%
  group_by(Parti)%>%
  mutate(Party_Female_Ratio= sum(Female==1)/sum(Female==0))

unique(a3$Party_Female_Ratio)
which(a3$Party_Female_Ratio==Inf)
a3$Party_Female_Ratio[c(570,1421,1854)] <- 0

  



model1 <- lm(formula = Change_in_list_position ~ Sessions_with_speech + Word_spoken+
               Magnitude + Education_level +Vulnerability + List_position+ Government_Party, data = a3)

model2 <- lm(formula = Change_in_list_position ~ Female + Magnitude + 
               Education_level+ List_position +Vulnerability +Government_Party*Female, data = a3)

model3 <- lm(formula = Change_in_list_position ~ Sessions_with_speech+Word_spoken+ Female + Magnitude + 
               Vulnerability+ Education_level + List_position +Government_Party*Female, data = a3)

model4 <- lm(formula = Change_in_list_position ~ Sessions_with_speech:Female+Word_spoken:Female+
               Magnitude + Education_level +Vulnerability + List_position+ Government_Party, data = a3)

summary(model1)
summary(model2)
summary(model3)
summary(model4)

stargazer(model1, model2, model3, model4, title="Regression Results",
align=TRUE,omit.stat=c("LL","ser","f"), no.space=TRUE)

model1 <- lm(formula = List_position ~ Sessions_with_speech + Word_spoken + Vulnerability + as.factor(Name), data = a3)
model3 <- lm(formula = List_position ~ Sessions_with_speech*Female+Word_spoken+ Vulnerability + as.factor(Name), data = a3)
model4 <- lm(formula = List_position ~ Sessions_with_speech*Female+Word_spoken*Female +  Vulnerability + as.factor(Name), data = a3)

summary(model1)
summary(model2)
summary(model3)

stargazer(model1, model3, model4,
          omit = c("as.factor"), type ="text")


model1 <- lm(formula = List_position ~ Sessions_with_speech + log(Word_spoken+1) + log(Sentence_spoken+1) + as.factor(Name), data = a3)
model2 <- lm(formula = List_position ~ Sessions_with_speech*as.factor(Female)+log(Word_spoken+1)+ log(Sentence_spoken+1) + as.factor(Name), data = a3)
model3 <- lm(formula = List_position ~ Sessions_with_speech+log(Word_spoken+1)*as.factor(Female)+ log(Sentence_spoken+1) + as.factor(Name), data = a3)
model4 <- lm(formula = List_position ~ Sessions_with_speech+log(Word_spoken+1) + log(Sentence_spoken+1) *as.factor(Female) + as.factor(Name), data = a3)
model5 <- lm(formula = List_position ~ Sessions_with_speech*Female+log(Word_spoken+1)*as.factor(Female) +log(Sentence_spoken+1)*Female + as.factor(Name), data = a3) 

stargazer(model1, model2, model3,model4,model5,
          omit = c("as.factor"), type ="text")

install.packages("lfe")
library("lfe")

model1 <-felm(formula = List_position ~ Sessions_with_speech + log(Word_spoken+1) + log(Sentence_spoken+1) | Name | 0 | Legislative_term, data = a3)
model2 <- felm(formula = List_position ~ Sessions_with_speech*Female+log(Word_spoken+1)+ log(Sentence_spoken+1) + as.factor(Name) | Name | 0 | Legislative_term, data = a3)
model3 <- felm(formula = List_position ~ Sessions_with_speech+log(Word_spoken+1)*Female+ log(Sentence_spoken+1)   | Name | 0 | Legislative_term, data = a3)
model4 <- felm(formula = List_position ~ Sessions_with_speech+log(Word_spoken+1) + log(Sentence_spoken+1) *Female  | Name | 0 | Legislative_term, data = a3)
model5 <- felm(formula = List_position ~ Sessions_with_speech*Female+log(Word_spoken+1)*Female +  +log(Sentence_spoken+1)*Female | Name | 0 | Legislative_term, data = a3)


##########THE ONE
model1 <- felm(formula = List_position ~ Sessions_with_speech  | Name | 0 | Legislative_term, data = a3)
model2 <- felm(formula = List_position ~ log(Word_spoken+1)  | Name | 0 | Legislative_term, data = a3)
model3 <- felm(formula = List_position ~ log(Sentence_spoken+1)  | Name | 0 | Legislative_term, data = a3)
model4 <- felm(formula = List_position ~ Sessions_with_speech+ Sessions_with_speech:Female | Name | 0 | Legislative_term, data = a3)
model5 <- felm(formula = List_position ~ log(Word_spoken+1) + log(Word_spoken+1):Female  | Name | 0 | Legislative_term, data = a3)
model6 <- felm(formula = List_position ~ log(Sentence_spoken+1) +  log(Sentence_spoken+1):Female  | Name | 0 | Legislative_term, data = a3)

stargazer(model1, model2, model3,model4,model5,model6,
          omit = c("as.factor"),type ="text")
stargazer(model1, model2, model3,model4,model5,model6,
          omit = c("as.factor"))


############ WITH ELECTION FIXED EFFECTS
model1 <- felm(formula = List_position ~ Sessions_with_speech  | Name + Legislative_term | 0 | Legislative_term, data = a3)
model2 <- felm(formula = List_position ~ log(Word_spoken+1)  | Name + Legislative_term | 0 | Legislative_term, data = a3)
model3 <- felm(formula = List_position ~ log(Sentence_spoken+1)  | Name + Legislative_term | 0 | Legislative_term, data = a3)
model4 <- felm(formula = List_position ~ Sessions_with_speech+ Sessions_with_speech:Female | Name + Legislative_term | 0 | Legislative_term, data = a3)
model5 <- felm(formula = List_position ~ log(Word_spoken+1) + log(Word_spoken+1):Female  | Name + Legislative_term | 0 | Legislative_term, data = a3)
model6 <- felm(formula = List_position ~ log(Sentence_spoken+1) +  log(Sentence_spoken+1):Female  | Name + Legislative_term | 0 | Legislative_term, data = a3)

stargazer(model1, model2, model3,model4,model5,model6,
          omit = c("as.factor"),type ="text")
stargazer(model1, model2, model3,model4,model5,model6,
          omit = c("as.factor"))

############ SUBSET PARTIES

unique(a3$Parti)

a3_akp <-subset(a3,a3$Parti=="AK Parti")
a3_chp <-subset(a3,a3$Parti=="CHP")
a3_mhp <-subset(a3,a3$Parti=="MHP")
a3_hdp <-subset(a3,a3$Parti=="HDP")
a3_iyi <-subset(a3,a3$Parti=="İYİ Parti")


model1_akp <- felm(formula = List_position ~ log(Word_spoken+1)  | Name | 0 | Legislative_term, data = a3_akp)
model2_akp <- felm(formula = List_position ~ log(Sentence_spoken+1)  | Name | 0 | Legislative_term, data = a3_akp)
model3_akp <- felm(formula = List_position ~ Sessions_with_speech+ Sessions_with_speech:Female | Name | 0 | Legislative_term, data = a3_akp)
model4_akp <- felm(formula = List_position ~ log(Word_spoken+1) + log(Word_spoken+1):Female  | Name | 0 | Legislative_term, data = a3_akp)
model5_akp <- felm(formula = List_position ~ log(Sentence_spoken+1) +  log(Sentence_spoken+1):Female  | Name | 0 | Legislative_term, data = a3_akp)

stargazer(model1_akp, model2_akp, model3_akp,model4_akp,model5_akp,
          omit = c("as.factor"),type ="text")



model1_chp <- felm(formula = List_position ~ log(Word_spoken+1)  | Name | 0 | Legislative_term, data = a3_chp)
model2_chp <- felm(formula = List_position ~ log(Sentence_spoken+1)  | Name | 0 | Legislative_term, data = a3_chp)
model3_chp <- felm(formula = List_position ~ Sessions_with_speech+ Sessions_with_speech:Female | Name | 0 | Legislative_term, data = a3_chp)
model4_chp <- felm(formula = List_position ~ log(Word_spoken+1) + log(Word_spoken+1):Female  | Name | 0 | Legislative_term, data = a3_chp)
model5_chp <- felm(formula = List_position ~ log(Sentence_spoken+1) +  log(Sentence_spoken+1):Female  | Name | 0 | Legislative_term, data = a3_chp)

stargazer(model1_chp, model2_chp, model3_chp,model4_chp,model5_chp,
          omit = c("as.factor"),type ="text")


model1_hdp <- felm(formula = List_position ~ log(Word_spoken+1)  | Name | 0 | Legislative_term, data = a3_hdp)
model2_hdp <- felm(formula = List_position ~ log(Sentence_spoken+1)  | Name | 0 | Legislative_term, data = a3_hdp)
model3_hdp <- felm(formula = List_position ~ Sessions_with_speech+ Sessions_with_speech:Female | Name | 0 | Legislative_term, data = a3_hdp)
model4_hdp <- felm(formula = List_position ~ log(Word_spoken+1) + log(Word_spoken+1):Female  | Name | 0 | Legislative_term, data = a3_hdp)
model5_hdp <- felm(formula = List_position ~ log(Sentence_spoken+1) +  log(Sentence_spoken+1):Female  | Name | 0 | Legislative_term, data = a3_hdp)

stargazer(model1_hdp, model2_hdp, model3_hdp,model4_hdp,model5_hdp,
          omit = c("as.factor"),type ="text")


model1_mhp <- felm(formula = List_position ~ log(Word_spoken+1)  | Name | 0 | Legislative_term, data = a3_mhp)
model2_mhp <- felm(formula = List_position ~ log(Sentence_spoken+1)  | Name | 0 | Legislative_term, data = a3_mhp)
model3_mhp <- felm(formula = List_position ~ Sessions_with_speech+ Sessions_with_speech:Female | Name | 0 | Legislative_term, data = a3_mhp)
model4_mhp <- felm(formula = List_position ~ log(Word_spoken+1) + log(Word_spoken+1):Female  | Name | 0 | Legislative_term, data = a3_mhp)
model5_mhp <- felm(formula = List_position ~ log(Sentence_spoken+1) +  log(Sentence_spoken+1):Female  | Name | 0 | Legislative_term, data = a3_mhp)

stargazer(model1_mhp, model2_mhp, model3_mhp,model4_mhp,model5_mhp,
          omit = c("as.factor"),type ="text")


stargazer(model4_akp, model4_chp, model4_mhp,model4_hdp,model5_akp, model5_chp, model5_mhp,model5_hdp,
          omit = c("as.factor"),type ="text")
stargazer(model4_akp, model4_chp, model4_mhp,model4_hdp,model5_akp, model5_chp, model5_mhp,model5_hdp,
          omit = c("as.factor"))
#####

a3$Legislative_term <- as.factor(a3$Legislative_term)

x <- a3%>%
  group_by(Parti)%>%
  count(a3$Legislative_term,a3$Female)
x1 <- subset(x,Parti=="AK Parti")
x2 <- subset(x,Parti=="CHP")
x3 <- subset(x,Parti=="MHP")
x4 <- subset(x,Parti=="HDP")
x <- rbind(x1,x2,x3,x4)


x <- x[,Parti=="AK Parti"|"CHP"|"MHP"|"HDP"]



######

a3 <- read.xlsx("a2.xlsx", rowNames = FALSE)
adaylar <- read.csv("ALL_ADAYLAR_30_11.csv")

unique(a3$Legislative_term)

adaylar$Dönem <- as.factor(adaylar$Dönem)
levels(adaylar$Dönem) <- c("2002_Election","2007_Election","2011_Election","2015_Election")
adaylar$merger <- paste0(adaylar$NAME,adaylar$Dönem)
a3$merger <- paste0(a3$Name,a3$Legislative_term)
adaylar <- adaylar[,c(4,12)]

data_united <- merge(adaylar, a3, by="merger", all.y = TRUE)


model1 <- felm(formula = SIRA~ Sessions_with_speech  | Name | 0 | Legislative_term, data = data_united)
model2 <- felm(formula = SIRA ~ log(Word_spoken+1)  | Name | 0 | Legislative_term, data = data_united)
model3 <- felm(formula = SIRA ~ log(Sentence_spoken+1)  | Name | 0 | Legislative_term, data = data_united)
model4 <- felm(formula = SIRA ~ Sessions_with_speech+ Sessions_with_speech:Female | Name | 0 | Legislative_term, data = data_united)
model5 <- felm(formula = SIRA ~ log(Word_spoken+1) + log(Word_spoken+1):Female  | Name | 0 | Legislative_term, data = data_united)
model6 <- felm(formula = SIRA~ log(Sentence_spoken+1) +  log(Sentence_spoken+1):Female  | Name | 0 | Legislative_term, data =data_united)

stargazer(model1, model2, model3,model4,model5,model6,
          omit = c("as.factor"),type ="text")
stargazer(model1, model2, model3,model4,model5,model6,
          omit = c("as.factor"))

library(stargazer)





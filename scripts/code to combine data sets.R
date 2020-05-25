library(readxl)
library(dplyr)

setwd("C:/Users/Kasia/Dropbox/Phethornis synchro")

lbh <- read_excel("LBH_rec_files.xlsx")
wwh1 <- read_excel("WWH_rec_files_p1.xlsx")
wwh2 <- read_excel("WWH_rec_files_p2.xlsx")

# To check two sets of WWH for doble ind
# rw1 <- unique(wwh1$BirdRing)
# rw2 <- unique(wwh2$BirdRing)
# intersect(rw1, rw2)


# Little adjustment of three data sets

nwwh1 <- nrow(wwh1)
wwh1 <- wwh1 %>% 
  select(Idrec_sub, Id_cowlog, Lek, BirdRing, Date) %>%
  rename(BirdID = BirdRing) %>%
  mutate(Species = rep("WWH", nwwh1))

nwwh2 <- nrow(wwh2)
wwh2 <- wwh2 %>% 
  select(Idrec_sub, Id_cowlog, Lek, BirdRing, Date)%>%
  rename(BirdID = BirdRing) %>%
  mutate(Species = rep("WWH", nwwh2))

lhb <- nrow(lbh)
lbh <- lbh %>% select(RecID, CowlogFile, Lek, BirdID, Date) %>%
  rename("Idrec_sub" = RecID , "Id_cowlog" :=  CowlogFile ) %>%
  mutate(Species = rep("LBH", lhb))


all <- rbind(lbh, wwh1, wwh2)
write.csv(file = "recfiles_descr.csv", all)
#################
# Quick summary
#################

# Total n clips
sum(table(all$BirdID))

# N clips per ind
max(table(all$BirdID))
min(table(all$BirdID))
mean(table(all$BirdID))

# N inds0 screened
all %>% group_by(Species, BirdID) %>% summarise (n = n()) %>% 
  group_by(Species) %>% summarise (n = n())


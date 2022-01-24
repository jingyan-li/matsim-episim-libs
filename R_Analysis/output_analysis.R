if(!require('tidyverse')){install.packages('tidyverse'); library('tidyverse')}
if(!require('reshape2')){install.packages('reshape2'); library('reshape2')}
if(!require('ggplot2')){install.packages('ggplot2'); library('ggplot2')}
if(!require('pmdplyr')){install.packages('pmdplyr'); library('pmdplyr')}


#load data and choose variables
actual <- read.csv('https://raw.githubusercontent.com/openZH/covid_19/master/fallzahlen_kanton_total_csv/COVID19_Fallzahlen_Kanton_ZH_total.csv')
actual <- actual %>% select(date, ncumul_hosp, ncumul_ICU)
colnames(actual) <- c("date", "actual_hospitalisation", "actual_ICU")
episim1 <- read.delim('//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/episim-output/infections.txt', header = TRUE, sep = "\t")
episim1 <- episim1 %>% select(date, nSeriouslySick, nCritical)
colnames(episim1) <- c("date", "episim1_seriously_sick", "episim1_critical")

#bring all together
compare <- actual %>% right_join(episim1)

#plot it
hosp_data <- compare %>% select(date, episim1_seriously_sick, actual_hospitalisation) %>% 
  gather(key = "variable", value = "value", -date) %>%
  filter(date > "2020-02-27" & date <"2020-07-05")

ggplot(hosp_data, aes(x = date, y = value, group = variable)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  ggtitle("Hospitalisations")

ICU_data <- compare %>% select(date, episim1_critical, actual_ICU) %>% 
  gather(key = "variable", value = "value", -date) %>%
  filter(date > "2020-02-27" & date <"2020-07-05")

ggplot(ICU_data, aes(x = date, y = value, group = variable)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  ggtitle("ICU")

test_data <- compare %>% filter(date > "2020-02-27" & date <"2020-07-05")
ks.test(test_data$actual_hospitalisation, test_data$episim1_seriously_sick)


###################################################################################
#################################################################################
#load all outputs

options(scipen = 100)
all <- as.data.frame(seq(as.Date("2020-02-05"), as.Date("2021-12-01"), by="days"))
colnames(all) <- "date"


sd <- list.files("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration")
sd <- sd[startsWith(sd, "sd_")] %>% substr(4, 14)
for (d in 1:length(sd)) {
  pt <- list.files(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/sd_",sd[d]))
  pt <- pt[startsWith(pt, "pt_")] %>% substr(4, 14)
for (p in 1:length(pt)) {
  cal <- list.files(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/sd_",sd[d] ,"/pt_", pt[p]))
  cal <- cal[startsWith(cal, "cal_")] %>% substr(5, 15)
  for (c in 1:length(cal)) {
    seed <- list.files(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/sd_",sd[d] ,"/pt_",pt[p],"/cal_",cal[c]))
    seed <- seed[startsWith(seed, "seed_")] %>% substr(6, 16)
    for (s in 1:length(seed)) {
      inp <- c(sd[d], pt[p],cal[c],seed[s])
      temp1 <- read.delim(paste0('//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/sd_',inp[1] ,'/pt_',inp[2] ,'/cal_',inp[3],'/seed_',inp[4],'/infections.txt'), header = TRUE, sep = "\t")
      temp2 <- temp1 %>% select(date, nSeriouslySick, nCritical, nTotalInfected, nShowingSymptoms)
      colnames(temp2) <- c("date", paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_seriously_sick"), 
                           paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_critical"),
                           paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_infected"),
                           paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_symptoms"))
      temp2 <- temp2 %>% mutate(date = as.Date(date))
      all <- all %>% left_join(temp2, by="date")
    }
  }
}
}

colnames(all)



list.files(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/sd_",sd[d] ,"/pt_",pt[p],"/cal_",cal[c]))

actual <- read.csv('https://raw.githubusercontent.com/openZH/covid_19/master/fallzahlen_kanton_total_csv/COVID19_Fallzahlen_Kanton_ZH_total.csv')
actual <- actual %>% select(date, ncumul_hosp, ncumul_ICU, ncumul_conf) %>% mutate(actual_conf = ncumul_conf  - lag(ncumul_conf))
colnames(actual) <- c("date", "actual_hospitalisation", "actual_ICU", "actual_conf_cumul", "actual_conf")

###################################################################################
#4.1 Results compared to official data
###################################################################################
# choose what you want to see
date <- "2020-02-04"
sample <- "0.25"
calpara <- "0.0000095"
###################################################################################

#Hospitalisations : 
hosp <- all %>% select(date, contains("seriously_sick")) %>% 
  select(date, contains(paste0(date))) %>% 
  select(date, contains(paste0("_", sample, "_"))) %>%
  select(date, contains(paste0("_", calpara, "_")))

compare <- actual %>% 
  mutate(date = as.Date(date)) %>% 
  select(date, actual_hospitalisation) %>% 
  right_join(hosp, by="date")

hosp_data <- compare %>%
  gather(key = "variable", value = "value", -date) %>%
  filter(date > "2020-02-22" & date <"2020-07-05")
actual_data <- hosp_data %>% filter(variable=="actual_hospitalisation")
# mutate(value = (1.2/1.5)*value)

hosp_data <- hosp_data %>% filter(variable!="actual_hospitalisation")
interval_95 <- hosp_data %>% group_by(date) %>% 
  summarise(lower = quantile(value, 0.025, na.rm = T), upper = quantile(value, 0.975, na.rm = T))
interval_65 <- hosp_data %>% group_by(date) %>% 
  summarise(lower = quantile(value, 0.175, na.rm = T), upper = quantile(value, 0.825, na.rm = T))



# ggplot(hosp_data, aes(x = date, y = value, group = variable)) + 
#   geom_line(aes(color = variable, linetype = variable)) +
#   geom_line(, col = "#ff5a32", size = 2)
#   ggtitle("Hospitalisations")


  
ggplot(hosp_data) +
  geom_ribbon(data = interval_95, aes(x = date, ymin=lower, ymax=upper), fill = "#c4c3c2") +
  geom_ribbon(data = interval_65, aes(x = date, ymin=lower, ymax=upper), fill = "#8f8f8f") +
  geom_line(aes(x = date, y = value, group = variable, color="simulations")) +
  geom_line(data = actual_data, aes(x = date, y = value,  color="actual"), size = 1.5) +
  scale_colour_manual(values = c( "simulations" = "black", "actual" = "red")) +
  ggtitle("Hospitalisations")


###################################################################################

#ICU
icu <- all %>% select(date, contains("critical")) %>% 
  select(date, contains(paste0(date))) %>% 
  select(date, contains(paste0("_", sample, "_"))) %>%
  select(date, contains(paste0("_", calpara, "_")))
compare <- actual %>% mutate(date = as.Date(date)) %>% select(date, actual_ICU) %>% right_join(icu, by="date")

icu_data <- compare %>%
  gather(key = "variable", value = "value", -date) %>%
  filter(date > "2020-02-07" & date <"2020-07-05")
actual_data_icu <- icu_data %>% filter(variable=="actual_ICU")
icu_data <- icu_data %>% filter(variable!="actual_ICU")
interval_icu <- icu_data %>% group_by(date) %>% 
  summarise(lower = quantile(value, 0.05, na.rm = T), upper = quantile(value, 0.95, na.rm = T))

ggplot(icu_data) +
  geom_ribbon(data = interval_icu, aes(x = date, ymin=lower, ymax=upper), fill = "#c4c3c2") +
  geom_line(aes(x = date, y = value, group = variable,  color="simulations")) +
  geom_line(data = actual_data_icu, aes(x = date, y = value,  color="actual"), size = 1.5) +
  scale_colour_manual(values = c( "simulations" = "black", "actual" = "red")) +
  ggtitle("ICU")


###################################################################################

#cases daily
cases <- all %>% select(date, contains("infected")) %>% 
  select(date, contains(paste0(date))) %>% 
  select(date, contains(paste0("_", sample, "_"))) %>%
  select(date, contains(paste0("_", calpara, "_")))
compare <- actual %>% mutate(date = as.Date(date)) %>% select(date, actual_conf) %>% right_join(cases, by="date")

cases_data <- compare %>%
  gather(key = "variable", value = "value", -date) %>%
  filter(date > "2020-02-07" & date <"2020-07-05")
actual_data_cases <- cases_data %>% filter(variable=="actual_conf")
cases_data <- cases_data %>% filter(variable!="actual_confl")

interval_cases <- cases_data %>% group_by(date) %>% 
  summarise(lower = quantile(value, 0.05, na.rm = T), upper = quantile(value, 0.95, na.rm = T))

ggplot(cases_data) +
  geom_ribbon(data = interval_cases, aes(x = date, ymin=lower, ymax=upper), fill = "#c4c3c2") +
  geom_line(aes(x = date, y = value, group = variable)) +
  geom_line(data = actual_data_cases, aes(x = date, y = value), colour="red", size = 1.5) +
  ggtitle("Infected and confirmed cases Daily")


symptoms <- all %>% select(date, contains("symptoms")) %>% 
  select(date, contains(paste0(date))) %>% 
  select(date, contains(paste0("_", sample, "_"))) %>%
  select(date, contains(paste0("_", calpara, "_")))
symptoms_7d_mean <- symptoms %>% gather(key = "variable", value = "value", -date) %>%
  group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T)) %>%
  mutate(symptoms_7d_mean = (lag(mean_simul, n = 3L) + lag(mean_simul, n = 2L) + lag(mean_simul, n = 1L) + mean_simul +
                            lead(mean_simul, n = 3L) + lead(mean_simul, n = 2L) + lead(mean_simul, n = 1L))/7 ) %>%
  select(date, symptoms_7d_mean)

actual_7d_cases <- actual_data_cases %>% mutate(actual_7d_mean = (lag(value, n = 3L) + lag(value, n = 2L) + lag(value, n = 1L) + value + 
                                                    lead(value, n = 3L) + lead(value, n = 2L) + lead(value, n = 1L))/7 ) %>%
  select(date, actual_7d_mean)

compare_cases <- cases_data %>% group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T)) %>%
  mutate(simucases_7d_mean = (lag(mean_simul, n = 3L) + lag(mean_simul, n = 2L) + lag(mean_simul, n = 1L) + mean_simul +
                             lead(mean_simul, n = 3L) + lead(mean_simul, n = 2L) + lead(mean_simul, n = 1L))/7 ) %>% 
  right_join(actual_7d_cases) %>% left_join(symptoms_7d_mean) %>% select(date, actual_7d_mean, simucases_7d_mean, symptoms_7d_mean) %>%
  mutate(share_sympt = actual_7d_mean/symptoms_7d_mean, share_cases = actual_7d_mean/simucases_7d_mean) %>% 
  mutate(date = as.Date(date)) %>% filter(date>"2020-03-01" & date<"2020-06-01")
compare_cases_data <- compare_cases %>% select(date, actual_7d_mean, simucases_7d_mean, symptoms_7d_mean) %>% 
  rename( "Actual Confirmed Cases" = actual_7d_mean, "Daily Cases Simulated" = simucases_7d_mean, "Daily Cases with Symptoms Simulated"= symptoms_7d_mean) %>% 
  gather(key = "variable", value = "value", -date)


ggplot(compare_cases_data, aes(x = date, y = value, fill=variable)) +
  geom_line(aes(colour=variable)) +
  scale_color_manual(name="Lines",values=c("red", "blue", "black"))

compare_cases_share_data <- compare_cases %>% select(date, share_sympt, share_cases) %>% 
  rename( "Share of Symtoms Detected" = share_sympt, "Share of Cases Detected" = share_cases) %>% 
  gather(key = "variable", value = "value", -date)

ggplot(compare_cases_share_data, aes(x = date, y = value, fill=variable)) +
  geom_line(aes(colour=variable)) +
  scale_color_manual(name="Lines",values=c("blue", "black")) +
  ggtitle("Share of cases and share of cases with symptoms detected")

ggplot(compare_cases) +
  geom_line(aes(x = date, y = share_sympt)) +
  geom_line(aes(x = date, y = share_cases), linetype = "dashed") +
  ggtitle("Share of cases and share of cases with symptoms detected")

###################################################################################

#cases dcumulated
cases_cumu <- all %>% select(date, contains("infected")) %>% 
  select(date, contains(paste0(date))) %>% 
  select(date, contains(paste0("_", sample, "_"))) %>%
  select(date, contains(paste0("_", calpara, "_")))
compare <- actual %>% mutate(date = as.Date(date)) %>% select(date, actual_conf_cumul) %>% right_join(cases_cumu, by="date")

cases_cumu_data <- compare %>%
  gather(key = "variable", value = "value", -date) %>%
  filter(date > "2020-02-07" & date <"2020-07-05")
actual_data_cases_cumu <- cases_cumu_data %>% filter(variable=="actual_conf_cumul")
cases_cumu_data <- cases_cumu_data %>% filter(variable!="actual_conf_cumul") %>%
  arrange(date) %>% 
  group_by(variable) %>%
  mutate(value =cumsum(value))

interval_cases_cumu <- cases_cumu_data %>% group_by(date) %>% 
  summarise(lower = quantile(value, 0.05, na.rm = T), upper = quantile(value, 0.95, na.rm = T))

ggplot(cases_cumu_data) +
  geom_ribbon(data = interval_cases_cumu, aes(x = date, ymin=lower, ymax=upper), fill = "#c4c3c2") +
  geom_line(aes(x = date, y = value, group = variable,  color="simulations")) +
  geom_line(data = actual_data_cases_cumu, aes(x = date, y = value,  color="actual"), size = 1.5) +
  scale_colour_manual(values = c( "simulations" = "black", "actual" = "red")) +
  ggtitle("Infected and confirmed cases Cumulative")


###################################################################################
# 4.2 Transmission Analysis
###################################################################################
options(scipen = 100)
all_infev <- c()

seed <- list.files(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/sd_2020-02-04/pt_0.25/cal_0.0000095"))
seed <- seed[startsWith(seed, "seed_")] %>% substr(6, 16)
for (s in 1:length(seed)) {
  inp <- seed[s]
  temp1 <- read.delim(paste0('//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/sd_2020-02-04/pt_0.25/cal_0.0000095/seed_',inp,'/infectionEvents.txt'), header = TRUE, sep = "\t")
  temp2 <- temp1 %>% select(date, infector, infected, infectionType) %>% mutate(seed = inp)
  all_infev <- rbind(all_infev,temp2)
}

#look at superspreders

supspr <- all_infev %>% group_by(infector, seed) %>% summarise(transmissions = n())
Transmissions <- supspr$transmissions
hist(Transmissions, breaks = 16)

#look at transmission places
places <- as.data.frame(cbind(rep(unique(all_infev$infectionType), times =length(unique(all_infev$seed))),rep(unique(all_infev$seed), each=length(unique(all_infev$infectionType)))))
colnames(places) <- c("infectionType", "seed")
temp1 <- all_infev %>% group_by(infectionType, seed) %>% summarise(transmissions = n()) %>% left_join(all_infev %>% group_by(seed) %>% summarise(total_trans_seed = n())) %>%
  mutate(share_of_trans = transmissions/total_trans_seed)
places <- places %>% left_join(temp1)
places[is.na(places)] <- 0
places <- places %>% group_by(infectionType) %>% summarise(share_of_trans = mean(share_of_trans))


###################################################################################
# 4.3 The Impact of Containment Measures
###################################################################################

options(scipen = 100)
all_nores<- as.data.frame(seq(as.Date("2020-02-05"), as.Date("2021-12-01"), by="days"))
colnames(all_nores) <- "date"

seed <- list.files(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/calibration_nores_2020-02-04/pt_0.25/cal_0.0000095"))
seed <- seed[startsWith(seed, "seed_")] %>% substr(6, 16)
for (s in 1:length(seed)) {
  inp <- c(2020-02-04, 0.25, 0.0000095, seed[s])
  temp1 <- read.delim(paste0('//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/calibration_nores_2020-02-04/pt_0.25/cal_0.0000095/seed_',inp[4],'/infections.txt'), header = TRUE, sep = "\t")
  temp2 <- temp1 %>% select(date, nSeriouslySick, nCritical, nTotalInfected, nShowingSymptoms)
  colnames(temp2) <- c("date", paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_seriously_sick"), 
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_critical"),
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_infected"),
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_symptoms"))
  temp2 <- temp2 %>% mutate(date = as.Date(date))
  all_nores <- all_nores %>% left_join(temp2, by="date")
}
colnames(all_nores)


all_resAct<- as.data.frame(seq(as.Date("2020-02-05"), as.Date("2021-12-01"), by="days"))
colnames(all_resAct) <- "date"

seed <- list.files(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/calibration_resAct_2020-02-04/pt_0.25/cal_0.0000095"))
seed <- seed[startsWith(seed, "seed_")] %>% substr(6, 16)
for (s in 1:length(seed)) {
  inp <- c(2020-02-04, 0.25, 0.0000095, seed[s])
  temp1 <- read.delim(paste0('//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/calibration_resAct_2020-02-04/pt_0.25/cal_0.0000095/seed_',inp[4],'/infections.txt'), header = TRUE, sep = "\t")
  temp2 <- temp1 %>% select(date, nSeriouslySick, nCritical, nTotalInfected, nShowingSymptoms)
  colnames(temp2) <- c("date", paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_seriously_sick"), 
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_critical"),
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_infected"),
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_symptoms"))
  temp2 <- temp2 %>% mutate(date = as.Date(date))
  all_resAct <- all_resAct %>% left_join(temp2, by="date")
}
colnames(all_resAct)

all_resEdu<- as.data.frame(seq(as.Date("2020-02-05"), as.Date("2021-12-01"), by="days"))
colnames(all_resEdu) <- "date"

seed <- list.files(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/calibration_resEdu_2020-02-04/pt_0.25/cal_0.0000095"))
seed <- seed[startsWith(seed, "seed_")] %>% substr(6, 16)
for (s in 1:length(seed)) {
  inp <- c(2020-02-04, 0.25, 0.0000095, seed[s])
  temp1 <- read.delim(paste0('//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/calibration/calibration_resEdu_2020-02-04/pt_0.25/cal_0.0000095/seed_',inp[4],'/infections.txt'), header = TRUE, sep = "\t")
  temp2 <- temp1 %>% select(date, nSeriouslySick, nCritical, nTotalInfected, nShowingSymptoms)
  colnames(temp2) <- c("date", paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_seriously_sick"), 
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_critical"),
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_infected"),
                       paste0(inp[1],"_", inp[2],"_", inp[3],"_", inp[4], "_symptoms"))
  temp2 <- temp2 %>% mutate(date = as.Date(date))
  all_resEdu <- all_resEdu %>% left_join(temp2, by="date")
}
colnames(all_resEdu)

#prepare data
temp1 <- all_nores %>% select(date, contains("seriously")) %>% gather(key = "variable", value = "value", -date) %>% 
  group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T), lower = quantile(value, 0.175, na.rm = T), upper = quantile(value, 0.825, na.rm = T) )
colnames(temp1) <- c("date" , "nores_sersick_mean", "nores_sersick_lower", "nores_sersick_upper")
temp2 <- all_resAct %>% select(date, contains("seriously")) %>% gather(key = "variable", value = "value", -date) %>% 
  group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T), lower = quantile(value, 0.175, na.rm = T), upper = quantile(value, 0.825, na.rm = T) ) %>% select(-date)
colnames(temp2) <- c("resAct_sersick_mean", "resAct_sersick_lower", "resAct_sersick_upper")
temp3 <- all_resEdu %>% select(date, contains("seriously")) %>% gather(key = "variable", value = "value", -date) %>% 
  group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T), lower = quantile(value, 0.175, na.rm = T), upper = quantile(value, 0.825, na.rm = T) )%>% select(-date)
colnames(temp3) <- c("resEdu_sersick_mean", "resEdu_sersick_lower", "resEdu_sersick_upper")
temp4 <- all %>% select(date, contains("seriously")) %>% select(date, contains("2020-02-04")) %>% select(date, contains("0.25_")) %>% select(date, contains(paste0("_0.0000095_"))) %>% 
  gather(key = "variable", value = "value", -date) %>% 
  group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T), lower = quantile(value, 0.175, na.rm = T), upper = quantile(value, 0.825, na.rm = T) )%>% select(-date)
colnames(temp4) <- c("both_sersick_mean", "both_sersick_lower", "both_sersick_upper")
sersick_compres <- cbind(temp1, temp2, temp3, temp4) %>% gather(key = "variable", value = "value", -date) %>% filter(date<"2020-06-01")
sersick_ribbons <- cbind(temp1, temp2, temp3, temp4) %>% select(date, contains("_lower")|contains("_upper")) %>% filter(date<"2020-06-01")

ggplot(sersick_compres, aes(x = date)) +
  geom_ribbon(data = sersick_ribbons, aes(x = date, ymin=nores_sersick_lower, ymax=nores_sersick_upper), fill = "green", alpha=0.05) +
  geom_ribbon(data = sersick_ribbons, aes(x = date, ymin=resAct_sersick_lower, ymax=resAct_sersick_upper), fill = "orange", alpha=0.05) +
  geom_ribbon(data = sersick_ribbons, aes(x = date, ymin=resEdu_sersick_lower, ymax=resEdu_sersick_upper), fill = "violet", alpha=0.05) +
  geom_ribbon(data = sersick_ribbons, aes(x = date, ymin=both_sersick_lower, ymax=both_sersick_upper), fill = "blue", alpha=0.05) +
  geom_line(data = sersick_compres %>% filter(variable == "nores_sersick_mean"), aes(x = date, y = value,  color="No Restrictions" ,linetype="Mean"), size = 1) +
  geom_line(data = sersick_compres %>% filter(variable == "nores_sersick_lower"), aes(x = date, y = value,  color="No Restrictions", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "nores_sersick_upper"), aes(x = date, y = value,  color="No Restrictions", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "resAct_sersick_mean"), aes(x = date, y = value,  color="Only Activity Restriction",linetype="Mean"), size = 1) +
  geom_line(data = sersick_compres %>% filter(variable == "resAct_sersick_lower"), aes(x = date, y = value,  color="Only Activity Restriction", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "resAct_sersick_upper"), aes(x = date, y = value,  color="Only Activity Restriction", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "resEdu_sersick_mean"), aes(x = date, y = value,  color="Only Education Restriction",linetype="Mean"), size = 1) +
  geom_line(data = sersick_compres %>% filter(variable == "resEdu_sersick_lower"), aes(x = date, y = value,  color="Only Education Restriction", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "resEdu_sersick_upper"), aes(x = date, y = value,  color="Only Education Restriction", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "both_sersick_mean"), aes(x = date, y = value,  color="Both Restriction",linetype="Mean"), size = 1) +
  geom_line(data = sersick_compres %>% filter(variable == "both_sersick_lower"), aes(x = date, y = value,  color="Both Restriction", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "both_sersick_upper"), aes(x = date, y = value,  color="Both Restriction", linetype="65% Confidence Interval"), size = 0.5) +
  scale_linetype_manual(values=c("dotdash", "solid"))+
  scale_color_manual(values=c("blue", "green", "orange", "violet"))
  

max(temp1$nores_sersick_mean, na.rm = T)
max(temp2$resAct_sersick_mean, na.rm = T)
max(temp3$resEdu_sersick_mean, na.rm = T)
max(temp4$both_sersick_mean, na.rm = T)
  
###################################################################################
# 4.4 The Scaling Problem
###################################################################################
temp1 <- all %>% select(date, contains("0.25_") & contains("seriously_sick") &  contains("0.0000095_")) %>% gather(key = "variable", value = "value", -date) %>% 
  group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T), lower = quantile(value, 0.175, na.rm = T), upper = quantile(value, 0.825, na.rm = T) )
colnames(temp1) <- c("date" , "tf_sersick_mean", "tf_sersick_lower", "tf_sersick_upper")
temp2 <- all %>% select(date, contains("0.5_") & contains("seriously_sick") &  contains("0.0000095_")) %>% gather(key = "variable", value = "value", -date) %>% 
  group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T), lower = quantile(value, 0.175, na.rm = T), upper = quantile(value, 0.825, na.rm = T))
colnames(temp2) <- c("date" ,"f_sersick_mean", "f_sersick_lower", "f_sersick_upper")
temp3 <- all %>% select(date, contains("0.75_") & contains("seriously_sick") &  contains("0.0000095_")) %>% gather(key = "variable", value = "value", -date) %>% 
  group_by(date) %>% summarise(mean_simul = mean(value, na.rm = T), lower = quantile(value, 0.175, na.rm = T), upper = quantile(value, 0.825, na.rm = T)) %>% filter(date<"2020-05-01")
colnames(temp3) <- c("date" ,"sf_sersick_mean", "sf_sersick_lower", "sf_sersick_upper")
sersick_compres <- temp1 %>% left_join(temp2) %>% left_join(temp3) %>% gather(key = "variable", value = "value", -date) %>% filter(date<"2020-06-01")
sersick_ribbons <- temp1 %>% left_join(temp2) %>% left_join(temp3) %>% select(date, contains("_lower")|contains("_upper")) %>% filter(date<"2020-06-01")


colnames(sersick_ribbons)

ggplot(sersick_compres, aes(x = date)) +
  geom_ribbon(data = sersick_ribbons, aes(x = date, ymin= tf_sersick_lower , ymax= tf_sersick_upper ), fill = "blue", alpha=0.05) +
  geom_ribbon(data = sersick_ribbons, aes(x = date, ymin= f_sersick_lower, ymax= f_sersick_upper), fill = "green", alpha=0.05) +
  geom_ribbon(data = sersick_ribbons, aes(x = date, ymin= sf_sersick_lower, ymax= sf_sersick_upper), fill = "orange", alpha=0.05) +
  geom_line(data = sersick_compres %>% filter(variable == "tf_sersick_mean"), aes(x = date, y = value,  color="25% Sample" ,linetype="Mean"), size = 1) +
  geom_line(data = sersick_compres %>% filter(variable == "tf_sersick_lower"), aes(x = date, y = value,  color="25% Sample", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "tf_sersick_upper"), aes(x = date, y = value,  color="25% Sample", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "f_sersick_mean"), aes(x = date, y = value,  color="50% Sample",linetype="Mean"), size = 1) +
  geom_line(data = sersick_compres %>% filter(variable == "f_sersick_lower"), aes(x = date, y = value,  color="50% Sample", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "f_sersick_upper"), aes(x = date, y = value,  color="50% Sample", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "sf_sersick_mean"), aes(x = date, y = value,  color="75% Sample",linetype="Mean"), size = 1) +
  geom_line(data = sersick_compres %>% filter(variable == "sf_sersick_lower"), aes(x = date, y = value,  color="75% Sample", linetype="65% Confidence Interval"), size = 0.5) +
  geom_line(data = sersick_compres %>% filter(variable == "sf_sersick_upper"), aes(x = date, y = value,  color="75% Sample", linetype="65% Confidence Interval"), size = 0.5) +
  scale_linetype_manual(values=c("dotdash", "solid"))+
  scale_color_manual(values=c("blue", "green", "orange"))

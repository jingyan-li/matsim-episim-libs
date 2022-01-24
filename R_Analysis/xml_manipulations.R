
#load necessery packages
list.of.packages <- c("R.utils", "XML", "kulife", "xml2", "tidyverse", "leaflet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, )
lapply(list.of.packages, require, character.only = TRUE)


#########################################
#make samples

for (pct_share in c(25, 50, 75)) {
  # R.utils::gunzip("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_1pct.xml.gz")
  population <- read_xml("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_100pct.xml") 
  length <- xml_length(population) #1234360
  length
  
  i <- round(length*pct_share/100,0)
  
  nodes <- xml_find_all(population, '//person')
  set.seed(pct_share*12+3)
  sample_nodes <- nodes[sample(seq_along(nodes), i)]
  
  population %>% xml_find_all("//person") %>% xml_remove()
  population %>% xml_find_all("//attributes") %>% xml_add_sibling(sample_nodes, .where="after")
  
  print(paste(xml_length(population)))
  write_xml(population, file = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml"))
  gzip(filename = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml"), 
       destname = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml.gz"),
       overwrite = T)
}

#########################################
#diffrentiate eduction events

#import population dtata and extract ages
population <- read_xml("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_100pct.xml") 

nodes <- xml_find_all(population, '//person')
ids <- nodes %>% map(xml_attrs) %>% as.data.frame() %>% t()
ages <- lapply(c(1:length(nodes)), FUN = function(x){xml_text(xml_contents(xml_child(xml_child(nodes[[x]], 1), 1)))} )
ages <- t(as.data.frame(ages))
ages <- as.data.frame(cbind(ids, ages))
rownames(ages) <- NULL
colnames(ages) <- c('id', 'age')
ages <- ages %>% arrange(age) %>% mutate(school = ifelse(age<19, "school", "university"))
rm(population, nodes, ids)

for (i in c(10, 25, 50, 75)) {
rm(events, nodes_event, nodes_act, nodes_educ)
#import events
#R.utils::gunzip(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",i,"pt/output_events.xml.gz"))
events <- read_xml(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",i,"pt/output_events.xml")) 

nodes_event <- xml_find_all(events, './/event')
nodes_act <- nodes_event[!is.na(xml_attr(nodes_event, "actType"))]
nodes_educ <- nodes_act[xml_attr(nodes_act, "actType")=="education"]

lapply(c(1:length(nodes_educ)), FUN = function(x){xml_set_attr(nodes_educ[x], "actType", ages$school[which(ages$id == xml_attr(nodes_educ[x], "person"))])})

write_xml(events, file = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",i,"pt/output_events_edu.xml"))
# gzip(filename = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",i,"pt/output_events_edu.xml"), 
#      destname = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",i,"pt/output_events_edu.xml.gz"),
#      overwrite = T)
}

#25 als speerspitze
rm(events, nodes_event, nodes_act, nodes_educ)
#import events
R.utils::gunzip(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",25,"pt/output_events.xml.gz"))
events <- read_xml(paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",25,"pt/output_events.xml")) 

nodes_event <- xml_find_all(events, './/event')
nodes_act <- nodes_event[!is.na(xml_attr(nodes_event, "actType"))]
nodes_educ <- nodes_act[xml_attr(nodes_act, "actType")=="education"]

lapply(c(1:length(nodes_educ)), FUN = function(x){xml_set_attr(nodes_educ[x], "actType", ages$school[which(ages$id == xml_attr(nodes_educ[x], "person"))])})

write_xml(events, file = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",25,"pt/output_events_edu.xml"))
gzip(filename = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",25,"pt/output_events_edu.xml"), 
     destname = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/equasim_events/eqasim-",25,"pt/output_events_edu.xml.gz"),
     overwrite = T)

#########################################
# old crap

sampler <- function(pct_share, population, seed){
  sample <- xml_new_root("population") 
  length <- xml_length(population)
  set.seed(seed)
  random <- runif(length, min = 0, max = 1)
  random[1] <- 1
  random <- runif(length, min = 0, max = 1)
  random[1] <- 1
  temp <- as.data.frame(cbind(c(1:length),random))
  colnames(temp) <- c("id", "random")
  random <- (temp %>% filter(random>(1-(pct_share/100))))$id
  start_time <- Sys.time()
  invisible(lapply(random, function(x){xml_add_child(sample, xml_child(population, x))}))
  end_time <- Sys.time()
  time <- end_time - start_time
  length_sample <- xml_length(sample)
  length_supposed <-length*(pct_share/100)
  diffrence <- xml_length(sample)/(length*(pct_share/100))
  write_xml(sample, file = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml"))
  gzip(filename = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml"), 
       destname = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml.gz"),
       overwrite = T)
  return(c(time, length_sample, length_supposed, diffrence))
}

sampler(25,population,654465)
sampler(50,population,54354)
sampler(75,population,43547)



#########################################

sampler <- function(pct_share, population, seed){
  sample <- xml_new_root("population") 
  length <- xml_length(population)
  set.seed(seed)
  random <- runif(length, min = 0, max = 1)
  random[1] <- 1
  random <- runif(length, min = 0, max = 1)
  random[1] <- 1
  temp <- as.data.frame(cbind(c(1:length),random))
  colnames(temp) <- c("id", "random")
  random <- (temp %>% filter(random>(1-(pct_share/100))))$id
  start_time <- Sys.time()
  invisible(lapply(random, function(x){xml_add_child(sample, xml_child(population, x))}))
  end_time <- Sys.time()
  time <- end_time - start_time
  length_sample <- xml_length(sample)
  length_supposed <-length*(pct_share/100)
  diffrence <- xml_length(sample)/(length*(pct_share/100))
  write_xml(sample, file = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml"))
  gzip(filename = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml"), 
       destname = paste0("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_" ,pct_share, "pct.xml.gz"),
       overwrite = T)
  return(c(time, length_sample, length_supposed, diffrence))
}


sample <- xml_new_root("population") 
length <- xml_length(population)
set.seed(seed)
random <- runif(length, min = 0, max = 1)
random[1] <- 1
random <- runif(length, min = 0, max = 1)
random[1] <- 1
temp <- as.data.frame(cbind(c(1:length),random))
colnames(temp) <- c("id", "random")
random <- (temp %>% filter(random>(1-(pct_share/100))))$id
start_time <- Sys.time()
invisible(lapply(random, function(x){xml_add_child(sample, xml_child(population, x))}))


#########################################

pct_share <- 1
sample <- list("population" = list())
length <- xml_length(population)
random <- runif(length, min = 0, max = 1)
random[1] <- 1
random <- runif(length, min = 0, max = 1)
random[1] <- 1
temp <- as.data.frame(cbind(c(1:length),random))
colnames(temp) <- c("id", "random")
random <- as.list((temp %>% filter(random>(1-(pct_share/100))))$id)

listbuilder <- function(x){
  a <- pop_list[["population"]][[(x*2)-1]] 
  b <-pop_list[["population"]][[(x*2)]]
  return(list(a,b))}
inthere <- lapply(random, function(x){pop_list[["population"]][[(x*2)-1]]})
append (inthere, pop_list[["population"]][[2]], after=1)



#########################################

R.utils::gunzip("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/output_events.xml.gz")
events <- read_xml("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/output_events.xml") 
xml_length(events) #2496846

xml_child(events, 1)

#########################################

R.utils::gunzip("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_facilities.xml.gz")
facilities <- read_xml("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/Zurich_facilities.xml") 
xml_length(facilities) #2496846

xml_child(facilities, 5)
xml_child(facilities, 10)


#########################################
population <- read_xml("//d.ethz.ch/groups/mtec/aeii/Users/SebastianoPapini/ABMT/ABMT_Zurich_Scenario_2021/zurich_population_1pct.xml") 
test <- as.data.frame(population)


for (i in c()) {
  temp <- xml_attrs(xml_child(xml_child(xml_child(population, i), 2), 1))
  homes <- rbind(homes,temp[4:5])
}





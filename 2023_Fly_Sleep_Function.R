# Graphing Sleep Data Trials 

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# data_file = .csv file holding the organized data
# sex_type = 'F' or 'M'
# genotype_type = single 'genotype label' 
#       (ex. '8765' or '8765 (M) x 25807 (F)')
# sex_genotype = 'Sleep Data of [insert sex] Flies'
#       (ex. 'Sleep Data of Female Flies')
# date_type = '[Name]'s Data: [Date]' or distinguish trial type (L:D, L:L, D:D, etc)
#       (ex. 'Mounia's Data: October 26, 2022')
# Filter some genotypes but not others example:  
#   data_altered <- data_altered %>%
#       filter(genotype == '8765'| genotype == '25807' | genotype == '34109')
# genotype_type_1 = first genotype being compared
# genotype_type_2 = second genotype that is being statistically compared to the first genotype
# variable_column = 'Dbout', 'Nbout', 'daybnum', 'nightbnum'

all_genotype_function <- function(data_file, sex_type, sex_genotype, date_type) {
  fly_data <- data_file
  head(fly_data)
  
  modified_data <- fly_data[complete.cases(fly_data),]
  
  data <- modified_data %>%
    filter(sex == sex_type)
  
  mean_data <- data %>%
    group_by(genotype) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  mean_data
  
  data_altered <- mean_data %>%
    gather('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', key = 'time', value = 'sleep')
  
  data_altered <- data_altered %>%
    mutate(time = as.numeric(time))
  data_altered
  
  fly_graph <- ggplot(data = data_altered, aes(x=factor(time), y=sleep, group=1, color=as.factor(genotype))) + scale_colour_discrete('Genotype') + geom_line(aes(group = genotype)) + labs(title = sex_genotype, subtitle = date_type, x = 'Time(ZT)', y = 'Sleep (min/hour)')
  fly_graph
}
# plots all genotype trends without error bars

single_genotype_function <- function(data_file, sex_type, genotype_type, sex_genotype, date_type) {
  fly_data <- data_file
    head(fly_data)
    
  modified_data <- fly_data[complete.cases(fly_data),]
  
  data <- modified_data %>%
    filter(sex == sex_type)

  mean_data <- data %>%
    group_by(genotype) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  mean_data

  data_altered <- mean_data %>%
    gather('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', key = 'time', value = 'sleep')

  data_altered <- data_altered %>%
    mutate(time = as.numeric(time))
  data_altered

  data_altered <- data_altered %>%
    filter(genotype == genotype_type)
  data_altered

  fly_graph <- ggplot(data = data_altered, aes(x=factor(time), y=sleep, group=1, color=as.factor(genotype))) + scale_colour_discrete('Genotype') + geom_line(aes(group = genotype)) + labs(title = sex_genotype, subtitle = date_type, x = 'Time(ZT)', y = 'Sleep (min/hour)')
  fly_graph
}
# graphs without error bars

fly_function_wE <- function(data_file, sex_type, genotype_type, sex_genotype, date_type) {
  fly_data <- data_file
  head(fly_data)
  
  modified_data <- fly_data[complete.cases(fly_data),]
  
  data <- modified_data %>%
    filter(sex == sex_type)
  
  mean_data <- data %>%
    group_by(genotype) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  mean_data
  
  sd <- data %>%
    group_by(genotype) %>%
    summarise_if(is.numeric, sd, na.rm = TRUE)
  
  
  se <- sd %>%
    select('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
  
  
  se_altered <- se %>%
    gather('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23', key = 'time', value = 'se_sleep')
  se_altered 
  
  
  data_altered <- mean_data %>%
    gather('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23', key = 'time', value = 'sleep')
  data_altered 
  
  
  se_sleep <- se_altered$se_sleep
  se_sleep
  
  data_altered$se_sleep <- se_sleep 
  data_altered
  
  data_altered <- data_altered %>%
    mutate(time = as.numeric(time))
  data_altered
  
  data_altered <- data_altered %>%
    filter(genotype == genotype_type)
  data_altered
  
  fly_graph <- ggplot(data = data_altered, aes(x=factor(time), y=sleep, group=1, color=as.factor(genotype))) + scale_colour_discrete('Genotype') + geom_line(aes(group = genotype)) + labs(title = sex_genotype, subtitle = date_type, x = 'Time(ZT)', y = 'Sleep (min/hour)') + geom_errorbar(aes(ymax=sleep+(se_sleep/2), ymin=sleep-(se_sleep/2)), width = 0.2) 
                                                                                                                                                                                           
  fly_graph
}
# graphs with error bars

count_sex_function <- function(data_file, sex_type, genotype_type) {
  fly_data <- data_file
    head(fly_data)
    
  modified_data <- fly_data[complete.cases(fly_data),]

  data <- modified_data %>%
    filter(sex == sex_type) %>%
    filter(genotype == genotype_type)
  count(data)
}
# calculates number of subjects in trial 

combined_function_wE <- function(data_file, sex_type, genotype_type, genotype_type1, genotype_type2, genotype_type3, sex_genotype, date_type) {
  fly_data <- data_file
  head(fly_data)
  
  modified_data <- fly_data[complete.cases(fly_data),]
  
  data <- modified_data %>%
    filter(sex == sex_type)
  
  mean_data <- data %>%
    group_by(genotype) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  mean_data
  
  sd <- data %>%
    group_by(genotype) %>%
    summarise_if(is.numeric, sd, na.rm = TRUE)
  
  
  se <- sd %>%
    select('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
  
  
  se_altered <- se %>%
    gather('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23', key = 'time', value = 'se_sleep')
  se_altered 
  
  
  data_altered <- mean_data %>%
    gather('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23', key = 'time', value = 'sleep')
  data_altered 
  
  
  se_sleep <- se_altered$se_sleep
  se_sleep
  
  data_altered$se_sleep <- se_sleep 
  data_altered
  
  data_altered <- data_altered %>%
    mutate(time = as.numeric(time))
  data_altered
  
  data_altered <- data_altered %>%
    filter(genotype == genotype_type | genotype == genotype_type1 | genotype == genotype_type2 | genotype == genotype_type3)
  data_altered
  
  fly_graph <- ggplot(data = data_altered, aes(x=factor(time), y=sleep, group=1, color=as.factor(genotype))) + scale_colour_discrete('Genotype') + geom_line(aes(group = genotype)) + labs(title = sex_genotype, subtitle = date_type, x = 'Time(ZT)', y = 'Sleep (min/hour)') + geom_errorbar(aes(ymax=sleep+(se_sleep/2), ymin=sleep-(se_sleep/2)), width = 0.2) 
  
  fly_graph
}

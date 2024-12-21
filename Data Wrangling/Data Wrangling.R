#####Q1####
library(dplyr)
case_death <- full_join(case,deaths
                        , by = c("Province/State"
                                 ,"Country/Region"
                                 ,"Lat","Long"))
######Q2#####
uni_country <- unique(case$`Country/Region`)
b <- case$`Country/Region`

repeat_name <- c()

## the for loop and the unique function below is to 
## see all the countries that repeatly appear in the list
for (i in 1:198){
  for (z in 1:284){
    if (uni_country[i] == b[z]){
      if(z != 284){
        z = z+1
        if (uni_country[i] == b[z]){
          repeat_name <- append(repeat_name, b[z])
          print(b[z])
        }
      }
    }
  }
}
repeat_name <- unique(repeat_name)

country_total <- c()
total = 0
## the for loop below is to see the total cases number that 
## occur in the countries that have repeatly appear
for (i in 1:length(repeat_name)) {
  num <- which(case$`Country/Region` == repeat_name[i])
  for (a in 1:length(num)) {
    total = case$`5/1/22`[num[a]] + total
  }
  print(total)
  country_total <- append(country_total, total)
  total = 0
}

repeat_country <- data.frame(Country = repeat_name,
                             case_number = country_total)

no_repeat_country <- uni_country
list_of_repeat <- c()
## the funtion below is to see the total cases number that 
## occur in the countries that don't repeatly appear
for (i in 1:length(repeat_name)) {
  list_of_repeat <-  append(list_of_repeat, which(no_repeat_country == repeat_name[i]))
}
no_repeat_country <- no_repeat_country[-list_of_repeat]

num_of_nonrepeat <- c()
for (i in 1:length(no_repeat_country)) {
  num_of_nonrepeat <-  append(num_of_nonrepeat, case$`5/1/22`[which(case$`Country/Region` == no_repeat_country[i])])
}

no_repeat_country_list <- data.frame(Country = no_repeat_country,
                             case_number = num_of_nonrepeat)

## the function below is to merge two dataframe
total_uni_case <- full_join(no_repeat_country_list, repeat_country)
order(total_uni_case$case_number,decreasing = TRUE)[1:3]                   
total_uni_case$Country[order(total_uni_case$case_number,decreasing = TRUE)[1:3]]

##############Q3######choose Canada and China###

change_name <- colnames(case[,-c(1:4)])
new_case_death_Canada_China <- case_death[which(case_death$`Country/Region` == "China" | case$`Country/Region` == "Canada"), ]
new_case_death_Canada_China <- new_case_death_Canada_China[,-c(1,3,4)]

for (i in 1:length(change_name)) {
  change_name[i] <- paste( change_name[i], "cases", sep = ".")
}
colnames(new_case_death_Canada_China)[2:840] <- change_name[1:839]

change_name <- colnames(case[,-c(1:4)])
for (i in 1:length(change_name)) {
  change_name[i] <- paste( change_name[i], "deaths", sep = ".")
}
colnames(new_case_death_Canada_China)[841:1679] <- change_name[1:839]

uni_case_death_Canada_China <- data.frame(Country = c("Canada", "China"))

## the for loop below is to calculate the total cases and deaths 
# number in Canada and China 
num_Canada_China = c()
for (i in 2:length(new_case_death_Canada_China)) {
  total = 0
  for (a in 1:16) {
    total = new_case_death_Canada_China[a,i] + total
  }
  num_Canada_China <- append(num_Canada_China, total)
  total = 0
  for (a in 17:50) {
    total = new_case_death_Canada_China[a,i] + total
  }
  num_Canada_China <- append(num_Canada_China, total)
  uni_case_death_Canada_China[colnames(new_case_death_Canada_China)[i]] <- num_Canada_China
  num_Canada_China = c()
}


which(colnames(uni_case_death_Canada_China) == "5/2/22.cases")
which(colnames(uni_case_death_Canada_China) == "5/8/22.cases")
average_cases <- c()
total = 0
for (i in 833:839) {
  total <- uni_case_death_Canada_China[1,i] + total
}
total = total/7
average_cases <- append(average_cases, total)

total = 0
for (i in 833:839) {
  total <- uni_case_death_Canada_China[2,i]
}
total = total/7
average_cases <- append(average_cases, total)

which(colnames(uni_case_death_Canada_China) == "5/2/22.deaths")
which(colnames(uni_case_death_Canada_China) == "5/8/22.deaths")
average_deaths <- c()
total = 0
for (i in 1672:1678) {
  total <- uni_case_death_Canada_China[1,i] + total
}
total = total/7
average_deaths <- append(average_deaths, total)

total = 0
for (i in 1672:1678) {
  total <- uni_case_death_Canada_China[2,i]
}
total = total/7
average_deaths <- append(average_deaths, total)

last_week_average_Canada_China <- data.frame(Country = c("Canada", "China"),
                                             Average_cases = average_cases,
                                             Average_deaths = average_deaths
                                             )


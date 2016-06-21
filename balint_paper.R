setwd("/media/balint/Storage/Tanulas/Social_and_Economic_Networks/offshore/offleaks")

library(igraph)
#contract.vertices
addresses <- read.csv(file = 'offshore_leaks_csvs-20160524/Addresses.csv')
edges <- read.csv(file = 'offshore_leaks_csvs-20160524/all_edges.csv')
entities <- read.csv(file = 'offshore_leaks_csvs-20160524/Entities.csv')
inters <- read.csv(file = 'offshore_leaks_csvs-20160524/Intermediaries.csv')
officers <- read.csv(file = 'offshore_leaks_csvs-20160524/Officers.csv')

entities$inactivation_date <- as.Date(x = as.character(entities$incorporation_date), format = "%d-%b-%Y")
entities$inactivation_date <- as.Date(x = as.character(entities$inactivation_date), format = "%d-%b-%Y")
sum(is.na(entities$incorporation_date))
sum(is.na(entities$inactivation_date))
a <- entities$inactivation_date - entities$incorporation_date
hist(as.numeric(a[!is.na(a)]))

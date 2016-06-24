# loading files
addresses <- read.csv(file = 'offshore_leaks_csvs-20160524/Addresses.csv')
edges <- read.csv(file = 'offshore_leaks_csvs-20160524/all_edges.csv')
entities <- read.csv(file = 'offshore_leaks_csvs-20160524/Entities.csv')
inters <- read.csv(file = 'offshore_leaks_csvs-20160524/Intermediaries.csv')
officers <- read.csv(file = 'offshore_leaks_csvs-20160524/Officers.csv')

officers$name_lenght <- nchar(as.character(officers$name))

# dealing with officers' names
officers$name <- tolower(officers$name)
officers_names <- data.frame(sort(table(officers$name), decreasing=TRUE))
### there are many different nodes that share the same name!

officers$type <- NA
company <- c("LTD", "Ltd", "ltd",
             "LIMITE", "limite", "Limite", "LMITED", "Lmited", "LIMIT", "Limit",
             "LIMITD", "LIMTED", "Limted", "LIITED",
             "Trust", "TRUST", "trust",
             "CORP", "corp", "Corp", "co\\.", "CO\\.",
             "GROUP", "Group", "group",
             "INC", "Inc", " inc",
             "&",
             "FOUNDATION", "Foundation", "foundation", "FUND", "Fund",
             "SOCIEDAD", "Sociedad", "sociedad",
             "gmbh", "GMBH", "Gmbh", "GmbH",
             "s.a.", "S.A.",
             "HOLD.", "HOLDING", "Holding", "holding",
             "CAPITAL", "Capital", "capital",
             "PROPERTIES", "Properties",
             "LLC", "L\\.L\\.C\\.",
             "INVESTMENT", "Investment",
             "CONSULT", "Consult", "CONSULTING",
             "DEVELOPMENT", "Development",
             "SYSTEM", "System",
             "LLP",
             " SA$", "SA\\.$",
             "SERVICES", "Services",
             "FINANC", "Financ",
             "GLOBAL", "Global",
             "MANAG", "Manag",
             "Propert", "PENSION",
             "TRADING", "TRADE", "Trad", "trade",
             "BANK ", "BANKING", "Bank ",
             "INDUST", "Indust", "Ind\\.",
             "PLC$", "Plc", "plc",
             "S\\.L\\.", " AG$", "A\\.G", "KFT", "BHD", "Bhd", "ZRT",
             "TLD", "LP$", "L\\.P",
             "PARTNER", "Partner", "partner",
             "INTERNATIONAL", "International", "international",
             "NOMINEES", "Nominees",
             "COMPANY", "Company", "Compania", "company",
             "business")
company <- tolower(company)
company <- company[!duplicated(company)]

#select bearer shares
bearer <- c("BEARER", "Bearer", "bearer", "bear",
            "BEARED", "BAERER", "BEAER", "EBARER",
            "PORTAD", "Portador", "portador")
bearer <- tolower(bearer)
bearer <- bearer[!duplicated(bearer)]

bearer_levels <- c("", " ", "? ??", "??", "???", "??????????",
                   "????????????", "???????????????", ".")
officers$type[grepl(paste(company,collapse="|"), officers$name)] <- "organization"
officers$type[grepl(paste(bearer,collapse="|"), officers$name)] <- "bearer share"
officers$type[officers$name %in% bearer_levels] <- "bearer share"
officers$type[is.na(officers$type)] <- "person"

table(officers$type) ### bearer share 86548; organization 83615; person 175431

# Select those officers, about which we have country information.
officers_w_country <- officers[!officers$country_codes %in% c("", "XXX"),]

#create people table with only people
people <- officers_w_country[officers_w_country$type == "person",]
people <- droplevels(people)

# title workarounds
title <- c("mr ","mr. ","m.r. ","mr.",
           "mrs ","mrs. ","mrs.",
           "ms ","ms. ","ms ","miss ","miss. ",
           "madam ","mdm ",
           "dr. ","professor ","prof ","prof. ",", m.d.",", md")
people$name <- gsub(paste(title,collapse="|"), "", people$name)

# syntax dealings, drop all question marks, question marks in brackets, empty brackets and (1)
length(people$name[grepl("\\?+|\\(\\?+\\)|\\(\\)|\\(1\\)", people$name)]) ### 2314
people$name <- gsub("\\?+|\\(\\?+\\)|\\(\\)|\\(1\\)", "", people$name)

# drop all last spaces
for (i in 1:40) {
  people$name[substr(people$name,nchar(people$name),nchar(people$name))==" "] <-
    substr(people$name[substr(people$name,nchar(people$name),nchar(people$name))==" "],1,nchar(people$name[substr(people$name,nchar(people$name),nchar(people$name))==" "])-1)
}

# insert spaces after commas
people$name[grepl(" ", people$name)==FALSE] <- gsub(",", ", ", people$name[grepl(" ", people$name)==FALSE])

# there are many-many frequencies belonging to one officer already
people_names <- data.frame(sort(table(people$name), decreasing=TRUE))

# deal with "X, Y" == "X Y" == "Y X"
people_names$name <- as.character(people_names$Var1)
people_names$comma <- grepl("\\, ",people_names$Var1)
people_names$name_wo_comma1 <- NA
people_names$name_wo_comma2 <- NA
people_names$wo_comma1 <- NA
people_names$wo_comma2 <- NA

s <- subset(people_names, comma==TRUE)
for (i in 1:nrow(s)) {
  s$name_wo_comma1[i] <- paste0(unlist(strsplit(s$name[i], ", "))[1]," ",unlist(strsplit(s$name[i], ", "))[2])
  s$name_wo_comma2[i] <- paste0(unlist(strsplit(s$name[i], ", "))[2]," ",unlist(strsplit(s$name[i], ", "))[1])
  }
s$wo_comma1 <- s$name_wo_comma1 %in% people_names$name
s$wo_comma2 <- s$name_wo_comma2 %in% people_names$name
table(s$wo_comma1, s$wo_comma2)
ss <- subset(s, wo_comma1==TRUE&wo_comma2==TRUE)[,c("name","name_wo_comma1","name_wo_comma2")] ### 100
people$name[people$name %in% ss$name_wo_comma2] <- ss$name_wo_comma1[match(people$name[people$name %in% ss$name_wo_comma2], ss$name_wo_comma2)]
people$name[people$name %in% ss$name] <- ss$name_wo_comma1[match(people$name[people$name %in% ss$name], ss$name)]
s1 <- subset(s, wo_comma1==FALSE&wo_comma2==TRUE) ### 220
people$name[people$name %in% s1$name_wo_comma2] <- s1$name_wo_comma1[match(people$name[people$name %in% s1$name_wo_comma2], s1$name_wo_comma2)]
s2 <- subset(s, wo_comma1==TRUE&wo_comma2==FALSE) ### 1159
people$name[people$name %in% s2$name] <- s2$name_wo_comma1[match(people$name[people$name %in% s2$name], s2$name)]

# this table suggests how many nodes one can collapse into each other
people_names <- data.frame(sort(table(people$name), decreasing=TRUE))
# down to 112498 names

# lengths of names left
people_names$name <- as.character(people_names$Var1)
people_names$name_lenght <- nchar(people_names$name)
# some are still very lengthy

# collapsing the people of the same name into one node
one_person <- people_names$name[people_names$Freq>1]
many_nodes <- list()
for (i in 1:length(one_person)) many_nodes[[i]] <- people$node_id[people$name == one_person[i]]
one_country <- numeric(length(one_person))
for (i in 1:length(one_person)) one_country[i] <- as.character(data.frame(sort(table(subset(people,node_id%in%many_nodes[[i]])$countries), decreasing=TRUE))[1,1])
for (i in 1:length(one_person)) {
  people[people$name==one_person[i],] <- people[people$name==one_person[i]&people$countries==one_country[i],][1,]
}
# there should be 112498 nodes in people rather than 137475 when with duplicates
people <- people[!duplicated(people),]

people$name_lenght <- nchar(as.character(people$name))

write.csv(people, "people.csv", row.names = FALSE)

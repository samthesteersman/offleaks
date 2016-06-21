setwd("/media/balint/Storage/Tanulas/Social_and_Economic_Networks/offshore/offleaks")

library(igraph)
#contract.vertices
addresses <- read.csv(file = 'offshore_leaks_csvs-20160524/Addresses.csv')
edges <- read.csv(file = 'offshore_leaks_csvs-20160524/all_edges.csv')
entities <- read.csv(file = 'offshore_leaks_csvs-20160524/Entities.csv')
intermediaries <- read.csv(file = 'offshore_leaks_csvs-20160524/Intermediaries.csv')
officers <- read.csv(file = 'offshore_leaks_csvs-20160524/Officers.csv')

table(entities$country_codes)
table(officers$country_codes)

a <- officers[officers$node_id %in% intermediaries$node_id,]
b <- officers[officers$node_id %in% entities$node_id,]

# Create new variable type with 3 possible values: person, organization, bearer share
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
             "COMPANY", "Company", "Compania")

#select bearer shares
bearer <- c("BEARER", "Bearer", "bearer",
            "BEARED", "BAERER", "BEAER", "EBARER",
            "PORTAD", "Portador", "portador")
bearer_levels <- c("", " ", "? ??", "??", "???", "??????????",
                   "????????????", "???????????????", ".")
officers$type[grepl(paste(company,collapse="|"), officers$name)] <- "organization"
officers$type[grepl(paste(bearer,collapse="|"), officers$name)] <- "bearer share"
officers$type[officers$name %in% bearer_levels] <- "bearer share"
officers$type[is.na(officers$type)] <- "person"

# Select those officers, about which we have country information.
officers <- officers[!officers$country_codes %in% c("", "XXX"),]

#create people table with only people
people <- officers[officers$type == "person",]
people <- droplevels(people)

#select person to entity edges (there are no entity to person edges)
edges_pe <- edges[edges$node_1 %in% people$node_id & edges$node_2 %in% entities$node_id,]
edges_pe$rel_type <- droplevels(edges_pe$rel_type)
# table(edges_pe$rel_type)

#select person to person edges
edges_pp <- edges[edges$node_1 %in% people$node_id & edges$node_2 %in% people$node_id,]
edges_pp$rel_type <- droplevels(edges_pp$rel_type)
# table(edges_pp$rel_type)

#Select relevant entities
entities_p <- entities[entities$node_id %in% edges_pe$node_2,]

#CREATE NETWORK

# bearers <- officers[officers$type == "bearer share",]

#select entity to entity edges
# edges_ee <- edges[edges$node_1 %in% entities$node_id & edges$node_2 %in% entities$node_id,]
# edges_ee$rel_type <- droplevels(edges_ee$rel_type)
# table(edges_ee$rel_type)

#select bearer to entity edges (there are no entity to person edges)
# edges_be <- edges[edges$node_1 %in% bearers$node_id & edges$node_2 %in% entities$node_id,]
# edges_be$rel_type <- droplevels(edges_be$rel_type)
# table(edges_be$rel_type)

#select bearer to bearer edges
# edges_bb <- edges[edges$node_1 %in% bearers$node_id & edges$node_2 %in% bearers$node_id,]
# edges_bb$rel_type <- droplevels(edges_bb$rel_type)
# table(edges_bb$rel_type)

#edge person-intermediary and intermediary-person
# edges_pi <- edges[edges$node_1 %in% people$node_id & edges$node_2 %in% intermediaries$node_id,]
# edges_ip <- edges[edges$node_1 %in% intermediaries$node_id & edges$node_2 %in% people$node_id,]




#TESTING CODE

# table(people$country_codes)
# as.character(people[grepl("limit", people$name), 1])
# head(levels(people$name), 20)
#  people[people$node_id == 51381,]
#  officers[officers$node_id == 101020,]
# people[people$name == "???",]
# head(levels(officers$name),20)
# table(people$name)[table(people$name)>20]
# entities$inactivation_date <- as.Date(x = as.character(entities$incorporation_date), format = "%d-%b-%Y")
# entities$inactivation_date <- as.Date(x = as.character(entities$inactivation_date), format = "%d-%b-%Y")
# sum(is.na(entities$incorporation_date))
# sum(is.na(entities$inactivation_date))
# a <- entities$inactivation_date - entities$incorporation_date
# hist(as.numeric(a[!is.na(a)]))

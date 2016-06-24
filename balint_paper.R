setwd("/media/balint/Storage/Tanulas/Social_and_Economic_Networks/offshore/offleaks")

library(igraph)
library(DescTools)
library(reshape2)
# addresses <- read.csv(file = 'offshore_leaks_csvs-20160524/Addresses.csv')
edges <- read.csv(file = 'offshore_leaks_csvs-20160524/all_edges.csv')
entities <- read.csv(file = 'offshore_leaks_csvs-20160524/Entities.csv')
# intermediaries <- read.csv(file = 'offshore_leaks_csvs-20160524/Intermediaries.csv')
officers <- read.csv(file = 'offshore_leaks_csvs-20160524/Officers.csv')

# table(entities$country_codes)
# table(officers$country_codes)

# a <- officers[officers$node_id %in% intermediaries$node_id,]
# b <- officers[officers$node_id %in% entities$node_id,]

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
# bearer_levels <- c("", " ", "? ??", "??", "???", "??????????",
#                    "????????????", "???????????????", ".")
officers$type[grepl(paste(company,collapse="|"), officers$name)] <- "organization"
officers$type[grepl(paste(bearer,collapse="|"), officers$name)] <- "bearer share"
# officers$type[officers$name %in% bearer_levels] <- "bearer share"
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

#Select relevant entities, prepare to set up network
entities$type <- "entity"
entities_p <- entities[entities$node_id %in% edges_pe$node_2, c(20,1,22,16,17,21)]
names(entities_p)[2] <- "real_name"
people <- people[, c(6,1,8,4,5,7)]
names(people)[2] <- "real_name"

#create vertices data frame
v <- rbind(people, entities_p)
v$type <- v$type == "person"

#JOIN 'SIMILAR' NODES
#select entity to entity edges
edges_similarity <- edges[(edges$node_1 %in% entities_p$node_id & edges$node_2 %in% entities_p$node_id) |
                      (edges$node_1 %in% people$node_id & edges$node_2 %in% people$node_id),]
#choose similarity edges
edges_similarity <- edges_similarity[edges_similarity$rel_type %in% c("same name and registration date as", "similar name and address as"),]

#version with related entities joined too
# edges_similarity <- edges_similarity[edges_similarity$rel_type %in% c("same name and registration date as", "similar name and address as", "related entity"),]

edges_similarity$rel_type <- droplevels(edges_similarity$rel_type)
# table(edges_similarity$rel_type)

edges_similarity <- edges_similarity[,c(1,3,2)]


g_similarity <- graph.data.frame(d = edges_similarity, vertices = v)
similar_nodes <- components(g_similarity)$membership

#select person to person edges
# edges_pp <- edges[edges$node_1 %in% people$node_id & edges$node_2 %in% people$node_id,]
# edges_pp$rel_type <- droplevels(edges_pp$rel_type)
# table(edges_pp$rel_type)

#remove objects which we do not need anymore
rm(g_similarity)
rm(edges_similarity)
rm(officers)
rm(entities)

#CREATE NETWORK
edges_pe <- edges_pe[,c(1,3,2)]

g <- graph.data.frame(d = edges_pe, vertices = v)

#joining similar nodes
g_sim <- contract(g, similar_nodes, vertex.attr.comb = "first")
g_sim <- simplify(g_sim)
# is.bipartite(g_sim)
# summary(g)
g_people <- bipartite.projection(g, multiplicity = FALSE)[[2]]
g_sim_people <- bipartite.projection(g_sim, multiplicity = FALSE)[[2]]

rm(g)
rm(g_sim)
rm(similar_nodes)

#ADD NOMINEE RELATIONSHIP EDGES
#select person to person edges
edges_pp <- edges[edges$node_1 %in% people$node_id & edges$node_2 %in% people$node_id,]
#select nominee edges
edges_pp_nominee <- edges_pp[edges_pp$rel_type %in% c("Nominee Beneficial Owner of", "Nominee Director of",
                                                      "Nominee Shareholder of", "Nominee Trust Settlor of"),]
edges_pp_nominee$rel_type <- droplevels(edges_pp_nominee$rel_type)
# table(edges_pp_nominee$rel_type)
#add nominee edges
edges_pp_nominee <- edges_pp_nominee[,c(1,3)]
g_people <- add_edges(g_people, as.vector(t(edges_pp_nominee)))
g_sim_people <- add_edges(g_sim_people, as.vector(t(edges_pp_nominee)))

g_people_decomposed <- decompose.graph(g_people, min.vertices = 100)
g_sim_people_decomposed <- decompose.graph(g_sim_people, min.vertices = 100)

#a function to calculate some key properties of a graph
component_info <- function(component){
component_nodes <- get.data.frame(component, what="vertices")
#create separate variable for each country
countries <- unique(unlist(strsplit(levels(as.factor(component_nodes$countries)), ";")))
countries <- countries[order(countries)]
countries <- countries[countries != "Not identified"]
for (country in countries) {
        component_nodes$new <- grepl(country, component_nodes$countries, fixed = TRUE)
        names(component_nodes)[ncol(component_nodes)] <- country
}
country_counts <- colSums(as.data.frame(component_nodes[,6:ncol(component_nodes)]))
return(list(number_of_people = nrow(component_nodes), most_frequent_country = names(which.max(country_counts)),
            share_of_people_from_most_frequent_country = max(country_counts)/sum(country_counts), Herfindahl_index = Herfindahl(country_counts)))
}

g_people_decomposed_stats <- as.data.frame(t(sapply(g_people_decomposed, component_info)))
g_people_decomposed_stats <- g_people_decomposed_stats[order(unlist(g_people_decomposed_stats$number_of_people), decreasing = TRUE),]
g_people_decomposed_stats[7,2] <- "China"

g_sim_people_decomposed_stats <- as.data.frame(t(sapply(g_sim_people_decomposed, component_info)))
g_sim_people_decomposed_stats <- g_sim_people_decomposed_stats[order(unlist(g_sim_people_decomposed_stats$number_of_people), decreasing = TRUE),]
g_sim_people_decomposed_stats[7,2] <- "China"

giant_component <- g_people_decomposed[[13]]
giant_component_sim <- g_sim_people_decomposed[[1]]

giant_component_decomposed <- cluster_louvain(giant_component)$membership




sizes(cluster_louvain(giant_component))
summary(giant_component)
component <- g_people_decomposed[[14]]
component_info(comp)
summary(comp)
head(sort(components(g_people)$csize, decreasing=TRUE), 100)
head(sort(components(g_sim_people)$csize, decreasing=TRUE), 100)

a <- decompose.graph(g_people, min.vertices = 100)
gg <- a[[13]]
aa <- get.data.frame(gg, what="vertices")
table(aa$country_codes)
get.graph.attribute(g, "country_codes")
list.graph.attributes(g)
b <- get.data.frame(gg)
c <- get.vertex.attribute(gg, "country_codes")
table(c)
table(get.vertex.attribute(a[[16]], "countries"))
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
#  people[people$node_id == 51115,]
#  officers[officers$node_id == 51115,]
# people[people$name == "???",]
# head(levels(officers$name),20)
# table(people$name)[table(people$name)>20]
# entities$inactivation_date <- as.Date(x = as.character(entities$incorporation_date), format = "%d-%b-%Y")
# entities$inactivation_date <- as.Date(x = as.character(entities$inactivation_date), format = "%d-%b-%Y")
# sum(is.na(entities$incorporation_date))
# sum(is.na(entities$inactivation_date))
# a <- entities$inactivation_date - entities$incorporation_date
# hist(as.numeric(a[!is.na(a)]))

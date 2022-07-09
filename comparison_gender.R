library(haven)
library(igraph)
library(dplyr)
library(colourvalues)


rm(list=ls())
setwd("/Users/eehyun/Documents/Works/MA thesis/")
data <- read_dta("/Users/eehyun/Documents/Works/MA thesis/test_100_1016.dta")
data <- read_dta("/Users/eehyun/Documents/Works/MA thesis/1016_omitted.dta")
label <- read.csv("/Users/eehyun/Documents/Works/MA thesis/label.csv",
				  header=FALSE)
colnames(label) <- c("asjc", "name")




edgelist <- data.frame(source=data["max_asjccode"], target=data["asjccode01"],
					   author_gender=data["author_gender"])
# Exclude N category
edgelist <- subset(edgelist, author_gender != 'N')

fem_edge <- subset(edgelist, author_gender == 'F')
mal_edge <- subset(edgelist, author_gender == 'M')


add_cov <- function(edgelist, label) {
	edge_freq <- edgelist %>% group_by(max_asjccode, asjccode01) %>%
		summarise(freq=n())
	max_move <- edgelist %>% group_by(max_asjccode) %>% summarise(source_n=n())
	cur_move <- edgelist %>% group_by(asjccode01) %>% summarise(target_n=n())
	
	edge_freq <- merge(edge_freq, max_move, by="max_asjccode")
	edge_freq <- merge(edge_freq, cur_move, by="asjccode01")
	
	edge_freq["source_prop"] <- edge_freq["freq"] / edge_freq["source_n"]
	edge_freq["same"] <- 0
	edge_freq["same"][edge_freq["max_asjccode"] == edge_freq["asjccode01"],] <- 1
	
	edge_freq <- merge(edge_freq, label, by.x="asjccode01", by.y="asjc")
	edge_freq <- merge(edge_freq, label, by.x="max_asjccode", by.y="asjc")
	edge_freq <- edge_freq %>%
		rename(target_label=name.x, source_label=name.y)
	return(edge_freq)
}

fem_agg <- add_cov(fem_edge, label)
mal_agg <- add_cov(mal_edge, label)


fem_g = graph.data.frame(fem_agg, directed=TRUE)
mal_g = graph.data.frame(mal_agg, directed=TRUE)


add_attributes <- function(g, g_edge, label) {
	E(g)$source_prop <- as.numeric(unlist(g_edge["source_prop"]))
	E(g)$same <- as.numeric(unlist(g_edge["same"]))
	E(g)$color = colour_values(E(g)$source_prop, palette = "RdYlBu")
	
	vertex_list <- as.data.frame(V(g)$name)
	colnames(vertex_list) <- c("asjc")
	vertex_list <- merge(vertex_list, label, by="asjc", all.x=TRUE)
	V(g)$label <- vertex_list$name
	return(g)
}

fem_g <- add_attributes(fem_g, fem_agg, label)
fem_g_sim <- subgraph.edges(fem_g, which(E(fem_g)$same==0))






mean(degree(fem_g))
mean(degree(mal_g))
mean(betweenness(fem_g))
mean(betweenness(mal_g))
mean(transitivity(fem_g))
mean(transitivity(mal_g))
graph.density(fem_g)
graph.density(mal_g)

unlist(calculate_measures(fem_g))
unlist(calculate_measures(mal_g))



plot(fem_g_sim, vertex.label = NA)

plot(fem_g_sim, vertex.size = 3, vertex.color = NA,
	 vertex.frame.color=NA, edge.width=E(fem_g)$source_prop*10, layout=layout_nicely,
	 edge.color=E(fem_g)$color, vertex.label=V(fem_g)$label,
	 vertex.label.color=E(fem_g)$source_prop*10, edge.arrow.size=0.1)

plot(mal_g, vertex.size = 3, vertex.color = "purple",
	 vertex.frame.color = NA, edge.width=1, layout=layout_nicely,
	 edge.color=E(fem_g)$color, vertex.label = NA, edge.arrow.size=0.1)


write.csv(fem_edge, "fem_edge.csv")



#################

data <- data[c("author", "author_gender")]
data <- unique(data)
write.csv(data, "gender_label.csv")

###
# think about contribution and status

data <- data[c("author", "Lcum_sjr_std")]
data["status"] = NA
data["status"][data["Lcum_sjr_std"] > 0] <- 'H'
data["status"][data["Lcum_sjr_std"] < 0] <- 'L'

table(data["status"])

data <- subset("")
data <- data[c("author", "status")]
data <- unique(data)
write.csv(data, "status_label.csv")

##################

data["status"] = NA
data["status"][data["Lcum_sjr_std"] > 0] <- 'H'
data["status"][data["Lcum_sjr_std"] < 0] <- 'L'

edgelist <- data.frame(source=data["asjccode01"], author=data["author"],
					   status=data["status"], gender=data["author_gender"])

library(dplyr)

asjc_count <- data %>% 
	group_by(author) %>%
	summarise(count=n_distinct(asjccode01))

asjc_count <- data %>%
	group_by(author) %>%
	summarise(count=n_distinct(max_asjccode))

data$max_asjccode
table(asjc_count$count)

high <- edgelist[edgelist["status"] == 'H' & !is.na(edgelist["status"]),]
low <- edgelist[edgelist["status"] == 'L' & !is.na(edgelist["status"]),]

write.csv(edgelist, "sum_status.csv")
write.csv(high, "sum_status_high.csv")
write.csv(low, "sum_status_low.csv")



data["status"] = NA

edgelist <- data.frame(author=data["author"], source=data["asjccode01"],
					   data["cum_year"])
edgelist_10 <- subset(edgelist, cum_year <= 10)
edgelist_20 <- subset(edgelist, cum_year <= 20 & cum_year > 10)

write.csv(edgelist_10, "cum10.csv")
write.csv(edgelist_20, "cum20.csv")



var <- c("author", "author_gender", "author_org", "max_asjccode", "sjr_std",
		 "order", "weight_asjc", "cum_year")
data_mat <- data[var]

tmp <- unique(data.frame(data_mat[c("author", "author_gender", "max_asjccode")]))
tmp <- subset(tmp, !is.na(author_gender))

data_mat <- subset(data_mat, cum_year > 10 & cum_year <= 20)
data_mat <- merge(data_mat, merge[c("author", "total_pub")], by="author", all.x=TRUE) 

tmp2 <- data_mat %>%
	group_by(author) %>%
	#summarise(total_pub=n())
	summarise(journal_avg=mean(sjr_std, na.rm=TRUE))
	#summarise(sum_weight=sum(weight_asjc))

which.max(table(data_mat$author_org))

#merge <- merge(tmp, tmp2, by="author", all.x=TRUE)
merge <- merge(merge, tmp2, by="author", all.x=TRUE)
merge <- merge[ , !(names(merge) %in% c("author_org.x", "author_org.y"))]


library(tidyverse)
tmp3 <- data_mat %>%
	# add a column n with count by categories
	add_count(author, author_org) %>%
	# select max or first occurrence by patient
	group_by(author) %>%
	# keep only first TRUE
	mutate(author_org = author_org[n == max(n)][1]) %>%
	# do not keep temp var
	select(-n)

tmp3 <- unique(tmp3[c("author", "author_org")])
merge <- merge(merge, tmp3, by="author", all.x=TRUE)

tmp3 <- data_mat[data_mat["total_pub"] == (data_mat["order"]+1), c("author", "author_org")]
colnames(tmp3) <- c("author", "last_org")
merge <- merge(merge, tmp3, by="author", all.x=TRUE)

merge_backup <- merge

#tmp3[i, "author_org"]
#nrow(tmp3)
for (i in 1:nrow(merge)) {
	if (is.na(merge[i, "author_org"])) {
		merge[i, "author_org"] <- merge[i, "last_org"]
	}
}

merge_com <- merge[complete.cases(merge),]
#write.csv(merge_com, "merge_com_20.csv")


cur_move <- data %>% group_by(asjccode01) %>% summarise(n=n())
write.csv(cur_move, "asjc_count.csv")



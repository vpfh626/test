
library(haven)
library(dplyr)
library(data.table)


setwd("/Users/eehyun/Documents/Works/MA thesis/")
data <- read_dta("/Users/eehyun/Documents/Works/MA thesis/1016_omitted.dta")

data <- data[order(data$author, data$order),]

# divide by 10 years if needed
data <- subset(data, cum_year <= 10)

data_trim <- data[,c("author", "order", "weight_asjc", "asjccode01")]

new_df <- data_trim %>% group_by(author, asjccode01) %>% mutate(max_weight=cumsum(weight_asjc))
new_df <- new_df %>% group_by(author) %>% mutate(cummax = cummax(max_weight))

#author = new_df[1, "author"]


new_df <- data.table(new_df)

prev_author = -1
biggest_asjc = -1

for (i in 1:nrow(new_df)) {
	
	author = new_df[i, "author"]
	cur_weight = new_df[i, "max_weight"]
	cummax = new_df[i, "cummax"]
	
	if (author == prev_author) {
		if (cur_weight == cummax) {
			new_df[i, "max_asjc"] = new_df[i, "asjccode01"]
			biggest_asjc = new_df[i, "asjccode01"]
		} else {
			new_df[i, "max_asjc"] = biggest_asjc
		}
	} else {
		new_df[i, "max_asjc"] = new_df[i, "asjccode01"]
		biggest_asjc = new_df[i, "asjccode01"]
	}
	
	if (i %% 10000 == 0) print(i)
	prev_author = author
}

# total
import_df = data.frame(new_df)
data["max_asjc_new"] = import_df["max_asjc"]

tmp_df <- subset(import_df, order >= 2) %>%
	group_by(author) %>% summarize(n=n_distinct(max_asjc))
table(tmp_df["n"])

data <- merge(data, tmp_df, by="author", all.x=TRUE, all.y=FALSE)


write_dta(data, "max_revised.dta")
write.csv(data, "max_revised.csv")


##############################################

rm(list=ls())

data <- read.csv("/Users/eehyun/Documents/Works/MA thesis/max_revised.csv")

data_10 <- subset(data, cum_year <= 10)
data_20 <- subset(data, cum_year > 10 & cum_year <= 20)


tmp_df <- subset(data_10, order >= 1) %>%
	group_by(author) %>% summarize(n=n_distinct(max_asjc_new))

tmp_df <- subset(data_20, order >= 0) %>%
	group_by(author) %>% summarize(n=n_distinct(max_asjccode))

table(tmp_df["n"])


tmp_df <- data_10 %>%
	group_by(author, max_asjc_new) %>% summarize(n=n())
tmp_df <- tmp_df %>%
	group_by(author) %>% mutate(max_n=max(n))


new_df <- data.table(tmp_df)

prev_author = -1
biggest_asjc = -1

for (i in 1:nrow(new_df)) {
	
	author = new_df[i, "author"]
	cur_weight = new_df[i, "n"]
	cummax = new_df[i, "max_n"]
	
	if (author == prev_author) {
		if (cur_weight == cummax) {
			new_df[i, "max_asjc"] = new_df[i, "max_asjc_new"]
			biggest_asjc = new_df[i, "max_asjc_new"]
		} else {
			new_df[i, "max_asjc"] = biggest_asjc
		}
	} else {
		new_df[i, "max_asjc"] = new_df[i, "max_asjc_new"]
		biggest_asjc = new_df[i, "max_asjc_new"]
	}
	
	if (i %% 10000 == 0) print(i)
	prev_author = author
}

new_df <- data.frame(new_df)
new_df_sum <- new_df[, c("author", "max_asjc")]
new_df_sum <- unique(new_df_sum)

tmp <- new_df_sum %>%
	group_by(author) %>% summarize(n=n())
table(tmp["n"])


new_df_sum <- cbind(ID = 1:nrow(new_df_sum), new_df_sum)
new_df_sum <- new_df_sum %>% group_by(author) %>% mutate(max_id=max(ID))
new_df_sum <- subset(new_df_sum, max_id==ID)

new_df_sum <- new_df_sum[,c("author", "max_asjc")]

write.csv(new_df_sum, "max_asjc_label_agg20.csv")

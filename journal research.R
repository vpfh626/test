#install.packages("haven")
library(haven)


data <- read_dta("/Users/eehyun/Documents/Works/MA thesis/test_100_1016.dta")
summary(data)

######################## org change

data2 <- data[complete.cases(data[, c('author_org', 'order')]),]
data2 <- data2[data2$max_order > 2,]

initial <- -1
data2$org_change <- -1
prev_org = ""
prev_author = ""

for (i in 1:nrow(data2)) {
	if (data2$order[i] == 0 | data2$author[i] != prev_author) {
		initial = 0
	} else {
		if (data2$author_org[i] != prev_org) {
			initial = initial + 1
		}
	}
	
	if (i %% 10000 == 0) {
		print(i)
	}
	
	data2$org_change[i] <- initial
	prev_org = data2$author_org[i]
	prev_author = data2$author[i]
}

table(data2$org_change)
head(data2[, c('author', 'author_org', 'org_change', 'max_order')], 50)

data2[data2$org_change == 0, c('author', 'author_org', 'org_change',
									'max_order', 'year')]

example <- data2[data2$author == 6506154857, c('author', 'author_org', 'org_change',
											   'max_order', 'year')]
example

for (j in 1:nrow(data2)) {
	i = nrow(data2) - j + 1
	if (data2$order[i] == 0 | data2$author[i] != prev_author) {
		initial = 0
	} else {
		if (data2$author_org[i] != prev_org) {
			initial = initial + 1
		}
	}
	
	if (i %% 10000 == 0) {
		print(i)
	}
	
	data2$org_change[i] <- initial
	prev_org = data2$author_org[i]
	prev_author = data2$author[i]
}

#################### matching

head(data)

data <- subset(data, select=c("author", "weight_asjc", "paper_id", "total_author",
							  "author_loc", "combined_year", "year", "journal_id",
							  "sjr", "hindex", "totalcites3years", "country", "citescore",
							  "citationcount", "percentcited", "quartile", "asjccode01",
							  "asjccode_num", "order", "cum_year", "author_type_total",
							  "max_asjccode", "move_type", "author_gender", "paper_freq",
							  "z_score", "max_z_score", "cum_sjr", "modality",
							  "cum_sjr_mn", "cum_sjr_std", "Lcum_sjr_std"))

write_dta(data, "/Users/eehyun/Downloads/Journal network/1016_omitted.dta")



############

library(foreign)
library(ggplot2)

data3 <- subset(data2, cum_year < 20)
data4 <- subset(data2, cum_year >= 20)
table(data2$cum_year)
ols <-lm(same_code ~ Lcum_sjr_std + author_gender + Lcum_sjr_std * author_gender, data=data4)
summary(ols)

data["Lcum_sjr_std_b"] = NA
data[data["Lcum_sjr_std"] < 0 & !is.na(data["Lcum_sjr_std"]), c("Lcum_sjr_std_b")] = "L"
data[data["Lcum_sjr_std"] >= 0 & !is.na(data["Lcum_sjr_std"]), c("Lcum_sjr_std_b")] = "H"
table(data["Lcum_sjr_std_b"])


data2 <- data[complete.cases(data[, c('order', 'author_gender', 'same_code', 'cum_year')]),]

#install.packages("Rmisc")
library(Rmisc)
#tgc <- summarySE(data2, measurevar="same_code", groupvars=c("author_gender","Lcum_sjr_std_b","cum_year"))
tgc <- summarySE(data, measurevar="same_code", groupvars=c("Lcum_sjr_std_b", "author_gender", "max_asjccode"))

tgc <- tgc[tgc["cum_year"] > 0,]
tgc <- tgc[tgc["cum_year"] < 41,]
tgc <- tgc[tgc["author_gender"] != 'N',]
tgc["author_gender"][tgc["author_gender"] == "F",] = "Female"
tgc["author_gender"][tgc["author_gender"] == "M",] = "Male"

# tgc["group"][tgc["group"] == "F 0",] = "Female * Low"
# tgc["group"][tgc["group"] == "M 0",] = "Male * Low"
# tgc["group"][tgc["group"] == "F 1",] = "Female * High"
# tgc["group"][tgc["group"] == "M 1",] = "Male * High"

tgc["group"] = apply(tgc[c("author_gender", "Lcum_sjr_std_b")], 1, paste, collapse=" ")

table(tgc["N"])
tgc <- tgc[tgc["N"] > 60,]

tgc <- tgc[tgc["max_asjccode"] >= 3300,]
tgc <- tgc[tgc["max_asjccode"] < 3400,]
tgc <- subset(tgc, !is.na(Lcum_sjr_std_b))


label = read.csv("label.csv", header=FALSE)
colnames(label) <- c("number", "label")
#label[1, 1]

for (i in 1:nrow(label)) {
	tgc["max_asjccode"][tgc["max_asjccode"] == label[i, 1]] = label[i, 2]
}

ggplot(tgc, aes(x=cum_year, y=same_code, colour=factor(max_asjccode))) + 
	geom_errorbar(aes(ymin=same_code-se, ymax=same_code+se), width=.1) +
	geom_line() +
	geom_point() +
	ylab("Probability to remain in one's main field") + 
	xlab("Cumulative years") +
	labs(title="Probability to remain in the same field across career",
		 color = "max_asjccode")


tgc <- tgc[!is.na(tgc["Lcum_sjr_std_b"]),]
tgc2 <- tgc[tgc["Lcum_sjr_std_b"] == "H",]


lab_list <- list(unique(tgc["max_asjccode"]))
tmp = unlist(lab_list)
tgc["deviate"] <- 1 - tgc["same_code"]
odds = c()
for (i in tmp) {
	fh = as.numeric(tgc["deviate"][tgc["max_asjccode"] == i & tgc["group"] == "Female H"])
	fl = as.numeric(tgc["deviate"][tgc["max_asjccode"] == i & tgc["group"] == "Female L"])
	mh = as.numeric(tgc["deviate"][tgc["max_asjccode"] == i & tgc["group"] == "Male H"])
	ml = as.numeric(tgc["deviate"][tgc["max_asjccode"] == i & tgc["group"] == "Male L"])
	odds = append(odds, ((fl/fh) / (ml/mh)))
}


tgc2 <- tgc[tgc["group"] == "Female H",]
tgc2["odds"] <- odds

g <- ggplot(tgc2, aes(x=max_asjccode, y=odds,
				 #fill=group
				 )) + 
	geom_bar(stat="identity", position=position_dodge(), width=0.6) +
	geom_errorbar(aes(ymin=odds-ci, ymax=odds+ci), width=.1, position=position_dodge(.8)) +
	ylab("Probability to remain in one's main field") + 
	xlab(NULL) +
	labs(title="Odds ratio of publishing in other field (women vs. men)",
		 #fill = "Status"
		 ) +
	coord_flip() + scale_fill_brewer(palette = "Paired")

g + geom_hline(yintercept = 1.0, color="red")

#survival!!!!






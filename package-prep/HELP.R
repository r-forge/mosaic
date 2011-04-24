ds = read.csv("http://www.math.smith.edu/r/data/help.csv", 
	stringsAsFactors=TRUE)
ds$sex = factor(ds$female, labels=c('male','female'))             # ifelse(ds$female==1, "female", "male")
ds$homeless = factor(ds$homeless, labels=c('housed','homeless'))  # ifelse(ds$homeless==1, "homeless", "housed")
ds$g1b = factor(ds$g1b, labels=c('no','yes'))                     # ifelse(ds$g1b==1, "yes", "no")
ds$anysub = factor(ds$anysubstatus, labels=c('no','yes'))         # ifelse(ds$anysubstatus==1, "yes", "no")
ds$link = factor(ds$linkstatus, labels=c('no','yes'))             # ifelse(ds$linkstatus==1, "yes", "no")
ds$satreat = factor(ds$satreat, labels=c('no','yes'))             # ifelse(ds$satreat==1, "yes", "no")
ds$treat = factor(ds$treat, labels=c('no','yes'))                 # ifelse(ds$treat==1, "yes", "no")

cols2keep <- c("age",
"anysub",
"cesd",
"d1",
"daysanysub",
"dayslink",
"drugrisk",
"e2b",
"female",
"sex",
"g1b",
"homeless",
"i1",
"i2",
"id",
"indtot",
"link",
"mcs",
"pcs",
"pss_fr",
"racegrp",
"satreat",
"sexrisk",
"substance",
"treat")

cols2keep0 <- c("age",
"anysubstatus",
"cesd",
"d1",
"daysanysub",
"dayslink",
"drugrisk",
"e2b",
"female",
"g1b",
"homeless",
"i1",
"i2",
"id",
"indtot",
"linkstatus",
"mcs",
"pcs",
"pss_fr",
"racegrp",
"satreat",
"sexrisk",
"substance",
"treat")

print(setdiff(cols2keep, names(ds)))
print(setdiff(cols2keep0, names(ds)))

HELP = ds[,cols2keep]
HELP0 = ds[,cols2keep0]

names(HELP)
summary(HELP)
# these need to be moved to the proper place to be part of the package.
save(HELP, file="HELP.rda")
save(HELP0, file="HELP0.rda")
prompt(HELP, 'HELP.Rd')
prompt(HELP0, 'HELP0.Rd')

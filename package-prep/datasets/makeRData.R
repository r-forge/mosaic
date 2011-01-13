
dataDir <- "data"
csvDir <- "csv"
manDir <- "man"
setwd(csvDir)
csv.files <- dir(".", pattern = ".csv$")
setwd('..')
for (file in csv.files) {
	name <- strsplit(file, '\\.')[[1]][1]
	data <- read.csv(file.path(csvDir,file))
	assign(name, data)
	objlist <- ls(pattern=name)
	save(list=objlist, file=file.path(dataDir, paste(name, '.rda',sep="")))
	prompt(name=name, file=file.path(manDir, paste(name, '.Rd',sep="")))
	}

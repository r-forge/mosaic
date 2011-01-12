
csv.files <- dir(".", pattern = ".csv$")

dataDir <- file.path("..","pkg","mosaicData","data")
dataDir <- file.path('.')
for (file in csv.files) {
	name <- strsplit(file, '\\.')[[1]][1]
	data <- read.csv(file)
	assign(name, data)
	objlist <- ls(pattern=name)
	save(list=objlist, file=paste("data/", name, '.rda',sep=""))
	prompt(name=name, file=paste("man/", name, '.Rd',sep=""))
	}

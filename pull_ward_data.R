library(tesseract)
eng <- tesseract("eng")
text <- tesseract::ocr("./data/raw_data/ward_bird_change.png", engine = eng)
cat(text)

t2 <- cat(text)
write.csv(test, "./data/raw_data/raw_ward_data.csv", row.names = FALSE, quote = FALSE)

t2 <- read.csv("./data/raw_data/raw_ward_data.csv", stringsAsFactors = FALSE, header = FALSE)

# get species names
spec <- apply(t2, 1, function(x) strsplit(x, "\\("))
spec <- sapply(spec, function(x) x[1])
spec <- sapply(spec, function(x) x[1])
spec <- trimws(spec)
spec <- tolower(spec)

# get the increase or decrease

nums <- apply(t2, 1, function(x) strsplit(x, "\\s"))
nums<- sapply(nums, function(x) x[length(x)])
nums<- sapply(nums, function(x) x[length(x)])
nums <- gsub("—", "-", nums)
nums <- as.numeric(nums)

# combine them

ward <- data.frame(species = spec,
									 change = nums)

# Do some slight changes to species names in the ward data
ward$species <- gsub(
	"N\\.",
	"Northern",
	ward$species
)

write.csv(ward, "./data/ward_changes.csv", row.names = FALSE)

	

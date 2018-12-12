
species <- unq_spe[164]

windows(7,7)
bird_scrape <- function(species = NULL){
	# make into URL
	spurl <- species <- as.character(species)
	# step 1: replace space with _
  if(length(grep("\\s", spurl)) == 1){
  	spurl <- gsub("\\s", "_", spurl)
  }
	# step 2" remove '
	if(length(grep("'", spurl)) == 1){
		spurl <- gsub("'", "", spurl)
	}
	# step 3: make into URL
	abburl <- paste0("https://www.allaboutbirds.org/guide/",
									 spurl, "/lifehistory")
	#
	# try to pull html from site
	site_html <- read_html(abburl)
	
	# if this is the correct url then the species name should
	#  be in an h1 header
	species_name <- html_text(html_nodes(site_html, "h1"))
	# grab only the nchar of species name
	species_name <- substring(species_name, 1, nchar(as.character(species)))
	
	# this will be true if we are on the correct page
	if(length(agrep(species, species_name)) == 1){
		
		# get subheaders
		alt_text <- html_nodes(site_html, "article")[1] %>% 
			html_nodes(., "img") %>% html_attr(., "alt")
		# the first 5 are all we need
		alt_text <- alt_text[1:5]
		
		# get the header info so we can drop it from alt text
		h2_names <- html_nodes(site_html, "article") %>% 
			html_nodes(., "div") %>% 
			html_nodes(., "h2") %>% 
			html_text(.)
		h2_names <- h2_names[1:5]
		h2_names_regex <- paste(h2_names, collapse = "|")
		
		alt_text <- gsub(h2_names_regex, "", alt_text) %>% trimws()
		
		# now we need to grab the map for this species
		abburl <- paste0("https://www.allaboutbirds.org/guide/",
										 spurl, "/maps-range")
		map_html <- read_html(abburl)
		
		map_url <- html_nodes(map_html, ".main-column") %>% html_nodes(.,"img") %>% 
		html_attr(., "data-interchange") %>% strsplit(",") %>% unlist
		map_url[1] %>% gsub("\\[", "", .) %>% image_read(.) %>% 
			plot(.)
		
		answer <- menu(c("Breeding", "Migrating", 
										 "Nonbreeding",  "Year-round", "Wintering",
										 "Out of range"),
				 title = species)
		
		migratory <- c("Breeding", "Migrating",
									 "Nonbreeding",  "Year-round",
"Wintering","Out of range")[answer]
		
		# make it into a raster
		
		if(file.exists("species_info.csv")){
			other_row <- matrix(c(species, alt_text, migratory), ncol = 7, nrow = 1)
			write.table(other_row, 
									"species_info.csv", append = TRUE, sep = ",",
									col.names = FALSE, row.names = FALSE)
		} else {
			first_row <- matrix(c(species, alt_text, migratory), ncol = 7, nrow = 1)
			colnames(first_row) <- c("species", h2_names, "status")
			write.csv(first_row, "species_info.csv", row.names = FALSE)
		}

	} else {
		err <- paste0(species, " was not found on all about birds.\n")
		cat(err)
	}
	
}

for(i in 164:length(unq_spe)){
	print(i)
 bird_scrape(unq_spe[i])
}

unq_spe[3]
test <- read.csv("species_info.csv", header = TRUE, stringsAsFactors = FALSE)

which(!as.character(unq_spe) %in% test$species)


# scrape family

scrape_family <- function(species = NULL){
spurl <- species <- as.character(species)
# step 1: replace space with _
if(length(grep("\\s", spurl)) == 1){
	spurl <- gsub("\\s", "_", spurl)
}
# step 2" remove '
if(length(grep("'", spurl)) == 1){
	spurl <- gsub("'", "", spurl)
}
# step 3: make into URL
abburl <- paste0("https://www.allaboutbirds.org/guide/",
								 spurl, "/overview")
#
# try to pull html from site
site_html <- read_html(abburl)

# if this is the correct url then the species name should
#  be in an h1 header
species_name <- html_text(html_nodes(site_html, "h1"))
# grab only the nchar of species name
species_name <- substring(species_name, 1, nchar(as.character(species)))

if(length(agrep(species, species_name)) == 1){
	
	# get subheaders
	fam_text <- html_nodes(site_html, ".species-info")[1] %>% 
		html_nodes(., "li") %>% html_text(.)
	fam_text <- strsplit(fam_text, ":") %>% sapply(., "[", 2) %>% trimws(.)
	
	to_ret <- matrix(c(species, fam_text), ncol = 3, nrow = 1)
	colnames(to_ret) <- c("species", "order", "family")
	return(to_ret)
}
}

sp_family <- matrix(NA, ncol = 3, nrow = length(unq_spe))

for(i in 1:length(unq_spe)){
	sp_family[i,] <- scrape_family(unq_spe[i])
}

sp_family <- data.frame(sp_family, stringsAsFactors = FALSE)
colnames(sp_family) <- c("species", "order", "family")

# read in sp info
sp_info <- read.csv("./species_info.csv", header = TRUE, stringsAsFactors = FALSE)

to_save <- left_join(sp_info, sp_family, by = "species")

write.csv(to_save, "./data/species_life_history.csv", row.names = FALSE)

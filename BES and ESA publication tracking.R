### DAGs systematic review ###
### Tracking publications in ESA and BES journals:
###  
### Date: August 2026 modified by A. Simler-Williamson
### Purpose: Count papers per year (2021-2025) in British Ecological Society and Ecological Society of America journals using the Crossref API.

library(httr)
library(jsonlite)


# Put your email here for verification from CrossRef (i.e., you are not a bot)
email <- "youremail@org.org"
yrs <- 2021:2025 ## Query these years

# "journal-article" counts research articles only (incl. perspectives, corrections)
types <- "journal-article"

# Specify society, journal, ISSN. 
journals <- data.frame(
  Society = c(rep("BES", 7), rep("ESA", 6)),
  Journal = c(
    "Journal of Ecology",
    "Journal of Animal Ecology",
    "Journal of Applied Ecology",
    "Functional Ecology",
    "Methods in Ecology and Evolution",
    "People and Nature",
    "Ecological Solutions and Evidence",
    "Ecology",
    "Ecological Applications",
    "Ecological Monographs",
    "Ecosphere",
    "Frontiers in Ecology and the Environment",
    "Bulletin of the Ecological Society of America"
  ),
  ISSN = c(
    "1365-2745", 
    "1365-2656", 
    "1365-2664", 
    "1365-2435",
    "2041-210X", 
    "2575-8314", 
    "2688-8319",
    "1939-9170", 
    "1939-5582", 
    "1557-7015",
    "2150-8925", 
    "1540-9309", 
    "2327-6096"),
  stringsAsFactors = FALSE)

ua <- user_agent(sprintf("journal-counter/1.0 (mailto:%s)", email))


# To calculate counts per year:

count_year <- function(issn, year) {
  ## Filter by dates:
  filters <- c(
    sprintf("from-pub-date:%d-01-01", year),
    sprintf("until-pub-date:%d-12-31", year)
  )
  url <- sprintf("https://api.crossref.org/journals/%s/works", issn)
  
  ## This is a stopgap against server timeouts:
  for (attempt in 1:3) {
    r <- GET(url, ua, query = list(
      filter = paste(filters, collapse = ","),
      rows   = 0, mailto = email), timeout(30))
    
    if (status_code(r) == 200) {
      out <- fromJSON(content(r, "text", encoding = "UTF-8"))
      return(out$message$`total-results`) }
    Sys.sleep(2 * attempt)
  }
  stop(sprintf("Crossref request failed!!", issn, year, status_code(r)))
}

# Get the counts:

# Empty matrix to hold:
counts <- matrix(NA_integer_, 
                 nrow = nrow(journals), 
                 ncol = length(yrs),
                 dimnames = list(NULL, as.character(yrs)))

## Pull the counts per journal:
for (i in seq_len(nrow(journals))) {
  issn <- journals$ISSN[i]
  cat(sprintf("[%2d/%d] %s\n", i, nrow(journals), journals$Journal[i]))
  for (j in seq_along(yrs)) {
    counts[i, j] <- count_year(issn, yrs[j])
  }
}

df <- cbind(journals, as.data.frame(counts))

head(df)



# Save counts:
write.csv(df, "data/ESA_BES_journal_counts.csv", row.names = FALSE, fileEncoding = "UTF-8")

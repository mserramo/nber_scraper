if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, lubridate, hrbrthemes, janitor, jsonlite, fredr, 
               listviewer, usethis, pdftools, tm, SnowballC, corpus, NLP, RWeka, tau)

endpoint = "https://www.nber.org/api/v1/working_page_listing/contentType/working_paper/_/_/search?page=1&perPage=100&sortBy=public_date"
work_papers_temp = fromJSON(endpoint)

working_papers =  work_papers_temp$results %>% 
  select(publisheddate:url) %>% 
  select(-c(newthisweek, type)) %>% 
  clean_names() %>% 
  select(title, publisheddate, abstract, nid, url) %>% 
  as_tibble()  

#### Create dataframe with the names of authors.
authors_list =  work_papers_temp$results$authors

N <- length(authors)
length <- data.frame(num=rep(NA, N))

#Get number of authors in paper.
for (i  in 1:length(authors)){
  length[i, 1] <- length(authors[[i]])
}

max <- max(length)
authors <- data.frame(matrix(ncol = max, nrow = N))

for (i  in 1:N){
  authors[i, 1:length(authors_list[[i]])] <- authors_list[[i]]
}

working_papers = cbind(working_papers, authors)

setwd("pdfs")

base_url <- "https://www.nber.org/system/files/working_papers/"

#### Download and save papers in dataframe.
for (i in 1:100){
  paper_code <- substring(working_papers[i, "url"], 9)
  url <-paste0(base_url, paper_code, "/", paper_code, ".pdf")
  filename <- paste0(paper_code, '.pdf')
  if (file.exists(filename)){
    next('Ya existe')
  } 
  else {download.file(url, destfile = filename, mode="wb")
  }
}

# ####TEXT MINING. (congelado, primero me enfoco en parte de)
# 
# #Create list of downloaded PDFs.
# 
# files <- list.files(pattern = "pdf$")
# 
# #Create corpus with the PDFs.
# 
# papers <- Corpus(URISource(files),
# readerControl = list(reader = readPDF))
# 
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# 
# tdm <- TermDocumentMatrix(papers, control = list(removePunctuation = TRUE,
#                                                  stopwords = TRUE,
#                                                  tolower = TRUE,
#                                                  removeNumbers = TRUE, tokenize = BigramTokenizer))
# 
# ft <- findFreqTerms(tdm, lowfreq = 1, highfreq = Inf)
# 
# ft.tdm <- as.matrix(tdm[ft,])
# sort(apply(ft.tdm, 1, sum), decreasing = TRUE)
# 

#TO-DO 1: Use pdftools to select first 2 or 3 pages, so that I can waste less resources in text mining.
#TO-DO 2: Download data from several NBER working pages (not only the first one).
#TO-DO 3: Do some plot on # of authors or something like that.
#TO-DO 4: Within the downloader loop, include the possibility of an error. 
#if there is an error, jump right to the next index and make an annotation of the index that
#... caused the error.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, lubridate,, janitor, jsonlite, fredr, 
               listviewer, usethis, pdftools, tm, SnowballC, corpus, NLP, RWeka, tau)


gather_metadata = 
  function(x){
    endpoint = paste0("https://www.nber.org/api/v1/working_page_listing/contentType/working_paper/_/_/search?page=",x,"&perPage=100&sortBy=public_date")
    work_papers_temp = fromJSON(endpoint)
    
    working_papers =  work_papers_temp$results %>% 
      select(publisheddate:url) %>% 
      select(-c(newthisweek, type)) %>% 
      clean_names() %>% 
      select(title, publisheddate, abstract, nid, url) %>% 
      as_tibble()  
    
    #### Create dataframe with the names of authors.
    authors_list =  work_papers_temp$results$authors
    
    N <- length(authors_list)
    length <- data.frame(num=rep(NA, N))
    
    #Get number of authors in paper.
    for (i  in 1:N){
      length[i, 1] <- length(authors_list[[i]])
    }
    
    max <- max(length)
    authors <- data.frame(matrix(ncol = max, nrow = N))
    
    for (i  in 1:N){
      authors[i, 1:length(authors_list[[i]])] <- authors_list[[i]]
    }

    working_papers = cbind(working_papers, authors)
    return(working_papers)
  }

paper_metadata = 
  function(first_page, last_page){
    
    #Create list of pages.
    pages = seq(first_page, last_page, by = 1)
    
    dataset =
      lapply(pages, gather_metadata) %>% ## Run the iteration
      bind_rows() ## Bind the resulting list of data frames into a single data frame
    
    return(dataset)
    }


download_papers = 
  function(x){
  endpoint = paste0("https://www.nber.org/api/v1/working_page_listing/contentType/working_paper/_/_/search?page=",x,"&perPage=100&sortBy=public_date")
  work_papers_temp = fromJSON(endpoint)
  
  working_papers =  work_papers_temp$results %>% 
    select(url)  %>% 
    as_tibble()  
  
  base_url <- "https://www.nber.org/system/files/working_papers/"
  
  #### Download and save papers in dataframe.
  for (i in 1:100){
    paper_code <- substring(working_papers[i, "url"], 9)
    url <-paste0(base_url, paper_code, "/", paper_code, ".pdf")
    filename <- paste0(paper_code, '.pdf')
    if (file.exists(filename)){
      next('Ya existe')
    } 
    else {
      tryCatch(download.file(url, destfile = filename, mode="wb"),
              error = function(e) print(paste(url, 'did not work out')))
    }
    Sys.sleep(3)
  }

  }


downloader = 
  function(first_page, last_page){
  ifelse(!dir.exists("pdfs"), dir.create(file.path("pdfs")), FALSE)
  setwd("pdfs")
  
  pages = seq(first_page, last_page, by = 1)
  lapply(pages, download_papers)  
  
  setwd("..")
  }


downloader(1, 2)

getwd()




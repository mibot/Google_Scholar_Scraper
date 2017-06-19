GScholar_Scraper <- function(input, since = 1900, write = F){
  require(httr)
  require(XML)
  require(RCurl)
  
  # putting together the search-URL:
  URL <- paste0("http://scholar.google.com/scholar?q=",input,"&btnG=&hl=en&as_sdt=1&as_vis=1&as_ylo=",since)
  
  cat("\nThe URL used is: ", "\n----\n", paste0("* ","http://scholar.google.com/scholar?q=",input,"&btnG=&hl=en&as_sdt=1&as_vis=1&as_ylo=", since, " *"))
  
  # get content and parse it:
  #html <- getURL(URL, .encoding = "CE_UTF8")
  doc <- htmlParse(URL)
  
  # number of hits:
  h1 <- xpathSApply(doc, "//div[@id='gs_ab_md']", xmlValue)
  
  h2 <- unlist(strsplit(h1, "\\s"))
  
  # in splitted string it is the second element which contains digits,
  # grab it and remove decimal signs and convert to integer
  num <- as.integer(gsub("[[:punct:]]", "", h2[grep("\\d", h2)[1]]))
  cat("\n\nNumber of hits: ", num, "\n----\n", "If this number is far from the returned results\nsomething might have gone wrong..\n\n", sep = "")
  # If there are no results, stop and throw an error message:
  if (num == 0 | is.na(num)) {
    stop("\n\n...There is no result for the submitted search string!")
  }
  
  pages.max <- ceiling(num/10)
  #print(pages.max)
  # 'start' as used in URL:
  start <- 10 * 1:pages.max - 10
  #print(start)
  # Collect URLs as list:
  URLs <- paste("http://scholar.google.com/scholar?start=",start,"&q=",input, 
                "&btnG=&hl=en&as_sdt=1&as_vis=1&as_ylo=",since,sep="")
  
  
  GS_xpathSApply <- function(doc, path, FUN) {
    path.base <- "//div[@class='gs_r']"
    
    # get xpaths to each child node of interest
    nodes.len <- length(xpathSApply(doc, path.base))
    paths <- sapply(1:nodes.len, function(i) paste(path.base, "[", i, "]", path, sep = ""))
    
    # extract child nodes
    if (identical(FUN,xmlGetAttr)) {
      xx <- sapply(paths, function(x) xpathSApply(doc, x, FUN, 'href'), USE.NAMES = FALSE)
    } else {
      xx <- sapply(paths, function(x) xpathSApply(doc, x, FUN), USE.NAMES = FALSE)
    }
    
    # convert NULL to NA in list
    xx[sapply(xx, length) < 1] <- NA
    
    # return node values as a vector
    xx <- as.vector(unlist(xx))
    return(xx)
  }
  
  scraper_internal <- function(URL){
    doc <- htmlParse(URL)
    
    #titles:
    title <- GS_xpathSApply(doc, "//h3", xmlValue)
    
    #types:
    type = GS_xpathSApply(doc, "//h3//span//span[@class='gs_ct1']", xmlValue)
    
    #authors:
    author = GS_xpathSApply(doc, "//div[@class='gs_ri']//div[@class='gs_a']", xmlValue)
    
    #links:
    main_url = GS_xpathSApply(doc,  "//h3//a[@href]", xmlGetAttr)
    direct_url = GS_xpathSApply(doc,  "//div[@class='gs_ggs gs_fl']//a[@href]", xmlGetAttr)
    
    options(warn=(-1))
    dat <- data.frame(TITLE = title, 
                      TYPE = type, 
                      AUTHORS = author,
                      YEAR = as.integer(gsub(".*\\s(\\d{4})\\s.*", "\\1", author)), 
                      MAIN_LINK = main_url,
                      DIRECT_LINK = direct_url,
                      stringsAsFactors = FALSE)
    
    options(warn=0)
    free(doc)
    dat$TITLE <- sub(".*\\] ", "", dat$TITLE)
    dat$TYPE <- gsub("\\]", "", gsub("\\[", "", dat$TYPE))
    return(dat)
  }
  
  result <- do.call("rbind", lapply(URLs, scraper_internal))
  if (write == T) {
    write.csv(result, "/Users/mbotto/Google_Scholar_Scraper/GScholar_Output.CSV",
              row.names = F)
    
    #shell.exec("GScholar_Output.CSV")
  } #else {
    return(result)
  #}
  
  
}
input <- "allintitle:live on mars"
df <- GScholar_Scraper(input, since = 2008, write = T)
print(xtable(df), type="html", file="/Users/mbotto/Google_Scholar_Scraper/GScholar_Output.html")

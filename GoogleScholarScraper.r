# Author: Miguel Botto Tobar
# Date: 2017-06-19
# Description: This function will retrieve as information as it can about each result on a Google Scholar search page
# Reference: Tony Breyal - https://github.com/tonybreyal/Blog-Reference-Functions/tree/master/R/googleScholarXScraper
# Copyright (c) 2017, under the Creative Commons Attribution-NonCommercial 4.0 Unported (CC BY-NC 4.0) License 
# For more information see: http://creativecommons.org/licenses/by-nc/4.0/
# All rights reserved.

GScholar_Scraper <- function(input, page.ini = 1, pages.max = 0, since = 1900, to = 2017, write = F){
  require(httr)
  require(XML)
  require(RCurl)
  
  # putting together the search-URL:
  URL <- paste0("http://scholar.google.com/scholar?q=",input,"&hl=en&as_sdt=1,5&as_vis=1&as_ylo=",since,"&as_yhi=",to)
  
  cat("\nThe URL used is: ", "\n----\n", paste0("* ","http://scholar.google.com/scholar?q=",input,"&hl=en&as_sdt=1,5&as_vis=1&as_ylo=", since, "&as_yhi=",to," *"))
  
  # get content and parse it:
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
  
  #if (page.ini == 0 | is.na(page.ini)) page.ini <- 1
  if (pages.max == 0 | is.na(pages.max)) pages.max <- ceiling(num/10)
  
  # 'start' as used in URL:
  start <- 10 * page.ini:pages.max - 10
  
  
  # Collect URLs as list:
  #URLs <- paste("http://scholar.google.com/scholar?start=",start,"&q=",input, 
  #              "&btnG=&hl=en&as_sdt=1&as_vis=1&as_ylo=",since,sep="")
  URLs <- paste("http://scholar.google.com/scholar?start=",start,"&q=",input, 
                "&hl=en&as_sdt=1,5&as_vis=1&as_ylo=",since,"&as_yhi=",to,sep="")
  
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
    xx <- sapply(1:length(xx), function(x) xx[[x]][1])
    
    # convert NULL to NA in list
    xx[sapply(xx, length) < 1] <- NA
    
    # return node values as a vector
    xx <- as.vector(unlist(xx))
    
    return(xx)
  }
  
  scraper_internal <- function(URL){
    print(URL)
    
    doc <- htmlParse(URL, encoding="UTF-8")
    
    #titles:
    title <- GS_xpathSApply(doc, "//div[@class='gs_ri']//h3[@class='gs_rt']", xmlValue)
    
    #types:
    #type = GS_xpathSApply(doc, "//h3//span//span[@class='gs_ct1']", xmlValue)
    
    #authors:
    author <-  GS_xpathSApply(doc, "//div[@class='gs_ri']//div[@class='gs_a']", xmlValue)
    
    #source:
    source  <-  GS_xpathSApply(doc, "//div[@class='gs_ggs gs_fl']//a[@href]", xmlValue)
    #print(source)
    #m.url <-  xpathSApply(doc, "//div[@class='gs_r']//div[@class='gs_ri']//h3[@class='gs_rt']", function(x) ifelse(is.null(xmlChildren(x)$a), NA, xmlAttrs(xmlChildren(x)$a, 'href')))
    #print(m.url) function(x) ifelse(x, NA, xmlGetAttr(x, 'href'))
    #o.url <- xpathSApply(doc, "//div[@class='gs_fl']//a[@class='gs_nph'][contains(., 'versions')]", function(x) ifelse(is.null(x), NA, xmlGetAttr(x, 'href')))
    #print(o.url)
    #d.url <-  xpathSApply(doc, "//div[@class='gs_r']//div[@class='gs_ggs gs_fl']", xmlValue)
    
    #links:
    mainLink <-  GS_xpathSApply(doc,  "//h3//a[@href]", xmlGetAttr)
    #print(mainLink)
    otherVersionLink <- GS_xpathSApply(doc, "//div[@class='gs_ri']//div[@class='gs_fl']//a[@class='gs_nph'][contains(., 'versions')]", xmlGetAttr)
    
    directLink <-  GS_xpathSApply(doc,  "//div[@class='gs_ggs gs_fl']//a[@href]", xmlGetAttr)
    #print(directLink)
    
    #cited by:
    cited.by = GS_xpathSApply(doc, "//div[@class='gs_ri']//div[@class='gs_fl']//a[contains(.,'Cited by')]/text()", xmlValue)
    
    #cited ref:
    #cited.ref = GS_xpathSApply(doc, "/div[@class='gs_ri']//div[@class='gs_fl']//a[contains(.,'Cited by')][@href]", xmlAttrs)
    
    
    #print(direct_url)
    
    #types:
    #type = GS_xpathSApply(doc, "//div[@class='gs_ggs gs_fl']//span[@class='gs_ctg2']", xmlValue)
    
    options(warn=(-1))
    dat <- data.frame(source = source,
                      title = title, 
                      #TYPE = type, 
                      authors = author,
                      year = as.integer(gsub(".*\\s(\\d{4})\\s.*", "\\1", author)), 
                      ncites = as.integer(as.integer(gsub("[^0-9]", "", cited.by))), 
                      #CITED_REF = cited.ref,
                      mainLink = mainLink,
                      otherVersionLink = otherVersionLink,
                      directLink = directLink,
                      stringsAsFactors = FALSE)
    
    options(warn=0)
    dat$source <- gsub(" ", "", gsub(".*\\]", "", dat$source))
    dat$title <- gsub(".*\\]", "", dat$title)
    #dat$otherVersionLink <- gsub("#", NA, dat$otherVersionLink)
    
    #to be used in Rascal
    dat$source <- sapply(dat$source, function(x) ifelse (is.na(x), NA, paste("|http://",x,"|", sep = "")))
    dat$mainLink <- sapply(dat$mainLink, function(x) ifelse (is.na(x), NA, paste("|",x,"|", sep = "")))
    dat$otherVersionLink <- sapply(dat$otherVersionLink, function(x) ifelse (is.na(x), NA, paste("|https://scholar.google.com",x,"|", sep = "")))
    dat$directLink <- sapply(dat$directLink, function(x) ifelse (is.na(x), NA, paste("|",x,"|", sep = "")))
    
    
    free(doc)
    return(dat)
  }
  
  result <- do.call("rbind", lapply(URLs, scraper_internal))
  if (write == T) {
    con <- file("/Users/mbotto/Google_Scholar_Scraper/GScholar_Output.CSV", encoding = "UTF-8")
    write.csv(result, con,
              row.names = F)
  } 
  return(result)
  
}

input <- '"crosslanguage" +copy +detection'
df <- GScholar_Scraper(input, 1, 0, since = 2008, to = 2017)



df <- cbind(id = as.numeric(rownames(df)), downloaded = as.character(FALSE), df)
df_save <- df[c(-5:-7)]
con <- file("/Users/mbotto/Google_Scholar_Scraper/ProgressData.csv", encoding = "UTF-8")
write.csv(df_save, con, row.names = F, na = "")


##check url
readUrl <- function(url) {
  out <- tryCatch(
    {
      HEAD(url)
    },
    error = function(cond) {
      return(NA)
    },
    warning = function(cond) {
      return(NA)
    },
    finally = { 
    }
  )    
  return(out)
}

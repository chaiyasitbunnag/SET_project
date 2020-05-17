library(xml2)
library(rvest)
library(httr)
library(readr)

# check if crawling allowed
robotstxt::paths_allowed(domain = "https://www.set.or.th")

# test zone ------------------------------------

# test to get url for each company
url <- "https://www.set.or.th/set/commonslookup.do?language=th&country=TH&prefix=A"
htmlpage <- read_html(url)
#htmlpage %>% html_node(xpath = "//div[@class = 'table']")
links <- htmlpage %>% html_node(".table") %>% html_nodes("a") %>% html_attr("href")
test <- links[1]
test <- paste0("https://www.set.or.th/", test)
company_name <- read_html(test) %>% html_node(".col-xs-12.col-md-9") %>% html_text()
company_info <- read_html(test) %>% html_nodes(".col-xs-9.col-md-5") %>% html_text()
#company_info[c(1,4,5,6)] #1 = PE; 4 = mkt cap; 5 = main index; 6 = industry
pe <- company_info[1]
mkt_cap <- company_info[4]
main_index <- company_info[5]
industry <- company_info[6]

# combine to data.frame
company_df <- data.frame(company_name = trimws(company_name),
                         pe = trimws(pe),
                         mkt_cap = parse_double((parse_character(gsub(",","",trimws(mkt_cap))))),
                         main_index = trimws(main_index),
                         industry = trimws(industry))




# 1. get company urls in SET -----------------
# prepare parameters
prefixes <- c("NUMBER", toupper(letters[1:26]))
search_pages <- paste0("https://www.set.or.th/set/commonslookup.do?language=th&country=TH&prefix=", prefixes)
# empty vector to store url data
company_paths <- vector()
#i = 1
for(i in 1:length(search_pages)) {
   page_url <- search_pages[i]
   # get xml from the page
   htmlpage <- read_html(page_url)
   # control flow: if xml can be found then crawl the company links if not return "cannot crawl"
   if(length(htmlpage)>0) {
      print(paste0("search page = ", i))
      company_paths_temp <-
         htmlpage %>%
         html_node(".table") %>%
         html_nodes("a") %>%
         html_attr("href")
      
   } else if(length(htmlpage)==0) {
      print("cannot crawl")
   }
   # store all paths
   company_paths <- c(company_paths, company_paths_temp)
}

# we received only paths, let's create full urls
company_full_urls <- paste0("https://www.set.or.th/", company_paths)
length(company_full_urls) == length(unique(company_full_urls))

# 2. get each company's details --------------
company_info_df <- data.frame()
#ii = 1
for(ii in 1:length(company_full_urls)) {
   company_url <- company_full_urls[ii]
   company_name_temp <- read_html(company_url) %>% html_node("h3") %>% html_text()
   company_info_temp <- read_html(company_url) %>% html_nodes(".col-xs-9.col-md-5") %>% html_text()
   # control flow: if crawlable then get target elements
   if(length(company_name_temp)>0 && length(company_info_temp)>0) {
      print(paste0("getting company's info; index = ", ii))
      pe_temp <- company_info_temp[1]
      mkt_cap_temp <- company_info_temp[4]
      main_index_temp <- company_info_temp[5]
      industry_temp <- company_info_temp[6]
   } else next
   # gather all elements
   combined <- data.frame( company_name = company_name_temp,
                           pe = pe_temp,
                           mkt_cap = mkt_cap_temp,
                           main_index = main_index_temp )
   # store every company's data
   company_info_df <- rbind(company_info_df, combined)

}

company_info_df_2 <- cbind(company_full_urls, company_info_df)

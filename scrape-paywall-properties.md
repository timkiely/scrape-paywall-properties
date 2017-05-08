Scrapping Example: Property Wall (Malaysian Real Estate)
================

Example taken from [Bob Rudis' Blog](https://rud.is/b/2017/05/05/scrapeover-friday-a-k-a-another-r-scraping-makeover/)

This is an example of combining httr, rvest and purrr to 1) create a list of pages to scrape and 2) apply the httr functions sequentially, without a for loop, and adding a progress bar.

``` r
library(httr)
library(rvest)
```

    ## Loading required package: xml2

``` r
library(stringi)
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
get_page <- function(i=1, pb=NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  POST(url = "http://www.propwall.my/wp-admin/admin-ajax.php", 
       body = list(action = "star_property_classified_list_change_ajax", 
                   tab = "Most Relevance", 
                   page = as.integer(i), location = "Mont Kiara", 
                   category = "", listing = "For Sale", 
                   price = "", keywords = "Mont Kiara, Kuala Lumpur", 
                   filter_id = "17", filter_type = "Location", 
                   furnishing = "", builtup = "", 
                   tenure = "", view = "list", 
                   map = "on", blurb = "0"), 
       encode = "form") -> res
  
  stop_for_status(res)
  
  res <- content(res, as="parsed") 
  
  Sys.sleep(sample(seq(0,2,0.5), 1))
  
  res
  
}
```

``` r
get_page(1) %>% 
  html_node(xpath=".//a[contains(., 'Classifieds:')]") %>% 
  html_text() %>% 
  stri_match_last_regex("([[:digit:],]+)$") %>% 
  .[,2] %>% 
  stri_replace_all_fixed(",", "") %>% 
  as.numeric() -> classified_ct

total_pages <- 1 + (classified_ct %/% 20)
```

``` r
get_listings <- function(pg) {
  data_frame(
    link = html_nodes(pg, "div#list-content > div.media * h4.media-heading > a:nth-of-type(1)" ) %>%  html_attr("href"),
    description = html_nodes(pg, "div#list-content > div.media * h4.media-heading > a:nth-of-type(1)" ) %>% html_text(trim = TRUE)  
  )
}
```

``` r
pb <- progress_estimated(total_pages)
if(!exists("listings_df")){
  (listings_df <- map_df(1:total_pages, ~get_listings(get_page(.x, pb))))
}
```

    ## # A tibble: 1,585 × 2
    ##                                                                           link
    ##                                                                          <chr>
    ## 1  http://www.propwall.my/classifieds/13904619/vista-kiara-mont-kiara-condomin
    ## 2  http://www.propwall.my/classifieds/13904590/mont-kiara-sophia-mont-kiara-co
    ## 3  http://www.propwall.my/classifieds/13856803/28-mont-kiara-mont-kiara-condom
    ## 4  http://www.propwall.my/classifieds/13909194/kiaramas-sutera-mont-kiara-cond
    ## 5  http://www.propwall.my/classifieds/13829707/kiara-hills-mont-kiara-house-fo
    ## 6  http://www.propwall.my/classifieds/11830564/10-mont-kiara-mont-kiara-condom
    ## 7  http://www.propwall.my/classifieds/13640370/kiara-designer-suites-mont-kiar
    ## 8  http://www.propwall.my/classifieds/13643311/hijauan-kiara-mont-kiara-condom
    ## 9  http://www.propwall.my/classifieds/13646440/kiaraville-mont-kiara-condomini
    ## 10 http://www.propwall.my/classifieds/13643359/verve-suites-mont-kiara-condomi
    ## # ... with 1,575 more rows, and 1 more variables: description <chr>

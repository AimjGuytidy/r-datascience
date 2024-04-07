library(rvest)
larecherche <- read_html("https://www.abebooks.com/servlet/SearchResults?pt=book&sortby=17&tn=a+la+recherche+du+temps+perdu&an=proust&cm_sp=pan-_-srp-_-ptbook")
titlehtml <- html_elements(larecherche, xpath = "//span[@data-cy='listing-title']")
titletext <-html_text2(titlehtml)
pricehtml<-html_elements(larecherche, xpath = "//p[@class='item-price']")
pricetext<-html_text2(pricehtml)

combined <- data_frame(title=titletext, `data and time`=Sys.time(), price=pricetext)

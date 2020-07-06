
# 네이버 영화제목 크롤링

# https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb/related

setwd("c:/Rdata")

library(rvest)
library(stringr)
library(dplyr)

rm(list = ls())
title = c()
grade = c()
url_b = "https://movie.naver.com/movie/point/af/list.nhn?&page="


for (i in 1:100){
craw_url = paste0(url_b,1,sep="") # paste = 자체적으로 붙음 
craw_url

t_css = ".color_b"

title_part = read_html(craw_url,encoding = "CP949") %>%
  html_nodes(t_css)%>%
  html_text

title = c(title, title_part)
}

View(title)















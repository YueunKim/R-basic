
# 성경 출애굽기 크롤링

setwd("c:/Rdata")

library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)

install.packages("wordcloud2")
library(wordcloud2)

cnt = c()
b_url = "https://www.bskorea.or.kr/bible/korbibReadpage.php?version=GAE&book=exo&chap="


for (i in 1:40) {
  cr_url = paste0(b_url, i)
  t_css = "#tdBible1 span"
  hdoc = read_html(cr_url, encoding = "UTF-8")
  n_css = html_nodes(hdoc,t_css)
  cnt_parts = html_text(n_css)
  cnt_parts = gsub("\\d+","",cnt_parts)
  cnt_parts
  cnt_parts = str_trim(cnt_parts, side = "both")
  cnt_parts
  cnt = c(cnt, cnt_parts)
}


library(KoNLP)
txt = sapply(cnt, extractNoun, USE.NAMES = F)
txt = unlist(txt)
count = Filter(function(x){nchar(x) >= 2}, txt)
word = table(count)

kk = head(sort(word, decreasing = T), 20)
kk

tt = barplot(kk, col = rainbow(20), ylim = c(0,350), las = 2)
text(tt, kk, label=paste0(kk,"개"), pos = 3, col = 2)


library(RColorBrewer)
display.brewer.all()
palate = brewer.pal(9,"Oranges")
wordcloud(names(word), freq = word, min.freq = 2, scale = c(5,0.5),
          random.order = F, random.color = T, colors = palate)


?wordcloud2()
wordcloud2(data = word, size = 0.4, shape = 'diamond')

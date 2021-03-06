data = read_csv('data/generated/pantheon_cleaned.csv')
data = data %>% 
  arrange(-TotalPageViews) %>% 
  mutate(part = ceiling(row_number() / n() * 100))

for(i in 1:100) {
  ids = data %>% filter(part == i) %>% .$en_curid
  partData = list()
  for(j in 1:length(ids)) {
    print(paste("part: ", i, ",  j: ", j))
    url = paste("https://en.wikipedia.org/?curid=", ids[j], sep = "")
    
    tryCatch(
      {
        doc = read_html(url)
        text = doc %>%
          html_nodes("#content div") %>%
          html_nodes("p") %>%
          html_text()
        partData[[j]] = list(id = ids[j], text = text)
      },
      warning = function(war) {},
      error = function(e) {print(paste("Error! in", i, j))}
    )
    
    # Sys.sleep(time = 0.1)
  }
  filePath <- paste('data/generated/wikipedia/raw/part', i, '.rds', sep = "")
  saveRDS(partData, filePath)
}

dfs = list()
for(i in 1:100) {
  filePath <- paste('data/generated/wikipedia/raw/part', i, '.rds', sep = "")
  partData = readRDS(filePath)
  partTexts = data.frame(en_curid = numeric(), text = character(), stringsAsFactors = F)
  for (j in 1:length(partData)) {
    en_curid = partData[[j]]['id'] %>% as.numeric()
    text = partData[[j]]['text'] %>% unlist() %>% 
      str_c(collapse = " ")
    partTexts = rbind(partTexts, data.frame(en_curid, text))
  }
  dfs[[i]] = partTexts
  print(i)
}
wikipediaTexts = rbind_list(dfs)
write_csv(wikipediaTexts, "data/generated/wikipedia/wikipediaTexts.csv")

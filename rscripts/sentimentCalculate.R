wikipediaTexts = read_csv('data/generated/wikipedia/wikipediaTexts.csv')

getSentiments = function(text) {
  words = text %>% 
    str_replace_all("[:punct:]", " ") %>% 
    str_replace_all("\\n", " ") %>% 
    str_replace_all("\\s+", " ") %>% 
    str_replace_all("\"", " ") %>% 
    str_split("\\s") %>% 
    unlist() %>% 
    table() %>% 
    as.data.frame(stringsAsFactor = F)
  colnames(words) = c('word', 'count')
  words = words %>% 
    filter(!str_to_lower(word) %in% stop_words$word) %>% 
    filter(str_length(word) > 2) %>% 
    filter(!str_detect(word, "\\d")) %>% 
    arrange(desc(count))
  
  dictionary = get_sentiments("nrc")
  sentiments = words %>% 
    full_join(dictionary) %>% 
    drop_na() %>% 
    group_by(sentiment) %>% 
    summarise(count = sum(count)) %>% 
    mutate(value = count / sum(count)) 
  result = paste(sentiments$value, "|")
  return(result)
}

sentiments1 = lapply(wikipediaTexts$text[1:1000], getSentiments)
sentiments = list()
for(i in 2:11) {
  sentiments[[i]] = lapply(wikipediaTexts$text[((i - 1) * 1000 + 1):(i * 1000)], getSentiments)
  print(i)
}
sentiments2 = lapply(wikipediaTexts$text[11001:11293], getSentiments)

s1 = lapply(sentiments1, function(x) {str_c(x, collapse = " ")})
s2 = lapply(sentiments2, function(x) {str_c(x, collapse = " ")})
s = list()
for(i in 2:11) {
  s[[i]] = lapply(sentiments[[i]], function(x) {str_c(x, collapse = " ")})
}

fullSentiments = s1
for(i in 2:11) {
  fullSentiments = c(fullSentiments, s[[i]])
}
fullSentiments =  c(fullSentiments, s2)
fullSentiments = unlist(fullSentiments)

sentimnetNames = c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive",    
                   "sadness", "surprise", "trust")
sentimentsData = data.frame(wikipediaTexts$en_curid, sentiments = fullSentiments)
sentimentsData = sentimentsData %>% 
  mutate(sentiments = str_replace_all(sentiments, "\\s", ""))
sentimentsData = sentimentsData %>% 
  separate(sentiments, into = sentimnetNames, sep = "\\|") %>% 
  drop_na()
sentimentsData = sentimentsData %>% 
  rename(en_curid = wikipediaTexts.en_curid)

write_csv(sentimentsData, 'data/generated/wikipedia/sentiments.csv')

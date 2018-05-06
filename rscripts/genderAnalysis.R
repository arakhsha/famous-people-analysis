visitData = read_tsv('data/pantheon/pageviews_2008-2013.tsv')
data = read_csv('data/generated/pantheon_cleaned.csv')

visitByGender = cbind( colSums(visitData %>% 
                                 filter(gender == 'Male') %>% 
                                 select(-(1:12))),
                       colSums(visitData %>% 
                                 filter(gender == 'Female') %>% 
                                 select(-(1:12))),
                       colnames(visitData)[-(1:12)]
                ) %>% 
  as.data.frame()
colnames(visitByGender) = c('Mvisit', 'Fvisit', 'Date')
visitByGender[,1:2] = lapply(visitByGender[,1:2], function(x) {as.numeric(as.character(x))})
visitByGender = visitByGender %>% 
  as.data.frame() %>% 
  mutate(Male = Mvisit / (Mvisit + Fvisit), Female = Fvisit / (Mvisit + Fvisit))
hchart(visitByGender, type = 'line', hcaes(x = Date, y = Female))  


library(readxl)
library(stringr)
HA = read_excel('data/HA/HA.xlsx')
comma = HA %>% filter(str_detect(Name, ','))
regular = HA %>% filter(!str_detect(Name, ','))
commaNames = comma$Name %>% 
  str_split_fixed(',', 2) %>% 
  as.data.frame() %>% 
  rename(last = V1, first = V2) %>% 
  mutate(original = comma$Name) %>% 
  mutate(fisrt = str_trim(first, side = 'both'),
         last = str_trim(last, side = 'both')) %>% 
  mutate(reformated = paste(first,last))
comma$Name = commaNames$reformated
HA = rbind(regular, comma) %>% 
  arrange(Name)

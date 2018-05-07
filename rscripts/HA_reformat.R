library(readxl)
library(stringr)
library(RecordLinkage)
library(stringdist)
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
  mutate(Name = str_trim(Name, side = 'both')) %>% 
  arrange(Name) 


pantheon = read_csv('data/generated/pantheon_cleaned.csv')

pantheon_sci = pantheon %>% 
  filter(domain %in% c("SCIENCE & TECHNOLOGY", "ARTS")) %>% 
  select(pname = name, pbirth = birthyear, en_curid)
HA_unique = HA %>%
  select(Name, Serial, Birth) %>%
  distinct() %>% 
  select(hname = Name, hbirth = Birth, Serial)

to.plain <- function(s) {
  
  # 1 character substitutions
  old1 <- "šžþàáâãäåçćčèéêëìíîïðñòóôõöùúûüý"
  new1 <- "szyaaaaaaccceeeeiiiidnooooouuuuy"
  s1 <- chartr(old1, new1, s)
  
  # 2 character substitutions
  old2 <- c("œ", "ß", "æ", "ø")
  new2 <- c("oe", "ss", "ae", "oe")
  s2 <- s1
  for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
  
  s2
}

standardizeName = function(s) {
  result = str_to_lower(to.plain(s))
  fname = str_extract(result, "^[[:graph:]]+ ")
  fname[is.na(fname)] = ""
  lname = str_extract(result, "[[:graph:]]+$")
  result = paste(fname, lname)
  result = str_trim(result, side = 'both')
  return(result)
}
pantheon_sci = pantheon_sci %>% mutate(pname = standardizeName(pname))
HA_unique = HA_unique %>% mutate(hname = standardizeName(hname))

matchings = expand.grid(
  HA$Serial,
  pantheon_sci$en_curid
)
colnames(matchings) = c('Serial', 'en_curid')
matchings = matchings %>% left_join(pantheon_sci) %>% left_join(HA_unique)
matchings = matchings %>% 
  mutate(birth_dist = abs(hbirth - pbirth)) %>% 
  filter(birth_dist < 30)
matchings = matchings %>%
  mutate(name_dist = stringdist(hname, pname))

matchings = matchings %>% 
  filter(name_dist < 2 | (name_dist == 2 & birth_dist <= 5))

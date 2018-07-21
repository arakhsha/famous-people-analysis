data = read_csv('data/generated/pantheon_cleaned.csv')
data = data %>% 
  mutate(century = ceiling(birthyear / 100))
visitData = read_tsv('data/pantheon/pageviews_2008-2013.tsv')

visitData = visitData %>% 
  gather(key = 'month', value = 'visits', `2008-01`:`2013-12`)

lastYearVisits = visitData %>% 
  filter(str_detect(month, "2013"))

visit2013 = lastYearVisits %>%
  group_by(en_curid, lang) %>% 
  summarise(visit2013 = sum(visits))

globalVisits = visit2013 %>% 
  group_by(en_curid) %>% 
  summarise(globalVisits = sum(visit2013))

persianVisits = visit2013 %>% 
  filter(lang == 'fa') %>% 
  select(en_curid, persianVisits = visit2013)

data = data %>% 
  left_join(globalVisits) %>% 
  left_join(persianVisits)

#global visits ----------------------

highVisitedOccupations = data %>% 
  group_by(occupation) %>% 
  summarise(totalVisits = sum(globalVisits / 1000000, na.rm = T)) %>% 
  arrange(-totalVisits) %>% 
  mutate(occupation = str_to_title(occupation)) %>% 
  slice(1:10) 

occ_glbl = hchart(highVisitedOccupations, type = 'column', hcaes(x = occupation, y = totalVisits), name = 'visits') %>% 
  hc_title(text = "Most Visited Occupations in 2013") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Number of Visits (Millions)"))

highVisitedIndustry = data %>% 
  group_by(industry) %>% 
  summarise(totalVisits = sum(globalVisits / 1000000, na.rm = T)) %>% 
  arrange(-totalVisits) %>% 
  mutate(industry = str_to_title(industry)) %>% 
  slice(1:10) 

ind_glbl = hchart(highVisitedIndustry, type = 'column', hcaes(x = industry, y = totalVisits), name = 'visits') %>% 
  hc_title(text = "Most Visited Industries in 2013") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Number of Visits (Millions)"))

highVisitedDomain = data %>% 
  group_by(domain) %>% 
  summarise(totalVisits = sum(globalVisits / 1000000, na.rm = T)) %>% 
  arrange(-totalVisits) %>% 
  mutate(domain = str_to_title(domain)) %>% 
  slice(1:10) 

dom_glbl = hchart(highVisitedDomain, type = 'column', hcaes(x = domain, y = totalVisits), name = 'visits') %>% 
  hc_title(text = "Most Visited Domains in 2013") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Number of Visits (Millions)"))

#persian visits ----------------------

highVisitedOccupations.fa = data %>% 
  group_by(occupation) %>% 
  summarise(totalVisits = sum(persianVisits / 1000000, na.rm = T)) %>% 
  arrange(-totalVisits) %>% 
  mutate(occupation = str_to_title(occupation)) %>% 
  slice(1:10) 

occ_fa = hchart(highVisitedOccupations.fa, type = 'column', hcaes(x = occupation, y = totalVisits), name = 'visits') %>% 
  hc_title(text = "Most Visited Occupations in 2013") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Number of Visits (Millions)"))

highVisitedIndustry.fa = data %>% 
  group_by(industry) %>% 
  summarise(totalVisits = sum(persianVisits / 1000000, na.rm = T)) %>% 
  arrange(-totalVisits) %>% 
  mutate(industry = str_to_title(industry)) %>% 
  slice(1:10) 

ind_fa = hchart(highVisitedIndustry.fa, type = 'column', hcaes(x = industry, y = totalVisits), name = 'visits') %>% 
  hc_title(text = "Most Visited Industries in 2013") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Number of Visits (Millions)"))

highVisitedDomain.fa = data %>% 
  group_by(domain) %>% 
  summarise(totalVisits = sum(persianVisits / 1000000, na.rm = T)) %>% 
  arrange(-totalVisits) %>% 
  mutate(domain = str_to_title(domain)) %>% 
  slice(1:10) 

dom_fa = hchart(highVisitedDomain.fa, type = 'column', hcaes(x = domain, y = totalVisits), name = 'visits') %>% 
  hc_title(text = "Most Visited Domains in 2013") %>% 
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Number of Visits (Millions)"))

saveRDS(occ_glbl, 'output/highVisit/occ_glbl.rds')
saveRDS(ind_glbl, 'output/highVisit/ind_glbl.rds')
saveRDS(dom_glbl, 'output/highVisit/dom_glbl.rds')
saveRDS(occ_fa, 'output/highVisit/occ_fa.rds')
saveRDS(ind_fa, 'output/highVisit/ind_fa.rds')
saveRDS(dom_fa, 'output/highVisit/dom_fa.rds')

#visit history -------------

industryVisitHistory = visitData %>% 
  group_by(domain, industry, month) %>% 
  summarise(visits = sum(visits)) %>% 
  group_by(month) %>% 
  mutate(ratio = visits / sum(visits) * 100) %>% 
  arrange(domain)


industryVisitHistoryPlot = hchart(industryVisitHistory, type = "area", hcaes(x = month, y = ratio, group = industry),
       marker = list(radius = 0)) %>% 
  hc_plotOptions(area = list(stacking = "percent")) %>% 
  hc_tooltip(valueDecimals = 2) %>% 
  hc_xAxis(title = list(text = "Century"), crosshair = T) %>% 
  hc_yAxis(title = list(text = "Count (%)"), max = 100) 
industryVisitHistoryPlot  
saveRDS(industryVisitHistoryPlot, 'output/industryVisitHistoryPlot.rds')
  
  
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

femaleVisitsByTime = hchart(visitByGender,
                            type = 'line',
                            name = 'Visit Proportion',
                            hcaes(x = Date, y = round(Female, 3)))  %>% 
  hc_yAxis(title = list(text = 'Female Pages Visit Proportion'))
femaleVisitsByTime
saveRDS(femaleVisitsByTime, 'output/femaleVisitsByTime.rds')

hchart(visitByGender,
       type = 'line',
       name = 'Male Visit',
       hcaes(x = Date, y = round(Mvisit, 3))) %>% 
  hc_add_series(visitByGender,
                type = 'line',
                name = 'Female Visit',
                hcaes(x = Date, y = round(Fvisit, 3)))


genderOccupation = data %>% 
  group_by(occupation, gender) %>% 
  summarise(count = n()) %>% 
  spread(gender, count) %>% 
  mutate(Male = ifelse(!is.na(Male), Male, 0),
         Female = ifelse(!is.na(Female), Female, 0)) %>% 
  mutate(rMale = Male / (Male + Female) , rFemale = Female / (Male + Female)) %>% 
  arrange(rMale)


genderOccupationPlot = hchart(
    genderOccupation,
    hcaes(x = occupation, y = rMale),
    type = "bar",
    name = "Male",
    showInLegend = T,
    step = 1
  )%>% 
  hc_add_series(
    data = genderOccupation,
    hcaes(x = occupation, y = rFemale),
    type = "bar",
    name = "Female",
    showInLegend = T
  ) %>% 
  hc_plotOptions(series = list(stacking = T))

saveRDS(genderOccupationPlot, "output/genderOccupationPlot.rds")

genderEQcountry = data %>% 
  group_by(countryCode3, gender) %>% 
  summarise(count = n()) %>% 
  right_join(expand.grid(countryCode3 = unique(data$countryCode3), gender = unique(data$gender))) %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  group_by(countryCode3) %>% 
  mutate(ratio = count / sum(count) * 100, total = sum(count)) %>% 
  filter(gender == 'Female') %>% 
  arrange(-ratio) %>% 
  ungroup()

map <- download_map_data()
genderEQcountryPlot = highchart(type = "map") %>% 
  hc_add_series_map(
    df = genderEQcountry,
    value = "ratio",
    joinBy = c("iso-a3", "countryCode3"),
    map = map,
    name = "Female Ratio"
  ) %>% 
  hc_colorAxis(stops = list(c(0, "#e84118"), c(0.25, "#fbc531"), c(0.5, "#4cd137"),
                            c(1, "#4cd137"))) %>% 
  hc_tooltip(valueDecimals = 2,
             pointFormat = "{series.name}:{point.value}",
             headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>') %>% 
  hc_title(text = "Ratio of Female Famous People")

genderEQcountryPlot
saveRDS(genderEQcountryPlot, "output/genderEQcountryPlot.rds")



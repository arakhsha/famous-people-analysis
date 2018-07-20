data = read_csv('data/generated/pantheon_cleaned.csv')
data = data %>% 
  mutate(century = ceiling(birthyear / 100))

centuries = min(data$century, na.rm = T):max(data$century, na.rm = T)

worldPop = read_csv('data/worldPopulation_selected.csv') %>% 
  mutate(century = year / 100) %>% 
  select(century, worldPop = world) %>% 
  right_join(data.frame(century = centuries)) %>% 
  fill(worldPop, .direction = "up")


centuryCount = data %>% 
  group_by(century) %>% 
  summarise(count = n()) %>% 
  filter(century > -35, century < 21) %>% 
  right_join(data.frame(century = centuries)) %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  left_join(worldPop) %>% 
  mutate(ratio = round(count / worldPop, 2))


centuryCountPlot = hchart(centuryCount, type = "line",
                          hcaes(x = century, y = count), name = "Number of Famous People",
                          showInLegend = T) %>% 
  hc_xAxis(title = list(text = "Century"), crosshair = T) %>% 
  hc_yAxis_multiples(
    list(title = list(text = "Number of Famous People"), showEmpty = F),
    list(title = list(text = "World Population (Million)"),  labels = F, showEmpty = F),
    list(title = list(text = "Famous Person Per Million"), opposite = T, showEmpty = F)
  ) %>% 
  hc_add_series(centuryCount, hcaes(x = century, y = worldPop), type = "line", yAxis = 1,
                name = "World Population (Million)", showInLegend = T) %>% 
  hc_add_series(centuryCount, hcaes(x = century, y = ratio), type = "line", yAxis = 2,
                name = "Famous Person Per Million", showInLegend = T) %>% 
  hc_tooltip(shared = T) %>% 
  hc_title(text = "Number of Famous People Over Time")

centuryCountPlot
saveRDS(centuryCountPlot, "output/centuryCountPlot.rds")

timeContinent = data %>% 
  group_by(century, continentName) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  filter(century > -35, century < 21) %>% 
  right_join(expand.grid(century = centuries, continentName = unique(data$continentName))) %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  drop_na() %>% 
  group_by(century) %>% 
  mutate(ratio = count / sum(count) ) %>% 
  group_by(century) %>% 
  mutate(ratio = ifelse(is.finite(ratio), ratio, NA)) %>% 
  group_by(continentName) %>% 
  fill(ratio, .direction = "up") %>% 
  filter(century >= -11, century < 21)

timeContinentPlot = hchart(timeContinent, type = "area", hcaes(x = century, y = count, group = continentName),
       marker = list(radius = 0)) %>% 
  hc_plotOptions(area = list(stacking = "percent")) %>% 
  hc_tooltip(shared = T) %>% 
  hc_xAxis(title = list(text = "Century"), crosshair = T) %>% 
  hc_yAxis(title = list(text = "Count (%)")) 
timeContinentPlot
saveRDS(timeContinentPlot, "output/timeContinentPlot.rds")
  
  
  
  

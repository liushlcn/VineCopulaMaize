library(terra)
s1 = list.files(path = path$calendar, pattern = "Maize") %>% 
  grep(pattern = paste0(2000:2014, collapse = "|"), value = T) %>% 
  grep(pattern = "HE", value = T) %>% 
  paste0(path$calendar, .) %>% 
  rast() %>% 
  app(mean, na.rm = T)
s2 = list.files(path = path$calendar, pattern = "Maize") %>% 
  grep(pattern = paste0(2000:2014, collapse = "|"), value = T) %>% 
  grep(pattern = "MA", value = T) %>% 
  paste0(path$calendar, .) %>% 
  rast() %>% 
  app(mean, na.rm = T)

hist(s1)
hist(s2)


boxplot(s2 - s1, freq = F)
hist(aoi_cale$maturity - aoi_cale$heading, add = T, freq = FALSE, col = "red")

aoi_cale


hist(aoi_cale$maturity - aoi_cale$heading )







data = read_tsv('data/pantheon/pantheon.tsv')

#birthyear
data[data$birthyear == '530s', "birthyear"] = 535
data[data$birthyear == '1237?', "birthyear"] = 1237
data$birthyear = as.numeric(data$birthyear)

write_csv(data, 'data/generated/pantheon_cleaned.csv')

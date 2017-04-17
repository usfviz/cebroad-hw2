
library(reshape)

setwd('~/Desktop/MSAN/MSAN622/Homework/HW2')

clean_input <- function(filename, type){
  raw_data <- read.csv(filename, header = TRUE, na.strings = c('NA', '', ' '), skip = 4)
  long_data <- melt(raw_data, id.vars = c('Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code'))
  colnames(long_data) <- c('Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code', 'Year', type)
  long_data$Year <- as.integer(substring(long_data$Year, 2))
  long_data$Indicator.Name <- NULL
  long_data$Indicator.Code <- NULL
  return(long_data)
}

get_meta <- function(df, filename){
  meta <- read.csv(filename, na.strings = c('NA', '', ' '), header = TRUE)
  meta[3:ncol(meta)] <- NULL
  df_full <- merge(x = df, y = meta, by = 'Country.Code', all.x = TRUE)
  return(df_full)
}

na_rm <- function(df){
  na_vec <- vector()
  for (i in 1:nrow(df)){
    thisrow <- df[i,]
    if (anyNA(thisrow[3:7]) == TRUE){
      na_vec <- c(na_vec, FALSE)
    } else {
      na_vec <- c(na_vec, TRUE)
    }
  }
  df_clean <- df[na_vec,]
  return(df_clean)
}

clean_exp <- clean_input('data/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', 'Life.Expectancy')
clean_fert <- clean_input('data/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', 'Fertility.Rate')
clean_pop <- clean_input('data/API_SP.POP.TOTL_DS2_en_csv_v2.csv', 'Total.Population')

full_exp <- get_meta(clean_exp, 'data/Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')
full_fert <- get_meta(clean_fert, 'data/Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv')
full_pop <- get_meta(clean_pop, 'data/Metadata_Country_API_SP.POP.TOTL_DS2_en_csv_v2.csv')

df_full <- merge(full_exp, full_fert, by = c('Country.Code', 'Country.Name', 'Year', 'Region'), all = TRUE)
df_full <- merge(df_full, full_pop, by = c('Country.Code', 'Country.Name', 'Year', 'Region'), all = TRUE)
df_full <- na_rm(df_full)

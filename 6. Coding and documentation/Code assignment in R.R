#Hanwen Miao(436819)
#Load packages
library(dplyr)
library(readxl)
library(glue)
library(stringr)
library(Hmisc)

###### Sets the path (I Copied the data file to this path)
path <- '/Users/lulut/Desktop/forked/6. Coding and documentation'
setwd(path)
task_datapath <- paste(path, '/Data/onet_tasks.csv', sep = '')
isco_datapath <- paste(path, '/Data/Eurostat_employment_isco.xlsx', sep = '')
######Load data by function
data_read <- function(data_path, sheet_name = NULL){
  names <- strsplit(data_path, split = '\\.')[[1]]
  suffix <- names[length(names)]
  
  # Ensure the file extension is either 'csv' or 'xlsx'
  stopifnot(suffix %in% c('csv', 'xlsx'))
  
  # If the file is an Excel file, ensure sheet_name is not NULL
  if (suffix == 'xlsx'){
    stopifnot(!is.null(sheet_name))
  }
  
  # Read the data based on the file type
  if (suffix == 'csv'){ # Process CSV file
    data <- read.csv(data_path, stringsAsFactors = FALSE)
  } else { # Process Excel file
    data <- read_xlsx(data_path, sheet = sheet_name)
  }
  
  # Return the read data
  return(data)
}
# read task data file path
task_datapath <- "C:/Users/lulut/Desktop/forked/Data/onet_tasks.csv"
task_data <- data_read(task_datapath)
head(task_data)

# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
#Read data and create variables
iscos <- list()
for (i in 1 : 9) {
  file <- data_read(isco_datapath, sheet_name = paste("ISCO", i, sep = ''))
  iscos[[i]] <- file
}
print(glue('Length of iscos: {length(iscos)}.'))
for (i in 1:9) {
  assign(paste0("isco", i), data_read(isco_datapath, sheet_name = paste0("ISCO", i)))
}


# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?
merge_tables <- function(data_list, selected_countries) {
  library(glue)
  
  # Calculate the total number of workers in each selected country
  totals <- list()
  for (country in selected_countries) {
    total <- 0
    for (i in 1:length(data_list)) {
      if (country %in% colnames(data_list[[i]])) {
        total <- total + data_list[[i]][[country]]
        data_list[[i]][['ISCO']] <- i
      }
    }
    totals[[glue('total_{country}')]] <- total
  }
  
  # Merge all data
  merged_data <- data_list[[1]]
  for (i in 2:length(data_list)) {
    merged_data <- rbind(merged_data, data_list[[i]])
  }
  
  return(list(merged_data = merged_data, totals = totals))
}

# We have 9 occupations and the same time range for each, so we can add the totals by
# adding a vector that is 9 times the previously calculated totals
# Add total number of workers and proportion columns
columns <- names(totals)
for (i in 1:length(columns)) {
  if (columns[i] %in% names(totals)) {
    value <- totals[[columns[i]]]
    merged_data[[columns[i]]] <- rep(value, nrow(merged_data))
    country <- strsplit(columns[i], split = '_')[[1]]
    country <- country[length(country)]
    if (country %in% colnames(merged_data)) {
      merged_data[[glue('share_{country}')]] <- merged_data[[country]] / merged_data[[columns[i]]]
    }
  } else {
    print(glue("Warning: {columns[i]} not found in totals"))
  }
}

return (merged_data)
}

selected_countries <- c('Belgium', 'Spain', 'Poland')
merged_data <- merge_tables(data_list = iscos, 
                            selected_countries = selected_countries)
head(merged_data)
View(merged_data)

# Now let's look at the task data. We want the first digit of the ISCO variable only
task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

#These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

#Let's combine the data.
combined <- left_join(merged_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)

# first task item
temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A2a4 = (combined$t_4A2a4-temp_mean)/temp_sd

# second task item
temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A2b2 = (combined$t_4A2b2-temp_mean)/temp_sd

# third task item
temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$t_4A4a1 , combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A4a1 , combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A4a1  = (combined$t_4A4a1 -temp_mean)/temp_sd

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

combined$Belgium_NRCA <- combined$std_Belgium_t_4A2a4 + combined$std_Belgium_t_4A2b2 + combined$std_Belgium_t_4A4a1 
combined$Poland_NRCA <- combined$std_Poland_t_4A2a4 + combined$std_Poland_t_4A2b2 + combined$std_Poland_t_4A4a1 
combined$Spain_NRCA <- combined$std_Spain_t_4A2a4 + combined$std_Spain_t_4A2b2 + combined$std_Spain_t_4A4a1 

# And we standardise NRCA in a similar way.
temp_mean <- wtd.mean(combined$Belgium_NRCA, combined$share_Belgium)
temp_sd <- wtd.var(combined$Belgium_NRCA, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_NRCA = (combined$Belgium_NRCA-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$Poland_NRCA, combined$share_Poland)
temp_sd <- wtd.var(combined$Poland_NRCA, combined$share_Poland) %>% sqrt()
combined$std_Poland_NRCA = (combined$Poland_NRCA-temp_mean)/temp_sd

temp_mean <- wtd.mean(combined$Spain_NRCA, combined$share_Spain)
temp_sd <- wtd.var(combined$Spain_NRCA, combined$share_Spain) %>% sqrt()
combined$std_Spain_NRCA = (combined$Spain_NRCA-temp_mean)/temp_sd

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
combined$multip_Spain_NRCA <- (combined$std_Spain_NRCA*combined$share_Spain)
combined$multip_Belgium_NRCA <- (combined$std_Belgium_NRCA*combined$share_Belgium)
combined$multip_Poland_NRCA <- (combined$std_Poland_NRCA*combined$share_Poland)

# Step 2: sum it up (it basically becomes another weighted mean)
agg_Spain <-aggregate(combined$multip_Spain_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)
agg_Belgium <-aggregate(combined$multip_Belgium_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)
agg_Poland <-aggregate(combined$multip_Poland_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)

# We can plot it now!
plot(agg_Poland$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Poland$Group.1[seq(1, 40, 3)])

plot(agg_Spain$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Spain$Group.1[seq(1, 40, 3)])

plot(agg_Belgium$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Belgium$Group.1[seq(1, 40, 3)])


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment


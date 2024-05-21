install.packages("quarto")
library(quarto)

# Get current working directory
current_dir <- getwd()
message("Current working directory: ", current_dir)

seasons <- 1:8

# Render files 
results <- lapply(seasons, function(season) {
  output_file <- sprintf("Assignment_season_%d.html", season)
  tryCatch({
    quarto_render("Assignment.qmd", 
                  execute_params = list(season_no = season), 
                  output_file = output_file)
    message(sprintf("Successfully created: %s", output_file))
    return(output_file)
  }, error = function(e) {
    message(sprintf("Failed to create: %s", output_file))
    message(e)
    return(NULL)
  })
})



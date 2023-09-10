library(rsconnect)


values_function <- function(name) {
  var <- Sys.getenv(name, unset = NA)
  if(is.na(var)) {
    stop(paste0("cannot find ", name, " !"), call. = FALSE)
  }
  gsub("\"", "", var)
}


setAccountInfo(name = values_function("SHINY_ACC_NAME"),
               token = values_function("TOKEN"),
               secret = values_function("SECRET"))


deployApp(appName="dolartrends",
          appFiles = c("app.R", 
                       "etl.R",
                       "data/dolar.xlsx",
                       "data/dolarmep.xlsx",
                       "data/dolaroficial.xlsx"))




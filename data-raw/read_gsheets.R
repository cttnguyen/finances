conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

gs4_auth(email = "crystalmarriesdaniel@gmail.com")
gpath <- "https://docs.google.com/spreadsheets/d/1r61W0v51hzpLTg9JXSO_GeyRbLyyUlar1TonYjsy8Qk/edit#gid=176417609"
gnames <- sheet_names(gpath) %>% 
  discard(~ . == "README")
var_names <- gnames %>% 
  tolower() %>% 
  str_replace(" ", "_")

map2(
  gnames,
  var_names,
  function(gname, var_name) {
    read_sheet(
      ss = gpath,
      sheet = gname
    ) %>% 
      mutate_if(lubridate::is.POSIXct, as.Date) %>% 
      mutate(
        notes = replace_na(notes, ""),
        notes = str_replace_all("; ", "<br>")
      ) %>% 
      assign(x = var_name, envir = .GlobalEnv)
    
    do.call("use_data", 
            list(
              as.name(var_name),
              overwrite = TRUE
            )
    )
  }
) 


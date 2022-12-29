conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

gs4_auth(email = "crystalmarriesdaniel@gmail.com")
gpath <- "https://docs.google.com/spreadsheets/d/1r61W0v51hzpLTg9JXSO_GeyRbLyyUlar1TonYjsy8Qk/edit#gid=176417609"
gnames <- sheet_names(gpath) %>% 
  discard(~ . == "README")
var_names <- gnames %>% 
  tolower() %>% 
  str_replace(" ", "_")

format_notes <- function(notes){
  as.character(notes) %>% #if all values are missing, could be read in as logical
    replace_na("") %>%  
    str_split("; ") %>% #break out multiple notes into multiple lines and format as sentence
    map_chr(
      ~str_to_sentence(.) %>% 
        paste0(collapse = "<br>")
    )
}

map2(
  gnames,
  var_names,
  function(gname, var_name) {
    read_sheet(
      ss = gpath,
      sheet = gname
    ) %>% 
      mutate_if(lubridate::is.POSIXct, as.Date) %>% 
      mutate(notes = format_notes(notes = notes)) %>% 
      assign(x = var_name, envir = .GlobalEnv)
    
    do.call("use_data", 
            list(
              as.name(var_name),
              overwrite = TRUE
            )
    )
  }
) 


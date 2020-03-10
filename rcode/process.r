# Basic Instructions:
# This file works by downloading airtable sheet and updating/syncing 
# progress on the live table. once processed it won't redo already 
# outputted files - 
#
# if you need to redo go online and change airtable post_process_indicator 
# variable to 0, and probably the post, or it will probably amend not overwrite?


# remotes::install_github("bergant/airtabler")
# remotes::install_github("hrbrmstr/rgeocodio")
library(tidyverse)
library(stringr)
library(airtabler)
library(rgeocodio)
library(glue)

#load environment variables:  API keys for geocodio and airtable
source(".//.env")

## functions
# not used but could be useful for cleanup in future 
# add_view <-norm_and_split_addr(data, Address) #used like this
# norm_and_split_addr <- function(.data, addr_col) {
#   .data %>%
#     mutate(mod_addr = str_to_upper(!!rlang::ensym(addr_col))) %>%
#     mutate(po_box = str_match(mod_addr, "P.? ?O.? BOX ([\\d]*)$")[,2], mod_addr = gsub("P.?O.? BOX ([\\d]*)$", "", mod_addr, perl = TRUE)) %>%
#     mutate(unit_1 = str_match(mod_addr, "^[\\d]*-?(\\w)* ")[,2], mod_addr = gsub("^([\\d]*)-?(\\w)* ", "\\1 ", mod_addr, perl = TRUE)) %>%
#     mutate(unit_2 = str_match(mod_addr, "[.,]* (?:APT|#|STE|UNIT|RM|SUITE) ?([\\w\\d]*)$")[,2], mod_addr = gsub("[.,]* (APT|#|STE|UNIT|RM|SUITE) ?([\\w\\d]*)$", "", mod_addr, perl = TRUE)) %>%
#     mutate(mod_addr = gsub(" W.? "," WEST ", mod_addr) %>% gsub(" S.? "," SOUTH ", .) %>%gsub(" E.? "," EAST ", .) %>% gsub(" N.? "," NORTH ", .)) %>%
#     mutate(mod_addr = gsub(" ALLEY( |,|$)"," ALY\\1", mod_addr) %>% gsub(" AVENUE( |,|$)"," AVE\\1", .) %>% gsub(" BOULEVARD( |,|$)"," BLVD\\1", .) %>% gsub(" CIRCLE( |,|$)"," CIR\\1", .) %>% gsub(" COURT( |,|$)"," CT\\1", .)%>% gsub(" DRIVE( |,|$)"," DR\\1", .)%>% gsub(" HIGHWAY( |,|$)"," HWY\\1", .) %>% gsub(" LANE( |,|$)"," LN\\1", .) %>% gsub(" PLACE( |,|$)"," PL\\1", .) %>% gsub(" ROAD( |,|$)"," RD\\1", .) %>% gsub(" STREET( |,|$)"," ST\\1", .) %>% gsub(" TERRACE( |,|$)"," TER\\1", .)) %>%
#     mutate(mod_addr = gsub("(O.FARRELL)","OFARRELL", mod_addr)) %>%
#     mutate(mod_addr = gsub("BROADWAY (\\w*)$","BROADWAY !", mod_addr) %>% gsub("BROADWAY$","BROADWAY !", .) %>% gsub("LA PLAYA (\\w*)$","LA PLAYA !", .) %>% gsub("LA PLAYA$","LA PLAYA !", .) %>% gsub("SOUTH PARK ST","SOUTH PARK !", .)) %>%
#     extract("mod_addr", into = c("st_no","st_name","st_type"), "^(\\d+) (.*) ([\\w!]+)?", remove = FALSE) %>%
#     mutate(st_name = gsub("^(\\d[A-Z]{2})$", "0\\1", st_name))  %>% # to turn "2ND" into "02ND"
#     mutate(st_type = ifelse(is.na(st_type), "!", st_type)) %>% # can't merge on a NA field, use "!" as the substitute
#     mutate(st_no = as.numeric(st_no)) %>%
#     select(-mod_addr)
# }

data <- air_select(sub_base, table_name = "Submissions")

# select unprocessed addresses using synced airtable indicator
data_filter <- data %>% filter(post_process_indicator == 0)

# batch geocode using geocodio - assumes Los Angeles revisit
data_gc <- gio_batch_geocode(paste(data_filter$Address, ", Los Angeles CA"))

# api data is located like this...
# slight differences for pulling lists see below
# data_gc[1,]$response_results[[1]]$formatted_address[1]
# data_gc[1,]$response_results[[1]]$location.lat[1]
# data_gc[1,]$response_results[[1]]$location.lng[1]

#combine wide data pulled from api 
data_gc_all <- bind_cols(data_filter, data_gc %>% select(-response_warnings))

for (n in 1:NROW(data_gc_all)) {                                  #check if post_process_indicator is 1, ie only do new posts
    if (data_gc_all$post_process_indicator[n] == 0) {
      air_record <- data_gc_all$id[[n]]
      print(paste("doing record", air_record, ' #', n, '/', NROW(data_gc_all)))
      
      record_data_gc_all <- list(
          latitude = data_gc_all[n,]$response_results[[1]]$location.lat[1],
          longitude = data_gc_all[n,]$response_results[[1]]$location.lng[1],
          address_clean = data_gc$formatted_address[[n]],
          post_process_indicator = 1)
        air_update(sub_base, "Submissions", record_id = air_record, record_data_gc_all)
        print(paste("updating airtable with ", air_record))
        Sys.sleep(50)
      }
      data_gc_all$post_process_indicator[n] = 1
      
      ## parameters for posts
      title = dQuote( data_gc_all[n,]$Address )
      postdate = as.Date( data_gc_all[n,]$createdTime )
      address = data_gc_all[n,]$address_clean
      lat = data_gc_all[n,]$response_results[[1]]$location.lat[1]
      long = data_gc_all[n,]$response_results[[1]]$location.lng[1]
      formatted_address = dQuote( data_gc_all$formatted_address[n] )
      StoryField = data_gc_all[n,]$StoryField
      pic1 = data_gc_all$Attachments[[n]][6][[1]][[2]][[1]]
      #pic2...9
      
      # Coding note, putting more than one picture requires accounting for 0, 1, 2, etc
      # pictures, so output would either be hard coded or dynamically looped 
      # to account for different out.
      
      ## Outputs are written out as below, notes on how 'glue' works
      # https://glue.tidyverse.org/reference/glue.html
      # my googlefu was failing at people who used glue with conditionals
      # alternative is to rewrite to use knitr to have R push out rendered md files
      # and we can just use the MDs, since conditionals are easy too use like that.
            
out <- glue('---
layout: post
title: {title}
date: {postdate}
categories: ["user-submitted"]
author: "nolympics"
lat: {lat}
lng: {long}
runningtitle: "Running title here"
desc: {formatted_address}
---
**Address**
{formatted_address}

**Story**
{StoryField}

**Pictures**

![Image title]({noquote(pic1)})
    {{:.image}}

   {{:.caption}}',

#  
StoryField = "",
pic1 = ""  # XXX add default picture
)

      writeLines(out, paste0(".//_posts//",postdate,stringr::str_trim(title),".md"))
     
    }  




# different ways of writing out in this SO post https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r




## todos
# different entry categories
# different type categories: hotels, 


# story field data_gc_all$StoryField
# other vairables to add
# as.Date(data_gc_all[1,]$createdTime)
# length(data_gc_all[2,]$Attachments)
# if exists pictures ( can have multiple pictures)
# nrow(data_gc_all[[3]][[n]]) #null or multiple
# is.null(data_gc_all$Attachments[[2]])
# if not null, then length 

# (data_gc_all$Attachments[[2]])
# #if exists airbnb link
# data_gc_all[n,]$Airbnb_link 

## also url (data_gc_all$Attachments)[[2]]$url
# ---  --- --- --- --- --- --- --- --- --- --- --- --- #
# import.R
# imports and processes aggregated vote info from Arlington
# ---  --- --- --- --- --- --- --- --- --- --- --- --- #


polarvars <- c("Precinct.Name", "Active.Turnout",
               "Mail.Ballot.Return.Rate", "Early.to.Mail.Ratio" , "Mail.Ballots.Requested",
               "Outstanding.Votes", "Total.Votes" )






          # load + process voting registeration file  ----

#page info
reg.raw.info <- pdf_info(register)

# import both pages and split
reg.raw1 <- pdf_text(register)
reg.raw2 <- reg.raw1[[2]]
reg.raw1 <- reg.raw1[[1]]


# look <- function(rx) str_view_all(reg.pg2.tib$Rest, rx)
# look.res <- look("(?<=[:alpha:])[:space:](?=[:digit:])")

# page 2

reg.pg2.split <- str_split(reg.raw2, "\\n", simplify = TRUE) %>% as.vector()

reg.pg2.tib <- tibble(RawText=reg.pg2.split) %>%
  filter(row_number() < 26) %>%
  filter(row_number() > 4) %>%
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)", "\\.")) %>%
  separate(TextSquish, into=c('precinct.no.long', 'Rest'), sep = "[:space:]",
           extra="merge") %>%
  mutate(Rest=str_squish(Rest),
         Rest=str_replace_all(Rest, "\\(X\\)", "NA")) %>%
  separate(Rest,
           into=c('precinct', 'Rest'),
           sep = "-",
           extra='merge') %>%
  separate(Rest,
           into=c('prec_name', 'Rest'),
           sep = "(?<=[:alpha:])[:space:](?=[:digit:])",
           extra='merge') %>%
  separate(Rest,
           into=c('active', 'Rest'),
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('inactive', 'Rest'),
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('all', 'Rest'),
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('military', 'Rest'),
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('overseas', 'federal'),
           sep = "[:space:]",
           extra='merge')

# page 1

reg.pg1.split <- str_split(reg.raw1, "\\n", simplify = TRUE) %>% as.vector()

reg.pg1.tib <- tibble(RawText=reg.pg1.split) %>%
  filter(row_number() < 40) %>%
  filter(row_number() > 6) %>%
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)", "\\.")) %>%
  separate(TextSquish, into=c('precinct.no.long', 'Rest'), sep = "[:space:]",
           extra="merge") %>%
  mutate(Rest=str_squish(Rest),
         Rest=str_replace_all(Rest, "\\(X\\)", "NA")) %>%
  separate(Rest,
           into=c('precinct', 'Rest'),
           sep = "-",
           extra='merge') %>%
  separate(Rest,
           into=c('prec_name', 'Rest'),
           sep = "(?<=[:alpha:])[:space:](?=[:digit:])",
           extra='merge') %>%
  separate(Rest,
           into=c('active', 'Rest'),
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('inactive', 'Rest'),
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('all', 'Rest'),
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('military', 'Rest'),
           sep = "[:space:]",
           extra='merge') %>%
  separate(Rest,
           into=c('overseas', 'federal'),
           sep = "[:space:]",
           extra='merge')




# append
vote.register <-
  bind_rows(reg.pg1.tib, reg.pg2.tib) %>%
  select(-RawText, -precinct.no.long) %>%
  mutate(
    prec_name = str_to_title(prec_name)
  )

# substitute out comma
vote.register$all <-  gsub(",", "", vote.register$all)
vote.register$active <-  gsub(",", "", vote.register$active)
vote.register$inactive <-  gsub(",", "", vote.register$inactive)
vote.register$military <-  gsub(",", "", vote.register$military)
vote.register$overseas <-  gsub(",", "", vote.register$overseas)
vote.register$federal <-  gsub(",", "", vote.register$federal)






          # merge with voter data with sf and registration data  ----
# if using precinct gis data, import
if (s.gis == 1) {
  arl.precinct <- readRDS(file = file.path(root.data, "precinct-gis.Rda"))
}


# convert columns to numeric en masse

## make lists of vars

### voter data
numvars1 <- c("precinct", "total.voted", "mailed", "counted", "total.mail",
             "mail.outstanding", "mail.received", "early.voted")
fctvars1 <- c('prec_name')

### precinct data
numvars2 <- c("objectid", "precinct", "senate")
fctvars2 <- c("prec_name", "label")

### registration data
numvars3 <- c("precinct", "active", "inactive", "all", "military", "overseas", "federal")
fctvars3 <- c("prec_name")


if (s.gis == 1) {
arl.precinct <-
  arl.precinct %>%
  convert(num(numvars2),
         fct(fctvars2))
}


vote.register <-
  vote.register %>%
  convert(num(numvars3),
          fct(fctvars3))


if (s.gis == 1) {
vote.data <- vote.data %>%
  left_join(., arl.precinct,
            by = "precinct",
            suffix = c('.x', '.y')
  )%>%
    mutate(
      prec_name = coalesce(prec_name.y, prec_name.x) # take vote.register names as it has house infused
    ) %>%
    select(precinct, prec_name, everything(),
           -prec_name.x, -prec_name.y) %>%
    st_as_sf()
}






# Append to all past datasets ----

# run all past dataset scripts
source(file = file.path(root.code, "code/votes-18-oct.R"))
source(file = file.path(root.code, "code/votes-15-oct.R"))
source(file = file.path(root.code, "code/votes-12-oct.R"))
#source(file = file.path(root.code, "code/votes-9-oct.R")) # don't use this yet, dif format.
source(file = file.path(root.code, "code/votes-19-oct.R"))
source(file = file.path(root.code, "code/votes-22-oct.R"))
source(file = file.path(root.code, "code/votes-23-oct.R"))
source(file = file.path(root.code, "code/votes-25-oct.R"))
source(file = file.path(root.code, "code/votes-26-oct.R"))
source(file = file.path(root.code, "code/votes-27-oct.R"))
source(file = file.path(root.code, "code/votes-28-oct.R"))
source(file = file.path(root.code, "code/votes-29-oct.R"))
source(file = file.path(root.code, "code/votes-30-oct.R"))
source(file = file.path(root.code, "code/votes-31-oct.R"))


# import the by-day objects the above scripts generate
vote18oct <- readRDS(file = file.path(votes, "18-oct.Rda"))
vote15oct <- readRDS(file = file.path(votes, "15-oct.Rda"))
vote12oct <- readRDS(file = file.path(votes, "12-oct.Rda"))
vote19oct <- readRDS(file = file.path(votes, "19-oct.Rda"))
#vote21oct <- readRDS(file = file.path(votes, "21-oct.Rda")) #document error?
vote22oct <- readRDS(file = file.path(votes, "22-oct.Rda"))
vote23oct <- readRDS(file = file.path(votes, "23-oct.Rda"))
vote25oct <- readRDS(file = file.path(votes, "25-oct.Rda"))
vote26oct <- readRDS(file = file.path(votes, "26-oct.Rda"))
vote27oct <- readRDS(file = file.path(votes, "27-oct.Rda"))
vote28oct <- readRDS(file = file.path(votes, "28-oct.Rda"))
vote29oct <- readRDS(file = file.path(votes, "29-oct.Rda"))
vote30oct <- readRDS(file = file.path(votes, "30-oct.Rda"))
vote31oct <- readRDS(file = file.path(votes, "31-oct.Rda"))



# append all
vote.data <-
  bind_rows(
    vote31oct,
    vote30oct,
    vote29oct,
    vote28oct,
    vote27oct,
    vote26oct,
    vote25oct,
    vote23oct,
    vote22oct,
    # vote21oct, # there's an error in the pdf that makes it hard to read
    vote19oct,
    vote18oct,
    vote15oct,
    vote12oct
  ) 













      # Create Scatterpolar objects ----


# create arlington average
scatterpolar <- vote.data[vote.data$date %in% latestdate,] %>%
 # st_drop_geometry() %>%
  select(polarvars) %>%
  as.data.frame() %>%
  filter(`Precinct.Name` != "Arlington Totals") # remove totals here since we create below

scatterpolar$`Precinct.Name` <-  as.character(scatterpolar$`Precinct.Name`)

# create averages
scatterpolar <- scatterpolar %>%
  rbind( c( "Arlington Average",
            round(mean(scatterpolar$`Active.Turnout`), 0),
            round(mean(scatterpolar$`Mail.Ballot.Return.Rate`), 2),
            round(mean(scatterpolar$`Early.to.Mail.Ratio`), 1),
            round(mean(scatterpolar$`Mail.Ballots.Requested`), 0),
            round(mean(scatterpolar$`Outstanding.Votes`), 0 ),
            round(mean(scatterpolar$`Total.Votes`), 0) )) %>%
  convert(num(c("Active.Turnout", "Mail.Ballot.Return.Rate", "Early.to.Mail.Ratio", "Mail.Ballots.Requested",
                "Outstanding.Votes", "Total.Votes")))

arlav  <- scatterpolar %>%
  filter(`Precinct.Name` == "Arlington Average") %>%
  select(-`Precinct.Name`) %>%
  as.vector() %>%
  gather()



# create normalized variables
sp.norm <- scatterpolar %>%
  mutate(across(where(is.numeric),
                ~round(normalize(.x), 2)))




# create misc values
arl.tot <- vote.data[vote.data$`Precinct.Name` %in% "Arlington Totals",] %>%
  filter(date == latestdate) 
vote.tot <- vote.data[vote.data$`Precinct.Name` %in% "Arlington Totals",] %>%
  filter(date == latestdate)
vote.tot.yest <- vote.data[vote.data$`Precinct.Name` %in% "Arlington Totals",] %>%
  filter(date == yesterday) # not really yesterday but -1 day
vote.pr <- vote.data %>%
  filter(Precinct.Name != "Arlington Totals") 







            # export and save ----
if (export == 1) {
  # save local copy
save(
  vote.data,
  arl.tot,
  vote.tot,
  vote.pr,
  vote.tot.yest,
  sp.norm,
  arl.tot,
  file = file.path(root.data, "rdata/arl-vote2020.Rdata")
)

  #also save copy to app
save(
  vote.data,
  arl.tot,
  vote.tot,
  vote.pr,
  vote.tot.yest,
  sp.norm,
  arl.tot,
  file = file.path(app, "data/arl-vote2020.Rdata")
)


}

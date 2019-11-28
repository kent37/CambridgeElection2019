# Read Election data
library(tidyverse)

all_ballots = function() {
  prm_paths = list.files(here::here('2019/City Council/'),
                         pattern='PRM', full.names=TRUE)
  map_dfr(prm_paths, read_prm)
}

#' Read ballot information from a PRM file
#' @return A data frame with columns ward, precinct,
#' ballot_id, first, and choices. Choices is a list-column
#' whose values are vectors of candidate codes.
read_prm = function(path) {
  wp_str = basename(path) %>% str_remove('.PRM')
  wp = parse_ward(wp_str) %>% as.list() %>% as_tibble()
  
  raw = read_lines(path)
  ballots = parse_ballots(raw) %>% 
    mutate(first=map_chr(choices, 1))
  cbind(wp, ballots)
}

#' Parse ward and precinct values
#' @return A vector with ward and precinct numbers
parse_ward = function(wp) {
  vals = str_match(wp, '^(\\d?\\d)(\\d\\d)$')
  vals[, 2:3, drop=TRUE] %>% 
    as.integer() %>% 
    set_names(c('ward', 'precinct'))
}

#' Parse a list of ballot strings
#' @return A tibble containing ballot_id and choices
parse_ballots = function(raw) {
  cooked = str_split_fixed(raw, '[,)]', 5) %>% # Split fields
    `[`(,c(1, 5)) %>% # Toss extra
    as_tibble() %>% 
    set_names(c('ballot_id', 'choices')) %>% 
    mutate(choices=choices %>% # Clean up the choices field
             str_trim() %>% 
             str_replace_all('\\[\\d\\d?\\]', '') %>% 
             str_split(','))
}

#' Tibble of candidates and codes
candidates = function() {
cooked = '.CANDIDATE C01, "Akiba, Sukia"
.CANDIDATE C02, "Azeem, Burhan"
.CANDIDATE C03, "Carlone, Dennis J."
.CANDIDATE C04, "Franklin, Charles J."
.CANDIDATE C05, "Kelley, Craig A."
.CANDIDATE C06, "Kopon, Derek Andrew"
.CANDIDATE C07, "Levy, Ilan"
.CANDIDATE C08, "Mallon, Alanna M."
.CANDIDATE C09, "McGovern, Marc C."
.CANDIDATE C10, "McNary, Jeffery"
.CANDIDATE C11, "Mednick, Risa"
.CANDIDATE C12, "Moree, Gregg J."
.CANDIDATE C13, "Musgrave, Adriane"
.CANDIDATE C14, "Nolan, Patricia M."
.CANDIDATE C15, "Pitkin, John"
.CANDIDATE C16, "Siddiqui, Sumbul"
.CANDIDATE C17, "Simmons, E. Denise"
.CANDIDATE C18, "Simon, Ben"
.CANDIDATE C19, "Sobrinho-Wheeler, Jivan"
.CANDIDATE C20, "Toomey, Jr., Timothy J."
.CANDIDATE C21, "Williams, Nicola A."
.CANDIDATE C22, "Zondervan, Quinton Y."' %>% 
    str_split('\n') %>% 
    `[[`(1) %>% 
    str_remove_all('.CANDIDATE |"') %>% 
    str_split_fixed(', ', 2) %>% 
    as_tibble() %>% 
    set_names('code', 'full_name') %>% 
    mutate(last_name=str_split(full_name, ', ') %>% 
             map_chr(1))
}
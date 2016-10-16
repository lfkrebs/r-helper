
# ┌──────────────────────────┐
# │ Basic settings           │
# │ Best placed in .Rprofile │
# └──────────────────────────┘

options(stringsAsFactors = FALSE)
options(max.print = 25)


# ┌───────────────────────────────────────────────────────┐
# │ GitHub Search in RStudio Viewer Pane by hrbrmstr      │
# │ https://gist.github.com/hrbrmstr/32e9c140129d7d51db52 │
# └───────────────────────────────────────────────────────┘

# ══  GHELP  ═══════════════════════════════════════════════════════════════════

ghelp <- function(topic, in_cran=TRUE) {

  require(htmltools) # for getting HTML to the viewer
  require(rvest)     # for scraping & munging HTML

  # github search URL base
  base_ext_url <- "https://github.com/search?utf8=%%E2%%9C%%93&q=%s+extension%%3AR"
  ext_url <- sprintf(base_ext_url, topic)

  # if searching with user:cran (the default) add that to the URL
  if (in_cran) ext_url <- paste(ext_url, "+user%3Acran", sep="", collapse="")

  # at the time of writing, "rvest" and "xml2" are undergoing some changes, so
  # accommodate those of us who are on the bleeding edge of the hadleyverse
  # either way, we are just extracting out the results <div> for viewing in
  # the viewer pane (it works in plain ol' R, too)
  if (packageVersion("rvest") < "0.2.0.9000") {
    require(XML)
    pg <- html(ext_url)
    res_div <- paste(capture.output(html_node(pg, "div#code_search_results")), collapse="")
  } else {
    require(xml2)
    pg <- read_html(ext_url)
    res_div <- as.character(html_nodes(pg, "div#code_search_results"))
  }

  # clean up the HTML a bit
  res_div <- gsub('How are these search results\\? <a href="/contact">Tell us!</a>', '', res_div)
  # include a link to the results at the top of the viewer
  res_div <- gsub('href="/', 'href="http://github.com/', res_div)
  # build the viewer page, getting CSS from github-proper and hiding some cruft
  for_view <- sprintf('<html><head><link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github/index-4157068649cead58a7dd42dc9c0f2dc5b01bcc77921bc077b357e48be23aa237.css" media="all" rel="stylesheet" /><style>body{padding:20px}</style></head><body><a href="%s">Show on GitHub</a><hr noshade size=1/>%s</body></html>', ext_url, res_div)
  # this makes it show in the viewer (or browser if you're using plain R)
  html_print(HTML(for_view))

}


# ┌─────────────────────────────────────────────┐
# │ StackOverflow answer by 3lix and kdarras    │
# │ http://stackoverflow.com/questions/2261079/ │
# └─────────────────────────────────────────────┘

# ══  TRIM  ════════════════════════════════════════════════════════════════════

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# ┌───────────────────────────────────────┐
# │ R Helper Functions by Gustavo Lacerda │
# │ https://github.com/gusl/R-helpers     │
# │ http://www.optimizelife.com           │
# │ Copyright (c) 2014, Gustavo Lacerda   │
# │ MIT License                           │
# └───────────────────────────────────────┘

# ══  INSEPCT  ═════════════════════════════════════════════════════════════════

jCat <- function(...) { cat(...,sep="",fill=TRUE) }

NO_INSPECT <- FALSE  ## set to TRUE to turn off 'inspect'
ACTIVE_TAGS <- NULL  ## adjust this to see different types/levels of detail

inspect <- function(stuff, tags=NULL){ ## if no tags specified, always print
  if (NO_INSPECT) return()
  intersection <- intersect(ACTIVE_TAGS, tags)
  if (!is.null(tags) && !is.null(ACTIVE_TAGS) && length(intersection)==0) return()
  exprString <- as.character(match.call()[2])
  if (length(stuff)==1 && !is.data.frame(stuff) && !is.function(stuff)) { ##if not a proper
    ##vector / matrix / complex object, then it can be printed with 'cat', in a single line.
    jCat(exprString, " = ", stuff); return(stuff)
}
  else { ## if complex object, must be printed with 'print'.
    jCat(exprString, " = "); print(stuff)
  }
}


# ══  STRINGS  ═════════════════════════════════════════════════════════════════

# ──  “char-ize”  ──────────────────────────────────────────────────────────────
# turn a string into a vector of length-1 strings

cz <- function(s){
  vec <- c()
  for (i in 1:nchar(s)){
    vec <- c(vec, substr(s,i,i))
  }
  vec
}

charize <- cz

# ──  concatenate  ─────────────────────────────────────────────────────────────
# the opposite of 'cz'.

concat <- function(charVector){ Reduce(jPaste,charVector) }


# ┌───────────────────────────────────────────────────────────────────────┐
# │ Standard Library of Commonly Used Functions by Bryan Shepherd         │
# │ https://gist.githubusercontent.com/programmingr/a0f3fc3a7ee7b56b3266/ │
# │ Version 0.2                                                           │
# └───────────────────────────────────────────────────────────────────────┘

# ══  DESCRIPTIVES  ════════════════════════════════════════════════════════════

descs <- function (x) {
  if(!hidetables) {
    if(length(unique(x))>30) {
      print("Summary results:")
      print(summary(x))
      print("")
      print("Number of categories is greater than 30, table not produced")
    } else {
      print("Summary results:")
      print(summary(x))
      print("")
      print("Table results:")
      print(table(x, useNA="always"))
    }

  } else {
    print("Tables are hidden")
  }
}


# ══  DUMMIES  ═════════════════════════════════════════════════════════════════
# Returns a dataframe of dummies for each level of a categorical variable

createDummies <- function(x, df, keepNAs = TRUE) {
  uniq.cats <- unique(df[, x])
  # Sanitize variable names
  # TODO: possibly set up to keep only a certain number of characters
  uniq.cats <- gsub("[[:space:][:punct:]]", "", uniq.cats)

  for (i in seq(1, length(uniq.cats))) {
    if(keepNAs) {
      df[, paste(x,".", uniq.cats[i], sep = "")] <- ifelse(df[, x] != uniq.cats[i], 0, 1)
    } else {
      df[, paste(x,".", uniq.cats[i], sep = "")] <- ifelse(df[, x] != uniq.cats[i] | is.na(df[, x]) , 0, 1)
    }
  }
  return(df)
}


# ══  RECODE NEGATIVES TO MISSING ══════════════════════════════════════════════
# Adds variable and returns dataframe

recodeNegs <- function(x, df) {
  df[, paste(x,".noneg", sep = "")] <- ifelse(df[, x] < 0, NA, df[, x])
  return(df)
}


# ══  PACKAGES  ════════════════════════════════════════════════════════════════
# Check if package is installed. If it is, load it. If it isn't, install then load it.

instalib <- function(x) {
  if(!suppressWarnings(require(x, character.only=TRUE))) {
  install.packages(x)
  library(x, character.only=TRUE)
  }
}

library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(reactable)
library(htmltools)
library(lingglosses)
knitr::opts_chunk$set(echo=FALSE, message=FALSE)

# Load data
# read.csv("nov23-all.csv",stringsAsFactors = FALSE) -> linkers
# read.xlsx("bezhta-17-12-2024.xlsx", na.strings=c("NA","")) -> linkers
read_excel("bezhta-17-12-2024.xlsx",
           na=c("NA",""),
           .name_repair = ~ janitor::make_clean_names(string=.x,ascii=FALSE,case="none",sep_out=".")) %>% janitor::remove_empty() -> linkers

# normalize_factor <- function(f) {
#   out <- str_to_lower(f)
#   out <- sub(":","",out)
#   out <- gsub(" +"," ",out)
#   return(out)
# }
# 
# normalize_clause.pos <- function(f) {
#   out <- sub(" зависимой клаузы","",f)
#   out <- sub("постпозиция и препозиция","препозиция и постпозиция",out)
#   out <- sub("свободный","свободная",out)
#   return(out)
# }

linebreaks <- function(x) {
  return(gsub("&#10;","<br>",x))
}

fix_commas <- function(x) {
  return(gsub(" ,", ",", x))
}

# Change POS, semantic fields, classes to factors
linkers %>% 
  mutate(
    across(all_of(names(.)), trimws)
  ) %>%
  mutate(
    # id = as.numeric(id),
    marker = as.character(Маркер),
    semfield1 = as.factor(Сем.зона1),    
    label = as.character(Термин),
    sem.comment = as.character(Сем.комментарий),
    ex = as.character(fix_commas(str_squish(Основной.пример))),
    ex.gl = as.character(fix_commas(str_squish(Глоссы))),
    ex.tr.ru = as.character(Перевод),
    submeaning = as.character(Подзначение),
    subord.pos = as.factor(Позиция.зависимой.клаузы.в.составе.главной),
    subord.pos.ex = as.character(fix_commas(str_squish(Пример.на.позицию))),
    subord.pos.ex.gl = as.character(fix_commas(str_squish(Глоссы_2))),
    subord.pos.ex.tr.ru = as.character(Перевод_2),
    mainpart.comp = as.factor(Состав.основной.части.коннектора),
    comps = as.character(Компоненты.коннектора),
    comps.sep = as.factor(Разрывность.частей.коннектора),
    morph.pos = as.character(Позиция.морфемы.союза),
    indep = as.factor(Способность.употребляться.в.независимом.предложении),
    additive = as.factor(Присоединение.аддитивной.частицы),
    .keep = 'none',
  ) -> linkers2
  # select(marker,
  #        semfield,
  #        gloss.ru,
  #        corr,
  #        example,
  #        tr.ru,
  #        clause.pos,
  #        clause.pos.ex,
  #        conn.pos,
  #        conn.pos.ex,
  #        correl.pos,
  #        correl.pos.ex,
  #        correl.omit,
  #        correl.omit.ex,
  #        correl.mod,
  #        correl.mod.ex,
  #        focus,
  #        focus.ex,
  #        clause.indep,
  #        clause.indep.ex,
  #        illoc,
  #        illoc.ex
  #   ) -> linkers

custom_aggr <- "
    function(values, rows) {
      // input:
      //  - values: an array of all values in the group
      //  - rows: an array of row data values for all rows in the group (optional)
      //
      // output:
      //  - an aggregated value, e.g. a comma-separated list
      set = [...new Set(values)];
      filtered = set.filter(function(e) { return e != null; });
      return filtered.join('; ');
    }
  "

filterList <- function(values, name) {
  tags$select(
    # Set to undefined to clear the filter
    onchange = sprintf("Reactable.setFilter('linkertbl', '%s', event.target.value || undefined)", name),
    # "All" has an empty value to clear the filter, and is the default option
    tags$option(value = "", "Все"),
    lapply(sort(unique(values)), tags$option),
    "aria-label" = sprintf("Filter %s", name),
    style = "width: 100%; height: 28px;"
  )
}

detailsFunc <- function(index, df) {
  htmltools::div(
    htmltools::p(
      htmltools::tags$b("Другие части речи: "), 
      df[index, ]$other_pos),
    htmltools::p(
      htmltools::tags$b("Другие значения: "), 
      df[index, ]$other_senses))
}

makegl <- function(l1, l2, tr) {
  if(!is.na(l1)) {
    w1 <- str_split_1(l1, " ")
    if(!is.na(l2)) {
      w2 <- str_split_1(l2, " ")
    }
    else w2 <- c("")
    if(!is.na(tr)) {
      ft <- tr
    }
    else ft <- ""
    htmltools::withTags(
      div(
      table(
        tr(lapply(w1, function(x) td(style = "padding-right: 10px;", x))),
        tr(lapply(w2, function(x) td(style = "padding-right: 10px;", x))),
      ),
      div(ft)
      )
    )
  }
}

#' Function to download data (at Brazilian level) from the official Brazilian's repository
#' @aliases downloadBR
#' @export
#' @param language language; currently only portuguese and english available.
#' @return tibble/data.frame with the downloaded data.
#'
#' @examples
#' \dontrun{
#' library(readCovid19)
#' brasil <- downloadBR()
#' brasil
#' }
#'

downloadBR <- function(language=c("pt", "en")){
  language <- match.arg(language)
  message("Downloading COVID-19 data from official Brazilian repository: https://covid.saude.gov.br/")
  name <- format(today()-1, "%Y%m%d")
  url <- "https://covid.saude.gov.br/assets/files/COVID19_"
  url <- paste0(url, name, ".csv")
  brasil <- as_tibble(fread(url))
  brasil <- mutate(brasil, data = dmy(data))
  brasil <- rename(brasil, obitosAcumulados = obitosAcumulado)
  if(language=="en"){
    brasil <- rename(brasil,
                     region  =  regiao,
                     state = estado,
                     date = data,
                     newCases = casosNovos,
                     accumCases = casosAcumulados,
                     newDeaths = obitosNovos,
                     accumDeaths = obitosAcumulados)
  }
  setattr(brasil, "language", language)
  return(brasil)
}


#' Function to download data (at world level) from the Johns Hopkins University's repository
#' @aliases downloadBR
#' @export
#' @param language language; currently only portuguese and english available.
#' @return tibble/data.frame with the downloaded data.
#'
#' @examples
#' \dontrun{
#' library(readCovid19)
#' world <- downloadWorld()
#' world
#'
#' # selecting data from Italy:
#' italy <- filter(world, local=="Italy")
#' }
#'

downloadWorld <- function(language=c("en", "pt")){
  language <- match.arg(language)
  message("Downloading COVID-19 data from the Johns Hopkins University's repository")
  message("Please, be patient...")

  url_confirmed <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"
  url_deaths <-    "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"
  url_recovered <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv&filename=time_series_covid19_recoverd_global.csv"

  confirmed <- fread(url_confirmed)
  deaths <- fread(url_deaths)
  recovered <- fread(url_recovered)

  message(" Done!")

  deaths <- deaths %>%
    rename(local = 'Country/Region')  %>%
    select(-c("Province/State", "Lat", "Long" )) %>%
    melt(id=c("local")) %>%
    rename(date=variable, deaths = value) %>%
    mutate(date = mdy(date)) %>%
    group_by(local, date) %>%
    summarise(deaths = sum(deaths))

  confirmed <- confirmed %>%
    rename(local = 'Country/Region')  %>%
    select(-c("Province/State", "Lat", "Long" )) %>%
    melt(id=c("local")) %>%
    rename(date=variable, confirmed = value) %>%
    mutate(date = mdy(date)) %>%
    group_by(local, date) %>%
    summarise(confirmed = sum(confirmed))

  recovered <- recovered %>%
    rename(local = 'Country/Region')  %>%
    select(-c("Province/State", "Lat", "Long" )) %>%
    melt(id=c("local")) %>%
    rename(date=variable, recovered = value) %>%
    mutate(date = mdy(date)) %>%
    group_by(local, date) %>%
    summarise(recovered = sum(recovered))


  world <- full_join(confirmed, deaths, by=c("local", "date"))
  world <-full_join(world, recovered, by=c("local", "date"))

  if(language=="pt"){
    world <- rename(world,
           data = date,
           casos = confirmed,
           mortes = deaths,
           recuperados  = recovered)

  }
  setattr(world, "language", language)
  return(world)
}


brasil <- downloadBR()
world <- downloadWorld()
usethis::use_data(brasil, overwrite = TRUE)
usethis::use_data(world, overwrite = TRUE)


#' Aggregate Brazilain data by date, state and region
#' @aliases aggregateBR
#' @export
#' @param language language; currently only portuguese and english available.
#' @param by areal level of aggregation (all brazilian territory, regions, states)
#' @param date logical (default is FALSE); if TRUE, then variables at each date are summed up.
#' @return tibble/data.frame with the aggregated data data.
#'
#' @examples
#' \dontrun{
#' library(readCovid19)
#'
#' # portuguese:
#' portuguese <-  downloadBR("pt")
#' aggregateBR(data=portuguese, date=FALSE)
#' aggregateBR(data=portuguese, by="regiao", date=FALSE)
#' aggregateBR(data=portuguese, by="estado", date=FALSE)
#' dim(aggregateBR(data=portuguese, by="estado", date=TRUE))
#' dim(aggregateBR(data=portuguese, by="regiao", date=TRUE))
#'
#' english:
#' english <- downloadBR("en")
#' aggregateBR(data=english, by="brazil", date=FALSE)
#' aggregateBR(data=english, by="region", date=FALSE)
#' aggregateBR(data=english, by="state", date=FALSE)
#' aggregateBR(data=english, by="state", date=TRUE)
#' aggregateBR(data=english, by="region", date=TRUE)
#'
#' }
#'

aggregateBR <- function(data,
                     by =  c("estado", "regiao", "brasil", "state", "region", "brazil"),
                     date = FALSE){
  language <- attributes(data)$language
  by <- match.arg(by)
  if(language == "pt"){
    mydata <- aggregateBRpt(data, by, date)
  }else{
    mydata <- aggregateBRen(data, by, date)
  }
  return(mydata)
}




aggregateBRpt <- function(data, by, date){
  language <- attributes(data)$language
  mydata <- data

  if(date==TRUE){
    if(by == "brasil" | by == "brazil"){
      mydata <- mydata %>%
        summarise(casosNovos = sum(casosNovos),
                  casosAcumulados = sum(casosAcumulados),
                  obitosNovos = sum(obitosNovos),
                  obitosAcumulados = sum(obitosAcumulados))
    }else if(by == "estado" | by == "state"){
      mydata <- mydata %>%
        group_by(estado) %>%
        summarise(casosNovos = sum(casosNovos),
                  casosAcumulados = sum(casosAcumulados),
                  obitosNovos = sum(obitosNovos),
                  obitosAcumulados = sum(obitosAcumulados))
    }else{
      mydata <- mydata %>%
        group_by(regiao) %>%
        summarise(casosNovos = sum(casosNovos),
                  casosAcumulados = sum(casosAcumulados),
                  obitosNovos = sum(obitosNovos),
                  obitosAcumulados = sum(obitosAcumulados))
      }
  }else if(by == "regiao" | by == "region"){
    mydata <- mydata %>%
      group_by(data, regiao) %>%
      summarise(casosNovos = sum(casosNovos),
              casosAcumulados = sum(casosAcumulados),
              obitosNovos = sum(obitosNovos),
              obitosAcumulados = sum(obitosAcumulados))
  }else{
    message("It only make sense to aggregate data by states when date=TRUE!!!", "\n")
    mydata <- data
    return(data)
  }
  return(mydata)
}


aggregateBRen <- function(data, by, date){
  language <- attributes(data)$language
  mydata <- data

  if(date==TRUE){
    if(by == "brasil" | by == "brazil"){
      mydata <- mydata %>%
        summarise(newCases = sum(newCases),
                  accumCases = sum(accumCases),
                  newDeaths = sum(newDeaths),
                  accumDeaths = sum(accumDeaths))
    }else if(by == "estado" | by == "state"){
      mydata <- mydata %>%
        group_by(state) %>%
        summarise(newCases = sum(newCases),
                  accumCases = sum(accumCases),
                  newDeaths = sum(newDeaths),
                  accumDeaths = sum(accumDeaths))
    }else{
      mydata <- mydata %>%
        group_by(region) %>%
        summarise(newCases = sum(newCases),
                  accumCases = sum(accumCases),
                  newDeaths = sum(newDeaths),
                  accumDeaths = sum(accumDeaths))
    }
  }else if(by == "regiao" | by == "region"){
    mydata <- mydata %>%
      group_by(date, region) %>%
      summarise(newCases = sum(newCases),
                accumCases = sum(accumCases),
                newDeaths = sum(newDeaths),
                accumDeaths = sum(accumDeaths))
  }else{
    message("It only make sense to aggregate data by states when date=TRUE!!!", "\n")
    mydata <- data
    return(data)
  }
  return(mydata)
}



aggregateW <- function(data, by=data$local, date = FALSE){
  language <- attributes(data)$language
  by <- match.arg(by)
  mydata <- data
  if(date==TRUE){
    if(by == "brasil" | by == "brazil"){
      mydata <- mydata %>%
        summarise(newCases = sum(newCases),
                  accumCases = sum(accumCases),
                  newDeaths = sum(newDeaths),
                  accumDeaths = sum(accumDeaths))
    }else if(by == "estado" | by == "state"){
      mydata <- mydata %>%
        group_by(state) %>%
        summarise(newCases = sum(newCases),
                  accumCases = sum(accumCases),
                  newDeaths = sum(newDeaths),
                  accumDeaths = sum(accumDeaths))
    }else{
      mydata <- mydata %>%
        group_by(region) %>%
        summarise(newCases = sum(newCases),
                  accumCases = sum(accumCases),
                  newDeaths = sum(newDeaths),
                  accumDeaths = sum(accumDeaths))
    }
  }else if(by == "regiao" | by == "region"){
    mydata <- mydata %>%
      group_by(date, region) %>%
      summarise(newCases = sum(newCases),
                accumCases = sum(accumCases),
                newDeaths = sum(newDeaths),
                accumDeaths = sum(accumDeaths))
  }else{
    message("It only make sense to aggregate data by states when date=TRUE!!!", "\n")
    mydata <- data
    return(data)
  }
  return(mydata)
}


dataBackup <- function(brasil, world){
  wd <- system.file(package="readCovid19")
  setwd(wd)
  brasil_backup <- brasil
  world_backup  <- world
  setattr(brasil_backup, "date", today())
  setattr(world_backup, "date", today())
  usethis::use_data(brasil_backup, overwrite = TRUE)
  usethis::use_data(world_backup, overwrite = TRUE)
  return()
}

dataBackup(brasil, world)

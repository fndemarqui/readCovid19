#' Function to seach for the most recent available data date
#' @aliases avdate
#' @export
#' @param url desired url for searching the data set
#' @param lagdays number of retrospective days to search for
#' @return the most recent available data date.
#' @description The function searchs and identifies, among all available data sets hosted at a given repository, the up-to-date data set.
#' @note This function has been adapted from the code provided by Pedro Seiti Fujita.
#'

available_date <- function(url, lagdays){
  Files <- function(date){paste0(url, date, '.csv')}
  dates <- format(seq(as.Date(Sys.time())-lagdays, as.Date(Sys.time()), by='days'), "%Y%m%d")
  status <- c()
  for (i in 1:length(dates)) {
    status[i] <- GET(Files(dates[i]))[2]$status_cod
  }
  return(dates[which.max(dates[status==200])])
}


#' Function to download data (at Brazilian level) from the official Brazilian's repository
#' @aliases downloadBR
#' @export
#' @param language language; currently only portuguese and english available
#' @param lagdays number of retrospective days to search for
#' @return tibble/data.frame with the downloaded data
#'
#' @examples
#' \dontrun{
#' library(readCovid19)
#' brasil <- downloadBR()
#' brasil
#' }
#'

downloadBR <- function(language=c("pt", "en"), lagdays=3){
  language <- match.arg(language)
  message("Downloading COVID-19 data from official Brazilian repository: https://covid.saude.gov.br/")
  url <- "https://covid.saude.gov.br/assets/files/COVID19_"
  ad <- available_date(url=url, lag=3)
  url <- paste0(url, ad, ".csv")
  brasil <- as_tibble(fread(url))
  brasil <- mutate(brasil, data = dmy(data))
  brasil <- rename(brasil, obitosAcumulados = obitosAcumulados)
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
#' @aliases downloadWorld
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

  world <- world %>%
    rename(accumCases = confirmed,
           accumDeaths = deaths,
           accumRecovered = recovered) %>%
    group_by(local) %>%
    mutate(newCases = diff(c(0, accumCases)),
           newDeaths = diff(c(0, accumDeaths)),
           newRecovered = diff(c(0, accumRecovered)))



  if(language=="pt"){
    world <- rename(world,
           data = date,
           novosCasos = newCases,
           novosObitos = newDeaths,
           novosRecuperados  = newRecovered,
           casosAcumulados = accumCases,
           mortesAcumuladas = accumDeaths,
           recuperadosAcumulados = accumRecovered)

  }
  setattr(world, "language", language)
  class(world) <-  class(world)[-1]
  return(world)
}


brasil <- downloadBR()
world <- downloadWorld()
usethis::use_data(brasil, overwrite = TRUE)
usethis::use_data(world, overwrite = TRUE)



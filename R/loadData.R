# Function to compute the mortality rate:
funMortalityRate <- function(accumCases, accumDeaths){
  n <- length(accumCases)
  rate <- rep(0, n)
  for(i in 1:n){
    if(accumCases[i]>0){
      rate[i] <- accumDeaths[i]/accumCases[i]
    }
  }
  return(rate)
}

# Function to download data (at Brazilian level) from the official Brazilian's repository
downloadBR <- function(language=c("pt", "en")){
  language <- match.arg(language)
  message("Downloading COVID-19 data from official Brazilian repository: https://covid.saude.gov.br/")
  cdnResponse <- httr::GET("https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeral", add_headers("X-Parse-Application-Id" = "unAFkcaNDeXajurGB7LChj8SgQYS2ptm"), accept_json())
  results <- fromJSON(content(cdnResponse, "text", encoding="UTF-8"))$results
  url <- results$arquivo$url
  brasil <- as_tibble(fread(url))
  brasil <- mutate(brasil,
                   data = dmy(as.Date(data)),
                   mortalidade = funMortalityRate(casosAcumulados, obitosAcumulados))
  if(language=="en"){
    brasil <- brasil %>%
      rename(
         region = regiao,
         state = estado,
         date = data,
         newCases = casosNovos,
         accumCases = casosAcumulados,
         newDeaths = obitosNovos,
         accumDeaths = obitosAcumulados,
         mortality = mortalidade) %>%
      mutate(region = recode(region,
                             Norte = "North",
                             Nordeste = "Northeast",
                             Sudeste = "Southeast",
                             Sul = "South",
                             'Centro-Oeste' = "Midwest"))
  }

  setattr(brasil, "language", language)
  return(brasil)
}


# Function to download data (at world level) from the Johns Hopkins University's repository
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
    rename(
      countries_territories =  local,
      accumCases = confirmed,
      accumDeaths = deaths,
      accumRecovered = recovered) %>%
    group_by(countries_territories) %>%
    mutate(newCases = diff(c(0, accumCases)),
           newDeaths = diff(c(0, accumDeaths)),
           newRecovered = diff(c(0, accumRecovered)))



  if(language=="pt"){
    world <- rename(world,
                    paises_territorios = countries_territories,
                    data = date,
                    casosNovos = newCases,
                    obitosNovos = newDeaths,
                    novosRecuperados  = newRecovered,
                    casosAcumulados = accumCases,
                    obitosAcumulados = accumDeaths,
                    recuperadosAcumulados = accumRecovered)

  }
  setattr(world, "language", language)
  class(world) <-  class(world)[-1]
  return(world)

}


#' Function to download COVID-19 data from web repositories
#' @aliases downloadCovid
#' @export
#' @param url data repository's url
#' @param language language; currently only portuguese and english available
#' @return tibble/data.frame containing the downloaded data
#' @description This function downloads the pandemic COVID-19 data from two repositories: the the official Brazilian's repository mantained by the Brazilian Government (https://covid.saude.gov.br), which contains only data of the pandemia in Brazil at states and region levels, and the data from the Johns Hopkins University's repository (https://github.com/CSSEGISandData/COVID-19), which has been widely used all over the world as a reliable source of data information on the COVID-19 pandemia at a global level (countries and territories). For more details, please see our package's vignette.
#'
#' @examples
#' \dontrun{
#' library(readCovid19)
#' brazil <- downloadCovid19(language="pt", url="brgov")
#' brazil
#'
#' world <- downloadCovid19(language="pt", url="jhu")
#' world
#' }
#'
downloadCovid19 <- function(url=c("brgov", "jhu"), language=c("en", "pt")){
  url <- match.arg(url)
  language <- match.arg(language)
  mydata <- switch(url,
                   "brgov" = downloadBR(language),
                   "jhu" = downloadWorld(language))
}





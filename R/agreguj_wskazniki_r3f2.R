#' @title Obliczanie wskaznikow dla R3 F1 na poziomie zagregowanym
#' @description Funkcja oblicza wartości wskaźników na poziomie zagregowanym
#' na podstawie ramki danych z wynikami ankiety CAWI.
#' @param wskazniki ramka danych z wynikami 3. rundy monitoringu
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}} z
#' pakietu \code{MLASZdane}.
#' @return data frame
#' @seealso \code{agreguj_wskazniki} z pakietu \code{MLASZdane} oraz
#' przekazywane do niej funkcje używane do obliczania konkretnych wskaźników
#' zagregowanych:
#' \itemize{
#'   \item{\code{\link{dane_szk}},}
#'   \item{\code{\link{formy}},}
#'   \item{\code{\link{firma}},}
#'   \item{\code{\link{l_abs}},}
#'   \item{\code{\link{l_abs_WT}},}
#'   \item{\code{\link{l_kobiet}},}
#'   \item{\code{\link{l_kobiet_WT}},}
#'   \item{\code{\link{zaw_licz}},}
#'   \item{\code{\link{zaw_licz_WT}},}
#'   \item{\code{\link{praca_zarobkowa}},}
#'   \item{\code{\link{praca_zarobkowa_WT}},}
#'   \item{\code{\link{bezrobocie}},}
#'   \item{\code{\link{bezrobocie_WT}},}
#'   \item{\code{\link{neet}},}
#'   \item{\code{\link{neet_WT}},}
#'   \item{\code{\link{status_2021_10}},}
#'   \item{\code{\link{status_2021_10_WT}},}
#'   \item{\code{\link{eduk_kontyn}},}
#'   \item{\code{\link{eduk_kontyn_WT}},}
#'   \item{\code{\link{zgodnosc_pracy}},}
#'   \item{\code{\link{zgodnosc_pracy_WT}},}
#'   \item{\code{\link{zad_ogolne}},}
#'   \item{\code{\link{zad_ogolne_WT}},}
#'   \item{\code{\link{zad_rozwoj}},}
#'   \item{\code{\link{zad_rozwoj_WT}},}
#'   \item{\code{\link{zad_zycie}},}
#'   \item{\code{\link{zad_zycie_WT}},}
#'   \item{\code{\link{zad_zarobki}},}
#'   \item{\code{\link{zad_zarobki_WT}},}
#'   \item{\code{\link{zad_utrzym}},}
#'   \item{\code{\link{zad_utrzym_WT}}}
#' }
#' @export
#' @importFrom dplyr .data
agreguj_cawi_r3f2 = function(wskazniki, grupy) {
  stopifnot(is.data.frame(wskazniki),
            is.data.frame(grupy))
  nazwy = c("ID_RSPO", "TYP_SZK", "woj_szk")
  sprawdz_nazwy(names(wskazniki), nazwy)

  wsk = agreguj_wskazniki(
    wskazniki, grupy,
    dane_szk = dane_szk(.data),
    formy = formy(.data),
    firma = firma(.data),
    l_abs = l_abs(.data),
    l_abs_WT = l_abs_WT(.data),
    l_kobiet = l_kobiet(.data),
    l_kobiet_WT = l_kobiet_WT(.data),
    zaw_licz = zaw_licz(.data),
    zaw_licz_WT = zaw_licz_WT(.data),
    praca_zarobkowa = praca_zarobkowa(.data),
    praca_zarobkowa_WT = praca_zarobkowa_WT(.data),
    bezrobocie = bezrobocie(.data),
    bezrobocie_WT = bezrobocie_WT(.data),
    neet = neet(.data),
    neet_WT = neet_WT(.data),
    status_2021_10 = status_2021_10(.data),
    status_2021_10_WT = status_2021_10_WT(.data),
    eduk_kontyn = eduk_kontyn(.data),
    eduk_kontyn_WT = eduk_kontyn_WT(.data),
    zgodnosc_pracy = zgodnosc_pracy(.data),
    zgodnosc_pracy_WT = zgodnosc_pracy_WT(.data),
    zad_ogolne = zad_ogolne(.data),
    zad_ogolne_WT = zad_ogolne_WT(.data),
    zad_rozwoj = zad_rozwoj(.data),
    zad_rozwoj_WT = zad_rozwoj_WT(.data),
    zad_zycie = zad_zycie(.data),
    zad_zycie_WT = zad_zycie_WT(.data),
    zad_zarobki = zad_zarobki(.data),
    zad_zarobki_WT = zad_zarobki_WT(.data),
    zad_utrzym = zad_utrzym(.data),
    zad_utrzym_WT = zad_utrzym_WT(.data)
  )

  return(wsk)
}

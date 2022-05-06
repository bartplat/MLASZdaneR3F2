#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja przechowująca nazwę i adres szkoły jako listę.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
dane_szk = function(x) {
  stopifnot(is.data.frame(x))

  list(nazwa = unique(x$B0),
       adres = unique(x$szk_adres)) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja przechowująca listę form gramatycznych różnych słów lub
#' wyrażeń, które pojawiają się w raporcie w zależności od typu szkoły.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% .data case_when distinct mutate select
#' @export
formy = function(x) {
  l_kobiet = x %>%
    filter(.data$PLC %in% 1) %>%
    count(.data$PLC) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    round()

  x %>%
    select(.data$woj_szk, .data$TYP_SZK) %>%
    distinct() %>%
    mutate(
      typ_szk_mian = case_when(
        .data$TYP_SZK %in% 1 ~ "branżowa szkoła pierwszego stopnia",
        .data$TYP_SZK %in% 2 ~ "technikum",
        .data$TYP_SZK %in% 3 ~ "szkoła policealna"),
      typ_szk_dop_mn = case_when(
        .data$TYP_SZK %in% 1 ~ "branżowych szkół pierwszego stopnia",
        .data$TYP_SZK %in% 2 ~ "techników",
        .data$TYP_SZK %in% 3 ~ "szkół policealnych"),
      woj_nazwa_dop = ifelse(length(unique(.data$woj_szk)) %in% 1,
                             paste0(.data$woj_szk, "go"),
                             "NA"),
      kobiet_a = case_when(
        l_kobiet %in% 0 ~ paste0("kobiet"),
        l_kobiet %in% 1 ~ paste0("kobieta"),
        l_kobiet %in% c(2:4) ~ paste0("kobiety"),
        l_kobiet %in% c(5:21) ~ paste0("kobiet"),
        l_kobiet > 21 & l_kobiet %% 10 %in% c(2:4) ~ paste0("kobiety"),
        l_kobiet > 21 & l_kobiet %% 10 %in% c(5:9, 0, 1) ~ paste0("kobiet")),
      # byla_o = ifelse(.data$typ_szk %in% c(1, 3), "była", "było"),
      la_lo = ifelse(.data$TYP_SZK %in% c(1, 3), "ła", "ło")) %>%
    select(-c(.data$woj_szk, .data$TYP_SZK)) %>%
    distinct() %>%
    as.list() %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja przechowująca informację o nazwie firmy realizującej
#' badanie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% summarise case_when n_distinct .data pull
#' @export
firma = function(x) {
  x %>%
    summarise(firma = case_when(n_distinct(.data$Wykonawca) > 1 ~ "ndt.",
                                all(.data$Wykonawca %in% 1) ~ "PBS sp. z o.o.",
                                all(.data$Wykonawca %in% 2) ~ "Danae sp. z o.o.")) %>%
    pull(firma) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych absolwentów
#' w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' @importFrom dplyr %>% count pull
#' @export
l_abs = function(x) {
  x %>%
    count(.data$PLC) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący \strong{ważonż} liczbę
#' zbadanych absolwentów w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' @importFrom dplyr %>% count pull
#' @export
l_abs_WT = function(x) {
  x %>%
    count(.data$PLC, wt = .data$waga) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący liczbę zbadanych kobiet w
#' grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' @importFrom dplyr %>% filter count pull
#' @export
l_kobiet = function(x) {
  x %>%
    filter(.data$PLC %in% 1) %>%
    count(.data$PLC) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja oblicza wskaźnik opisujący \strong{ważoną} liczbę
#' zbadanych kobiet w grupie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return liczba
#' @importFrom dplyr %>% filter count pull
#' @export
l_kobiet_WT = function(x) {
  x %>%
    filter(.data$PLC %in% 1) %>%
    count(.data$PLC, wt = .data$waga) %>%
    pull(.data$n) %>%
    sum(na.rm = TRUE) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca liczbę absolwentów danego zawodu. Ponadto,
#' dodana jest informacja o branży, do której należy zawód.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% select .data filter distinct count rename left_join
#' @export
zaw_licz = function(x) {
  n = list(n = sum(!is.na(x$zawod_nazwa), na.rm = TRUE))

  mapp = x %>%
    select(.data$B3, .data$zawod_nazwa) %>%
    filter(!(is.na(.data$B3) | is.na(.data$zawod_nazwa))) %>%
    distinct()

  zaw = x %>%
    count(.data$zawod_nazwa) %>%
    rename(zawod = .data$zawod_nazwa, n_zaw = .data$n)

  zaw_tab = mapp %>%
    left_join(zaw, by = c("zawod_nazwa" = "zawod")) %>%
    rename(branza = .data$B3, zawod = .data$zawod_nazwa) %>%
    as.list()

  return(c(n, zaw_tab))
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca \strong{ważoną} liczbę absolwentów danego
#' zawodu. Ponadto, dodana jest informacja o branży, do której należy zawód.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>% select .data filter distinct count rename left_join
#' @export
zaw_licz_WT = function(x) {
  n = list(n = sum(as.numeric(!is.na(x$zawod_nazwa)) * x$waga, na.rm = TRUE))

  mapp = x %>%
    select(.data$B3, .data$zawod_nazwa) %>%
    filter(!(is.na(.data$B3) | is.na(.data$zawod_nazwa))) %>%
    distinct()

  zaw = x %>%
    count(.data$zawod_nazwa, wt = .data$waga) %>%
    rename(zawod = .data$zawod_nazwa, n_zaw = .data$n)

  zaw_tab = mapp %>%
    left_join(zaw, by = c("zawod_nazwa" = "zawod")) %>%
    rename(branza = .data$B3, zawod = .data$zawod_nazwa) %>%
    as.list()

  return(c(n, zaw_tab))
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów pracujących zarobkowo w
#' okresie \strong{lipiec-październik 2021} przez minimum 2 tygodnie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
praca_zarobkowa = function(x) {
  nka = sum(x$SZ5_3_1 %in% c(1:3, 7) |
              x$SZ5_3_2 %in% c(1:3, 7) |
              x$SZ5_3_3 %in% c(1:3, 7) |
              x$SZ5_3_4 %in% c(1:3, 7), na.rm = TRUE)

  list(n = nka,
       ods_ever = sum(x$SZ5_3_1 %in% 1 |
                        x$SZ5_3_2 %in% 1 |
                        x$SZ5_3_3 %in% 1 |
                        x$SZ5_3_4 %in% 1, na.rm = TRUE) / nka,
       ods_07 = sum(x$SZ5_3_1 %in% 1, na.rm = TRUE) / nka,
       ods_08 = sum(x$SZ5_3_2 %in% 1, na.rm = TRUE) / nka,
       ods_09 = sum(x$SZ5_3_3 %in% 1, na.rm = TRUE) / nka,
       ods_10 = sum(x$SZ5_3_4 %in% 1, na.rm = TRUE) / nka) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca \strong{ważony} odsetek absolwentów
#' pracujących zarobkowo w okresie \strong{lipiec-październik 2021} przez
#' minimum 2 tygodnie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
praca_zarobkowa_WT = function(x) {
  nka = sum(as.numeric(
    x$SZ5_3_1 %in% c(1:3, 7) |
      x$SZ5_3_2 %in% c(1:3, 7) |
      x$SZ5_3_3 %in% c(1:3, 7) |
      x$SZ5_3_4 %in% c(1:3, 7)) * x$waga, na.rm = TRUE)

  list(n = nka,
       ods_ever = sum(as.numeric(
         x$SZ5_3_1 %in% 1 |
           x$SZ5_3_2 %in% 1 |
           x$SZ5_3_3 %in% 1 |
           x$SZ5_3_4 %in% 1) * x$waga, na.rm = TRUE) / nka,
       ods_07 = sum(as.numeric(x$SZ5_3_1 %in% 1) * x$waga, na.rm = TRUE) / nka,
       ods_08 = sum(as.numeric(x$SZ5_3_2 %in% 1) * x$waga, na.rm = TRUE) / nka,
       ods_09 = sum(as.numeric(x$SZ5_3_3 %in% 1) * x$waga, na.rm = TRUE) / nka,
       ods_10 = sum(as.numeric(x$SZ5_3_4 %in% 1) * x$waga, na.rm = TRUE) / nka) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów zarejestrowanych w
#' Urzędzie Pracy jako bezrobotni w okresie \strong{lipiec-październik 2021}
#' przez minimum 2 tygodnie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
bezrobocie = function(x) {
  nka = sum(x$SZ5_5_1 %in% c(1:3, 7) |
              x$SZ5_5_2 %in% c(1:3, 7) |
              x$SZ5_5_3 %in% c(1:3, 7) |
              x$SZ5_5_4 %in% c(1:3, 7), na.rm = TRUE)

  list(n = nka,
       ods_ever = sum(x$SZ5_5_1 %in% 1 |
                        x$SZ5_5_2 %in% 1 |
                        x$SZ5_5_3 %in% 1 |
                        x$SZ5_5_4 %in% 1, na.rm = TRUE) / nka,
       ods_07 = sum(x$SZ5_5_1 %in% 1, na.rm = TRUE) / nka,
       ods_08 = sum(x$SZ5_5_2 %in% 1, na.rm = TRUE) / nka,
       ods_09 = sum(x$SZ5_5_3 %in% 1, na.rm = TRUE) / nka,
       ods_10 = sum(x$SZ5_5_4 %in% 1, na.rm = TRUE) / nka) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca \strong{ważony} odsetek absolwentów
#' zarejestrowanych w Urzędzie Pracy jako bezrobotni w okresie
#' \strong{lipiec-październik 2021} przez minimum 2 tygodnie.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
bezrobocie_WT = function(x) {
  nka = sum(as.numeric(
    x$SZ5_5_1 %in% c(1:3, 7) |
      x$SZ5_5_2 %in% c(1:3, 7) |
      x$SZ5_5_3 %in% c(1:3, 7) |
      x$SZ5_5_4 %in% c(1:3, 7)) * x$waga, na.rm = TRUE)

  list(n = nka,
       ods_ever = sum(as.numeric(
         x$SZ5_5_1 %in% 1 |
           x$SZ5_5_2 %in% 1 |
           x$SZ5_5_3 %in% 1 |
           x$SZ5_5_4 %in% 1) * x$waga, na.rm = TRUE) / nka,
       ods_07 = sum(as.numeric(x$SZ5_5_1 %in% 1) * x$waga, na.rm = TRUE) / nka,
       ods_08 = sum(as.numeric(x$SZ5_5_2 %in% 1) * x$waga, na.rm = TRUE) / nka,
       ods_09 = sum(as.numeric(x$SZ5_5_3 %in% 1) * x$waga, na.rm = TRUE) / nka,
       ods_10 = sum(as.numeric(x$SZ5_5_4 %in% 1) * x$waga, na.rm = TRUE) / nka) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca odsetek absolwentów biernych edukacyjnie i
#' zawodowo w okresie \strong{lipiec-październik 2021} przez minimum 2 tygodnie.
#' Wskaźnik liczony jest na 4 zmiennych:
#' \itemize{
#'   \item{\code{SZ5_1} - studia lub nauka w szkole}
#'   \item{\code{SZ5_2} - praca w gospodarstwie rolnym}
#'   \item{\code{SZ5_3} - praca zarobkowa u pracodawcy}
#'   \item{\code{SZ5_4} - praca na własny rachunek}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
neet = function(x) {
  nka = sum(x$SZ5_1_1 %in% c(1:3, 7) |
              x$SZ5_1_2 %in% c(1:3, 7) |
              x$SZ5_1_3 %in% c(1:3, 7) |
              x$SZ5_1_4 %in% c(1:3, 7), na.rm = TRUE)

  list(n = nka,
       n_neet = sum(x$SZ5_1_1 %in% 2 &
                      x$SZ5_1_2 %in% 2 &
                      x$SZ5_1_3 %in% 2 &
                      x$SZ5_1_4 %in% 2 &
                      x$SZ5_2_1 %in% 2 &
                      x$SZ5_2_2 %in% 2 &
                      x$SZ5_2_3 %in% 2 &
                      x$SZ5_2_4 %in% 2 &
                      x$SZ5_3_1 %in% 2 &
                      x$SZ5_3_2 %in% 2 &
                      x$SZ5_3_3 %in% 2 &
                      x$SZ5_3_4 %in% 2 &
                      x$SZ5_4_1 %in% 2 &
                      x$SZ5_4_2 %in% 2 &
                      x$SZ5_4_3 %in% 2 &
                      x$SZ5_4_4 %in% 2, na.rm = TRUE),
       ods_ever = sum(x$SZ5_1_1 %in% 2 &
                        x$SZ5_1_2 %in% 2 &
                        x$SZ5_1_3 %in% 2 &
                        x$SZ5_1_4 %in% 2 &
                        x$SZ5_2_1 %in% 2 &
                        x$SZ5_2_2 %in% 2 &
                        x$SZ5_2_3 %in% 2 &
                        x$SZ5_2_4 %in% 2 &
                        x$SZ5_3_1 %in% 2 &
                        x$SZ5_3_2 %in% 2 &
                        x$SZ5_3_3 %in% 2 &
                        x$SZ5_3_4 %in% 2 &
                        x$SZ5_4_1 %in% 2 &
                        x$SZ5_4_2 %in% 2 &
                        x$SZ5_4_3 %in% 2 &
                        x$SZ5_4_4 %in% 2, na.rm = TRUE) / nka,
       ods_07 = sum(x$SZ5_1_1 %in% 2 &
                      x$SZ5_2_1 %in% 2 &
                      x$SZ5_3_1 %in% 2 &
                      x$SZ5_4_1 %in% 2, na.rm = TRUE) / nka,
       ods_08 = sum(x$SZ5_1_2 %in% 2 &
                      x$SZ5_2_2 %in% 2 &
                      x$SZ5_3_2 %in% 2 &
                      x$SZ5_4_2 %in% 2, na.rm = TRUE) / nka,
       ods_09 = sum(x$SZ5_1_3 %in% 2 &
                      x$SZ5_2_3 %in% 2 &
                      x$SZ5_3_3 %in% 2 &
                      x$SZ5_4_3 %in% 2, na.rm = TRUE) / nka,
       ods_10 = sum(x$SZ5_1_4 %in% 2 &
                      x$SZ5_2_4 %in% 2 &
                      x$SZ5_3_4 %in% 2 &
                      x$SZ5_4_4 %in% 2, na.rm = TRUE) / nka) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca \strong{ważony} odsetek absolwentów biernych
#' edukacyjnie i zawodowo w okresie \strong{lipiec-październik 2021} przez
#' minimum 2 tygodnie. Wskaźnik liczony jest na 4 zmiennych:
#' \itemize{
#'   \item{\code{SZ5_1} - studia lub nauka w szkole}
#'   \item{\code{SZ5_2} - praca w gospodarstwie rolnym}
#'   \item{\code{SZ5_3} - praca zarobkowa u pracodawcy}
#'   \item{\code{SZ5_4} - praca na własny rachunek}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
neet_WT = function(x) {
  nka = sum(as.numeric(x$SZ5_1_1 %in% c(1:3, 7) |
              x$SZ5_1_2 %in% c(1:3, 7) |
              x$SZ5_1_3 %in% c(1:3, 7) |
              x$SZ5_1_4 %in% c(1:3, 7)) * x$waga, na.rm = TRUE)

  list(n = nka,
       n_neet = sum(as.numeric(
         x$SZ5_1_1 %in% 2 &
           x$SZ5_1_2 %in% 2 &
           x$SZ5_1_3 %in% 2 &
           x$SZ5_1_4 %in% 2 &
           x$SZ5_2_1 %in% 2 &
           x$SZ5_2_2 %in% 2 &
           x$SZ5_2_3 %in% 2 &
           x$SZ5_2_4 %in% 2 &
           x$SZ5_3_1 %in% 2 &
           x$SZ5_3_2 %in% 2 &
           x$SZ5_3_3 %in% 2 &
           x$SZ5_3_4 %in% 2 &
           x$SZ5_4_1 %in% 2 &
           x$SZ5_4_2 %in% 2 &
           x$SZ5_4_3 %in% 2 &
           x$SZ5_4_4 %in% 2) * x$waga, na.rm = TRUE),
       ods_ever = sum(as.numeric(
         x$SZ5_1_1 %in% 2 &
           x$SZ5_1_2 %in% 2 &
           x$SZ5_1_3 %in% 2 &
           x$SZ5_1_4 %in% 2 &
           x$SZ5_2_1 %in% 2 &
           x$SZ5_2_2 %in% 2 &
           x$SZ5_2_3 %in% 2 &
           x$SZ5_2_4 %in% 2 &
           x$SZ5_3_1 %in% 2 &
           x$SZ5_3_2 %in% 2 &
           x$SZ5_3_3 %in% 2 &
           x$SZ5_3_4 %in% 2 &
           x$SZ5_4_1 %in% 2 &
           x$SZ5_4_2 %in% 2 &
           x$SZ5_4_3 %in% 2 &
           x$SZ5_4_4 %in% 2) * x$waga, na.rm = TRUE) / nka,
       ods_07 = sum(as.numeric(
         x$SZ5_1_1 %in% 2 &
           x$SZ5_2_1 %in% 2 &
           x$SZ5_3_1 %in% 2 &
           x$SZ5_4_1 %in% 2) * x$waga, na.rm = TRUE) / nka,
       ods_08 = sum(as.numeric(
         x$SZ5_1_2 %in% 2 &
           x$SZ5_2_2 %in% 2 &
           x$SZ5_3_2 %in% 2 &
           x$SZ5_4_2 %in% 2) * x$waga, na.rm = TRUE) / nka,
       ods_09 = sum(as.numeric(
         x$SZ5_1_3 %in% 2 &
           x$SZ5_2_3 %in% 2 &
           x$SZ5_3_3 %in% 2 &
           x$SZ5_4_3 %in% 2) * x$waga, na.rm = TRUE) / nka,
       ods_10 = sum(as.numeric(
         x$SZ5_1_4 %in% 2 &
           x$SZ5_2_4 %in% 2 &
           x$SZ5_3_4 %in% 2 &
           x$SZ5_4_4 %in% 2) * x$waga, na.rm = TRUE) / nka) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o sytuacji
#' edukacyjno-zawodowej absolwentów w \strong{październiku} 2021 roku:
#' \itemize{
#'  \item{tylko pracował}{SZ5_1=2 & SZ5_3=1}
#'  \item{tylko uczył się}{SZ5_1=1 & SZ5_3=2}
#'  \item{uczył się i pracował}{SZ5_1=1 & SZ5_3=1}
#'  \item{nie uczył się ani nie pracował}{SZ5_1= & SZ5_3=2}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
status_2021_10 = function(x) {
  nka = sum(x$SZ5_1_4 %in% c(1:3, 7) |
              x$SZ5_3_4 %in% c(1:3, 7), na.rm = TRUE)

  list(n = nka,
       tylko_praca = sum(x$SZ5_1_4 %in% 2 & x$SZ5_3_4 %in% 1, na.rm = TRUE) / nka,
       tylko_nauka = sum(x$SZ5_1_4 %in% 1 & x$SZ5_3_4 %in% 2, na.rm = TRUE) / nka,
       nauka_praca = sum(x$SZ5_1_4 %in% 1 & x$SZ5_3_4 %in% 1, na.rm = TRUE) / nka,
       brak_nauka_brak_praca = sum(x$SZ5_1_4 %in% 2 & x$SZ5_3_4 %in% 2, na.rm = TRUE) / nka,
       nie_dotyczy_liczba = sum(x$SZ5_1_4 %in% 3 & x$SZ5_3_4 %in% 3, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca \strong{ważoną }informację o
#' sytuacji edukacyjno-zawodowej absolwentów w \strong{październiku} 2021 roku:
#' \itemize{
#'  \item{tylko pracował}{SZ5_1=2 & SZ5_3=1}
#'  \item{tylko uczył się}{SZ5_1=1 & SZ5_3=2}
#'  \item{uczył się i pracował}{SZ5_1=1 & SZ5_3=1}
#'  \item{nie uczył się ani nie pracował}{SZ5_1= & SZ5_3=2}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
status_2021_10_WT = function(x) {
  nka = sum(as.numeric(x$SZ5_1_4 %in% c(1:3, 7) |
              x$SZ5_3_4 %in% c(1:3, 7)) * x$waga, na.rm = TRUE)

  list(n = nka,
       tylko_praca = sum(as.numeric(x$SZ5_1_4 %in% 2 & x$SZ5_3_4 %in% 1) * x$waga, na.rm = TRUE) / nka,
       tylko_nauka = sum(as.numeric(x$SZ5_1_4 %in% 1 & x$SZ5_3_4 %in% 2) * x$waga, na.rm = TRUE) / nka,
       nauka_praca = sum(as.numeric(x$SZ5_1_4 %in% 1 & x$SZ5_3_4 %in% 1) * x$waga, na.rm = TRUE) / nka,
       brak_nauka_brak_praca = sum(as.numeric(x$SZ5_1_4 %in% 2 & x$SZ5_3_4 %in% 2) * x$waga, na.rm = TRUE) / nka,
       nie_dotyczy_liczba = sum(as.numeric(x$SZ5_1_4 %in% 3 & x$SZ5_3_4 %in% 3) * x$waga, na.rm = TRUE)
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o kontynuacji
#' nauki przez absolwentów po zakończeniu nauki w następujących formach:
#' \itemize{
#'  \item{Branżowa szkoła 2. stopnia}
#'  \item{Liceum dla dorosłych}
#'  \item{Szkoła policealna}
#'  \item{Studia - związane z zawodem}
#'  \item{Studia - niezwiązane z zawodem}
#'  \item{Studia - dowolna forma}
#'  \item{Kwalifikacyjny Kurs Zawodowy}
#'  \item{Dowonla forma}
#'  \item{Dowonla forma - BS1}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
eduk_kontyn = function(x) {
 nka = sum(x$KN1_6 %in% c(1:3), na.rm = TRUE)

 list(n = nka,
      bs2 = sum(x$KN1_1 %in% 1, na.rm = TRUE) / nka,
      lodd = sum(x$KN1_2 %in% 1, na.rm = TRUE) / nka,
      spolic = sum(x$KN1_3 %in% 1, na.rm = TRUE) / nka,
      stud_zw = sum(x$KN1_4 %in% 1, na.rm = TRUE) / nka,
      stud_niezw = sum(x$KN1_5 %in% 1, na.rm = TRUE) / nka,
      stud_ogolem = sum(x$KN1_4 %in% 1 | x$KN1_5 %in% 1, na.rm = TRUE) / nka,
      kkz = sum(x$KN1_6 %in% 1, na.rm = TRUE) / nka,
      any = sum(x$KN1_1 %in% 1 |
                  x$KN1_2 %in% 1 |
                  x$KN1_3 %in% 1 |
                  x$KN1_4 %in% 1 |
                  x$KN1_5 %in% 1 |
                  x$KN1_6 %in% 1, na.rm = TRUE) / nka,
      any_bs1 = sum(x$KN1_1 %in% 1 |
                      x$KN1_2 %in% 1 |
                      x$KN1_6 %in% 1, na.rm = TRUE) / nka) %>%
   return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca \strong{ważoną} informację o
#' kontynuacji nauki przez absolwentów po zakończeniu nauki w następujących
#' formach:
#' \itemize{
#'  \item{Branżowa szkoła 2. stopnia}
#'  \item{Liceum dla dorosłych}
#'  \item{Szkoła policealna}
#'  \item{Studia - związane z zawodem}
#'  \item{Studia - niezwiązane z zawodem}
#'  \item{Studia - dowolna forma}
#'  \item{Kwalifikacyjny Kurs Zawodowy}
#'  \item{Dowonla forma}
#'  \item{Dowonla forma - BS1}
#' }
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
eduk_kontyn_WT = function(x) {
  nka = sum(as.numeric(x$KN1_6 %in% c(1:3)) * x$waga, na.rm = TRUE)

  list(n = nka,
       bs2 = sum(as.numeric(x$KN1_1 %in% 1) * x$waga, na.rm = TRUE) / nka,
       lodd = sum(as.numeric(x$KN1_2 %in% 1) * x$waga, na.rm = TRUE) / nka,
       spolic = sum(as.numeric(x$KN1_3 %in% 1) * x$waga, na.rm = TRUE) / nka,
       stud_zw = sum(as.numeric(x$KN1_4 %in% 1) * x$waga, na.rm = TRUE) / nka,
       stud_niezw = sum(as.numeric(x$KN1_5 %in% 1) * x$waga, na.rm = TRUE) / nka,
       stud_ogolem = sum(as.numeric(x$KN1_4 %in% 1 | x$KN1_5 %in% 1) * x$waga, na.rm = TRUE) / nka,
       kkz = sum(as.numeric(x$KN1_6 %in% 1) * x$waga, na.rm = TRUE) / nka,
       any = sum(as.numeric(x$KN1_1 %in% 1 |
                              x$KN1_2 %in% 1 |
                              x$KN1_3 %in% 1 |
                              x$KN1_4 %in% 1 |
                              x$KN1_5 %in% 1 |
                              x$KN1_6 %in% 1) * x$waga, na.rm = TRUE) / nka,
       any_bs1 = sum(as.numeric(x$KN1_1 %in% 1 |
                                  x$KN1_2 %in% 1 |
                                  x$KN1_6 %in% 1) * x$waga, na.rm = TRUE) / nka) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o zgodności
#' pierwszej pracy z wykształceniem - na podstawie pytania SZ3.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zgodnosc_pracy = function(x){
  nka = sum(x$SZ3_1 %in% c(1, 2, 7), na.rm = TRUE)

  list(
    n = nka,
    wyucz = sum(x$SZ3_1 %in% 1, na.rm = TRUE) / nka,
    branza = sum(x$SZ3_1 %in% 2 & x$SZ3_2 %in% 1, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca \strong{ważoną} informację o
#' zgodności pierwszej pracy z wykształceniem - na podstawie pytania SZ3.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zgodnosc_pracy_WT = function(x){
  nka = sum(as.numeric(x$SZ3_1 %in% c(1, 2, 7)) * x$waga, na.rm = TRUE)

  list(
    n = nka,
    wyucz = sum(as.numeric(x$SZ3_1 %in% 1) * x$waga, na.rm = TRUE) / nka,
    branza = sum(as.numeric(x$SZ3_1 %in% 2 & x$SZ3_2 %in% 1) * x$waga, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o ogólnym
#' zadowoleniu z pracy - na podstawie pytania PP2_11.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_ogolne = function(x) {
  nka = sum(x$PP2_11 %in% c(1:6), na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(x$PP2_11 %in% c(1, 2), na.rm = TRUE) / nka,
    zdec_zadow = sum(x$PP2_11 %in% 1, na.rm = TRUE) / nka,
    racz_zadow = sum(x$PP2_11 %in% 2, na.rm = TRUE) / nka,
    ani_ani = sum(x$PP2_11 %in% 3, na.rm = TRUE) / nka,
    racz_nzaw = sum(x$PP2_11 %in% 4, na.rm = TRUE) / nka,
    zdec_nzaw = sum(x$PP2_11 %in% 5, na.rm = TRUE) / nka,
    b2b = sum(x$PP2_11 %in% c(4, 5), na.rm = TRUE) / nka,
    trud_pow = sum(x$PP2_11 %in% 6, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca \strong{ważoną} informację o
#' ogólnym zadowoleniu z pracy - na podstawie pytania PP2_11.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_ogolne_WT = function(x) {
  nka = sum(as.numeric(x$PP2_11 %in% c(1:6)) * x$waga, na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(as.numeric(x$PP2_11 %in% c(1, 2)) * x$waga, na.rm = TRUE) / nka,
    zdec_zadow = sum(as.numeric(x$PP2_11 %in% 1) * x$waga, na.rm = TRUE) / nka,
    racz_zadow = sum(as.numeric(x$PP2_11 %in% 2) * x$waga, na.rm = TRUE) / nka,
    ani_ani = sum(as.numeric(x$PP2_11 %in% 3) * x$waga, na.rm = TRUE) / nka,
    racz_nzaw = sum(as.numeric(x$PP2_11 %in% 4) * x$waga, na.rm = TRUE) / nka,
    zdec_nzaw = sum(as.numeric(x$PP2_11 %in% 5) * x$waga, na.rm = TRUE) / nka,
    b2b = sum(as.numeric(x$PP2_11 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$PP2_11 %in% 6) * x$waga, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o zadowoleniu z
#' pracy, a w szczególności pod względem możliwości rozwoju umiejętności
#' zawodowych - na podstawie pytania PP2_5.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_rozwoj = function(x) {
  nka = sum(x$PP2_5 %in% c(1:6), na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(x$PP2_5 %in% c(1, 2), na.rm = TRUE) / nka,
    zdec_zadow = sum(x$PP2_5 %in% 1, na.rm = TRUE) / nka,
    racz_zadow = sum(x$PP2_5 %in% 2, na.rm = TRUE) / nka,
    ani_ani = sum(x$PP2_5 %in% 3, na.rm = TRUE) / nka,
    racz_nzaw = sum(x$PP2_5 %in% 4, na.rm = TRUE) / nka,
    zdec_nzaw = sum(x$PP2_5 %in% 5, na.rm = TRUE) / nka,
    b2b = sum(x$PP2_5 %in% c(4, 5), na.rm = TRUE) / nka,
    trud_pow = sum(x$PP2_5 %in% 6, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca \strong{ważoną} informację o
#' zadowoleniu z pracy, a w szczególności pod względem możliwości rozwoju
#' umiejętności zawodowych - na podstawie pytania PP2_5.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_rozwoj_WT = function(x) {
  nka = sum(as.numeric(x$PP2_5 %in% c(1:6)) * x$waga, na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(as.numeric(x$PP2_5 %in% c(1, 2)) * x$waga, na.rm = TRUE) / nka,
    zdec_zadow = sum(as.numeric(x$PP2_5 %in% 1) * x$waga, na.rm = TRUE) / nka,
    racz_zadow = sum(as.numeric(x$PP2_5 %in% 2) * x$waga, na.rm = TRUE) / nka,
    ani_ani = sum(as.numeric(x$PP2_5 %in% 3) * x$waga, na.rm = TRUE) / nka,
    racz_nzaw = sum(as.numeric(x$PP2_5 %in% 4) * x$waga, na.rm = TRUE) / nka,
    zdec_nzaw = sum(as.numeric(x$PP2_5 %in% 5) * x$waga, na.rm = TRUE) / nka,
    b2b = sum(as.numeric(x$PP2_5 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$PP2_5 %in% 6) * x$waga, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o zadowoleniu z
#' pracy, a w szczególności pod względem możliwości godzenia pracy z życiem poza
#' pracą - na podstawie pytania PP2_6.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_zycie = function(x) {
  nka = sum(x$PP2_6 %in% c(1:6), na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(x$PP2_6 %in% c(1, 2), na.rm = TRUE) / nka,
    zdec_zadow = sum(x$PP2_6 %in% 1, na.rm = TRUE) / nka,
    racz_zadow = sum(x$PP2_6 %in% 2, na.rm = TRUE) / nka,
    ani_ani = sum(x$PP2_6 %in% 3, na.rm = TRUE) / nka,
    racz_nzaw = sum(x$PP2_6 %in% 4, na.rm = TRUE) / nka,
    zdec_nzaw = sum(x$PP2_6 %in% 5, na.rm = TRUE) / nka,
    b2b = sum(x$PP2_6 %in% c(4, 5), na.rm = TRUE) / nka,
    trud_pow = sum(x$PP2_6 %in% 6, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca \strong{ważoną} informację o
#' zadowoleniu z pracy, a w szczególności pod względem możliwości godzenia pracy
#' z życiem poza pracą - na podstawie pytania PP2_6.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_zycie_WT = function(x) {
  nka = sum(as.numeric(x$PP2_6 %in% c(1:6)) * x$waga, na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(as.numeric(x$PP2_6 %in% c(1, 2)) * x$waga, na.rm = TRUE) / nka,
    zdec_zadow = sum(as.numeric(x$PP2_6 %in% 1) * x$waga, na.rm = TRUE) / nka,
    racz_zadow = sum(as.numeric(x$PP2_6 %in% 2) * x$waga, na.rm = TRUE) / nka,
    ani_ani = sum(as.numeric(x$PP2_6 %in% 3) * x$waga, na.rm = TRUE) / nka,
    racz_nzaw = sum(as.numeric(x$PP2_6 %in% 4) * x$waga, na.rm = TRUE) / nka,
    zdec_nzaw = sum(as.numeric(x$PP2_6 %in% 5) * x$waga, na.rm = TRUE) / nka,
    b2b = sum(as.numeric(x$PP2_6 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$PP2_6 %in% 6) * x$waga, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o zadowoleniu z
#' pracy, a w szczególności pod względem wysokości zarobków - na podstawie
#' pytania PP2_1.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_zarobki = function(x) {
  nka = sum(x$PP2_1 %in% c(1:6), na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(x$PP2_1 %in% c(1, 2), na.rm = TRUE) / nka,
    zdec_zadow = sum(x$PP2_1 %in% 1, na.rm = TRUE) / nka,
    racz_zadow = sum(x$PP2_1 %in% 2, na.rm = TRUE) / nka,
    ani_ani = sum(x$PP2_1 %in% 3, na.rm = TRUE) / nka,
    racz_nzaw = sum(x$PP2_1 %in% 4, na.rm = TRUE) / nka,
    zdec_nzaw = sum(x$PP2_1 %in% 5, na.rm = TRUE) / nka,
    b2b = sum(x$PP2_1 %in% c(4, 5), na.rm = TRUE) / nka,
    trud_pow = sum(x$PP2_1 %in% 6, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca \strong{ważoną} informację o
#' zadowoleniu z pracy, a w szczególności pod względem możliwości godzenia pracy
#' z życiem poza pracą - na podstawie pytania PP2_1.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_zarobki_WT = function(x) {
  nka = sum(as.numeric(x$PP2_1 %in% c(1:6)) * x$waga, na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(as.numeric(x$PP2_1 %in% c(1, 2)) * x$waga, na.rm = TRUE) / nka,
    zdec_zadow = sum(as.numeric(x$PP2_1 %in% 1) * x$waga, na.rm = TRUE) / nka,
    racz_zadow = sum(as.numeric(x$PP2_1 %in% 2) * x$waga, na.rm = TRUE) / nka,
    ani_ani = sum(as.numeric(x$PP2_1 %in% 3) * x$waga, na.rm = TRUE) / nka,
    racz_nzaw = sum(as.numeric(x$PP2_1 %in% 4) * x$waga, na.rm = TRUE) / nka,
    zdec_nzaw = sum(as.numeric(x$PP2_1 %in% 5) * x$waga, na.rm = TRUE) / nka,
    b2b = sum(as.numeric(x$PP2_1 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$PP2_1 %in% 6) * x$waga, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca informację o zadowoleniu z
#' pracy, a w szczególności pod względem pewności jej utrzymania - na podstawie
#' pytania PP2_4.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_utrzym = function(x) {
  nka = sum(x$PP2_4 %in% c(1:6), na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(x$PP2_4 %in% c(1, 2), na.rm = TRUE) / nka,
    zdec_zadow = sum(x$PP2_4 %in% 1, na.rm = TRUE) / nka,
    racz_zadow = sum(x$PP2_4 %in% 2, na.rm = TRUE) / nka,
    ani_ani = sum(x$PP2_4 %in% 3, na.rm = TRUE) / nka,
    racz_nzaw = sum(x$PP2_4 %in% 4, na.rm = TRUE) / nka,
    zdec_nzaw = sum(x$PP2_4 %in% 5, na.rm = TRUE) / nka,
    b2b = sum(x$PP2_4 %in% c(4, 5), na.rm = TRUE) / nka,
    trud_pow = sum(x$PP2_4 %in% 6, na.rm = TRUE) / nka
  ) %>%
    return()
}
#' @title Obliczanie wskaznikow dla R3 F2 na poziomie zagregowanym
#' @description Funkcja obliczająca i przechowująca \strong{ważoną} informację o
#' zadowoleniu z pracy, a w szczególności pod względem pewności jej utrzymania -
#' na podstawie pytania PP2_4.
#' @param x ramka danych ze wskaźnikami na poziomie indywidualnym
#' @return lista
#' @importFrom dplyr %>%
#' @export
zad_utrzym_WT = function(x) {
  nka = sum(as.numeric(x$PP2_4 %in% c(1:6)) * x$waga, na.rm = TRUE)

  list(
    n = nka,
    t2b = sum(as.numeric(x$PP2_4 %in% c(1, 2)) * x$waga, na.rm = TRUE) / nka,
    zdec_zadow = sum(as.numeric(x$PP2_4 %in% 1) * x$waga, na.rm = TRUE) / nka,
    racz_zadow = sum(as.numeric(x$PP2_4 %in% 2) * x$waga, na.rm = TRUE) / nka,
    ani_ani = sum(as.numeric(x$PP2_4 %in% 3) * x$waga, na.rm = TRUE) / nka,
    racz_nzaw = sum(as.numeric(x$PP2_4 %in% 4) * x$waga, na.rm = TRUE) / nka,
    zdec_nzaw = sum(as.numeric(x$PP2_4 %in% 5) * x$waga, na.rm = TRUE) / nka,
    b2b = sum(as.numeric(x$PP2_4 %in% c(4, 5)) * x$waga, na.rm = TRUE) / nka,
    trud_pow = sum(as.numeric(x$PP2_4 %in% 6) * x$waga, na.rm = TRUE) / nka
  ) %>%
    return()
}

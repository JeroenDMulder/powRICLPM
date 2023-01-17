open_envelop <- function() {

  ANSWER_name <- readline("Hallo! Wie is dit? ")

  if (!ANSWER_name %in% c("Wouter", "Wouter Pijper", "wouter", "wouter pijper")) {
    stop("Sorry, ik ken je niet. Probeer het opnieuw.")
  }

  cat("Hmm, je zegt dat je Wouter bent... Maar hoe weet ik dat zeker?\n \n")
  Sys.sleep(5)
  cat("Okay, ik ga je een paar vragen stellen waar alleen de echte Wouter het antwoord op weet.\n\n")

  ANSWER_check_feitje <- utils::menu(
    choices = c("De Domtoren", "De Oude Gracht", "Het monument op de Dam"),
    title = "Ik heb je ooit een kaartje gestuurd met daarop een feitje. Waarover ging dit feitje?"
  )

  if (ANSWER_check_feitje != 1) {
    stop("Fout! Jij bent Wouter helemaal niet!")
  } else {
    cat("\nHmm, okay. Maar dit kan ook een gelukkige gok zijn geweest. Nog een vraag!\n\n")
  }

  ANSWER_check_Stockholm <- utils::menu(
    choices = c("Handstand push-ups", "Jumping jacks", "Touwklimmen"),
    title = "In Stockholm hebben wij samen gesport. Welke oefening hebben wij afsluiter gedaan?"
  )

  if (ANSWER_check_Stockholm != 3) {
    stop("Fout! Jij bent Wouter helemaal niet!")
  } else {
    cat("\nWederom goed. Okay, laatste vraag...\n\n")
  }

  ANSWER_check_bijnaam <- utils::menu(
    choices = c("Stockholmse stoeipoes", "De mooiboy uit Mijdrecht", "Zweedse hottie"),
    title = "Welke bijnaam heeft een vriendin van mij aan jou gegeven?"
  )

  if (ANSWER_check_bijnaam != 3) {
    stop("Fout! Jij bent Wouter helemaal niet!")
  } else {
    cat("\nJaaaaaa!")
  }

  Sys.sleep(2)
  cat("\n\nHeey schatje!\n")

  n_days <- as.numeric(
    difftime(
      as.Date("2023/02/24", format = "%Y/%m/%d"),
      Sys.Date(),
      units = "days"
    )
  )

  cat("\nIk heb even zitten tellen, en wat blijkt, nog maar", n_days, "tot ik je weer zie. Dat vind ik iets heel leuks om naar uit te kijken!\n\nDikke digitale kus!\nxx Jeroen")

  Sys.sleep(15)

  cat("\n\nPs: Wat een R wonder ben je toch :)")

}



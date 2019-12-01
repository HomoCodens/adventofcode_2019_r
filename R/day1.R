day1 <- function(input_path) {
  masses <- as.numeric(readLines(input_path))
  fuel_per_module <- floor(masses/3)-2
  fuel_required <- sum(fuel_per_module)
  message(sprintf("The total fuel requirement is %d!", fuel_required))

  get_fuel_for_fuel <- function(fuel_mass) {
    out <- 0
    while((fuel_mass <- (floor(fuel_mass/3)-2)) > 0) {
      out <- out + fuel_mass
    }
    out
  }

  fuel_for_fuel <- sum(sapply(fuel_per_module, get_fuel_for_fuel))

  message(sprintf("The total fuel required after taking fuel mass into account is %s!", fuel_for_fuel + fuel_required))

}

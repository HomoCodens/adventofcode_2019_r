day1 <- function(input_path) {
  masses <- as.numeric(readLines(input_path))
  fuel_per_module <- floor(masses/3)-2
  fuel_required <- sum(fuel_per_module)
  message(sprintf("The total fuel requirement is %d!", fuel_required))

  get_fuel_for_fuel <- function(fuel_masses) {
    out <- numeric(length(fuel_masses))
    while(any((fuel_masses <- (floor(fuel_masses/3)-2)) > 0)) {
      fuel_masses[fuel_masses < 0] <- 0
      out <- out + fuel_masses
    }
    out
  }

  fuel_for_fuel <- sum(get_fuel_for_fuel(fuel_per_module))

  message(sprintf("The total fuel required after taking fuel mass into account is %s!", fuel_for_fuel + fuel_required))

}

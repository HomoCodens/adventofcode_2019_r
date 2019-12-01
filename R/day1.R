day1 <- function(input_path) {
  masses <- as.numeric(readLines(input_path))
  fuel_per_module <- floor(masses/3)-2
  fuel_required <- sum(fuel_per_module)
  message(sprintf("The total fuel requirement is %d!", fuel_required))

  get_fuel_for_fuel <- function(fuel_masses) {
    out <- numeric(length(fuel_masses))
    residual_fuel <- numeric(length(fuel_masses))

    while(TRUE) {
      fm <- floor(fuel_masses/3) - 2
      residual_fuel[fm <= 0 & residual_fuel == 0] <- fuel_masses[fm <= 0 & residual_fuel == 0]
      fm[fm < 0] <- 0
      fuel_masses <- fm
      out <- out + fm

      if(all(fm <= 0)) {
        break;
      }
    }

    list(
      fuel_required = out,
      residual_mass = residual_fuel
    )
  }

  fuel_for_fuel_per_module <- get_fuel_for_fuel(fuel_per_module)
  fuel_for_fuel <- sum(fuel_for_fuel_per_module$fuel_required)
  fuel_to_wish_for <- sum(fuel_for_fuel_per_module$residual_mass)

  message(sprintf("The total fuel required after taking fuel mass into account is %s!", fuel_for_fuel + fuel_required))
  message(sprintf("You need to wish really hard for another %d mass...", fuel_to_wish_for))
}

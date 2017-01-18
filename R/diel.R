## http://quantitative-ecology.blogspot.ca/2007/10/approximate-sunrise-and-sunset-times.html
sun_calc <- function(d, Lat = 49.49, Long = -117.3) {
  ## d is the day of year Lat is latitude in decimal degrees Long is longitude in
  ## decimal degrees (negative == West)

  ## This method is copied from: Teets, D.A. 2003. Predicting sunrise and sunset
  ## times.  The College Mathematics Journal 34(4):317-321.

  ## At the default location the estimates of sunrise and sunset are within seven
  ## minutes of the correct times (http://aa.usno.navy.mil/data/docs/RS_OneYear.php)
  ## with a mean of 2.4 minutes error.

  ## Function to convert degrees to radians
  rad <- function(x) pi * x/180

  ## Radius of the earth (km)
  R = 6378

  ## Radians between the xy-plane and the ecliptic plane
  epsilon = rad(23.45)

  ## Convert observer's latitude to radians
  L = rad(Lat)

  ## Calculate offset of sunrise based on longitude (min) If Long is negative, then
  ## the mod represents degrees West of a standard time meridian, so timing of
  ## sunrise and sunset should be made later.
  timezone = -4 * (abs(Long)%%15) * sign(Long)

  ## The earth's mean distance from the sun (km)
  r = 149598000

  theta = 2 * pi/365.25 * (d - 80)

  z.s = r * sin(theta) * sin(epsilon)
  r.p = sqrt(r^2 - z.s^2)

  t0 = 1440/(2 * pi) * acos((R - z.s * sin(L))/(r.p * cos(L)))

  ## a kludge adjustment for the radius of the sun
  that = t0 + 5

  ## Adjust 'noon' for the fact that the earth's orbit is not circular:
  n = 720 - 10 * sin(4 * pi * (d - 80)/365.25) + 8 * sin(2 * pi * d/365.25)

  ## now sunrise and sunset are:
  sunrise = (n - that + timezone)/60
  sunset = (n + that + timezone)/60

  return(list(sunrise = sunrise, sunset = sunset))
}

#' @title Diel period
#'
#' @description
#' Calculates whether a date/time is Day or Night based on the
#' latitude and longitude. The default is for the Poisson Consulting Ltd
#' offices.
#'
#' @param x date/time
#' @param Lat a numeric element indicating the latitude.
#' @param Long a numeric element indicating the longitude.
#' @return A factor with the levels Day and Night.
#' @importFrom lubridate yday minute second
#' @export
diel_period <- function(x, Lat = 49.49, Long = -117.3) {

  hours <- hour(x) + minute(x)/60 + second(x)/(60 * 60)

  suntimes <- sun_calc(d = yday(x), Lat = Lat, Long = Long)

  period <- factor(rep("Night", length(hours)), levels = c("Day", "Night"))
  period[hours >= suntimes$sunrise & hours <= suntimes$sunset] <- "Day"

  return(period)
}

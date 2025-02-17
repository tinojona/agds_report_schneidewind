### Getting Started

### Exercises



## Dimensions of a circle

# function
geometry <- function(r){
  area = pi * r^2
  circ = 2 * pi * r

  print(paste0("the area of the circle is: ",
               round(area, digits = 2)))

  print(paste0("the circumference of the circle is: ",
               round(circ, digits=2)))

}

geometry(4)

## sequence of numbers
vec <- seq(0, pi ,length = 5)

## gauss sum
sum(1:100)

## magic trick algorithm
x_init <- 3 # redefine

x <- x_init + 1
x <- x * 2
x <- x + 4
x <- x / 2
x <- x - x_init

print(x)

## vectors
data <- datasets::rivers
class(data); length(data)

mean(data); min(data);max(data); quantile(data, c(.5,.33))

## data frames
data_quakes <- datasets::quakes

dim(data_quakes)

magnitude <- data_quakes$mag
magnitude_max <- max(magnitude)

data_quakes$lat[data_quakes$mag == magnitude_max]; data_quakes$lon[data_quakes$mag == magnitude_max]



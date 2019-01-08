#' # Statistics - Statistics by Power
#' Copyright (C) 2017-2018 Gürol CANBEK  
#' This file is licensed under  
#' 
#'   A p a c h e   L i c e n s e   2 . 0  
#' 
#' A permissive license whose main conditions require preservation of copyright  
#' and license notices. Contributors provide an express grant of patent rights.  
#' Licensed works, modifications, and larger works may be distributed under  
#' different terms and without source code.  
#'  
#' See the license file in <https://github.com/gurol/gcategory>  
#' 
#' @author Gürol Canbek, <gurol44@gmail.com>  
#' @references <http://gurol.canbek.com 
#' @keywords mean, average, arithmetic mean, geometric mean, quadratic mean,
#' harmonic mean, power mean, pythagorean mean, generalized mean, hölder mean,
#' mean of degree, root-mean square, cube-root mean cube, cubic mean,
#' standard deviation, geometric standard deviation, geometric z-score
#' @title PowerStat - Statistics by Power
#' @date 10 March 2018
#' @version 1.2
#' @note version history
#' 1.1 10 March 2018, Power means
#' 1.0 December 2017, The first version
#' @description Varios mean, standard deviation, and z-scores statistics based
#' on the power category. The method is proposed by Gürol Canbek.
#' @note http://mathworld.wolfram.com/PowerMean.html
#' https://www.wikiwand.com/en/Generalized_mean
#' @todo Heronian Mean: http://mathworld.wolfram.com/HeronianMean.html
#' 
#' Power: mean types proposed by Gürol Canbek:
#' Copyright (C) 2017-2018 Gürol Canbek
#
# -1    : Harmonic mean
#  0    : Geometric mean (Pure)
#  0.25 : Geometric mean, Geometric SD, Geometric z-score, Arithmetic categorization
#  0.75 : Geometric mean, Arithmetic SD, Arithmetic z-score, Arithmetic categorization
#  1    : Arithmetic mean (Pure)
#  2    : Mean square
#  3    : Mean cube

#' Give description of the power types
#'
#' This helper function that can be used for other purposes returns the
#' description of the power types proposed by Gürol Canbek.
#'
#' @param power power coefficient (-1: Harmonic; 0: Geometric (pure);
#' 0.25: Geometric mean, Geometric SD, Geometric z-score, Arithmetic
#' categorization; 0.75: Geometric mean, Arithmetic SD, Arithmetic z-score,
#' Arithmetic categorization; 1: Arithmetic mean (Pure); 2: Mean square;
#' 3: Mean cube)
#'
#' @return Power description text
#'
#' @example powerMeanTypes(0)
#' @note Proposed by Gürol Canbek.
powerMeanTypes<-function(power)
{
  if (power < -1) {
    description <- 'Invalid'
  }
  else if (power < 0) {
    description <- 'Harmonic Mean (Not Implemented)'
  }
  else if (power < 1) {
    if (power == 0) {
      description <- 'Pure Geometric Approach'
    }
    if (power == 0.25) {
      description <-
        'Mixed Geometric/Arithmetic Approach '
      '(Geometric Mean/Z-score, Arithmetic Categorization)'
    }
    else if (power == 0.75) {
      description <-
        'Mixed Geometric/Arithmetic Approach '
      '(Geometric Mean, Arithmetic Z-score/Categorization)'
    }
  }
  else if (power == 1) {
    description <- 'Pure Arithmetic Approach'
  }
  else if (power == 2) {
    description <- 'Mean Square (Not Implemented)'
  }
  else if (power == 3) {
    description <- 'Mean Cube (Not Implemented)'
  }
  else {
    description <- 'Invalid'
  }
  
  description <- paste0(description, ' (Power: ', power, ')')
  
  return (description)
}

#' Calculate power statistics
#'
#' This helper function that can be used for other purposes calculates
#' mean and standard deviation of a vector given based on power type
#'
#' @param x a numeric vector
#' @param power power coefficient (-1: Harmonic; 0: Geometric (pure);
#' 0.25: Geometric mean, Geometric SD, Geometric z-score, Arithmetic
#' categorization; 0.75: Geometric mean, Arithmetic SD, Arithmetic z-score,
#' Arithmetic categorization; 1: Arithmetic mean (Pure); 2: Mean square;
#' 3: Mean cube) (default: \code{1} for pure arithmetic mean)
#' @param na.rm NA values are removed before calculations (default: \code{TRUE})
#'
#' @return A list with two single values: $mean and $sd (standard deviation)
#' @export
#'
#' @examples
#' x <- c(1:11)
#' powerStatistics(x, power=0)
#' powerStatistics(x, power=1)
#' @note Proposed by Gürol Canbek.
powerStatistics <- function(x, power=1, na.rm=TRUE)
{
  if (power == -1) {
    # Harmonic mean: 1/mean(1/x)
    power_mean <- 1/mean(1/x, na.rm=na.rm)
    power_sd <- NULL
  }
  else if (power == 0 || power == 0.25) {
    
    if (na.rm)
      x <- na.omit(x)
    
    # Geometric mean: prod(x)^(1/n)
    power_mean <- prod(x)^(1/length(x))
    # Geometric SD: exp(sd(log(x, ...), na.rm = na.rm, ...))
    power_sd <- exp(sd(log(x)))
  }
  else if (power == 0.75) {
    
    if (na.rm)
      x <- na.omit(x)
    
    # Geometric mean: prod(x)^(1/n)
    power_mean <- prod(x)^(1/length(x))
    
    # Arithmetic SD (around geometric mean)
    power_sd <- sqrt(sum((x-power_mean)^2/(length(x)-1)))
  }
  else if (power == 1) {
    # Arithmetic mean: sum(x)/length(x)
    power_mean <- mean(x, na.rm=na.rm)
    # Arithmetic SD:
    power_sd <- sd(x, na.rm=na.rm)
  }
  else if (power == 2) {
    # Mean square: sqrt(mean(x^2))
    power_mean <- sqrt(mean(x^2, na.rm=na.rm))
    power_sd <- NULL
  }
  else if (power == 3) {
    # Mean cube: mean(x^3)^(1/3)
    power_mean <- mean(x^3, na.rm=na.rm)^(1/3)
    power_sd <- NULL
  }
  
  return(list(mean=power_mean, sd=power_sd))
}

#' Calculate Z-scores of an element in a vector
#'
#' This helper function that can be used for other purposes calculates
#' Z-scores of an element in a vector given based on power type
#'
#' @param i the index of the element in a vector
#' @param x a numeric vector
#' @param power power coefficient (-1: Harmonic; 0: Geometric (pure);
#' 0.25: Geometric mean, Geometric SD, Geometric z-score, Arithmetic
#' categorization; 0.75: Geometric mean, Arithmetic SD, Arithmetic z-score,
#' Arithmetic categorization; 1: Arithmetic mean (Pure); 2: Mean square;
#' 3: Mean cube) (default: \code{1} for pure arithmetic mean)
#' @param na.rm NA values are removed before calculations (default: \code{TRUE})
#' @param power_statistics power statistics already calculated (default:
#' \code{NULL} for not calculated)
#' @seealso \code{\link{powerStatistics}}
#'
#' @return Z-score (single value)
#' @export
#'
#' @examples
#' x <- c(1:11)
#' powerZScore(6, x, power=0)
#' powerZScore(6, x, power=1)
#' @note Proposed by Gürol Canbek.
powerZScore <- function(i, x, power=1, na.rm=TRUE, power_statistics=NULL)
{
  z_score <- NA
  
  if (is.na(x[i]))
    return (z_score)
  
  if (TRUE==is.null(power_statistics)) {
    # No power-statistics given. Calculate
    power_statistics <- powerStatistics(x, power=power, na.rm=na.rm)
  }
  
  if (power == -1) {
    # @todo 
    # Harmonic z-score
  }
  else if (power == 0 || power == 0.25) {
    # Geometric z-score
    z_score <- exp((log(x[i]) - log(power_statistics$mean))/
                     log(power_statistics$sd))
  }
  else if (power == 1 || power == 0.75) {
    # Arithmetic z-score
    z_score <- (x[i] - power_statistics$mean)/power_statistics$sd
  }
  else if (power == 2) {
    # @todo
    # Mean square z-score
  }
  else if (power == 3) {
    # @todo
    # Mean cube z-score
  }
  
  return (z_score)
}

#' Calculate Z-scores of all elements in a vector
#'
#' This helper function that can be used for other purposes calculates
#' Z-scores of all elements in a vector given based on power type
#'
#' @param x a numeric vector
#' @param power power coefficient (-1: Harmonic; 0: Geometric (pure);
#' 0.25: Geometric mean, Geometric SD, Geometric z-score, Arithmetic
#' categorization; 0.75: Geometric mean, Arithmetic SD, Arithmetic z-score,
#' Arithmetic categorization; 1: Arithmetic mean (Pure); 2: Mean square;
#' 3: Mean cube) (default: \code{1} for pure arithmetic mean)
#' @param na.rm NA values are removed before calculations (default: \code{TRUE})
#' @seealso \code{\link{powerStatistics}} and \code{\link{powerZScore}} for
#' single element
#'
#' @return Z-scores (vector)
#' @export
#'
#' @examples
#' x <- c(1:11)
#' powerZScores(x, power=0)
#' powerZScores(x, power=1)
#' @note Proposed by Gürol Canbek.
powerZScores <- function(x, power=1, na.rm=TRUE)
{
  # Not to calculate each item
  power_statistics <- powerStatistics(x, power=power, na.rm=na.rm)
  z_scores <- lapply(seq_along(x),
                     function(x, i) powerZScore(
                       i, x, power=power, na.rm=na.rm,
                       power_statistics=power_statistics),
                     x=x)
  
  return (unlist(z_scores))
}

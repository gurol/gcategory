#' # G-Category - Greatness Category for Dataset Profiling
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
#' @keywords dataset, data profiling, big data, big dataset, sample space,
#' feature space, space size, dataset profiling, mean, average,
#' arithmetic mean, geometric mean, quadratic mean, harmonic mean, power mean,
#' pythagorean mean, generalized mean, hölder mean, mean of degree,
#' root-mean square, cube-root mean cube, cubic mean
#' @title G-Category - Dataset Greatness Category
#' @date 2 January 2019
#' @version 1.3
#' @note version history
#' January 2019
#' 1.3 2 January 2019, Improvements, documentation
#' 1.2 15 March 2018, Tabular representation and plots
#' 1.1 10 March 2018, Power means
#' 1.0 December 2017, The first version
#' @description R scripts for dataset profiling via a novel method called
#' G-Category to categorize datasets showing the greatness in both spaces:
#' sample space and feature space. The method is proposed by Gürol Canbek.
#' @note http://mathworld.wolfram.com/PowerMean.html
#' https://www.wikiwand.com/en/Generalized_mean
#' @todo Heronian Mean: http://mathworld.wolfram.com/HeronianMean.html
#' If you get this error:
#' Error in grid.Call.graphics(C_upviewport, as.integer(n)) : 
#'   cannot pop the top-level viewport ('grid' and 'graphics' output mixed?)
#' Use plot.new() between plots

#' libraries
library(grid) # textGrob
library(gridExtra)
library(gtable) # gtable
library(scales)
library(ggplot2)

# Globals for functions

# G-category: indexes
gc_small <- 1
gc_average <- 2
gc_shallow <- 3
gc_skinny <- 4
gc_large <- 5
gc_na <- 6

# G-category: Names
names_gc <- c('small', 'average', 'shallow', 'skinny', 'large', 'NA')
names_gc_TR <- c('küçük', 'vasat', 'sığ', 'sıska', 'geniş', 'U/D')
# G-category: Colors
#             Small      Average    Shallow    Skinny     Large      NA
cols_gc <- c('#FC9ACF', '#FCB09A', '#9ABEFC', '#BC9AFC', '#D1FD9B', 'pink')

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

# Example dataset specifications
# DSs <- paste0(rep('DS', 6), 0:5)

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

#' Calculate greatness category of a single dataset of which feature and sample
#' space size vectors based on a novel method called G-Category.

# The method is proposed by Gürol Canbek. See the reference for citation.
#'
#' @param i the index of the dataset in a dataset vector
#' @param x a numeric feature space sizes vector (n)
#' @param y a numeric sample space sizes vector (m)
#' @param power power coefficient (-1: Harmonic; 0: Geometric (pure);
#' 0.25: Geometric mean, Geometric SD, Geometric z-score, Arithmetic
#' categorization; 0.75: Geometric mean, Arithmetic SD, Arithmetic z-score,
#' Arithmetic categorization; 1: Arithmetic mean (Pure); 2: Mean square;
#' 3: Mean cube) (default: \code{1} for pure arithmetic mean)
#' @param theta threshold value for the categorization'
#' @param na.rm NA values are removed before calculations (default: \code{TRUE})
#' @param power_statistics_x power statistics already calculated for x vector
#' (default: \code{NULL} for not calculated)
#' @param power_statistics_y power statistics already calculated for y vector
#' (default: \code{NULL} for not calculated)
#' @param gcs G-Category names. You can give localized texts (e.g. names_gc_TR)
#' @seealso \code{\link{powerStatistics}} and \code{\link{powerZScore}} for
#' single element
#'
#' @return G-Category (single text value)
#' @export
#'
#' @examples
#' x <- c(1:11)
#' y <- seq(110, 10, -10)
#' greatnessCategory(1, x, y, power=0)
#' greatnessCategory(1, x, y, power=1)
#' @note The method is proposed by Gürol Canbek. See the reference for citation.
greatnessCategory <- function(i, x, y, power=0, theta=1, na.rm=TRUE,
                              power_statistics_x=NULL,
                              power_statistics_y=NULL,
                              gcs=names_gc)
{
  if (is.na(x[i]))
    return ('NA')
  
  if (TRUE == is.null(power_statistics_x)) {
    power_statistics_x <- powerStatistics(x, power=power, na.rm=na.rm)
  }
  z_score_x <- powerZScore(i, x, power=power, na.rm=na.rm,
                           power_statistics=power_statistics_x)
  
  if (TRUE == is.null(power_statistics_y)) {
    power_statistics_y <- powerStatistics(y, power=power, na.rm=na.rm)
  }
  z_score_y <- powerZScore(i, y, power=power, na.rm=na.rm,
                           power_statistics=power_statistics_y)
  
  if (power == 0) {
    # Geometric
    if (is.na(z_score_x) || is.na(z_score_y)) {
      gc_type <- gc_na
    }
    else if (((theta/2 <= z_score_x || z_score_x <= theta) &&
              (theta/2 <= z_score_y && z_score_y <= theta)) ||
             ((z_score_x > theta && z_score_x < 2*theta) &&
              (z_score_y > theta && z_score_y < 2*theta))
    ) {
      gc_type <- gc_average
    }
    else if (z_score_y > theta && z_score_x > theta) {
      gc_type <- gc_large
    }
    else if (z_score_y < theta && z_score_x < theta) {
      gc_type <- gc_small
    }
    else if ((z_score_y / z_score_x) > theta) {
      gc_type <- gc_skinny
    }
    else if ((z_score_x / z_score_y) > theta) {
      gc_type <- gc_shallow
    }
    else {
      stopifnot(FALSE)
      gc_type <- gc_na
    }
  }
  else if (power > 0 && power <= 1) {
    theta_scaled <- theta*log(2)
    if (is.na(z_score_x) || is.na(z_score_y)) {
      gc_type <- gc_na
    }
    else if ((abs(z_score_x) <= theta_scaled || abs(z_score_y) <= theta_scaled) ||
             ((z_score_x > theta_scaled && z_score_x < 2*theta_scaled) &&
              (z_score_y > theta_scaled && z_score_y < 2*theta_scaled))
    ) {
      gc_type <- gc_average
    }
    else if (z_score_x > theta_scaled && z_score_y > theta_scaled) {
      gc_type <- gc_large
    }
    else if (z_score_x < theta_scaled && z_score_y < theta_scaled) {
      gc_type <- gc_small
    }
    else if ((z_score_y - z_score_x) > theta_scaled) {
      gc_type <- gc_skinny
    }
    else if ((z_score_x - z_score_y) > theta_scaled) {
      gc_type <- gc_shallow
    }
    else {
      stopifnot(FALSE)
      gc_type <- gc_na
    }
  }
  else {
    gc_type <- gc_na
  }
  
  return (gcs[gc_type])
}

#' @param x a numeric feature space sizes vector (n)
#' @param y a numeric sample space sizes vector (m)
#' @param power power coefficient (-1: Harmonic; 0: Geometric (pure);
#' 0.25: Geometric mean, Geometric SD, Geometric z-score, Arithmetic
#' categorization; 0.75: Geometric mean, Arithmetic SD, Arithmetic z-score,
#' Arithmetic categorization; 1: Arithmetic mean (Pure); 2: Mean square;
#' 3: Mean cube) (default: \code{1} for pure arithmetic mean)
#' @param theta threshold value for the categorization'
#' @param na.rm NA values are removed before calculations (default: \code{TRUE})
#' @seealso \code{\link{greatnessCategory}} for
#' single element
#'
#' @return G-Categories (text vector)
#' @export
#'
#' @examples
#' nN <- c(84, 94, 83, 99, 118)
#' mN <- c(264303, 254, 310926, 1000, 207865)
#' gcN <- greatnessCategories(nN, mN)
#' gcN
#' nP <- c(90, 81, 69, 75, 73)
#' mP <- c(399353, 280, 4868, 1000, 378)
#' gcP <- greatnessCategories(nP, mP)
#' gcP
#' x <- c(1:11)
#' y <- seq(110, 10, -10)
#' greatnessCategories(x, y, power=0)
#' greatnessCategories(x, y, power=1)
#' @note The method is proposed by Gürol Canbek. See the reference for citation.
greatnessCategories <- function(x, y, power=0, theta=1, na.rm=TRUE)
{
  if (na.rm) {
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  # Not to calculate each item
  power_statistics_x <- powerStatistics(x, power=power, na.rm=na.rm)
  power_statistics_y <- powerStatistics(y, power=power, na.rm=na.rm)
  gcs <- lapply(seq_along(x),
                function(x, i) greatnessCategory(
                  i, x, y, power=power, theta=theta, na.rm=na.rm,
                  power_statistics_x=power_statistics_x,
                  power_statistics_y=power_statistics_y),
                x=x)
  
  return (unlist(gcs))
}

# dumpGCategoriesZScores(nN, mN, round_digit=1)
# dumpGCategoriesZScores(nP, mP, round_digit=1)
dumpGCategoriesZScores<-function(x, y, power=0, theta=1, na.rm=TRUE, round_digit=6)
{
  gcs <- greatnessCategories(x, y, power=power, theta=theta, na.rm=na.rm)
  zsc_x <- powerZScores(x, power=power, na.rm=na.rm)
  zsc_y <- powerZScores(y, power=power, na.rm=na.rm)
  
  return(list(Gc=t(gcs), Zx=t(round(zsc_x, round_digit)), Zy=t(round(zsc_y, round_digit))))
}

plotGCategoriesZScoresSeperated <- function(x, y, DSs, power=0, round_digit=1,
                                            arrange=TRUE, theta=1, na.rm=TRUE,
                                            boldDiagonals=FALSE)
{
  result <- tabulateGreatnessCategories(
    x, y, DSs, power=power, arrange=arrange,
    theta=theta, na.rm=na.rm)
  
  # Extract only G-categories
  # "DS2 (skinny)" -> "skinny"
  # result_gc <- gsub("DS.+ \\(|\\)", "", result)
  result_gc <- gsub(".+ \\(|\\)", "", result)
  # Extract only Datasets
  # "DS2 (skinny)" -> "skinny"
  result_ds <- gsub(" \\(.+\\)", "", result)
  
  cols <- as.table(matrix('white', nrow(result), ncol(result)))
  
  inds <- which(result_gc == names_gc[gc_small], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_small]
  inds <- which(result_gc == names_gc[gc_skinny], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_skinny]
  inds <- which(result_gc == names_gc[gc_shallow], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_shallow]
  inds <- which(result_gc == names_gc[gc_average], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_average]
  inds <- which(result_gc == names_gc[gc_large], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(result), ncol(result)))
  if (boldDiagonals) {
    inds <- which(row(result) == col(result), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core = list(fg_params=list(cex=0.7, fontface=fontfaces),
                bg_params=list(fill=cols)),
    colhead = list(fg_params=list(cex=0.8)),
    rowhead = list(fg_params=list(cex=0.8)))
  
  grobGC <- tableGrob(result, theme=mytheme)
  grobGC <- addTitleToGrob(grobGC, 'G-Categories')
  
  zsc_x <- powerZScores(x, power=power, na.rm=na.rm)
  zsc_y <- powerZScores(y, power=power, na.rm=na.rm)
  
  result_zx <- result_ds
  result_zy <- result_ds
  interpret <- result_ds
  
  for (ds in DSs) {
    zx <- round(zsc_x[which(DSs == ds, arr.ind=TRUE)], round_digit)
    result_zx[which(result_zx == ds, arr.ind=TRUE)] <- zx
    
    zy <- round(zsc_y[which(DSs == ds, arr.ind=TRUE)], round_digit)
    result_zy[which(result_zy == ds, arr.ind=TRUE)] <- zy
    
    if (power == 0) {
      interpret[which(interpret == ds, arr.ind=TRUE)] <- paste(
        round(sqrt(zy/zx), round_digit), '(', round(sqrt(zy*zx), round_digit), ')')
    }
    else if (power > 0 && power <= 1) {
      interpret[which(interpret == ds, arr.ind=TRUE)] <- paste(
        round(sqrt(zy/zx), round_digit), '(', round((zy + zx)/2.0, round_digit), ')')
    }
  }
  
  x_name <- deparse(substitute(x))
  
  grobZx <- tableGrob(result_zx, theme=mytheme)
  grobZx <- addTitleToGrob(grobZx, paste0('Z-score (', x_name, ')'))
  
  y_name <- deparse(substitute(y))
  
  grobZy <- tableGrob(result_zy, theme=mytheme)
  grobZy <- addTitleToGrob(grobZy, paste0('Z-score (', y_name, ')'))
  
  grobZyZxRatio <- tableGrob(interpret, theme=mytheme)
  grobZyZxRatio <- addTitleToGrob(
    grobZyZxRatio,
    paste('Z-score',
          ifelse(power == 0,
                 paste0(y_name, '/', x_name, ' (G.Mean = [', x_name, '*', y_name, ']^.5)'),
                 paste0(y_name, '/', x_name, ' (A.Mean = [', x_name, '+', y_name, ']/2)'))))
  
  grid.arrange(grobGC, grobZy,
               grobZx, grobZyZxRatio,
               ncol=2, nrow=2,
               layout_matrix=rbind(c(1, 2),
                                   c(3, 4)),
               top=paste('G-Categories, Z(n), Z(m), and Z(m)/Z(n) with (geometric/aritmetic means) of Data Sets via',
                         powerMeanTypes(power)))
}

plotGCategoriesZScores <- function(x, y, DSs,
                                   power=0, theta=1, round_digit=1,
                                   arrange=TRUE, na.rm=TRUE,
                                   boldDiagonals=FALSE)
{
  result <- tabulateGreatnessCategories(
    x, y, DSs, power=power, arrange=arrange,
    theta=theta, na.rm=na.rm)
  
  # Extract only G-categories
  # "DS2 (skinny)" -> "skinny"
  # result_gc <- gsub("DS.+ \\(|\\)", "", result)
  result_gc <- gsub(".+ \\(|\\)", "", result)
  # Extract only Datasets
  # "DS2 (skinny)" -> "skinny"
  result_ds <- gsub(" \\(.+\\)", "", result)
  
  cols <- as.table(matrix('white', nrow(result), ncol(result)))
  
  inds <- which(result_gc == names_gc[gc_small], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_small]
  inds <- which(result_gc == names_gc[gc_skinny], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_skinny]
  inds <- which(result_gc == names_gc[gc_shallow], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_shallow]
  inds <- which(result_gc == names_gc[gc_average], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_average]
  inds <- which(result_gc == names_gc[gc_large], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(result), ncol(result)))
  if (boldDiagonals) {
    inds <- which(row(result) == col(result), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  zsc_x <- powerZScores(x, power=power, na.rm=na.rm)
  zsc_y <- powerZScores(y, power=power, na.rm=na.rm)
  
  for (ds in DSs) {
    ind <- which(DSs == ds, arr.ind=TRUE)
    zx <- round(zsc_x[ind], round_digit)
    zy <- round(zsc_y[ind], round_digit)
    interpret <- ''
    
    if (power == 0) {
      interpret <- paste(
        round(zsc_y[ind]/zsc_x[ind], round_digit), '(',
        round(sqrt(zsc_y[ind]*zsc_x[ind]), round_digit), ')')
    }
    else if (power > 0 && power <= 1) {
      interpret <- paste(
        round(zsc_y[ind] - zsc_x[ind], round_digit), '(',
        round((zsc_y[ind] + zsc_x[ind])/2.0, round_digit), ')')
    }
    
    inds <- which(result_ds == ds, arr.ind=TRUE)
    original <- result[inds]
    more_detailed <- paste0(
      original,
      '\n', x[ind], 'x', y[ind],
      ' (', zx, 'x', zy,
      ')\n', interpret
    )
    result[inds] <- more_detailed
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core = list(fg_params=list(cex=0.65, fontface=fontfaces),
                bg_params=list(fill=cols)),
    colhead = list(fg_params=list(cex=0.7)),
    rowhead = list(fg_params=list(cex=0.7)))
  
  grobGC <- tableGrob(result, theme=mytheme)
  
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  grid.arrange(grobGC,
               ncol=1, nrow=1,
               top=paste0('Data Sets\' G-Categories via ',
                          powerMeanTypes(power), '\n',
                          length(DSs), ' datasets, ',
                          'n (', x_name, ') statistics: min=',
                          round(min(zsc_x), 1),
                          ', average=', round(mean(zsc_x), 1),
                          ', max=', round(max(zsc_x), 1),
                          '. m (', y_name, ') statistics: min=',
                          round(min(zsc_y), 1),
                          ', average=', round(mean(zsc_y), 1),
                          ', max=', round(max(zsc_y), 1))
  )
}

# Simulation
# x <- c(1:11)
# y <- seq(110, 10, -10)
# gcs <- greatnesCategoriesCombination(x, y)
# wclip(gcs) # Paste to spreadsheet
# gcs <- greatnesCategoriesCombination(x, y, power=0)
# wclip(gcs) # Paste to spreadsheet
# gcs <- greatnesCategoriesCombination(x, y, power=1)
# wclip(gcs) # Paste to spreadsheet
#
# Benign (N) data sets
# nN <- c(84, 94, 83, 99, NA, 118)
# mN <- c(264303, 254, 310926, 1000, NA, 207865)
# gcs <- greatnesCategoriesCombination(nN, mN)
#
# Malware (P) data sets
# nP <- c(90, 81, 69, 75, 83, 73)
# mP <- c(399353, 280, 4868, 1000, 1260, 378)
# gcs <- greatnesCategoriesCombination(nP, mP)
greatnesCategoriesCombination <- function(x, y, power=0, arrange=TRUE, theta=1, na.rm=TRUE)
{
  if (na.rm == TRUE) {
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  ncol=length(x)
  nrow=length(y)
  xx <- matrix(nrow=nrow, ncol=ncol)
  yy <- matrix(nrow=nrow, ncol=ncol)
  if (arrange) {
    x <- sort(x)
    y <- sort(y, decreasing=TRUE)
  }
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      xx[i, j] <- x[i]
      yy[i, j] <- y[j]
    }
  }
  xx <- as.vector(xx)
  yy <- as.vector(yy)
  gcs <- greatnessCategories(xx, yy, power=power, theta=theta, na.rm=na.rm)
  dim(gcs) <- c(ncol, nrow)
  dimnames(gcs) <- list(x, y)
  
  return(t(gcs))
}

# x <- c(1:11)
# y <- seq(110, 10, -10)
# result <- dumpAllDSCombinationGCs(x, y)
dumpAllDSCombinationGCs <- function(x, y, power=0,
                                    arrange=TRUE, theta=1, na.rm=TRUE)
{
  if (na.rm == TRUE) {
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  
  ncol=length(x)
  nrow=length(y)
  xx <- matrix(nrow=nrow, ncol=ncol)
  yy <- matrix(nrow=nrow, ncol=ncol)
  
  if (arrange) {
    x <- sort(x)
    y <- sort(y, decreasing=TRUE)
  }
  
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      xx[i, j] <- x[i]
      yy[i, j] <- y[j]
    }
  }
  
  xx <- as.vector(xx)
  yy <- as.vector(yy)
  gcs <- greatnessCategories(x=xx, y=yy, power=power, theta=theta, na.rm=na.rm)
  dim(gcs) <- c(ncol, nrow)
  dimnames(gcs) <- list(x, y)
  zsc_x <- powerZScores(x=xx, power=power, na.rm=na.rm)
  dim(zsc_x) <- c(ncol, nrow)
  dimnames(zsc_x) <- list(x, y)
  zsc_y <- powerZScores(x=yy, power=power, na.rm=na.rm)
  dim(zsc_y) <- c(ncol, nrow)
  dimnames(zsc_y) <- list(x, y)
  
  return(list(Gc=t(gcs), Zx=t(zsc_x), Zy=t(zsc_y)))
}

# DSs <- paste0(rep('DS', 6), 0:5)
# plotGCategoriesTable(nN, mN, DSs)
# plot.new()
# plotGCategoriesTable(nP, mP, DSs)
plotGCategoriesTable <- function(x, y, DSs, power=0,
                                 arrange=TRUE, theta=1, na.rm=TRUE,
                                 boldDiagonals=FALSE)
{
  result <- tabulateGreatnessCategories(
    x, y, DSs, power=power, arrange=arrange,
    theta=theta, na.rm=na.rm)
  
  # result_pure <- gsub("DS.+ \\(|\\)", "", result)
  result_pure <- gsub(".+ \\(|\\)", "", result)
  
  cols <- as.table(matrix('white', nrow(result), ncol(result)))
  
  inds <- which(result_pure == names_gc[gc_small], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_small]
  inds <- which(result_pure == names_gc[gc_skinny], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_skinny]
  inds <- which(result_pure == names_gc[gc_shallow], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_shallow]
  inds <- which(result_pure == names_gc[gc_average], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_average]
  inds <- which(result_pure == names_gc[gc_large], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(result), ncol(result)))
  if (boldDiagonals) {
    inds <- which(row(result) == col(result), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core = list(fg_params=list(cex=0.7, fontface=fontfaces),
                bg_params=list(fill=cols)),
    colhead = list(fg_params=list(cex=0.8)),
    rowhead = list(fg_params=list(cex=0.8)))
  
  grobGC <- tableGrob(result, theme=mytheme)
  
  grid.arrange(grobGC,
               ncol=1, nrow=1,
               top=paste('Data Sets\' G-Categories via',
                         powerMeanTypes(power)))
}

# tabulateGreatnessCategories(nN, mN, DSs)
# tabulateGreatnessCategories(nP, mP, DSs)
tabulateGreatnessCategories <- function(
  x, y, DSs, power=0, arrange=TRUE, theta=1, na.rm=TRUE)
{
  NA_DSs <- DSs[is.na(y) == TRUE]
  
  if (na.rm == TRUE) {
    DSs <- DSs[is.na(y) == FALSE]
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  
  GCs <- greatnessCategories(x, y, power=power, theta=theta, na.rm=na.rm)
  
  x_y <- rbind(cbind(paste(x), paste(y), DSs, GCs))
  
  if (arrange) {
    x <- sort(x)
    y <- sort(y, decreasing=TRUE)
  }
  
  ncol=length(x)
  nrow=length(y)
  xy <- matrix('', nrow=nrow, ncol=ncol)
  
  colnames_xy <- paste(x)
  rownames_xy <- paste(y)
  
  colnames(xy) <- colnames_xy
  rownames(xy) <- rownames_xy
  for (i in 1:nrow) {
    col_ds <- match(x_y[i, 1], colnames_xy)
    row_ds <- match(x_y[i, 2], rownames_xy)
    xy[row_ds, col_ds] <- paste0(x_y[i, 3], ' (', x_y[i, 4], ')')
  }
  
  
  # Remove empty rows and columns (for the same n and m values)
  n <- 1
  nlast <- nrow(xy)
  while (n <= nlast) {
    if (all(xy[n, ] == '')) {
      xy <- xy[-n, ]
      nlast <- nrow(xy)
    }
    else {
      n <- n + 1
    }
  }
  
  n <- 1
  nlast <- ncol(xy)
  while (n <= nlast) {
    if (all(xy[, n] == '')) {
      xy <- xy[, -n]
      nlast <- ncol(xy)
    }
    else {
      n <- n + 1
    }
  }
  
  return (xy)
}

plotCombinationAll <- function(x, y, power=0, round_digit=1,
                               arrange=TRUE, theta=1, na.rm=TRUE,
                               boldDiagonals=FALSE)
{
  result <- dumpAllDSCombinationGCs(x, y, power=power, arrange=arrange,
                                    theta=theta, na.rm=na.rm)
  
  cols <- as.table(matrix('white', nrow(result$Gc), ncol(result$Gc)))
  
  inds <- which(result$Gc == names_gc[gc_small], arr.ind=T)
  cols[inds] <- cols_gc[gc_small]
  inds <- which(result$Gc == names_gc[gc_skinny], arr.ind=T)
  cols[inds] <- cols_gc[gc_skinny]
  inds <- which(result$Gc == names_gc[gc_shallow], arr.ind=T)
  cols[inds] <- cols_gc[gc_shallow]
  inds <- which(result$Gc == names_gc[gc_average], arr.ind=T)
  cols[inds] <- cols_gc[gc_average]
  inds <- which(result$Gc == names_gc[gc_large], arr.ind=T)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(result$Gc), ncol(result$Gc)))
  
  if (boldDiagonals) {
    inds <- which(row(result$Gc) == col(result$Gc), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core = list(fg_params=list(cex=0.6, fontface=fontfaces),
                bg_params=list(fill=cols)),
    colhead = list(fg_params=list(cex=0.7)),
    rowhead = list(fg_params=list(cex=0.7)))
  
  grobGC <- tableGrob(result$Gc, theme=mytheme)
  grobGC <- addTitleToGrob(grobGC, 'G-Categories')
  
  x_name <- deparse(substitute(x))
  
  grobZx <- tableGrob(round(result$Zx, round_digit), theme=mytheme)
  grobZx <- addTitleToGrob(grobZx, paste0('Z-score (', x_name, ')'))
  
  y_name <- deparse(substitute(y))
  
  grobZy <- tableGrob(round(result$Zy, round_digit), theme=mytheme)
  grobZy <- addTitleToGrob(grobZy, paste0('Z-score (', y_name, ')'))
  
  if (power == 0) {
    interpret <- matrix(paste0(round(result$Zy/result$Zx, round_digit), ' (', round(sqrt(result$Zy*result$Zx), round_digit), ')'), nrow=nrow(result$Zy), dimnames=dimnames(result$Zy))
    # interpret <- round(result$Zy/result$Zx, round_digit)
  }
  else if (power > 0 && power <= 1) {
    interpret <- matrix(paste0(round(result$Zy/result$Zx, round_digit), ' (', round((result$Zy + result$Zx)/2.0, round_digit), ')'), nrow=nrow(result$Zy), dimnames=dimnames(result$Zy))
    # interpret <- round(result$Zy/result$Zx, round_digit)
  }
  
  grobZyZxRatio <- tableGrob(interpret, theme=mytheme)
  grobZyZxRatio <- addTitleToGrob(
    grobZyZxRatio,
    paste('Z-score',
          ifelse(power == 0,
                 paste0(y_name, '/', x_name, ' (G.Mean = [', x_name, '*', y_name, ']^.5)'),
                 paste0(y_name, '/', x_name, ' (A.Mean = [', x_name, '+', y_name, ']/2)'))))
  
  grid.arrange(grobGC, grobZy,
               grobZx, grobZyZxRatio,
               ncol=2, nrow=2,
               layout_matrix=rbind(c(1, 2),
                                   c(3, 4)),
               top=paste('G-Categories of Data Sets with n (', length(x),
                         ') x ', 'm (', length(y), ') Combinations via',
                         powerMeanTypes(power)))
}

addTitleToGrob<-function(grob, title, fontsize=12)
{
  padding <- unit(1, 'line')
  titleGrob <- textGrob(title, gp=gpar(fontsize=fontsize))
  
  grob <- gtable_add_rows(grob, heights=grobHeight(titleGrob) + padding, pos=0)
  grob <- gtable_add_grob(grob, titleGrob, t=1, l=1, r=ncol(grob))
  grob$layout$clip <- 'off'
  
  return (grob)
}


# DSsP <- paste0(rep('DS', 6), 0:5)
# plotGCategories(nP, mP, gcP, DSsP)
# plot.new()
# DSsN <- paste0(rep('DS', 6), 0:5)[-5]
# plotGCategories(nN, mN, gcN, DSsN)
# sample size (m) logaritmic scale
# plotGCategories(nP, mP, gcP, DSsP, trans='log10')
# plot.new()
# plotGCategories(nN, mN, gcN, DSsN, trans='log10')
plotGCategories<-function(x, y, GCs, DSs,
                          na.rm=TRUE, draw=TRUE, trans='identity')
{
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  if (na.rm == TRUE) {
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  
  ds_dist <- cbind.data.frame(x, y, GCs, DSs)
  
  mark_shapes <- c(15, charToRaw('|'), charToRaw('-'), 15, 15, 8)
  names(mark_shapes) <- names_gc
  mark_colors <- cols_gc
  names(mark_colors) <- names_gc
  mark_sizes <- c(3, 9, 9, 6, 9, 9)
  names(mark_sizes) <- names_gc
  
  p <- ggplot(ds_dist, aes(x=x, y=y)) +
    scale_shape_manual(values=mark_shapes) +
    scale_color_manual(values=mark_colors) +
    scale_size_manual(values=mark_sizes) +
    theme_bw() +
    labs(x=x_name, y=y_name) +
    geom_point(aes(shape=GCs, size=GCs, color=GCs)) +
    geom_text(aes(label=DSs, hjust=0.5, vjust=-2), size=4) +
    scale_x_continuous(labels=comma) +
    scale_y_continuous(labels=comma, trans=trans) +
    geom_smooth(span=1, method='loess',
                linetype='dashed', color='gray', fill='gray90')
  
  if (draw)
    p
  else
    return(p)
}

loadTestData <- function() {
  DSs <<- paste0(rep('DS', 5), 0:4)
  nN <<- c(84, 94, 83, 99, 118)
  mN <<- c(264303, 254, 310926, 1000, 207865)
  nP <<- c(90, 81, 69, 75, 73)
  mP <<- c(399353, 280, 4868, 1000, 378)
  
  DSxy <<- paste0(rep('DS', 11), 0:10)
  x1 <<- c(1, 2:12, 13)
  y1 <<- c(10, seq(120, 20, -10), 130)
  xr1 <<- floor(runif(11, 1, 11))
  yr1 <<- floor(runif(11, 10, 110))
  
  # Sample space size (m):  1,260  5,555  23,743  1,929  4,725  2,421
  # Feature space size (n):    65     94     105     78    111  85
  
  DSMs <<- c('AMGP', 'Drebin', 'AMD', 'ABot', 'VT2018', 'UpDroid')
  nMalware <<- c(65, 94, 105, 78, 111, 85)
  mMalware <<- c(1260, 5555, 23743, 1929, 4725, 2421)
}

testPlot <- function(power_1=1, power_2=0) {
  # x1 and y1
  dss <- paste0(rep('DS', length(x1)), 1:length(x1))
  plotGCategoriesZScores(x1, y1, dss, power=power_1)
  invisible(readline(prompt=paste('x1 vs. y1 (Arithmetic)',
                                  'Press [enter] to continue')))
  plotGCategoriesZScores(x1, y1, dss, power=power_2)
  invisible(readline(prompt=paste('x1 vs. y1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # xr1 and yr1
  dss <- paste0(rep('DS', length(xr1)), 1:length(xr1))
  plotGCategoriesZScores(xr1, yr1, dss, power=power_1)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Arithmetic)',
                                  'Press [enter] to continue')))
  plotGCategoriesZScores(xr1, yr1, dss, power=power_2)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # nN and mN
  dss <- paste0(rep('DS', length(nN)), 1:length(nN))
  plotGCategoriesZScores(nN, mN, dss, power=power_1)
  invisible(readline(prompt=paste('nN vs. mN (Arithmetic)',
                                  'Press [enter] to continue')))
  plotGCategoriesZScores(nN, mN, dss, power=power_2)
  invisible(readline(prompt=paste('nN vs. mN (Geometric)',
                                  'Press [enter] to continue')))
  
  # nP and mP
  dss <- paste0(rep('DS', length(nP)), 1:length(nP))
  plotGCategoriesZScores(nP, mP, dss, power=power_1)
  invisible(readline(prompt=paste('nP vs. mP (Arithmetic)',
                                  'Press [enter] to continue')))
  plotGCategoriesZScores(nP, mP, dss, power=power_2)
  invisible(readline(prompt=paste('nP vs. mP (Geometric)',
                                  'Press [enter] to continue')))
  
  # nMalware and mMalware
  plotGCategoriesZScores(nMalware, mMalware, DSMs, power=power_1)
  invisible(readline(prompt=paste('nP vs. mP (Arithmetic)',
                                  'Press [enter] to continue')))
  plotGCategoriesZScores(nMalware, mMalware, DSMs, power=power_2)
  invisible(readline(prompt=paste('nP vs. mP (Geometric)',
                                  'Press [enter] to continue')))
}

testDump <- function(power_1=1, power_2=0) {
  # x1 and y1
  dss <- paste0(rep('DS', length(x1)), 1:length(x1))
  result <- dumpGCategoriesZScores(x1, y1, power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('x1 vs. y1 (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- dumpGCategoriesZScores(x1, y1, power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('x1 vs. y1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # xr1 and yr1
  dss <- paste0(rep('DS', length(xr1)), 1:length(xr1))
  result <- dumpGCategoriesZScores(xr1, yr1, power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- dumpGCategoriesZScores(xr1, yr1, power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # nN and mN
  dss <- paste0(rep('DS', length(nN)), 1:length(nN))
  result <- dumpGCategoriesZScores(nN, mN, power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('nN vs. mN (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- dumpGCategoriesZScores(nN, mN, power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('nN vs. mN (Geometric)',
                                  'Press [enter] to continue')))
  
  # nP and mP
  dss <- paste0(rep('DS', length(nP)), 1:length(nP))
  result <- dumpGCategoriesZScores(nP, mP, power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('nP vs. mP (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- dumpGCategoriesZScores(nP, mP, power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('nP vs. mP (Geometric)',
                                  'Press [enter] to continue')))
}
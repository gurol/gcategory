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
#' @references <http://gurol.canbek.com>  
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

source('powerstat.R')

# Globals for functions

# G-category: indexes
gc_small <- 1
gc_medium <- 2
gc_shallow <- 3
gc_skinny <- 4
gc_large <- 5
gc_na <- 6

# G-Category: Names
gc.names <- c(
  'Small', 'Medium', 'Shallow', 'Skinny', 'Large', 'NA')
gc.names.TR <- c(
  'Küçük', 'Orta', 'Sığ', 'Sıska', 'Geniş', 'U/D')
# G-Category: Colors
#  Small      Medium     Shallow    Skinny     Large      NA
gc.cols <- c(
  '#FC9ACF', '#FCB09A', '#9ABEFC', '#BC9AFC', '#D1FD9B', 'pink')
gc.cols.bw <- c(
  '#7F7F7F', '#A6A6A6', '#BFBFBF', '#D9D9D9', '#F2F2F2', 'white')

# See plotGraphGCs for mark shapes for G-Categories

#' Power: mean types proposed by Gürol Canbek:
#' Copyright (C) 2017-2018 Gürol Canbek

#' Calculate greatness category of a single dataset of which feature and sample
#' space size vectors based on a novel method called G-Category.

## Greatness Category ##########################################################

# The method is proposed by Gürol Canbek. See the reference for citation.
#'
#' @param i the index of the dataset in a dataset vector
#' @param x a numeric feature space sizes vector (m)
#' @param y a numeric sample space sizes vector (n)
#' @param power power coefficient (-1: Harmonic; 0: Geometric (pure);
#' 0.25: Geometric mean, Geometric SD, Geometric z-score, Arithmetic
#' categorization; 0.75: Geometric mean, Arithmetic SD, Arithmetic z-score,
#' Arithmetic categorization; 1: Arithmetic mean (Pure); 2: Mean square;
#' 3: Mean cube) (default: \code{1} for pure arithmetic mean)
#' @param theta threshold value for the categorization'
#' @param na.rm NA values are removed before calculations (default: \code{TRUE})
#' @param power_stat_x power statistics already calculated for x vector
#' (default: \code{NULL} for not calculated)
#' @param power_stat_y power statistics already calculated for y vector
#' (default: \code{NULL} for not calculated)
#' @param names_gc G-Category names. You can give localized texts
#' (e.g. names_gc_TR)
#' @seealso \code{\link{powerStatistics}} and \code{\link{powerZScore}} for
#' single element
#'
#' @return G-Category (single text value)
#' @export
#'
#' @examples
#' x1 <- c(1:11)
#' y1 <- seq(110, 10, -10)
#' greatnessCategory(1, x1, y1, power=0)
#' greatnessCategory(1, x1, y1, power=1)
#' @note The method is proposed by Gürol Canbek. See the reference for citation.
greatnessCategory <- function(i, x, y, power=0, theta=1, na.rm=TRUE,
                              power_stat_x=NULL,
                              power_stat_y=NULL,
                              names_gc=gc.names)
{
  if (is.na(x[i]))
    return ('NA')
  
  if (TRUE == is.null(power_stat_x)) {
    power_stat_x <- powerStatistics(x, power=power, na.rm=na.rm)
  }
  zx <- powerZScore(i, x, power=power, na.rm=na.rm,
                    power_statistics=power_stat_x)
  
  if (TRUE == is.null(power_stat_y)) {
    power_stat_y <- powerStatistics(y, power=power, na.rm=na.rm)
  }
  zy <- powerZScore(i, y, power=power, na.rm=na.rm,
                    power_statistics=power_stat_y)
  
  if (power == 0) {
    # Geometric
    if (is.na(zx) || is.na(zy)) {
      gc_type <- gc_na
    }
    else if (((theta/2 <= zx || zx <= theta) &&
              (theta/2 <= zy && zy <= theta)) ||
             ((zx > theta && zx < 2*theta) &&
              (zy > theta && zy < 2*theta))
    ) {
      gc_type <- gc_medium
    }
    else if (zy > theta && zx > theta) {
      gc_type <- gc_large
    }
    else if (zy < theta && zx < theta) {
      gc_type <- gc_small
    }
    else if ((zy / zx) > theta) {
      gc_type <- gc_skinny
    }
    else if ((zx / zy) > theta) {
      gc_type <- gc_shallow
    }
    else {
      stopifnot(FALSE)
      gc_type <- gc_na
    }
  }
  else if (power > 0 && power <= 1) {
    theta_scaled <- theta*log(2)
    if (is.na(zx) || is.na(zy)) {
      gc_type <- gc_na
    }
    else if (
      (abs(zx) <= theta_scaled || abs(zy) <= theta_scaled) ||
      ((zx > theta_scaled && zx < 2*theta_scaled) &&
       (zy > theta_scaled && zy < 2*theta_scaled))
    ) {
      gc_type <- gc_medium
    }
    else if (zx > theta_scaled && zy > theta_scaled) {
      gc_type <- gc_large
    }
    else if (zx < theta_scaled && zy < theta_scaled) {
      gc_type <- gc_small
    }
    else if ((zy - zx) > theta_scaled) {
      gc_type <- gc_skinny
    }
    else if ((zx - zy) > theta_scaled) {
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
  
  return (names_gc[gc_type])
}

#' @param x a numeric feature space sizes vector (m)
#' @param y a numeric sample space sizes vector (n)
#' @param power power coefficient (-1: Harmonic; 0: Geometric (pure);  
#' 0.25: Geometric mean, Geometric SD, Geometric z-score, Arithmetic  
#' categorization; 0.75: Geometric mean, Arithmetic SD, Arithmetic z-score,  
#' Arithmetic categorization; 1: Arithmetic mean (Pure); 2: Mean square;  
#' 3: Mean cube) (default: \code{1} for pure arithmetic mean)  
#' @param theta threshold value for the categorization'  
#' @param na.rm NA values are removed before calculations (default: \code{TRUE})
#' @seealso \code{\link{greatnessCategory}} for a single element
#'
#' @return G-Categories (text vector)
#' @export
#'
#' @examples
#' mN <- c(84, 94, 83, 99, 118)
#' nN <- c(264303, 254, 310926, 1000, 207865)
#' gcN <- greatnessCategories(mN, nN)
#' gcN
#' mP <- c(90, 81, 69, 75, 73)
#' nP <- c(399353, 280, 4868, 1000, 378)
#' gcP <- greatnessCategories(mP, nP)
#' gcP
#' x1 <- c(1:11)
#' y1 <- seq(110, 10, -10)
#' greatnessCategories(x1, y1, power=0)
#' greatnessCategories(x1, y1, power=1)
#' @note The method is proposed by Gürol Canbek. See the reference for citation.
greatnessCategories <- function(x, y, power=0, theta=1, na.rm=TRUE,
                                names_gc=gc.names)
{
  if (na.rm) {
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  # Not to calculate each item
  power_stat_x <- powerStatistics(x, power=power, na.rm=na.rm)
  power_stat_y <- powerStatistics(y, power=power, na.rm=na.rm)
  GCs <- lapply(seq_along(x),
                function(x, i) greatnessCategory(
                  i, x, y, power=power, theta=theta, na.rm=na.rm,
                  power_stat_x=power_stat_x,
                  power_stat_y=power_stat_y,
                  names_gc=names_gc),
                x=x)
  
  return (unlist(GCs))
}

# dumpGCsWithZ(mN, nN, round_digit=1)
# dumpGCsWithZ(mP, nP, round_digit=1)
dumpGCsWithZ<-function(x, y, power=0, theta=1,
                       na.rm=TRUE, round_digit=7, names_gc=gc.names)
{
  GCs <- greatnessCategories(x, y, power=power, theta=theta, na.rm=na.rm,
                             names_gc=names_gc)
  zx <- powerZScores(x, power=power, na.rm=na.rm)
  zy <- powerZScores(y, power=power, na.rm=na.rm)
  
  pmean_zxy <- numeric()
  
  if (power == 0) {
    pmean_zxy <- round(sqrt(zx*zy), round_digit)
  }
  else {
    pmean_zxy <- round((zx+zy)/2, round_digit)
  }
  
  result <- list(
    GCs=GCs,
    zx=round(zx, round_digit),
    zy=round(zy, round_digit),
    pmean_zxy=pmean_zxy
  )
  
  return (result)
}

# tabulateGCs(nN, mN, DSs)
# tabulateGCs(nP, mP, DSs)
tabulateGCs <- function(
  x, y, DSs, power=0, theta=1, include_power_mean=FALSE,
  arrange=TRUE, na.rm=TRUE, round_digit=1, names_gc=gc.names)
{
  NA_DSs <- DSs[is.na(y) == TRUE]
  
  if (na.rm == TRUE) {
    DSs <- DSs[is.na(y) == FALSE]
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  
  result <- dumpGCsWithZ(x, y, power=power, theta=theta,
                         na.rm=na.rm, round_digit=round_digit,
                         names_gc=names_gc)
  
  #                  1         2         3
  x_y <- rbind(cbind(paste(x), paste(y), DSs,
                     #                  4           5
                     result$GCs, paste(result$pmean_zxy)))
  
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
    xy[row_ds, col_ds] <- paste0(
      x_y[i, 3], ' (', x_y[i, 4],
      ifelse(include_power_mean,
             paste0(', ', x_y[i, 5]), ''), ')')
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
  
  m <- 1
  mlast <- ncol(xy)
  while (m <= mlast) {
    if (all(xy[, m] == '')) {
      xy <- xy[, -m]
      mlast <- ncol(xy)
    }
    else {
      m <- m + 1
    }
  }
  
  return (xy)
}

# DSs <- paste0(rep('DS', 6), 0:5)
# plotTableGCs(nN, mN, DSs)
# plot.new()
# plotTableGCs(nP, mP, DSs)
# plotTableGCs(nP, mP, DSs, cols_gc=gc.cols.bw)
plotTableGCs <- function(x, y, DSs, power=0, include_power_mean=FALSE,
                         arrange=TRUE, theta=1, na.rm=TRUE,
                         boldDiagonals=FALSE,
                         names_gc=gc.names, cols_gc=gc.cols)
{
  # To extract the G-Category from DSX (G-Category, zxy)
  # Example: DS2 (Skinny, 1.1)
  # Do not include power mean zx and zy
  # Example: DS2 (Skinny)
  result <- tabulateGCs(
    x, y, DSs, power=power, theta=theta,
    include_power_mean=FALSE,
    arrange=arrange, na.rm=na.rm, names_gc=names_gc)
  
  result_pure <- gsub(".+ \\(|\\)", "", result)
  
  cols <- as.table(matrix('white', nrow(result), ncol(result)))
  
  inds <- which(result_pure == names_gc[gc_small], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_small]
  inds <- which(result_pure == names_gc[gc_skinny], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_skinny]
  inds <- which(result_pure == names_gc[gc_shallow], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_shallow]
  inds <- which(result_pure == names_gc[gc_medium], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_medium]
  inds <- which(result_pure == names_gc[gc_large], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(result), ncol(result)))
  if (boldDiagonals) {
    inds <- which(row(result) == col(result), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core=list(fg_params=list(cex=0.7, fontface=fontfaces),
              bg_params=list(fill=cols)),
    colhead=list(fg_params=list(cex=0.8)),
    rowhead=list(fg_params=list(cex=0.8)))
  
  if (include_power_mean) {
    # Nov use the required tabulate style (include_power_mean)
    result <- tabulateGCs(
      x, y, DSs, power=power, theta=theta,
      include_power_mean=include_power_mean,
      arrange=arrange, na.rm=na.rm, names_gc=names_gc)
  }
  
  grobGCs <- tableGrob(result, theme=mytheme)
  
  grid.arrange(grobGCs,
               ncol=1, nrow=1,
               top=paste('Data Sets\' G-Categories via',
                         powerMeanTypes(power)))
}

plotTableGCsDetailed <- function(x, y, DSs, info,
                                 power=0, theta=1, round_digit=1,
                                 arrange=TRUE, na.rm=TRUE,
                                 boldDiagonals=FALSE,
                                 names_gc=gc.names, cols_gc=gc.cols)
{
  result <- tabulateGCs(
    x, y, DSs, power=power, theta=theta,
    arrange=arrange, na.rm=na.rm, names_gc=names_gc)
  
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
  inds <- which(result_gc == names_gc[gc_medium], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_medium]
  inds <- which(result_gc == names_gc[gc_large], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(result), ncol(result)))
  if (boldDiagonals) {
    inds <- which(row(result) == col(result), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  zx <- powerZScores(x, power=power, na.rm=na.rm)
  zy <- powerZScores(y, power=power, na.rm=na.rm)
  
  for (ds in DSs) {
    ind <- which(DSs == ds, arr.ind=TRUE)
    interpret <- ''
    
    if (power == 0) {
      interpret <- paste0(
        '√(zm·zn)=', round(sqrt(zy[ind]*zx[ind]), round_digit),
        ' (zn/zm=', round(zy[ind]/zx[ind], round_digit), ')')
    }
    else if (power > 0 && power <= 1) {
      interpret <- paste0(
        '(zm+zn)/2=', round((zy[ind] + zx[ind])/2.0, round_digit),
        ' (zn-zm=', round(zy[ind] - zx[ind], round_digit), ')')
    }
    
    inds <- which(result_ds == ds, arr.ind=TRUE)
    original <- result[inds]
    more_detailed <- paste0(
      original,
      '\nm·n=', x[ind], '·', y[ind],
      ' zm·zn=', round(zx[ind], round_digit),
      '·', round(zy[ind], round_digit),
      '\n', interpret
    )
    result[inds] <- more_detailed
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core=list(fg_params=list(cex=0.65, fontface=fontfaces),
              bg_params=list(fill=cols)),
    colhead=list(fg_params=list(cex=0.7)),
    rowhead=list(fg_params=list(cex=0.7)))
  
  grobGC <- tableGrob(result, theme=mytheme)
  
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  grid.arrange(grobGC,
               ncol=1, nrow=1,
               top=paste0('Data Sets\' G-Categories via ',
                          powerMeanTypes(power), ': ',
                          info),
               bottom=paste0(length(DSs), ' datasets, ',
                             'm (', x_name, ') statistics: min=',
                             round(min(zx), 1),
                             ', average=', round(mean(zx), 1),
                             ', max=', round(max(zx), 1),
                             '. n (', y_name, ') statistics: min=',
                             round(min(zy), 1),
                             ', average=', round(mean(zy), 1),
                             ', max=', round(max(zy), 1))
  )
  
  return (result)
}

plotTablesGCsWithZ2x2 <- function(x, y, DSs, power=0, round_digit=1,
                                  arrange=TRUE, theta=1, na.rm=TRUE,
                                  boldDiagonals=FALSE,
                                  names_gc=gc.names, cols_gc=gc.cols)
{
  result <- tabulateGCs(
    x, y, DSs, power=power, theta=theta,
    arrange=arrange, na.rm=na.rm, names_gc=names_gc)
  
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
  inds <- which(result_gc == names_gc[gc_medium], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_medium]
  inds <- which(result_gc == names_gc[gc_large], arr.ind=TRUE)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(result), ncol(result)))
  if (boldDiagonals) {
    inds <- which(row(result) == col(result), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core=list(fg_params=list(cex=0.7, fontface=fontfaces),
              bg_params=list(fill=cols)),
    colhead=list(fg_params=list(cex=0.8)),
    rowhead=list(fg_params=list(cex=0.8)))
  
  grobGC <- tableGrob(result, theme=mytheme)
  grobGC <- addTitleToGrob(grobGC, 'G-Categories')
  
  zx <- powerZScores(x, power=power, na.rm=na.rm)
  zy <- powerZScores(y, power=power, na.rm=na.rm)
  
  result_zx <- result_ds
  result_zy <- result_ds
  interpret <- result_ds
  
  for (ds in DSs) {
    zx_value <- round(zx[which(DSs == ds, arr.ind=TRUE)], round_digit)
    result_zx[which(result_zx == ds, arr.ind=TRUE)] <- zx_value
    
    zy_value <- round(zy[which(DSs == ds, arr.ind=TRUE)], round_digit)
    result_zy[which(result_zy == ds, arr.ind=TRUE)] <- zy_value
    
    if (power == 0) {
      interpret[which(interpret == ds, arr.ind=TRUE)] <- paste(
        round(sqrt(zy_value/zx_value), round_digit),
        '(', round(sqrt(zy_value*zx_value), round_digit), ')')
    }
    else if (power > 0 && power <= 1) {
      interpret[which(interpret == ds, arr.ind=TRUE)] <- paste(
        round(sqrt(zy_value/zx_value), round_digit),
        '(', round((zy_value + zx_value)/2.0, round_digit), ')')
    }
  }
  
  x_name <- deparse(substitute(x))
  
  grobZx <- tableGrob(result_zx, theme=mytheme)
  grobZx <- addTitleToGrob(grobZx, paste0('z-score (', x_name, ')'))
  
  y_name <- deparse(substitute(y))
  
  grobZy <- tableGrob(result_zy, theme=mytheme)
  grobZy <- addTitleToGrob(grobZy, paste0('z-score (', y_name, ')'))
  
  grobZyZxRatio <- tableGrob(interpret, theme=mytheme)
  grobZyZxRatio <- addTitleToGrob(
    grobZyZxRatio,
    paste('z-score',
          ifelse(power == 0,
                 paste0(y_name, '/', x_name,
                        ' (G.Mean = [', x_name, '*', y_name, ']^.5)'),
                 paste0(y_name, '/', x_name,
                        ' (A.Mean = [', x_name, '+', y_name, ']/2)'))))
  
  grid.arrange(
    grobGC, grobZy, grobZx, grobZyZxRatio, ncol=2, nrow=2,
    layout_matrix=rbind(c(1, 2),
                        c(3, 4)),
    top=paste0('G-Categories, zm, zn, and zn/zm ',
               'with (geometric/aritmetic means) of Data Sets via',
               powerMeanTypes(power)))
}

# DSsP <- paste0(rep('DS', 6), 0:5)
# plotGraphGCs(mP, nP, gcP, DSsP)
# plot.new()
# DSsN <- paste0(rep('DS', 6), 0:5)[-5]
# plotGraphGCs(mN, nN, gcN, DSsN)
# sample size (m) logaritmic scale
# plotGraphGCs(mP, nP, gcP, DSsP, trans='log10')
# plot.new()
# plotGraphGCs(mN, nN, gcN, DSsN, trans='log10')
plotGraphGCs<-function(x, y, GCs, DSs,
                       na.rm=TRUE, draw=TRUE, trans='identity',
                       subtitle=NULL,
                       names_gc=gc.names, cols_gc=gc.cols)
{
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  if (na.rm == TRUE) {
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  
  ds_dist <- cbind.data.frame(x, y, GCs, DSs)
  
  # 1:Small, 2:Medium, 3:Shallow, 4:Skinny, 5:Large, 6:NA
  #                1   2   3               4               5   6
  mark_shapes <- c(15, 15, charToRaw('-'), charToRaw('|'), 15, 8)
  names(mark_shapes) <- names_gc
  mark_colors <- cols_gc
  names(mark_colors) <- names_gc
  #               1  2  3  4  5  6
  mark_sizes <- c(3, 6, 17, 9, 9, 9)
  names(mark_sizes) <- names_gc
  
  p <- ggplot(ds_dist, aes(x=x, y=y)) +
    scale_shape_manual(values=mark_shapes) +
    scale_color_manual(values=mark_colors) +
    scale_size_manual(values=mark_sizes) +
    theme_bw() +
    labs(x=x_name, y=y_name, subtitle=subtitle) +
    geom_point(aes(shape=GCs, size=GCs, color=GCs)) +
    geom_text(aes(label=DSs, hjust=0.5, vjust=-2), size=4) +
    scale_x_continuous(labels=comma) +
    scale_y_continuous(labels=comma, trans=trans) +
    geom_smooth(span=1, method='loess',
                linetype='dashed', color='gray', fill='gray96')
  
  if (draw)
    p
  else
    return(p)
}

## G-Category for Two Space Combinations #######################################

# Simulation
# x1 <- c(1:11)
# y1 <- seq(110, 10, -10)
# GCs <- getGCsOfSpaceSizeCombs(x1, y1)
# wclip(GCs) # Paste to spreadsheet
# GCs <- getGCsOfSpaceSizeCombs(x1, y1, power=0)
# wclip(GCs) # Paste to spreadsheet
# GCs <- getGCsOfSpaceSizeCombs(x1, y1, power=1)
# wclip(GCs) # Paste to spreadsheet
#
# Benign (N) data sets
# nN <- c(84, 94, 83, 99, NA, 118)
# mN <- c(264303, 254, 310926, 1000, NA, 207865)
# GCs <- getGCsOfSpaceSizeCombs(nN, mN)
#
# Malware (P) data sets
# nP <- c(90, 81, 69, 75, 83, 73)
# mP <- c(399353, 280, 4868, 1000, 1260, 378)
# GCs <- getGCsOfSpaceSizeCombs(nP, mP)
getGCsOfSpaceSizeCombs <- function(x, y, power=0, arrange=TRUE, theta=1,
                                   na.rm=TRUE, names_gc=gc.names)
{
  if (na.rm == TRUE) {
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  
  ncol <- length(x)
  nrow <- length(y)
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
  GCs <- greatnessCategories(xx, yy, power=power, theta=theta, na.rm=na.rm,
                             names_gc=names_gc)
  
  dim(GCs) <- c(ncol, nrow)
  dimnames(GCs) <- list(x, y)
  
  return(t(GCs))
}

# x1 <- c(1:11)
# y1 <- seq(110, 10, -10)
# result <- dumpGCsOfSpaceSizeCombs(x1, y1)
dumpGCsOfSpaceSizeCombs <- function(x, y, power=0, theta=1,
                                    arrange=TRUE, na.rm=TRUE,
                                    names_gc=gc.names)
{
  if (na.rm == TRUE) {
    x <- x[is.na(x) == FALSE]
    y <- y[is.na(y) == FALSE]
  }
  
  ncol <- length(x)
  nrow <- length(y)
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
  GCs <- greatnessCategories(x=xx, y=yy, power=power, theta=theta, na.rm=na.rm,
                             names_gc=names_gc)
  
  dim(GCs) <- c(ncol, nrow)
  dimnames(GCs) <- list(x, y)
  
  zx <- powerZScores(x=xx, power=power, na.rm=na.rm)
  
  dim(zx) <- c(ncol, nrow)
  dimnames(zx) <- list(x, y)
  
  zy <- powerZScores(x=yy, power=power, na.rm=na.rm)
  
  dim(zy) <- c(ncol, nrow)
  dimnames(zy) <- list(x, y)
  
  pmean_zxy <- numeric()
  
  if (power == 0) {
    pmean_zxy <- sqrt(zx*zy)
  }
  else {
    pmean_zxy <- (zx+zy)/2
  }
  
  return(list(GCs=t(GCs), zx=t(zx), zy=t(zy), pmean_zxy=t(pmean_zxy)))
}

plotTableGCsOfSpaceSizeCombs <- function(
  x, y, power=0, theta=1, include_power_mean=FALSE,
  round_digit=1, arrange=TRUE, na.rm=TRUE,
  boldDiagonals=FALSE,
  names_gc=gc.names, cols_gc=gc.cols)
{
  result <- dumpGCsOfSpaceSizeCombs(
    x, y, power=power, arrange=arrange,
    theta=theta, na.rm=na.rm, names_gc=names_gc)
  GCs <- result$GCs
  
  cols <- as.table(matrix('white', nrow(GCs), ncol(GCs)))
  
  inds <- which(GCs == names_gc[gc_small], arr.ind=T)
  cols[inds] <- cols_gc[gc_small]
  inds <- which(GCs == names_gc[gc_skinny], arr.ind=T)
  cols[inds] <- cols_gc[gc_skinny]
  inds <- which(GCs == names_gc[gc_shallow], arr.ind=T)
  cols[inds] <- cols_gc[gc_shallow]
  inds <- which(GCs == names_gc[gc_medium], arr.ind=T)
  cols[inds] <- cols_gc[gc_medium]
  inds <- which(GCs == names_gc[gc_large], arr.ind=T)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(GCs), ncol(GCs)))
  
  if (boldDiagonals) {
    inds <- which(row(GCs) == col(GCs), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core=list(fg_params=list(cex=0.8, fontface=fontfaces),
              bg_params=list(fill=cols)),
    colhead=list(fg_params=list(cex=1)),
    rowhead=list(fg_params=list(cex=0.9)))
  
  if (include_power_mean) {
    GCs <- matrix(paste(GCs, round(result$pmean_zxy, round_digit),
                        sep=", "), nrow=nrow(GCs), ncol=ncol(GCs),
                  dimnames=dimnames(GCs))
    # GCs <- paste0(GCs, ', ', round(result$pmean_zxy, round_digit))
  }
  
  grobGCs <- tableGrob(GCs, theme=mytheme)
  
  grid.arrange(grobGCs,
               ncol=1, nrow=1,
               top=paste0('G-Categories of Data Sets with m (', length(x),
                          ') x ', 'n (', length(y), ') Combinations\nvia ',
                          powerMeanTypes(power)))
}

plotTablesGCsOfSpaceSizeCombs2x2 <- function(x, y, power=0, round_digit=1,
                                             arrange=TRUE, theta=1, na.rm=TRUE,
                                             boldDiagonals=FALSE,
                                             names_gc=gc.names, cols_gc=gc.cols)
{
  result <- dumpGCsOfSpaceSizeCombs(x, y, power=power, arrange=arrange,
                                    theta=theta, na.rm=na.rm, names_gc=names_gc)
  
  cols <- as.table(matrix('white', nrow(result$GCs), ncol(result$GCs)))
  
  inds <- which(result$GCs == names_gc[gc_small], arr.ind=T)
  cols[inds] <- cols_gc[gc_small]
  inds <- which(result$GCs == names_gc[gc_skinny], arr.ind=T)
  cols[inds] <- cols_gc[gc_skinny]
  inds <- which(result$GCs == names_gc[gc_shallow], arr.ind=T)
  cols[inds] <- cols_gc[gc_shallow]
  inds <- which(result$GCs == names_gc[gc_medium], arr.ind=T)
  cols[inds] <- cols_gc[gc_medium]
  inds <- which(result$GCs == names_gc[gc_large], arr.ind=T)
  cols[inds] <- cols_gc[gc_large]
  
  fontfaces <- as.table(matrix('plain', nrow(result$GCs), ncol(result$GCs)))
  
  if (boldDiagonals) {
    inds <- which(row(result$GCs) == col(result$GCs), arr.ind=T)
    fontfaces[inds] <- 'bold'
  }
  
  mytheme <- gridExtra::ttheme_minimal(
    core=list(fg_params=list(cex=0.6, fontface=fontfaces),
              bg_params=list(fill=cols)),
    colhead=list(fg_params=list(cex=0.7)),
    rowhead=list(fg_params=list(cex=0.7)))
  
  grobGCs <- tableGrob(result$GCs, theme=mytheme)
  grobGCs <- addTitleToGrob(grobGCs, 'G-Categories')
  
  x_name <- deparse(substitute(x))
  
  grobZx <- tableGrob(round(result$zx, round_digit), theme=mytheme)
  grobZx <- addTitleToGrob(grobZx, paste0('z-score (', x_name, ')'))
  
  y_name <- deparse(substitute(y))
  
  grobZy <- tableGrob(round(result$zy, round_digit), theme=mytheme)
  grobZy <- addTitleToGrob(grobZy, paste0('z-score (', y_name, ')'))
  
  if (power == 0) {
    interpret <- matrix(paste0(round(result$zy/result$zx, round_digit), ' (',
                               round(sqrt(result$zy*result$zx), round_digit),
                               ')'),
                        nrow=nrow(result$zy), dimnames=dimnames(result$zy))
  }
  else if (power > 0 && power <= 1) {
    interpret <- matrix(paste0(round(result$zy/result$zx, round_digit), ' (',
                               round((result$zy + result$zx)/2.0, round_digit),
                               ')'),
                        nrow=nrow(result$zy), dimnames=dimnames(result$zy))
  }
  
  grobZyZxRatio <- tableGrob(interpret, theme=mytheme)
  grobZyZxRatio <- addTitleToGrob(
    grobZyZxRatio,
    paste('z-score',
          ifelse(power == 0,
                 paste0(y_name, '/', x_name,
                        ' (G.Mean = [', x_name, '*', y_name, ']^.5)'),
                 paste0(y_name, '/', x_name,
                        ' (A.Mean = [', x_name, '+', y_name, ']/2)'))))
  
  grid.arrange(grobGCs, grobZy,
               grobZx, grobZyZxRatio,
               ncol=2, nrow=2,
               layout_matrix=rbind(c(1, 2),
                                   c(3, 4)),
               top=paste('G-Categories of Data Sets with m (', length(x),
                         ') x ', 'n (', length(y), ') Combinations via',
                         powerMeanTypes(power)))
}

## Other #######################################################################

addTitleToGrob<-function(grob, title, fontsize=12)
{
  padding <- unit(1, 'line')
  titleGrob <- textGrob(title, gp=gpar(fontsize=fontsize))
  
  grob <- gtable_add_rows(grob, heights=grobHeight(titleGrob) + padding, pos=0)
  grob <- gtable_add_grob(grob, titleGrob, t=1, l=1, r=ncol(grob))
  grob$layout$clip <- 'off'
  
  return (grob)
}

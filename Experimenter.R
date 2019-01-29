source('gcategory.R')

infos <- c(
  'Linear sample/feature space with other two extends', # 1
  'Random sample/feature space sizes', # 2
  'Benign (negative-class) software datasets', # 3
  'Malign (positive-class) software datasets', # 4
  'Malign (positive-class) software (specialized) datasets' # 5
)

dir_sub <- c(
  '1_SyntheticDSs_Linear/',
  '2_SyntheticDSs_Random/',
  '3_BenignDSs/',
  '4_MalignDSs/',
  '5_MalwareFamilyDSs/'
)

loadTestData <- function(count_ds=9) {
  # Synthetic data sets
  DSxy <<- paste0(rep('DS', count_ds), 1:count_ds)
  # Linear sample/feature space sizes
  x1 <<- c(1:count_ds)
  y1 <<- c(10, seq((count_ds-1)*10, 20, -10), count_ds*10)
  # Random sample/feature space sizes
  xr1 <<- floor(runif(count_ds, 1, count_ds))
  yr1 <<- floor(runif(count_ds, 10, count_ds*10))
  
  # Two-class data sets (see the article for the references)
  DSs <<- paste0(rep('DS', 5), 0:4)
  # Negative (benign mobile apps)
  mN <<- c(84, 94, 83, 99, 118)
  nN <<- c(264303, 254, 310926, 1000, 207865)
  # Positive (malign mobile apps (i.e. malware))
  mP <<- c(90, 81, 69, 75, 73)
  nP <<- c(399353, 280, 4868, 1000, 378)
  
  # Specialized data sets (see the article for the references) 
  # Sample space size (m):  1,260  5,555  23,743  1,929  4,725  2,421
  # Feature space size (n):    65     94     105     78    111  85
  
  DSMs <<- c('AMGP', 'Drebin', 'AMD', 'ABot', 'VT2018', 'UpDroid')
  mMalware <<- c(65, 94, 105, 78, 111, 85)
  nMalware <<- c(1260, 5555, 23743, 1929, 4725, 2421)
}

processDsConfiguration <- function(
  m, n, dss, show_ds_info,
  info,
  n_name, m_name, power, power_method, theta,
  fig_1_name,
  width_1, height_1,
  fig_2_name, fig_3_name,
  tab_name,
  plot_to_file, y_transform,
  names_gc=gc.names, cols_gc=gc.cols, csv_seperator=',') {
  # Display information about the data sets
  if (show_ds_info) {
    cat(paste0(
      'Testing for ',
      length(dss), ' Data Sets:\n   ', paste(dss, collapse='\t'),
      '\nwith Sample Space Sizes (n):\n   ', paste(n, collapse='\t'),
      '\nwith Feature Space Sizes (m):\n   ', paste(m, collapse='\t'), '\n\n'
    ))  
  }
  
  # Figure: Data Sets' G-Categories with Z-scores
  cat(paste0(
    'G-Categories calculated via ', power_method,
    ifelse(
      power==0,
      paste0(' (correct) approach (theta = ', round(theta, 3),
             ').\n   See the files having "geo" in their names.\n'),
      paste0(' (erroneous) approach (theta = ', round(theta, 3),
             ').\n   See the files having "ari" in their names.\n')
    )))
  
  if (plot_to_file) {
    png(filename=fig_1_name, width=width_1, height=height_1, units='cm', res=300)
  }
  result <- plotTableGCsDetailed(m, n, dss, info, power=power, theta=theta,
                                 names_gc=names_gc, cols_gc=cols_gc)
  
  if (plot_to_file) {
    dev.off()
  }
  
  if (plot_to_file) {
    # Tabular Data: G-Categories with Z-scores
    write.table(result, file=tab_name,
                sep=csv_seperator, dec='.', row.names=TRUE, col.names=NA,
                fileEncoding="UTF-8")
  }
  
  if (plot_to_file) {
    png(filename=fig_2_name, width=35, height=20, units='cm', res=300)
  }
  
  print(
    plotGraphGCs(
      m, n,
      greatnessCategories(m, n, power=power, theta=theta),
      dss, trans=y_transform,
      subtitle=paste('G-Categories of the data sets with', info,
                     '\nCalculated via', powerMeanTypes(power),
                     ifelse(y_transform == 'log10',
                            'sample space is drawn in logarithmic scale', '')
      ),
      names_gc=names_gc, cols_gc=cols_gc
    )
  )
  
  if (plot_to_file) {
    dev.off()
  }
  
  # Figure: G-Categories with Z-scores of The Combination of The Data Sets
  if (plot_to_file) {
    png(filename=fig_3_name, width=27, height=20, units='cm', res=300)
  }
  
  result <- plotTableGCsOfSpaceSizeCombs(m, n, power=power, theta=theta,
                                         include_power_mean=TRUE,
                                         names_gc=names_gc, cols_gc=cols_gc)
  
  if (plot_to_file) {
    dev.off()
  }
  
  if (plot_to_file) {
    result <- tabulateGCsOfSpaceSizeCombs(m, n, power=power, theta=theta,
                                           include_power_mean=TRUE,
                                           names_gc=names_gc)
    # Tabular Data: G-Categories with G-C factor
    write.table(
      result, file=gsub('_d', '_e', gsub('s.csv', 'Combination.csv', tab_name)),
      sep=csv_seperator, dec='.', row.names=TRUE, col.names=NA,
      fileEncoding="UTF-8")
  }
}

# testGreatnessCategory(plot_to_file=TRUE, csv_seperator=';')
testGreatnessCategory <- function(power_1=1, power_2=0,
                                  theta_1=log(2), theta_2=1,
                                  names_gc=gc.names, cols_gc=gc.cols,
                                  plot_to_file=FALSE, dir_parent='../results/',
                                  csv_seperator=',') {
  if (exists('x1') == FALSE) {
    loadTestData()
  }
  
  count_conf <- length(infos)
  # 1) x1 and y1
  conf <- 1
  cat(paste0('Configuration ', conf, '/', count_conf, ': ', infos[conf], '\n'))
  cat('*********************************************************************\n')
  dss <- paste0(rep('DS', length(x1)), 1:length(x1))
  processDsConfiguration(
    m=x1, n=y1, dss=dss, show_ds_info=TRUE,
    info=infos[conf],
    n_name='nLin', m_name='mLin',
    power=power_1, power_method='Arithmetic', theta=theta_1,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '1ari_aLinearDSSizes.png'),
    width_1=45, height_1=15,
    fig_2_name=paste0(dir_parent, dir_sub[conf], '1ari_bLinearDSSizeGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '1ari_cLinearDSSizeCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '1ari_dLinearDSSizes.csv'),
    plot_to_file=plot_to_file, y_transform='identity',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  processDsConfiguration(
    m=x1, n=y1, dss=dss, show_ds_info=FALSE,
    info=infos[conf],
    n_name='nLin', m_name='mLin',
    power=power_2, power_method='Geometric', theta=theta_2,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '1geo_aLinearDSSizes.png'),
    width_1=45, height_1=15,
    fig_2_name=paste0(dir_parent, dir_sub[conf], '1geo_bLinearDSSizeGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '1geo_cLinearDSSizeCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '1geo_dLinearDSSizes.csv'),
    plot_to_file=plot_to_file, y_transform='identity',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  # 2) xr1 and yr1
  conf <- conf + 1
  cat(paste0('\n\nConfiguration ', conf, '/', count_conf, ': ', infos[conf],
             '\n'))
  cat('*********************************************************************\n')
  dss <- paste0(rep('DS', length(xr1)), 1:length(xr1))
  processDsConfiguration(
    m=xr1, n=yr1, dss=dss, show_ds_info=TRUE,
    info=infos[conf],
    n_name='nRnd', m_name='mRnd',
    power=power_1, power_method='Arithmetic', theta=theta_1,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '2ari_aRandomDSSizes.png'),
    width_1=30, height_1=15,
    fig_2_name=paste0(dir_parent, dir_sub[conf], '2ari_bRandomDSSizeGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '2ari_cRandomDSSizeCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '2ari_dRandomDSSizes.csv'),
    plot_to_file=plot_to_file, y_transform='identity',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  processDsConfiguration(
    m=xr1, n=yr1, dss=dss, show_ds_info=FALSE,
    info=infos[conf],
    n_name='nRnd', m_name='mRnd',
    power=power_2, power_method='Geometric', theta=theta_2,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '2geo_aRandomDSSizes.png'),
    width_1=30, height_1=20,
    fig_2_name=paste0(dir_parent, dir_sub[conf], '2geo_bRandomDSSizeGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '2geo_cRandomDSSizeCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '2geo_dRandomDSSizes.csv'),
    plot_to_file=plot_to_file, y_transform='identity',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  # 3) mN and nN
  conf <- conf + 1
  cat(paste0('\n\nConfiguration ', conf, '/', count_conf, ': ', infos[conf],
             '\n'))
  cat('*********************************************************************\n')
  dss <- paste0(rep('DS', length(mN)), 0:(length(mN)-1))
  processDsConfiguration(
    m=mN, n=nN, dss=dss, show_ds_info=TRUE,
    info=infos[conf],
    n_name='mN', m_name='nN',
    power=power_1, power_method='Arithmetic', theta=theta_1,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '3ari_aBenignDSs.png'),
    width_1=30, height_1=15,
    fig_2_name=paste0(dir_parent, dir_sub[conf], '3ari_bBenignDSsGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '3ari_cBenignDSCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '3ari_dBenignDSs.csv'),
    plot_to_file=plot_to_file, y_transform='identity',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  processDsConfiguration(
    m=mN, n=nN, dss=dss, show_ds_info=FALSE,
    info=infos[conf],
    n_name='mN', m_name='nN',
    power=power_2, power_method='Geometric', theta=theta_2,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '3geo_aBenignDSs.png'),
    width_1=30, height_1=20,
    fig_2_name=paste0(dir_parent, dir_sub[conf], '3geo_bBenignDSsGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '3geo_cBenignDSCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '3geo_dBenignDSs.csv'),
    plot_to_file=plot_to_file, y_transform='identity',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  # 4) mP and nP
  conf <- conf + 1
  cat(paste0('\n\nConfiguration ', conf, '/', count_conf, ': ', infos[conf],
             '\n'))
  cat('*********************************************************************\n')
  dss <- paste0(rep('DS', length(mP)), 1:(length(mP)-1))
  processDsConfiguration(
    m=mP, n=nP, dss=dss, show_ds_info=TRUE,
    info=infos[conf],
    n_name='mP', m_name='nP',
    power=power_1, power_method='Arithmetic', theta=theta_1,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '4ari_aMalignDSs.png'),
    width_1=30, height_1=15,
    fig_2_name=paste0(dir_parent, dir_sub[conf], '4ari_bMalignDSsGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '4ari_cMalignDSCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '4ari_dMalignDSs.csv'),
    plot_to_file=plot_to_file, y_transform='log10',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  processDsConfiguration(
    m=mP, n=nP, dss=dss, show_ds_info=FALSE,
    info=infos[conf],
    n_name='mP', m_name='nP',
    power=power_2, power_method='Geometric', theta=theta_2,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '4geo_aMalignDSs.png'),
    width_1=30, height_1=20,
    fig_2_name=paste0(dir_parent, dir_sub[conf], '4geo_bMalignDSsGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '4geo_cMalignDSCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '4geo_dMalignDSs.csv'),
    plot_to_file=plot_to_file, y_transform='log10',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  # 5) mMalware and nMalware
  conf <- conf + 1
  cat(paste0('\n\nConfiguration ', conf, '/', count_conf, ': ', infos[conf],
             '\n'))
  cat('*********************************************************************\n')
  processDsConfiguration(
    m=mMalware, n=nMalware, dss=DSMs, show_ds_info=TRUE,
    info=infos[conf],
    n_name='mMalware', m_name='nMalware',
    power=power_1, power_method='Arithmetic', theta=theta_1,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '5ari_aMalwareFamilyDSs.png'),
    width_1=40, height_1=15,
    fig_2_name=paste0(dir_parent, dir_sub[conf],
                      '5ari_bMalwareFamilyDSsGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '5ari_cMalwareFamilyDSCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '5ari_dMalwareFamilyDSs.csv'),
    plot_to_file=plot_to_file, y_transform='log10',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
  
  processDsConfiguration(
    m=mMalware, n=nMalware, dss=DSMs, show_ds_info=FALSE,
    info=infos[conf],
    n_name='mMalware', m_name='nMalware',
    power=power_2, power_method='Geometric', theta=theta_2,
    fig_1_name=paste0(dir_parent, dir_sub[conf], '5geo_aMalwareFamilyDSs.png'),
    width_1=40, height_1=20,
    fig_2_name=paste0(dir_parent, dir_sub[conf],
                      '5geo_bMalwareFamilyDSsGraph.png'),
    fig_3_name=paste0(dir_parent, dir_sub[conf],
                      '5geo_cMalwareFamilyDSCombination.png'),
    tab_name=paste0(dir_parent, dir_sub[conf], '5geo_dMalwareFamilyDSs.csv'),
    plot_to_file=plot_to_file, y_transform='log10',
    names_gc=names_gc, cols_gc=cols_gc, csv_seperator=csv_seperator
  )
}


testPlot <- function(power_1=1, power_2=0) {
  # x1 and y1
  dss <- paste0(rep('DS', length(x1)), 1:length(x1))
  result <- plotTableGCategoriesDetailed(
    x1, y1, dss,
    'Linear sample/feature space with other two extends', power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('x1 vs. y1 (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotTableGCategoriesDetailed(
    x1, y1, dss,
    'Linear sample/feature space size with other two extends', power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('x1 vs. y1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # xr1 and yr1
  dss <- paste0(rep('DS', length(xr1)), 1:length(xr1))
  result <- plotTableGCategoriesDetailed(xr1, yr1, dss,
                                         'Random sample/feature space size',
                                         power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotTableGCategoriesDetailed(xr1, yr1, dss,
                                         'Random sample/feature space size',
                                         power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # mN and nN
  dss <- paste0(rep('DS', length(mN)), 1:length(mN))
  result <- plotTableGCategoriesDetailed(
    mN, nN, dss,
    'Benign (negative-class) software datasets', power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('mN vs. nN (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotTableGCategoriesDetailed(
    mN, nN, dss,
    'Benign (negative-class) software datasets', power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('mN vs. nN (Geometric)',
                                  'Press [enter] to continue')))
  
  # mP and nP
  dss <- paste0(rep('DS', length(mP)), 1:length(mP))
  result <- plotTableGCategoriesDetailed(
    mP, nP, dss,
    'Malign (positive-class) software datasets', power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('mP vs. nP (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotTableGCategoriesDetailed(
    mP, nP, dss,
    'Malign (positive-class) software datasets', power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('mP vs. nP (Geometric)',
                                  'Press [enter] to continue')))
  
  # mMalware and nMalware
  result <- plotTableGCategoriesDetailed(
    mMalware, nMalware, DSMs,
    'Malign (positive-class) software (specialized) datasets', power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('mP vs. nP (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotTableGCategoriesDetailed(
    mMalware, nMalware, DSMs,
    'Malign (positive-class) software (specialized) datasets', power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('mP vs. nP (Geometric)',
                                  'Press [enter] to continue')))
}

testDump <- function(power_1=1, power_2=0) {
  # x1 and y1
  dss <- paste0(rep('DS', length(x1)), 1:length(x1))
  result <- dumpGCategoriesWithZ(x1, y1, power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('x1 vs. y1 (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- dumpGCategoriesWithZ(x1, y1, power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('x1 vs. y1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # xr1 and yr1
  dss <- paste0(rep('DS', length(xr1)), 1:length(xr1))
  result <- dumpGCategoriesWithZ(xr1, yr1, power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- dumpGCategoriesWithZ(xr1, yr1, power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # mN and nN
  dss <- paste0(rep('DS', length(mN)), 1:length(mN))
  result <- dumpGCategoriesWithZ(mN, nN, power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('mN vs. nN (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- dumpGCategoriesWithZ(mN, nN, power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('mN vs. nN (Geometric)',
                                  'Press [enter] to continue')))
  
  # mP and nP
  dss <- paste0(rep('DS', length(mP)), 1:length(mP))
  result <- dumpGCategoriesWithZ(mP, nP, power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('mP vs. nP (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- dumpGCategoriesWithZ(mP, nP, power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('mP vs. nP (Geometric)',
                                  'Press [enter] to continue')))
}
source('gcategory.R')

infos <- c(
  'Linear sample/feature space with other two extends', # 1
  'Random sample/feature space sizes', # 2
  'Benign (negative-class) software datasets', # 3
  'Malign (positive-class) software datasets', # 4
  'Malign (positive-class) software (specialized) datasets' # 5
)

dir_sub <- c(
  'linear/',
  'random/',
  'benign/',
  'malign/',
  'malwarefamily/'
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
  nN <<- c(84, 94, 83, 99, 118)
  mN <<- c(264303, 254, 310926, 1000, 207865)
  # Positive (malign mobile apps (i.e. malware))
  nP <<- c(90, 81, 69, 75, 73)
  mP <<- c(399353, 280, 4868, 1000, 378)
  
  # Specialized data sets (see the article for the references) 
  # Sample space size (m):  1,260  5,555  23,743  1,929  4,725  2,421
  # Feature space size (n):    65     94     105     78    111  85
  
  DSMs <<- c('AMGP', 'Drebin', 'AMD', 'ABot', 'VT2018', 'UpDroid')
  nMalware <<- c(65, 94, 105, 78, 111, 85)
  mMalware <<- c(1260, 5555, 23743, 1929, 4725, 2421)
}

processDsConfiguration <- function(n, m, dss, show_ds_info,
                                   info,
                                   n_name, m_name, power, power_method, theta,
                                   fig_1_name,
                                   width_1, height_1,
                                   fig_2_name, fig_3_name,
                                   tab_name,
                                   plot_to_file, y_transform) {
  # Display information about the data sets
  if (show_ds_info) {
    cat(paste0(
      'Testing for ',
      length(dss), ' Data Sets:\n   ', paste(dss, collapse='\t'),
      '\nwith Sample Space Sizes (m):\n   ', paste(m, collapse='\t'),
      '\nwith Feature Space Sizes (m):\n   ', paste(n, collapse='\t'), '\n\n'
    ))  
  }
  
  # Figure: Data Sets' G-Categories with Z-scores
  cat(paste0('G-Categories calculated via ', power_method, ifelse(power==0, ' (b: Correct Approach)\n', ' (a: Erroneous Approach)\n')))
  
  if (plot_to_file) {
    png(filename=fig_1_name, width=width_1, height=height_1, units='cm', res=300)
  }
  result <- plotGCategoriesZScores(n, m, dss, info, power=power, theta=theta)
  
  if (plot_to_file) {
    dev.off()
  }
  
  if (plot_to_file) {
    # Tabular Data: G-Categories with Z-scores
    write.table(result, file=tab_name,
                sep=',', dec='.', row.names=TRUE, col.names=NA,
                fileEncoding="UTF-8")
  }
  
  if (plot_to_file) {
    png(filename=fig_2_name, width=35, height=20, units='cm', res=300)
  }
  
  print(plotGCategories(n, m,
                        greatnessCategories(n, m, power=power, theta=theta),
                        dss, trans=y_transform))
  
  if (plot_to_file) {
    dev.off()
  }

  # Figure: G-Categories with Z-scores of The Combination of The Data Sets
  if (plot_to_file) {
    png(filename=fig_3_name, width=45, height=20, units='cm', res=300)
  }
  
  result <- plotCombination(n, m, power=power, theta=theta)
  
  if (plot_to_file) {
    dev.off()
  }
}

greatnessCategoryTest <- function(power_1=1, power_2=0, theta=1,
                                  plot_to_file=FALSE, dir_parent='../results/') {
  count_conf <- length(infos)
  # 1) x1 and y1
  conf <- 1
  cat(paste0('Configuration ', conf, '/', count_conf, ': ', infos[conf], '\n'))
  cat('********************************************************************************\n')
  dss <- paste0(rep('DS', length(x1)), 1:length(x1))
  processDsConfiguration(n=x1, m=y1, dss=dss, show_ds_info=TRUE,
                         info=infos[conf],
                         n_name='nLin', m_name='mLin',
                         power=power_1, power_method='Arithmetic', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F1a_LinearSizes_Arithmetic.png'),
                         width_1=45, height_1=15,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F1a_LinearSizeGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F1a_LinearSizeComb_Arithmetic.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T1a_LinearSizes_Arithmetic.csv'),
                         plot_to_file=plot_to_file, y_transform='identity'
  )
  
  processDsConfiguration(n=x1, m=y1, dss=dss, show_ds_info=FALSE,
                         info=infos[conf],
                         n_name='nLin', m_name='mLin',
                         power=power_2, power_method='Geometric', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F1b_LinearSizes_Geometric.png'),
                         width_1=45, height_1=15,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F1b_LinearSizeGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F1b_LinearSizeComb_Arithmetic.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T1b_LinearSizes_Geometric.csv'),
                         plot_to_file=plot_to_file, y_transform='identity'
  )
  
  # 2) xr1 and yr1
  conf <- conf + 1
  cat(paste0('\n\nConfiguration ', conf, '/', count_conf, ': ', infos[conf], '\n'))
  cat('********************************************************************************\n')
  dss <- paste0(rep('DS', length(xr1)), 1:length(xr1))
  processDsConfiguration(n=xr1, m=yr1, dss=dss, show_ds_info=TRUE,
                         info=infos[conf],
                         n_name='nRnd', m_name='mRnd',
                         power=power_1, power_method='Arithmetic', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F2a_RandomSizes_Arithmetic.png'),
                         width_1=30, height_1=15,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F2a_RandomSizeGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F2a_RandomSizeComb_Arithmetic.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T2a_RandomSizes_Arithmetic.csv'),
                         plot_to_file=plot_to_file, y_transform='identity'
  )
  
  processDsConfiguration(n=xr1, m=yr1, dss=dss, show_ds_info=FALSE,
                         info=infos[conf],
                         n_name='nRnd', m_name='mRnd',
                         power=power_2, power_method='Geometric', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F2b_RandomSizes_Geometric.png'),
                         width_1=30, height_1=20,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F2b_RandomSizeGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F2b_RandomSizeComb_Geometric.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T2b_RandomSizes_Geometric.csv'),
                         plot_to_file=plot_to_file, y_transform='identity'
  )
  
  # 3) nN and mN
  conf <- conf + 1
  cat(paste0('\n\nConfiguration ', conf, '/', count_conf, ': ', infos[conf], '\n'))
  cat('********************************************************************************\n')
  dss <- paste0(rep('DS', length(nN)), 1:length(nN))
  processDsConfiguration(n=nN, m=mN, dss=dss, show_ds_info=TRUE,
                         info=infos[conf],
                         n_name='nN', m_name='mN',
                         power=power_1, power_method='Arithmetic', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F3a_BenignDSs_Arithmetic.png'),
                         width_1=30, height_1=15,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F3a_BenignDSGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F3a_BenignDSComb_Arithmetic.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T3a_BenignDSs_Arithmetic.csv'),
                         plot_to_file=plot_to_file, y_transform='identity'
  )
  
  processDsConfiguration(n=nN, m=mN, dss=dss, show_ds_info=FALSE,
                         info=infos[conf],
                         n_name='nN', m_name='mN',
                         power=power_2, power_method='Geometric', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F3b_BenignDSs_Geometric.png'),
                         width_1=30, height_1=20,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F3b_BenignDSGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F3b_BenignDSComb_Geometric.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T3b_GC_BenignDSs_Geometric.csv'),
                         plot_to_file=plot_to_file, y_transform='identity'
  )
  
  # 4) nP and mP
  conf <- conf + 1
  cat(paste0('\n\nConfiguration ', conf, '/', count_conf, ': ', infos[conf], '\n'))
  cat('********************************************************************************\n')
  dss <- paste0(rep('DS', length(nP)), 1:length(nP))
  processDsConfiguration(n=nP, m=mP, dss=dss, show_ds_info=TRUE,
                         info=infos[conf],
                         n_name='nP', m_name='mP',
                         power=power_1, power_method='Arithmetic', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F4a_MalignDSs_Arithmetic.png'),
                         width_1=30, height_1=15,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F4a_MalignDSGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F4a_MalignDSComb_Arithmetic.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T4a_MalignDSs_Arithmetic.csv'),
                         plot_to_file=plot_to_file, y_transform='log10'
  )
  
  processDsConfiguration(n=nP, m=mP, dss=dss, show_ds_info=FALSE,
                         info=infos[conf],
                         n_name='nP', m_name='mP',
                         power=power_2, power_method='Geometric', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F4b_MalignDSs_Geometric.png'),
                         width_1=30, height_1=20,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F4b_MalignDSGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F4b_MalignDSComb_Geometric.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T4b_MalignDSs_Geometric.csv'),
                         plot_to_file=plot_to_file, y_transform='log10'
  )
  
  # 5) nMalware and mMalware
  conf <- conf + 1
  cat(paste0('\n\nConfiguration ', conf, '/', count_conf, ': ', infos[conf], '\n'))
  cat('********************************************************************************\n')
  processDsConfiguration(n=nMalware, m=mMalware, dss=DSMs, show_ds_info=TRUE,
                         info=infos[conf],
                         n_name='nMalware', m_name='mMalware',
                         power=power_1, power_method='Arithmetic', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F5a_MalwareFamilyDSs_Arithmetic.png'),
                         width_1=40, height_1=15,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F5a_MalwareFamilyDSGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F5a_MalwareFamilyDSComb_Arithmetic.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T5a_MalwareFamilyDSs_Arithmetic.csv'),
                         plot_to_file=plot_to_file, y_transform='log10'
  )
  
  processDsConfiguration(n=nMalware, m=mMalware, dss=DSMs, show_ds_info=FALSE,
                         info=infos[conf],
                         n_name='nMalware', m_name='mMalware',
                         power=power_2, power_method='Geometric', theta=theta,
                         fig_1_name=paste0(dir_parent, dir_sub[conf], 'F5b_MalwareFamilyDSs_Geometric.png'),
                         width_1=40, height_1=20,
                         fig_2_name=paste0(dir_parent, dir_sub[conf], 'F5b_MalwareFamilyDSGraph_Arithmetic.png'),
                         fig_3_name=paste0(dir_parent, dir_sub[conf], 'F5b_MalwareFamilyDSComb_Geometric.png'),
                         tab_name=paste0(dir_parent, dir_sub[conf], 'T5b_MalwareFamilyDSs_Geometric.csv'),
                         plot_to_file=plot_to_file, y_transform='log10'
  )
}


testPlot <- function(power_1=1, power_2=0) {
  # x1 and y1
  dss <- paste0(rep('DS', length(x1)), 1:length(x1))
  result <- plotGCategoriesZScores(x1, y1, dss,
                                   'Linear sample/feature space with other two extends',
                                   power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('x1 vs. y1 (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotGCategoriesZScores(x1, y1, dss,
                                   'Linear sample/feature space size with other two extends',
                                   power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('x1 vs. y1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # xr1 and yr1
  dss <- paste0(rep('DS', length(xr1)), 1:length(xr1))
  result <- plotGCategoriesZScores(xr1, yr1, dss,
                                   'Random sample/feature space size',
                                   power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotGCategoriesZScores(xr1, yr1, dss,
                                   'Random sample/feature space size',
                                   power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('xr1 vs. yr1 (Geometric)',
                                  'Press [enter] to continue')))
  
  # nN and mN
  dss <- paste0(rep('DS', length(nN)), 1:length(nN))
  result <- plotGCategoriesZScores(nN, mN, dss,
                                   'Benign (negative-class) software datasets',
                                   power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('nN vs. mN (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotGCategoriesZScores(nN, mN, dss,
                                   'Benign (negative-class) software datasets',
                                   power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('nN vs. mN (Geometric)',
                                  'Press [enter] to continue')))
  
  # nP and mP
  dss <- paste0(rep('DS', length(nP)), 1:length(nP))
  result <- plotGCategoriesZScores(nP, mP, dss,
                                   'Malign (positive-class) software datasets',
                                   power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('nP vs. mP (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotGCategoriesZScores(nP, mP, dss,
                                   'Malign (positive-class) software datasets',
                                   power=power_2)
  wclip(result)
  invisible(readline(prompt=paste('nP vs. mP (Geometric)',
                                  'Press [enter] to continue')))
  
  # nMalware and mMalware
  result <- plotGCategoriesZScores(nMalware, mMalware, DSMs,
                                   'Malign (positive-class) software (specialized) datasets',
                                   power=power_1)
  wclip(result)
  invisible(readline(prompt=paste('nP vs. mP (Arithmetic)',
                                  'Press [enter] to continue')))
  result <- plotGCategoriesZScores(nMalware, mMalware, DSMs,
                                   'Malign (positive-class) software (specialized) datasets',
                                   power=power_2)
  wclip(result)
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
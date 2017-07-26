

#######################################################################################################################-
# Libraries + Parallel Processing Start ----
#######################################################################################################################-

skip = function() {
  install.packages(c("corrplot","vcd","doSNOW","lattice","ggplot2","plyr","reshape2","ROCR","Hmisc",
                     "e1071","pROC","caret","glmnet","randomForest","gbm","rpart"))
}

#library(doSNOW)
library(doParallel)
library(plyr)
library(tidyverse)

library(corrplot)
library(vcd)
library(gridExtra)
library(Hmisc)

library(d3heatmap)
library(htmlwidgets)
library(caret)
library(ROCR)
library(stringr)
library(rgl)

# 
# library(lattice)
# library(ggplot2)
# library(plyr)
# library(reshape2)
# 
# 
# 
# library(pROC)
# library(glmnet)
# library(randomForest)
# library(gbm)
# library(rpart)


# 
# library(pROC)
# library(mboost)
# library(kernlab)
# library(xgboost)
# library(latticeExtra)
# library(partykit)
# library(ICEbox)
# library(gridExtra)




#######################################################################################################################-
# Parameter ----
#######################################################################################################################-

plotloc = ""

theme_my = theme_bw() +  theme(plot.title = element_text(hjust = 0.5))

twocol = c("blue","red")

col3d = colorRampPalette(c("cyan", "white", "magenta"))(100)

manycol = c('#00FF00','#0000FF','#FF0000','#01FFFE','#FFA6FE','#FFDB66','#006401','#010067','#95003A',
            '#007DB5','#FF00F6','#FFEEE8','#774D00','#90FB92','#0076FF','#D5FF00','#FF937E','#6A826C','#FF029D',
            '#FE8900','#7A4782','#7E2DD2','#85A900','#FF0056','#A42400','#00AE7E','#683D3B','#BDC6FF','#263400',
            '#BDD393','#00B917','#9E008E','#001544','#C28C9F','#FF74A3','#01D0FF','#004754','#E56FFE','#788231',
            '#0E4CA1','#91D0CB','#BE9970','#968AE8','#BB8800','#43002C','#DEFF74','#00FFC6','#FFE502','#620E00',
            '#008F9C','#98FF52','#7544B1','#B500FF','#00FF78','#FF6E41','#005F39','#6B6882','#5FAD4E','#A75740',
            '#A5FFD2','#FFB167','#009BFF','#E85EBE')




#######################################################################################################################-
# My Functions ----
#######################################################################################################################-

## Calculate probabilty on all data from probabilt from sample data and the corresponding (prior) base probabilities 
prob_samp2full = function(p_sample, b_sample, b_all) {
  p_all = b_all * ((p_sample - p_sample*b_sample) / 
                   (b_sample - p_sample*b_sample + b_all*p_sample - b_sample*b_all))
  p_all
}



## Workaround for ggsave and marrangeGrob not to create first page blank
grid.draw.arrangelist <- function(x, ...) {
  for (ii in seq_along(x)) {
    if (ii > 1) grid.newpage()  # skips grid.newpage() call the first time around
    grid.draw(x[[ii]])
  }
}



## Plot distribution of metric variables per stratum
plot_distr_metr = function(outpdf, df = df.plot, vars = metr, nbins = 20, misspct, color = twocol, 
                           ncols = 4, nrows = 2, w = 12, h = 8) {

  # Univariate variable importance
  varimp = (filterVarImp(df[vars], df$target, nonpara = TRUE)["Y"] %>% 
              mutate(Y = round(ifelse(Y < 0.5, 1-Y, Y),2)))[[1]]
  names(varimp) = vars
  
  # Loop over vars
  plots = map(vars, ~ {
    #. = vars[1]
    # Start histogram plot
    p = ggplot(data = df, aes_string(x = .)) +
      geom_histogram(aes(y = ..density.., fill = target), bins = nbins, position = "identity") +
      geom_density(aes(color = target)) +
      scale_fill_manual(values = alpha(color, .2), name = "Target") + 
      scale_color_manual(values = color, name = "Target") +
      labs(title = paste0(.," (VI: ", round(varimp[.],2),")"),
           x = paste0(.," (NA: ", misspct[.] * 100,"%)"))
    
    # Get underlying data for max of y-value and range of x-value
    tmp = ggplot_build(p)
    
    # Inner boxplot
    p.inner = ggplot(data = df, aes_string("target", y = .)) +
      geom_boxplot(aes_string(color = "target")) +
      coord_flip() +
      scale_y_continuous(limits = c(min(tmp$data[[1]]$xmin), max(tmp$data[[1]]$xmax))) +
      scale_color_manual(values = color, name = "Target") +
      scale_x_discrete(limits = rev(levels(df$target))) +
      theme_void() +
      theme(legend.position = "none")
    
    # Put all together
    p = p + 
      scale_y_continuous(limits = c(-tmp$layout$panel_ranges[[1]]$y.range[2]/14, NA)) +
      theme_my +
      annotation_custom(ggplotGrob(p.inner), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0) 
    if (. != vars[1]) p = p + theme(legend.position = "none") 
    p 
  })
  ggsave(outpdf, marrangeGrob(plots, ncol = ncols, nrow = nrows, top = NULL), width = w, height = h)
}



## Plot distribution of nominal variables per stratum
plot_distr_nomi = function(outpdf, df = df.plot, vars = nomi, color = twocol,
                           ncols = 4, nrows = 2, w = 12, h = 8) {
  # Univariate variable importance
  varimp = (filterVarImp(df[vars], df$target, nonpara = TRUE)["Y"] %>% 
              mutate(Y = round(ifelse(Y < 0.5, 1-Y, Y),2)))[[1]]
  names(varimp) = vars
  
  # Loop over vars
  plots = map(vars, ~ {
    #.=vars[1]
    print(.)
    # # Corrected contingency coefficient
    # tab = table(df[c(.,"target")])
    # M = min(dim(tab))
    # contig = round(assocstats(tab)$cont * sqrt(M/(M - 1)), 2) 
    
    # Proportion of Target=Y (height of bars) and relative frequency of levels (width of bars)
    tab = table(df[c(.,"target")])
    s = rowSums(tab)
    df.tmp = data.frame(factor(rownames(tab), levels = rev(levels(df[[.]]))), 
                        "prop" = (tab/s)[,2], "perc" = s/max(s), "freq" = s/sum(s))
    colnames(df.tmp)[1] = c(.)
    df.tmp[[.]] = paste0(as.character(df.tmp[[.]]), " (", round(100 * df.tmp[["freq"]],1), "%)")
    
    # Plot
    p = ggplot(df.tmp, aes_string(., "prop")) +
      geom_bar(stat = "identity", width = df.tmp$perc, color = twocol[2], fill = alpha(twocol[2], 0.2)) +
      labs(title = paste0(.," (VI:", varimp[.], ")"), x = "", y = "Proportion Target=Y") +
      geom_hline(yintercept = sum(df[["target"]]=="Y")/nrow(df), linetype = 2, color = "darkgrey") +
      scale_x_discrete(limits = rev(df.tmp[[.]])) + 
      coord_flip() +
      theme_my 
    p
  })
  ggsave(outpdf, marrangeGrob(plots, ncol = ncols, nrow = nrows, top = NULL), width = w, height = h)
}



## Plot correlation of metric variables
plot_corr_metr <- function(outpdf, df, vars=metr, misspct, method = "Spearman", 
                           w = 8, h = 8) {

  # Correlation matrix
  if (tolower(method) %in% c("spearman","pearson")) {
    m.corr = abs(cor(df[vars], method = tolower(method), use = "pairwise.complete.obs"))
  } 
  if (tolower(method) ==  "hoeffding") {
    m.corr = hoeffd(as.matrix(df[vars]))$D
  }
  
  # Adapt labels
  rownames(m.corr) = colnames(m.corr) =
    paste0(rownames(m.corr)," (", round(100*misspct[rownames(m.corr)],1),"% imp.)")
  
  # Put in clustered order
  m.corr[which(is.na(m.corr))] = 0 #set NAs to 0
  ord = corrMatOrder(m.corr , order = "hclust")
  m.corr = m.corr[ord, ord]
  d3heatmap(m.corr, colors = "Blues", sym = TRUE, xaxis_font_size = paste0(h, "pt"), xaxis_height = 120, yaxis_width = 160)
  
  # Output as widget (clusters again and might therefore create different order)
  saveWidget(d3heatmap(m.corr, colors = "Blues", sym = TRUE, xaxis_font_size = paste0(h, "pt"),
                         xaxis_height = h*20, yaxis_width = w*40), 
             file = normalizePath(paste0(str_split(outpdf,".pdf", simplify = TRUE)[1,1],".html"), mustWork = FALSE))

  # Output as ggplot
  df.corr = as.data.frame(m.corr) %>% 
    mutate(rowvar = rownames(m.corr)) %>% 
    gather(key = colvar, value = corr, -rowvar) 
  p = ggplot(df.corr, aes(rowvar, colvar)) +
    geom_tile(aes(fill = corr)) + 
    geom_text(aes(label = round(corr, 2))) +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_x_discrete(limits = rev(rownames(m.corr))) +
    scale_y_discrete(limits = rownames(m.corr)) +
    labs(title = paste0(method," Correlation"), fill = "", x = "", y = "") +
    theme_my + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  ggsave(outpdf, p, width = w, height = h)
}



## Plot correlation of nominal variables
plot_corr_nomi <- function(outpdf, df, vars, 
                           w = 8, h = 8) {
  
  p = length(vars)
  m.chi = matrix(NA, p, p)
  rownames(m.chi) = colnames(m.chi) = paste0(vars," (#Levs: ", map_int(df[vars], ~ length(levels(.))), ")")
  for (i in 1:p) {
    for (j in 1:p) {
      #print(paste0(i,"...", j))
      
      # Corrected contingency coefficient
      tab = table(df[vars[c(i,j)]])
      M = min(dim(tab))
      m.chi[i,j] = assocstats(tab)$cont * sqrt(M / (M - 1))
    }
  }

  # Put in clustered order
  m.chi[which(is.na(m.chi))] = 0 #set NAs to 0
  ord = corrMatOrder(m.chi , order = "hclust")
  m.chi = m.chi[ord, ord]
  
  # Output as widget (clusters again and might therefore create different order)
  saveWidget(d3heatmap(m.chi, colors = "Blues", sym = TRUE, xaxis_font_size = paste0(h, "pt")), 
             file = normalizePath(paste0(str_split(outpdf,".pdf", simplify = TRUE)[1,1],".html"), mustWork = FALSE))
    
  # Output as ggplot
  df.chi = as.data.frame(m.chi) %>% 
    mutate(rowvar = rownames(m.chi)) %>% 
    gather(key = colvar, value = chi, -rowvar)
  p = ggplot(df.chi, aes(rowvar, colvar)) +
    geom_tile(aes(fill = chi)) + 
    geom_text(aes(label = round(chi, 2))) +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_x_discrete(limits = rev(rownames(m.chi))) +
    scale_y_discrete(limits = rownames(m.chi)) +
    labs(title = "Contig.Coef", fill = "", x = "", y = "") +
    theme_my +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  ggsave(outpdf, p, width = w, height = h)
}


## ROC, Calibration, Gain, Lift, Confusion
plot_performance = function(outpdf, yhat_holdout, y_holdout, color = "blue", colors = twocol,
                            ncols = 4, nrows = 2, w = 18, h = 12) {
  
  # Prepare
  df.distr = data.frame(y = y_holdout, yhat = yhat_holdout)
  pred_obj = prediction(yhat_holdout, y_holdout)
  auc = performance(pred_obj, "auc" )@y.values[[1]]
  tprfpr = performance( pred_obj, "tpr", "fpr")
  df.roc = data.frame("fpr" = tprfpr@x.values[[1]], "tpr" = tprfpr@y.values[[1]])
  i.alpha = map_int(seq(0.1,0.9,0.1), function(.) which.min(abs(tprfpr@alpha.values[[1]] - .)))
  precrec = performance( pred_obj, "ppv", "rec")
  df.precrec = data.frame("rec" = precrec@x.values[[1]], "prec" = precrec@y.values[[1]], 
                          x = 100*(1:length(precrec@x.values[[1]]))/length(precrec@x.values[[1]]))
  df.precrec[is.nan(df.precrec$prec),"prec"] = 1
  
  # ROC
  p_roc = ggplot(df.roc, aes(x = fpr, y = tpr)) +
    geom_line(color = "blue", size = .5) +
    geom_abline(intercept = 0, slope = 1, color = "grey") + 
    geom_point(aes(fpr, tpr), data = df.roc[i.alpha,], color = "red", size = 0.8) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) + 
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
    
    labs(title = paste0("ROC (auc=", round(auc,3), ")"), x = expression(paste("fpr: P(", hat(y), "=1|y=0)", sep = "")),
         y = expression(paste("tpr: P(", hat(y), "=1|y=1)", sep = ""))) + 
    #geom_label(data = data.frame(x = 0.9, y = 0.1, text = paste0("AUC: ",round(auc,3))), aes(x = x, y = y, label = text)) +
    theme_my 
  
  # Stratified distribution of predictions (plot similar to plot_distr_metr)
  p_distr = ggplot(data = df.distr, aes_string("yhat")) +
    geom_histogram(aes(y = ..density.., fill = y), bins = 40, position = "identity") +
    geom_density(aes(color = y)) +
    scale_fill_manual(values = alpha(colors, .2), name = "Target (y)") + 
    scale_color_manual(values = colors, name = "Target (y)") +
    labs(title = "Predictions", x = expression(paste("Prediction (", hat(y),")", sep = ""))) +
    guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))
  tmp = ggplot_build(p_distr)
  p.inner = ggplot(data = df.distr, aes_string("y", "yhat")) +
    geom_boxplot(aes_string(color = "y")) +
    coord_flip() +
    scale_y_continuous(limits = c(min(tmp$data[[1]]$xmin), max(tmp$data[[1]]$xmax))) +
    scale_color_manual(values = colors, name = "Target") +
    theme_void() +
    theme(legend.position = "none")
  p_distr = p_distr + 
    scale_y_continuous(limits = c(-tmp$layout$panel_ranges[[1]]$y.range[2]/10, NA)) +
    theme_my +
    annotation_custom(ggplotGrob(p.inner), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0) 
  
  # Gain + Lift: NOT CORRECT for all data due to undersampling -> need to oversample positives (y_holdout) and ...  
  # ... adapt predictions (yhat_holdout)
  tmp = ifelse(y_holdout == "Y", 1, 0)[order(yhat_holdout, decreasing = TRUE)] 
  df.gain = data.frame("x" = 100*(1:length(yhat_holdout))/length(yhat_holdout),
                       "gain" = 100*cumsum(tmp)/sum(tmp))
  df.gain$lift = df.gain$gain/df.gain$x  
  p_gain = ggplot(df.gain) +
    geom_polygon(aes(x, y), data.frame(x = c(0,100,100*sum(tmp)/length(tmp)), y = c(0,100,100)), 
                 fill = "grey90", alpha = 0.5) +
    geom_line(aes(x, gain), df.gain, color = "blue", size = .5) +
    scale_x_continuous(limits = c(0,100), breaks = seq(0,100,10)) + 
    labs(title = "Gain", x = "% Samples Tested", y = "% Samples Found") +
    theme_my 
  p_lift = ggplot(df.gain) +
    geom_line(aes(x, lift), df.gain, color = "blue", size = .5) +
    scale_x_continuous(limits = c(0,100), breaks = seq(0,100,10)) + 
    labs(title = "Lift", x = "% Samples Tested", y = "Lift") +
    theme_my 
  
  # Calibrate:  CORRECT also for all data as invariant to undersampling 
  df.calib = calibration(y~yhat, data.frame(y = y_holdout, yhat = 1 - yhat_holdout), cuts = 5)$data
  p_calib = ggplot(df.calib, aes(midpoint, Percent)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +  
    geom_abline(intercept = 0, slope = 1, color = "grey") + 
    scale_x_continuous(limits = c(0,100), breaks = seq(10,90,20)) + 
    scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
    labs(title = "Calibration", x = "Midpoint Predicted Event Probability", y = "Observed Event Percentage") +
    theme_my 
  
  # Precision Recall: NOT CORRECT for all data due to undersampling -> need to oversample positives (y_holdout) and ...  
  # ... adapt predictions (yhat_holdout)
  p_precrec = ggplot(df.precrec, aes(rec, prec)) +
    geom_line(color = "blue", size = .5) +
    geom_point(aes(x = ), df.precrec[i.alpha,], color = "red", size = 0.8) +
    scale_x_continuous(breaks = seq(0,1,0.1)) +
    labs(title = "Precision Recall Curve", x = expression(paste("recall=tpr: P(", hat(y), "=1|y=1)", sep = "")),
         y = expression(paste("precision: P(y=1|", hat(y), "=1)", sep = ""))) +
    theme_my 
  p_prec = ggplot(df.precrec, aes(x, prec)) +
    geom_line(color = "blue", size = .5) +
    geom_point(aes(x, prec), df.precrec[i.alpha,], color = "red", size = 0.8) +
    scale_x_continuous(breaks = seq(0,100,10)) +
    labs(title = "Precision", x = "% Samples Tested",
         y = expression(paste("precision: P(y=1|", hat(y), "=1)", sep = ""))) +
    theme_my
  
  # Condusion Matrix: NOT CORRECT for all data due to undersampling
  conf_obj = confusionMatrix(ifelse(yhat_holdout > 0.5,"Y","N"), y_holdout)
  df.confu = as.data.frame(conf_obj$table)
  p_confu = ggplot(df.confu, aes(Prediction, Reference)) +
    geom_tile(aes(fill = Freq)) + 
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_y_discrete(limits = rev(levels(df.confu$Reference))) +
    labs(title = paste0("Confusion Matrix (Accuracy = ", round(conf_obj$overall["Accuracy"], 3), ")")) +
    theme_my 

  # Plot
  plots = list(p_roc, p_confu, p_distr, p_calib, p_gain, p_lift, p_precrec, p_prec)
  ggsave(outpdf, marrangeGrob(plots, ncol = ncols, nrow = nrows, top = NULL), width = w, height = h)
}



## Variable Importance
plot_variableimportance = function(outpdf, vars, fit = fit.gbm, l.boot = NULL, 
                                   ncols = 5, nrows = 2, w = 18, h = 12) {
  # Group importances
  df.tmp = varImp(fit)$importance %>% 
    mutate(variable = rownames(varImp(fit)$importance)) %>% 
    filter(variable %in% vars) %>% 
    arrange(desc(Overall)) %>% 
    mutate(color = cut(Overall, c(-1,10,50,100), labels = c("low","middle","high"))) 
  
  # Plot
  p = ggplot(df.tmp) +
    geom_bar(aes(variable, Overall, fill = color), stat = "identity") +
    scale_x_discrete(limits = rev(df.tmp$variable)) +
    scale_fill_manual(values = c("blue","orange","red")) +
    labs(title = paste0("Top ", min(topn, length(predictors))," Important Variables (of ", length(predictors), ")"), 
         x = "", y = "") +
    coord_flip() +
    #geom_hline(yintercept = c(10,50), color = "grey", linetype = 2) +
    theme_my + theme(legend.position = "none")
  
  # Bootstrap lines
  if (!is.null(l.boot)) {
    df.tmpboot = map_df(l.boot, ~ {
      df = varImp(.)$importance
      df$variable = rownames(df)
      df
    } , .id = "run")
    p = p + geom_line(aes(variable, Overall, group = run), df.tmpboot, color = "grey", size = 0.1) +
      geom_point(aes(variable, Overall, group = run), df.tmpboot, color = "black", size = 0.3)
  }
  ggsave(outpdf, p, width = 8, height = 6)
} 



## Partial Depdendence
plot_partialdependence = function(outpdf, vars, df = df.interpret, fit = fit.gbm, l.boot = NULL, CI = FALSE,
                                  ylim = c(0,1), ncols = 5, nrows = 2, w = 18, h = 12) {

  # Final model
  model = fit$finalModel
  
  # Derive offset
  offset = model$initF - plot(model, i.var = "INT", type = "link", return.grid = TRUE)[1,"y"]
  
  # Plot
  plots = map(vars, ~ {
    #.=vars[2]
    # Plot data (must be adapted due to offset of plot(gbm) and possible undersampling)
    
    df.plot = plot(model, i.var = ., type = "link", return.grid = TRUE) #get plot data on link level
    p_sample = 1 - (1 - 1/(1 + exp(df.plot$y + offset))) #add offset and calcualte dependence on response level
    df.plot$y = prob_samp2full(p_sample, b_sample, b_all) #switch to correct probability of full data
    
    if (is.factor(df[[.]])) {
      # Width of bars correspond to freqencies
      tmp = table(df[,.])
      df.plot$width = as.numeric(tmp[df.plot[[.]]])/max(tmp)
      
      # Plot for a nominal variable
      p = ggplot(df.plot, aes_string(., "y")) +
        geom_bar(stat = "identity",  width = df.plot$width, fill = "grey", color = "black") +
        labs(title = ., x = "", y = expression(paste("P(", hat(y), "=1)"))) +
        scale_y_continuous(limits = ylim) +
        theme_my         
    } else {
      # Plot for a metric variable
      df.rug = data.frame(q = quantile(df[,.], prob = seq(.05, .95, .1)), y = 0)
      p = ggplot(df.plot, aes_string(., "y")) +
        geom_line(stat = "identity", color = "black") +
        geom_rug(aes(q, y), df.rug, sides = "b", col = "red") +
        labs(title = ., x = "", y = expression(paste("P(", hat(y), "=1)"))) +
        scale_y_continuous(limits = ylim) +
        theme_my      
    }
    
    # Add Bootstrap lines and dots
    if (!is.null(l.boot)) {
      
      # Do the same as above for each bootstrapped model
      varactual = .
      df.tmpboot = map_df(l.boot, ~ {
        model_boot = .$finalModel
        offset_boot = model_boot$initF - plot(model_boot, i.var = "INT", type = "link", return.grid = TRUE)[1,"y"]
        df.plot_boot = plot(model_boot, i.var = varactual, type = "link", return.grid = TRUE)
        p_sample_boot = 1 - (1 - 1/(1 + exp(df.plot_boot$y + offset_boot)))
        df.plot_boot$y = prob_samp2full(p_sample_boot, b_sample, b_all)
        df.plot_boot
      } , .id = "run")
      
      df.plot = left_join(df.plot, df.tmpboot %>% group_by_(varactual) %>% summarise(sd = sd(y))) %>% 
        mutate(ymin = y - 1.96*sd, ymax = y + 1.96*sd)
      
      if (is.factor(df[,.])) {
        if (CI == TRUE) {
          p = p + 
            geom_errorbar(aes_string(., ymin = "ymin", ymax = "ymax"), data = df.plot, size = 0.5, width = 0.25)
          
        } else {
          p = p + 
            geom_line(aes_string(., "y", group = "run"), df.tmpboot, color = "lightgrey", size = 0.1) +
            geom_point(aes_string(., "y", group = "run"), df.tmpboot, color = "black", size = 0.3)
          }
      } else {
        if (CI == TRUE) {
          p = p +             
            geom_ribbon(aes_string(., ymin = "ymin", ymax = "ymax"), data = df.plot, alpha = 0.2)
        } else {
          p = p + 
            geom_line(aes_string(., "y", group = "run"), df.tmpboot, color = "lightgrey") +
            geom_line(aes_string(., "y"), df.plot, stat = "identity", color = "black") #plot black line again
        }
      }
    }
    # Add (prior) base probability
    p + geom_hline(yintercept = b_all, linetype = 3)
  })
  ggsave(outpdf, marrangeGrob(plots, ncol = ncols, nrow = nrows, top = NULL), width = w, height = h)
}



## Plot Interactiontest
plot_interactiontest = function(outpdf, vars, df = df.interpret, fit = fit.gbm, l.boot = NULL, 
                                ncols = 4, w = 18, h = 12) {
  
  # Derive interaction matrix for topn important variables
  pred_inter = setdiff(vars,"INT") #remove INT from testing variables
  k = length(pred_inter)
  m.inter = matrix(0, k, k)
  for (i in 1:(k - 1)) {
    for (j in (i + 1):k) {
      # Interaction Test
      m.inter[i,j] = interact.gbm(fit$finalModel, df[pred_inter], pred_inter[c(i,j)], fit$finalModel$tuneValue$n.trees)
      m.inter[j,i] = m.inter[i,j]
    }
  }
  colnames(m.inter) = pred_inter
  rownames(m.inter) = pred_inter
  m.inter[is.na(m.inter)] = 0
  
  
  ## Plot in correlation matrix style
  df.inter = as.data.frame(m.inter) %>% 
    mutate(rowvar = rownames(m.inter)) %>% 
    gather(key = colvar, value = inter, -rowvar)
  p = ggplot(df.inter, aes(rowvar, colvar)) +
    geom_tile(aes(fill = inter)) + 
    geom_text(aes(label = round(inter, 2))) +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_x_discrete(limits = rownames(m.inter)) + 
    scale_y_discrete(limits = rev(rownames(m.inter))) +
    labs(title = "Interaction", fill = "", x = "", y = "") +
    theme_my +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  ggsave(outpdf, p, width = w, height = h)
  
  
  
  if (!is.null(l.boot)) {
    # Do the same as above for each bootstrapped model and collect
    df.inter_boot = map_df(l.boot, ~ {
      for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
          # Interaction Test
          m.inter[i,j] = interact.gbm(.$finalModel, df[pred_inter], pred_inter[c(i,j)], .$finalModel$tuneValue$n.trees)
          m.inter[j,i] = m.inter[i,j]
        }
      }
      m.inter[is.na(m.inter)] = 0
      
      df.inter = as.data.frame(m.inter) %>% 
        mutate(rowvar = rownames(m.inter)) %>% 
        gather(key = colvar, value = inter, -rowvar)
      df.inter
    }, .id = "run")
    
    
    # Same plot but now facetting
    p_boot = ggplot(df.inter_boot, aes(rowvar, colvar)) +
      geom_tile(aes(fill = inter)) + 
      geom_text(aes(label = round(inter, 2))) +
      scale_fill_gradient(low = "white", high = "blue") +
      scale_x_discrete(limits = rownames(m.inter)) + 
      scale_y_discrete(limits = rev(rownames(m.inter))) +
      labs(title = "Interaction per Bootstrap Run", fill = "", x = "", y = "") +
      theme_my +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
      facet_wrap( ~ run, ncol = ncols)
  }
  ggsave(paste0(str_split(outpdf,".pdf", simplify = TRUE)[1,1],"_boot.pdf"), p_boot, width = w, height = h)
}




## Plot interactions of m.gbm
plot_inter = function(outpdf, vars = inter, df = df.interpret, fit = fit.gbm, 
                      ylim = c(0,1), w = 12, h = 8) {
  # outpdf="./output/interaction1.pdf"; vars=inter1; df=df.interpret; fit=fit; w=12; h=8; ylim = c(0,.3)
  
  # Final model
  model = fit$finalModel
  
  # Derive offset
  offset = model$initF - plot(model, i.var = "INT", type = "link", return.grid = TRUE)[1,"y"]
  
  # Marginal plots for digonal
  plots_marginal = map(vars, ~ {
    #.=vars
    # Get interaction data
    df.plot = plot(model, i.var = ., type = "link", return.grid = TRUE) #get plot data on link level
    p_sample = 1 - (1 - 1/(1 + exp(df.plot$y + offset))) #add offset and calcualte dependence on response level
    df.plot$y = prob_samp2full(p_sample, b_sample, b_all) #switch to correct probability of full data
    
    if (is.factor(df[[.]])) {
      # Width of bars correspond to freqencies
      tmp = table(df[,.])
      df.plot$width = as.numeric(tmp[df.plot[[.]]])/max(tmp)
      
      # Plot for a nominal variable
      p = ggplot(df.plot, aes_string(., "y", fill = .)) +
        geom_bar(stat = "identity",  width = df.plot$width, color = "black") +
        labs(title = ., x = "", y = expression(paste("P(", hat(y), "=1)"))) +
        scale_fill_manual(values = manycol) +
        scale_y_continuous(limits = ylim) +
        theme_my + 
        theme(legend.position = "none")
    } else {
      # Plot for a metric variable
      df.rug = data.frame(q = quantile(df[,.], prob = seq(.05, .95, .1)), y = 0)
      p = ggplot(df.plot, aes_string(., "y")) +
        geom_line(stat = "identity", color = "black") +
        geom_rug(aes(q, y), df.rug, sides = "b", col = "red") +
        labs(title = ., x = "", y = expression(paste("P(", hat(y), "=1)"))) +
        scale_y_continuous(limits = ylim) +
        theme_my      
    }
    p + geom_hline(yintercept = b_all, linetype = 3)
  })
  
  # Interaction plots 
  df.plot = plot(model, i.var = vars, type = "link", return.grid = TRUE) #get plot data on link level
  p_sample = 1 - (1 - 1/(1 + exp(df.plot$y + offset))) #add offset and calcualte dependence on response level
  df.plot$y = prob_samp2full(p_sample, b_sample, b_all) #switch to correct probability of full data
  # quantile(df$TT4, seq(0,1, length.out=100))
  # (max(df$TT4) - min(df$TT4))/99 + min(df$TT4)
  
  if (sum(map_lgl(df.plot[vars], ~ is.factor(.))) == 2) {
    # Mosaicplot for nominal-nominal interaction
    plots_inter = map(1:2, ~ { 
      if (.==2) vars = rev(vars)
      #tmp = table(df[vars[2]])
      #df.plot[[vars[1]]] = factor(df.plot[[vars[1]]], levels = rev(levels(df.plot[[vars[1]]])))
      ggplot(df.plot, aes_string(vars[2], "y", fill = vars[1])) +
        geom_bar(stat = "identity", position = "fill") + #, width = rep(tmp/max(tmp), 5)) +
        scale_fill_manual(values = manycol) +
        labs(y = "", x = "") +
        theme_my      
    })
  }  
  if (sum(map_lgl(df.plot[vars], ~ is.factor(.))) == 1) {
    # Grouped line chart for metric-nominal interaction
    p = ggplot(df.plot, aes_string(vars[1], "y", color = vars[2])) +
      geom_line(stat = "identity") +
      labs(y = expression(paste("P(", hat(y), "=1)"))) +
      scale_color_manual(values = manycol) +
      scale_y_continuous(limits = ylim*2) +
      guides(color = guide_legend(reverse = TRUE)) +
      theme_my      
    plots_inter = list(p,p)
  }   
  if (sum(map_lgl(df.plot[vars], ~ is.factor(.))) == 0) {
    # Grouped (by quantiles) line chart for metric-metric interaction
    plots_inter = map(1:2, ~ { 
      if (.==2) vars = rev(vars)
      val_near_quant =   map_dbl(quantile(df[[vars[2]]], seq(.05,.95,.1)), ~ {
        df.plot[[vars[2]]][which.min(abs(df.plot[[vars[2]]] - .))]})
      i.tmp = df.plot[[vars[2]]] %in% val_near_quant
      df.tmp = df.plot[i.tmp,]
      df.tmp[vars[2]] = factor( round(df.tmp[[vars[2]]],2) )
      
      ggplot(df.tmp, aes_string(vars[1], "y", color = vars[2])) +
        geom_line(stat = "identity") +
        labs(y = expression(paste("P(", hat(y), "=1)"))) +
        scale_color_manual(values = manycol) +
        scale_y_continuous(limits = ylim) +
        guides(color = guide_legend(reverse = TRUE)) +
        theme_my   
    })
  } 
  
  # Arrange plots
  plots = list(plots_marginal[[1]], plots_inter[[1]], plots_inter[[2]], plots_marginal[[2]])
  ggsave(outpdf, marrangeGrob(plots, ncol = 2, nrow = 2, top = NULL), width = w, height = h)
}




# Animated Interaction of 2 metric variables
plot_inter_active = function(outfile, vars = inter, df = df.interpret, fit = fit.gbm, duration = 15) {
  
  # Final model
  model = fit$finalModel
  
  # Derive offset
  offset = model$initF - plot(model, i.var = "INT", type = "link", return.grid = TRUE)[1,"y"]
  
  # Get interaction data
  df.plot = plot(model, i.var = vars, type = "link", return.grid = TRUE) #get plot data on link level
  p_sample = 1 - (1 - 1/(1 + exp(df.plot$y + offset))) #add offset and calcualte dependence on response level
  df.plot$y = prob_samp2full(p_sample, b_sample, b_all) #switch to correct probability of full data
  
  # Prepare 3d plot
  x = unique(df.plot[[vars[1]]])
  y = unique(df.plot[[vars[2]]])
  z = matrix(df.plot$y, length(x), length(y), byrow = FALSE)
  nrz = nrow(z)
  colcut = cut((z[-1, -1] + z[-1, -nrz] + z[-nrz, -1] + z[-nrz, -ncol(z)])/4, 100)
  
  # html Widget
  persp3d(x, y, z, col = col3d[colcut], phi = 30, theta = 50, axes = T, ticktype = 'detailed',
          xlab = vars[1], ylab = vars[2], zlab = "")
  writeWebGL(dir = file.path(paste0(outfile)), width = 1000)
  rgl.close()
  
  # animated gif
  open3d("windowRect" = 2*c(20,20,400,400))
  persp3d(x, y, z, col = col3d[colcut], phi = 30, theta = 50, axes = T, ticktype = 'detailed',
          xlab = vars[1], ylab = vars[2], zlab = "")
  movie3d(spin3d(axis = c(0,0,1), rpm = 2), duration = duration, convert = NULL, clean = TRUE, movie = "test",
          dir = paste0(outfile))
  rgl.close()
  
}



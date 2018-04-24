#.libPaths("");setwd('C:\\projects\\curveHTML'); source("curvefitfunc.R"); library(drc); library(caTools); library(ggplot2); library(plotly); library(gsubfn); library(openxlsx); library(xtable); library(parallel); library(doSNOW); drug_annot_tbl_full = read.xlsx('FA2A annotations.xlsx'); DSS_typ = 2; HTMLreport = T; shortReport = T

lapply(c("drc", "caTools", "ggplot2", "gsubfn", "gtools", "data.table", "doSNOW"), library, character.only = !0)
library(RMySQL)
library(shinytoastr)
options(stringsAsFactors = F)
dss = compiler::cmpfun(dss)
headerCurvePath <- "/projects/breeze/code/DSRT3/headercurve.txt"

################################################################################################### 
# Calculate IC50/EC50/DSS. (...) .LIST VERSION OF FUNC. 

CALC_IC50_EC50_DSS <- compiler::cmpfun(function(i, drug_wells_, xpr_tbl, DSS_typ, readoutCTX = F)
{
  
  tryCatch({
    gc(T);
    TEC50 = ifelse(readoutCTX, "TC50", "EC50"); drug_wells = drug_wells_[i,];
    
    #find indices of wells with drugs 
    idx_filt <- xpr_tbl$ProductId %in% drug_wells$ProductId & xpr_tbl$ProductName %in% drug_wells$ProductName
    #extract inhib. and viab. for wells with drugs in current plate
    inhibition = inhibition2 <- xpr_tbl$inhibition_percent[idx_filt]; viability2 = 100 - inhibition2; # with 2 used for ploting of real values.
    
    # if there are identical values in inhibition, add a bit noise
    if(all(inhibition <= 0)) inhibition <- rep(0, length(inhibition)) 
    if(any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition; 
    
    viability = 100-inhibition; believe_ = xpr_tbl$believe[idx_filt][[1]];
    
    # extract concentrations, unique drug names and product ids for wells with drugs in current plate
    dose <- as.numeric(xpr_tbl$Concentration[idx_filt])
    drug_name <- unique(as.character(xpr_tbl$ProductName)[idx_filt])
    product_id <- unique(as.character(xpr_tbl$ProductId)[idx_filt])
    
    #combine the data and sort by dose.
    mat_tbl <- data.frame(inhibition,dose,logconc = log10(dose),viability, inhibition2, viability2)
    mat_tbl <- mat_tbl[order(mat_tbl[,"dose"]),]  
    
    print(paste0(product_id,",   ",drug_name));print(mat_tbl);
    
    
    if(DSS_typ == "AUC"){
      
     mat_tbl$indexx = 1:nrow(mat_tbl)
      model = approx(x = mat_tbl$indexx, y = mat_tbl$inhibition2, xout = seq(1,nrow(mat_tbl),length.out = 100), method="linear")
      loess_fit <- loess(y ~ x, model)
      model$y = predict(loess_fit)
      
      # #model = smooth.spline(mat_tbl$indexx, mat_tbl$inhibition2)
      # model = lm(inhibition2 ~ poly(indexx,length(mat_tbl$indexx)-1), mat_tbl)
      # model = predict(model, data.frame(indexx=seq(1,nrow(mat_tbl),length.out = 100))); 
      # model = data.frame(x = seq(1,nrow(mat_tbl),length.out = 100), y = as.numeric(model))
      # #model = predict(model, seq(1,nrow(mat_tbl),length.out = 100))
      # id <- order(model$x)
      #AUC <- round(sum(diff(model$x[id])*zoo::rollmean(model$y[id],2)) / 5, 2)
     # modelAUC = model; modelAUC$y[modelAUC$y<0]=0
      AUC <- round(sum(diff(model$x) * (head(model$y,-1)+tail(model$y,-1)))/2 / 5, 2)
      
      perInh <- t(matrix(mat_tbl[,"inhibition"],dimnames=
                           list(paste0(rep("D", length(mat_tbl[,"inhibition"])), 1:length(mat_tbl[,"inhibition"])))))
      IC50_dataframe <- data.frame(ID=product_id,DRUG_NAME=drug_name,ANALYSIS_NAME="IC50", IC50="",SLOPE="",MAX=max(model$y),MIN=min(model$y), 
                                   Min.Conc.tested=min(mat_tbl$dose),Max.Conc.tested=max(mat_tbl$dose), IC50_std_error="", perInh, GRAPH="", 
                                   DSS = as.numeric(AUC), sDSS = "", SE_of_estimate = "")
      EC50_dataframe <- data.frame(ID=product_id,DRUG_NAME=drug_name,ANALYSIS_NAME="EC50", EC50="",SLOPE="",MAX=100-max(model$y),MIN=100-min(model$y), 
                                   Min.Conc.tested=min(mat_tbl$dose),Max.Conc.tested=max(mat_tbl$dose),TEC50_std_error="",perInh, GRAPH="", 
                                   DSS = as.numeric(AUC), sDSS = "", SE_of_estimate = "")
      
            icpl <- ggplot2::ggplot(mat_tbl, aes(indexx, inhibition2)) + scale_x_continuous(breaks=1:nrow(mat_tbl),labels=mat_tbl$dose) +
        geom_point(color = "blue", size = 2.8) + geom_line(data = data.frame(x = model$x, y = model$y), aes(x, y), color="blue", size = 0.8) + 
        ggtitle(paste0(drug_name," (AUC:",AUC,")\n"))+
        theme(legend.title = element_text(size = 9)) + theme_bw() + labs(y = "response", x = "conc(nM)")  + 
        theme(plot.background = element_rect(fill = "transparent",colour = NA),
              panel.background =element_rect(fill = "transparent",colour = NA), plot.title = element_text(hjust = 0.5))

			  
      graphics.off()
      filename_ = file.path(getwd(), "www", "Results", "Curve_fits", "IC50", paste0(product_id,gsub("/",".",drug_name),"_IC50_curve_drug.png"))
      png(filename = filename_,width=190,height=190, bg = "transparent")
      print(icpl)
      dev.off()  
      
      ecpl <- ggplot2::ggplot(mat_tbl, aes(indexx, viability2)) + scale_x_continuous(breaks=1:nrow(mat_tbl),labels=mat_tbl$dose) +
        geom_point(color = "blue", size = 2.8) + geom_line(data = data.frame(x = model$x, y = 100-model$y), aes(x, y), color="blue", size = 0.8) + 
        ggtitle(paste0(drug_name," (AUC:",AUC,")\n"))+
        theme(legend.title = element_text(size = 9)) + theme_bw() + labs(y = "response", x = "conc(nM)")  +  ylim(-25, 125) +
        theme(plot.background = element_rect(fill = "transparent",colour = NA),
              panel.background =element_rect(fill = "transparent",colour = NA), plot.title = element_text(hjust = 0.5))
      
      
      graphics.off()
      png(filename = file.path(getwd(), "www", "Results", "Curve_fits", TEC50, paste0(product_id,gsub("/",".",drug_name),"_", TEC50,"_curve_drug.png")),width=190,height=190, bg = "transparent")
      print(ecpl)
      dev.off()  
      
      TEC50base64 <- gsub("\r?\n|\r", " ", base64::img(filename_))
      
      #return list with 3 nodes - 1 row for IC50 table and 1 row for EC50 table and EC50 image in base64
      list(IC50_dataframe, EC50_dataframe, TEC50base64, believe_ = T)
      
    }else if(nrow(mat_tbl) <= 3){
      
      print("Less than 3 rows... skipping...")
      NULL
    } else {
      
      ############################# 
      #############    IC50
      
      estimate_param <- tryCatch({drm(inhibition ~ logconc, data = mat_tbl, fct = LL.4(fixed = c(NA, NA, NA,NA),names = c("SLOPE","MIN","MAX","IC50")),logDose=10,control = drmc(errorm = F))}, 
                                 warning=function(w){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)},
                                 error=function(e){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)})
      # (extract and name coefficients)
      coef_estim <- coef(estimate_param); names(coef_estim) <- c("SLOPE","MIN","MAX","IC50")
      # see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4696819/
      coef_estim["SLOPE"] <- coef_estim["SLOPE"]*-1 
      
      # if curve decreases or IC50 is higher than max (i.e. IC50 is "outlier"), set IC50 to max conc.
      coef_estim["IC50"] <- ifelse(coef_estim["MAX"]<=coef_estim["MIN"] | coef_estim["IC50"]>max(mat_tbl$dose,na.rm=T), max(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
      # if IC50 is less than 0 set it to min. conc. and if even min. conc. < 0, then set IC50 to mean of all conc.
      coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,min(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
      coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,mean(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
      # similar to previous step but now compare log10(IC50) with log(min. conc.).
      coef_estim["IC50"] <- log10(coef_estim["IC50"])
      coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<min(mat_tbl$logconc),max(mat_tbl$logconc),coef_estim["IC50"])
      # if all inhib. < 0 set IC50 to max. log. conc !!!!! not obvious why!
      coef_estim["IC50"] <- ifelse(all(mat_tbl$inhibition<0),max(mat_tbl$logconc,na.rm=T),coef_estim["IC50"])
      #(Trying to fix curves that need outlier kickout)
      coef_estim["MIN"] <- 0; coef_estim["MAX"] <- max(mat_tbl$inhibition,na.rm=T)
      #(Fix off minimums) Find lowest inhibition value. If it is not in (0:100), fix it whether to 0 or 99.  
      min_lower <- ifelse(min(mat_tbl$inhibition,na.rm=T) > 0,min(mat_tbl$inhibition,na.rm=T),0)
      min_lower <- ifelse(min_lower >= 100,99,min_lower)
      #similar to previous step but for MAX
      coef_estim["MAX"] <- ifelse(coef_estim["MAX"]>100,100,coef_estim["MAX"])
      coef_estim["MAX"] <- ifelse(coef_estim["MAX"]<0,100,coef_estim["MAX"])
      #max_lower and max_upper - lower and upper bounds for 'nl2sol' algorithm in nonlinear least-squares
      max_lower <- ifelse(max(mat_tbl$inhibition,na.rm=T)>100,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T))
      max_lower <- ifelse(max_lower < 0,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T))
      max_lower <- ifelse(max_lower < 0,0,max_lower)
      max_lower <- ifelse(max_lower > 100,100,max_lower)
      #(Fix upper maximum for negative slopes)
      run_avg <- caTools::runmean(mat_tbl$inhibition, 10)
      max_upper <- ifelse(any(run_avg[-nrow(mat_tbl)]>run_avg[nrow(mat_tbl)]),max(mat_tbl$inhibition[run_avg>run_avg[nrow(mat_tbl)]]),coef_estim["MAX"])
      max_upper <- ifelse(any(mat_tbl$inhibition > max_upper),mean(mat_tbl$inhibition[mat_tbl$inhibition > max_upper])+5,max_upper)
      max_upper <- ifelse(max_upper < 0,coef_estim["MAX"],max_upper)
      max_upper <- ifelse(max_upper > 100,100,max_upper) #coef_estim["MAX"]
      max_upper <- ifelse(max_lower > max_upper,coef_estim["MAX"],max_upper)
      # left it as it was, just rewritten a bit (ALEKS). not clear how values 25, 60 and 5 are chosen. 
      mean_inh_last = mean(tail(mat_tbl$inhibition,2),na.rm=T)
      if(mean_inh_last < 60) {
        if(mean_inh_last > 25) coef_estim["IC50"] <- mean(mat_tbl$logconc,na.rm=T)
        else if(mean_inh_last < 25) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)}
      if(mean(mat_tbl$inhibition[1:3],na.rm=T)<5) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)
      #add a bit of positive noise to MAX if it is the same as MIN. 
      if(unname(coef_estim["MIN"]) == unname(coef_estim["MAX"])) coef_estim["MAX"] <- coef_estim["MAX"] + 0.001
      
      #adaptive nonlinear Least-Squares algorithm NL2SOL to estimate parameters.
      nls_result_ic50_old <- function(){
        tryCatch({
          nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", 
              start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),
              lower=list(SLOPE=0.1,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
              upper=list(SLOPE=2.5,MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
              control=list(warnOnly=T,minFactor = 1/2048))
        }, error = function(e) {
          
          # allows higher residual sum-of-squares
          minpack.lm::nlsLM(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl,
                            start=list(SLOPE=1, MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),
                            lower=c(SLOPE=0.1, MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
                            upper=c(SLOPE=2.5, MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)))
        })
      } 
      
      # IC50 first
      nls_result_ic50 <- nls_result_ic50_old();
      
      # IC50 second
      nls_result_ic50_2 <- tryCatch({
        # allows higher residual sum-of-squares
        nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", 
            start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]], IC50=median(mat_tbl$logconc)),
            lower=list(SLOPE=0.1,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
            upper=list(SLOPE=2.5,MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
            control=list(warnOnly=T,minFactor = 1/2048))
      },warning = function(w) {
        nls_result_ic50_old()
      },error = function(e) {
        nls_result_ic50_old()
      })
	  
	  #element (4, 4) is zero, so the inverse cannot be computed
      nls_result_ic50 = tryCatch({summary(nls_result_ic50); nls_result_ic50},error=function(e){nls_result_ic50_2})
      
      #Calculate the standard error scores
      sumIC50 = list(summary(nls_result_ic50), summary(nls_result_ic50_2))
      
      ic50std_resid <- round(sqrt(sum((sumIC50[[1]]$residuals)^2)/(length(sumIC50[[1]]$residuals)-1)),1);
      ic50std_resid2 <- round(sqrt(sum((sumIC50[[2]]$residuals)^2)/(length(sumIC50[[2]]$residuals)-1)),1);
      
      # continue with the best
      switch_ = which.min(c(ic50std_resid, ic50std_resid2))
      nls_result_ic50 = list(nls_result_ic50, nls_result_ic50_2)[[switch_]]
      
      
      #if SLOPE <= 0.2, decrease IC50, change lower bound for SLOPE to 0.1 and repeat.
      if(coef(nls_result_ic50)["SLOPE"] <= 0.2)
      {
        if(mean_inh_last > 60)
          coef_estim["IC50"] <- min(mat_tbl$logconc,na.rm=T)
        nls_result_ic50 <- nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port",
                               start=list(SLOPE=1, MIN=unname(coef_estim["MIN"]),MAX=unname(coef_estim["MAX"]),IC50=unname(coef_estim["IC50"])),
                               lower=list(SLOPE=0.1,MIN=min_lower,MAX=max_lower,IC50=min(mat_tbl$logconc)),
                               upper=list(SLOPE=2.5, MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
                               control=list(warnOnly=T,minFactor = 1/2048))
      }
      
      #Calculate the standard error scores
      sumIC50 = summary(nls_result_ic50); 
      ic50std_Error <- sumIC50$coefficients["IC50","Std. Error"]; #tec50std_Error <- sumTEC50$coefficients["TEC50","Std. Error"]
      ic50std_resid <- round(sqrt(sum((sumIC50$residuals)^2)/(length(sumIC50$residuals)-1)),1);
      max_signal <- max(mat_tbl$dose,na.rm=T); min_signal <- min(mat_tbl$dose,na.rm=T)
      
      #############################  
      #############   Final modification & STD error
      
      #prepare final data and convert IC50 back from log scale (inverse)
      coef_ic50 <- coef(nls_result_ic50)[c("IC50", "SLOPE","MAX","MIN")]; coef_ic50["IC50"] <- 10^coef_ic50["IC50"]
      #(Fix ic50 for curves in wrong direction)
      coef_ic50["IC50"] <- ifelse(coef_ic50["SLOPE"]<0,max_signal,coef_ic50["IC50"])
      #(Fix based on MAX)
      coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<0,max_signal,coef_ic50["IC50"])
      coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<10,max_signal,coef_ic50["IC50"])
      coef_ic50["MAX"] <- ifelse(coef_ic50["MAX"]<0,0,coef_ic50["MAX"])
      #(Fix over sensitive drugs)
      coef_ic50["IC50"] <- ifelse(all(c(max(mat_tbl$inhibition,na.rm=T),min(mat_tbl$inhibition,na.rm=T))>50),min_signal,coef_ic50["IC50"])
      
      
      # for ploting
      x <- seq(min(mat_tbl$logconc),max(mat_tbl$logconc), length=100)
      yic <- predict(nls_result_ic50, data.frame(logconc=x))
      
      
      ##average replicates
      mat_tblCp <- mat_tbl[, c("inhibition", "dose")]
      cols_ <- colnames(mat_tblCp)[!grepl("inhibition", colnames(mat_tblCp))] # columns which should be equal to average PI
      X <- as.data.table(mat_tblCp)
      mat_tblCp <- as.data.frame(X[,list(inhibition = mean(inhibition)),cols_], stringAsFactors = !1)
      
      
      perInh <- t(matrix(mat_tblCp[,"inhibition"],dimnames=
                           list(paste0(rep("D", length(mat_tblCp[,"inhibition"])), 1:length(mat_tblCp[,"inhibition"])))))
      
      coef_tec50 = coef_ic50; 
      coef_tec50["IC50"] <- ifelse(coef_tec50["MAX"] > 25, coef_tec50["IC50"], max(mat_tbl$dose,na.rm=T))
      if(readoutCTX){
        names(coef_tec50) <- c("TC50","SLOPE","MAX","MIN"); ytec <- yic; perViaTox <- perInh;
      } else{
        names(coef_tec50) <- c("EC50","SLOPE","MAX","MIN");
        coef_tec50["SLOPE"] = -1 * coef_tec50["SLOPE"]; # min - 0, max - 77 in ec50 it is max - 100, min - 23
        tmp = coef_tec50["MAX"]; coef_tec50["MAX"] = 100 - coef_tec50["MIN"]; coef_tec50["MIN"] = 100 - tmp; ytec <- 100 - yic;
        perViaTox <- 100 - perInh;
      }
      
      
      ############################# 
      #############    DSS
      
      dss_score <- round(as.numeric(dss(coef_ic50["IC50"],coef_ic50["SLOPE"],coef_ic50["MAX"],min_signal,max_signal, DSS.type=as.integer(DSS_typ))),1);
      coef_ic50 <- c(coef_ic50,Min.Conc.tested=min_signal,Max.Conc.tested=max_signal,IC50_std_error=ic50std_Error)
      coef_tec50 <- c(coef_tec50,Min.Conc.tested=min_signal,Max.Conc.tested=max_signal,TEC50_std_error=ic50std_Error)
      
      
      #dataframe for IC50
      IC50_dataframe <- data.frame(ID=product_id,DRUG_NAME=drug_name,ANALYSIS_NAME="IC50", t(as.matrix(coef_ic50)), perInh,
                                   GRAPH=NA, DSS = as.numeric(dss_score), sDSS = "", SE_of_estimate = as.numeric(ic50std_resid))
      #dataframe for EC50
      TEC50_dataframe <- data.frame(ID=product_id,DRUG_NAME=drug_name,ANALYSIS_NAME=TEC50,t(as.matrix(coef_tec50)), perViaTox, 
                                    GRAPH=NA, DSS = as.numeric(dss_score), sDSS = "", SE_of_estimate = as.numeric(ic50std_resid))
      
      #round by 2 dex. all the numeric colums
      numeric_cols <- sapply(IC50_dataframe, is.numeric); IC50_dataframe[,numeric_cols] <- round(IC50_dataframe[,numeric_cols],1)
      numeric_cols <- sapply(TEC50_dataframe, is.numeric); TEC50_dataframe[,numeric_cols] <- round(TEC50_dataframe[,numeric_cols],1)
      
      # plot IC50
      #mat_tbl$inhibition = xpr_tbl$inhibition_percent[idx_filt]; # if we have all values < 0, they will be replaced
      #mat_tbl$viability = 100 - mat_tbl$inhibition;  # we are replacing them back here.
      icpl <- ggplot2::ggplot(mat_tbl, aes(logconc, inhibition2)) + scale_x_continuous(breaks=mat_tbl$logconc,labels=mat_tbl$dose) +
        geom_point(color = "blue", size = 2.8) + geom_line(data = data.frame(x = x, y = yic), aes(x, yic), color="blue", size = 0.8) +
        geom_vline(xintercept = log10(coef_ic50["IC50"]), colour="grey", size = 0.8) + ggtitle(paste0(drug_name," (dss:",dss_score,")\n"))+
        theme_bw() + labs(y = "% inhibition", x = "conc(nM)")  +  ylim(-25, 125) +
        geom_text(mapping=aes(x2,y2,label = text2), data=data.frame(x2=log10(coef_ic50["IC50"])*0.95, y2=115, text2="IC50"), color="grey", parse=T) +
        theme(plot.background = element_rect(fill = "transparent",colour = NA),
              panel.background =element_rect(fill = "transparent",colour = NA), plot.title = element_text(hjust = 0.5, size = 12.5))
      
      graphics.off()
      filename_ = file.path(getwd(), "www", "Results", "Curve_fits", "IC50", paste0(product_id,gsub("/",".",drug_name),"_IC50_curve_drug.png"))
      png(filename = filename_,width=190,height=190, bg = "transparent")
      print(icpl)
      dev.off()  
      
      # plot TEC50
      #y <- predict(nls_result_tec50, data.frame(logconc=x)) 
      #if(readoutCTX) {y <- 100 - y; mat_tbl$viability = 100 - mat_tbl$viability}
      if(readoutCTX) aes_ <- aes(logconc, inhibition2) else aes_ <- aes(logconc, viability2)
      ecpl <- ggplot2::ggplot(mat_tbl, aes_) + scale_x_continuous(breaks=mat_tbl$logconc,labels=mat_tbl$dose) +
        geom_point(color = "blue", size = 2.8) + geom_line(data = data.frame(x = x, y = ytec), aes(x, ytec), color="blue", size = 0.8) +
        geom_vline(xintercept = log10(coef_tec50[TEC50]), colour="grey", size = 0.8) + ggtitle(paste0(drug_name," (dss:",dss_score,")\n"))+
        theme_bw() + labs(y = ifelse(readoutCTX, "% toxicity", "% viability"), x = "conc(nM)") +  ylim(-25, 125) +
        geom_text(mapping=aes(x2,y2,label = text2), data=data.frame(x2=log10(coef_tec50[TEC50])*0.95, y2=115, text2=TEC50), color="grey", parse=T) +
        theme(plot.background = element_rect(fill = "transparent",colour = NA),
              panel.background =element_rect(fill = "transparent",colour = NA),
              plot.title = element_text(hjust = 0.5, size = 12.5)) 
      
      
      graphics.off()
      png(filename = file.path(getwd(), "www", "Results", "Curve_fits", TEC50, paste0(product_id,gsub("/",".",drug_name),"_", TEC50,"_curve_drug.png")),width=190,height=190, bg = "transparent")
      print(ecpl)
      dev.off()  
      
      TEC50base64 <- gsub("\r?\n|\r", " ", base64::img(filename_))
      
      # check believe
      #if(IC50_dataframe$DSS > 1){
      #  if(IC50_dataframe$SE_of_estimate > 40) believe_ = F; # if SE > 40
      #  if(IC50_dataframe[[paste0("D", length(mat_tbl$inhibition))]] - IC50_dataframe[[paste0("D", length(mat_tbl$inhibition)-1)]] < -25) believe_ = F; # if last is less effective than penultimate
      #  if(sumIC50$residuals[[1]] > 20 && sumIC50$residuals[[2]] > 20) believe_ = F; # if residuals for first 2 points more than 10
      #  #if(max(sumIC50$residuals) > 30) believe_ = F; # if any of individual points deviates more than 30
      #}
      
      resid_ = as.numeric(sumIC50$residuals); resp_ = as.numeric(perInh); cond = 0;
      if(sum(abs(resid_)>15)>1) {believe_ = !1; cond = 2};if(tail(resp_,2)[[1]]-tail(resp_,1) > 7 && tail(resp_,1)-tail(resp_,3)[[1]] > 7) {believe_ = !1; cond = 3}
      if(any(abs(resid_)>10) && (ic50std_resid>10)) {believe_ = !1; cond = 5};if(ic50std_resid>100) {believe_ = !1; cond = 6}
	  if(sum(abs(resid_)>10)>2){believe_ = !1; cond = 8};if(sum(abs(resid_)>10)>1 && any(abs(resid_)>15)) {believe_ = !1; cond = 9};
	  if(sum(resp_ < 2) >= (length(resp_)-1)) {believe_ = !0; cond = 1} 
      print(paste0(product_id,"   ",believe_,"   ",cond));
      
      #return list with 3 nodes - 1 row for IC50 table and 1 row for EC50 table and EC50 image in base64
      list(IC50_dataframe, TEC50_dataframe, TEC50base64, believe_)
    }
  }, error = function(e) {
    print(paste0("error in ", product_id, ", in ", xpr_tbl$screen_id[idx_filt][[1]]));
    print(e);
  })
})
# ID DRUG_NAME ANALYSIS_NAME IC50 SLOPE MAX MIN Min.Conc.tested Max.Conc.tested IC50_std_error D1 D2 D3 D4 D5 GRAPH DSS sDSS SE_of_estimate
# ID DRUG_NAME ANALYSIS_NAME EC50 SLOPE MAX MIN Min.Conc.tested Max.Conc.tested TEC50_std_error D1 D2 D3 D4 D5 GRAPH DSS sDSS SE_of_estimate
# TEC50base64


################################################################################################### 
# MAIN (BODY)

dirCur = getwd(); load(file.path(dirCur, "www" ,"RDA", "screen_table.rda")); 
screen_table$ProductId = gsub("/",".",screen_table$ProductId) # so that ProductIds not confused with pathes
screen_table$ProductId = as.character(sapply(screen_table$ProductId, function(i) if(grepl("FIMM",i)) gsub("[-].*","",i) else i));
screen_table$ProductName = gsub(":","-",screen_table$ProductName)
screen_table = screen_table[!grepl("FIMM133814|FIMM003726", screen_table$ProductId), ] # eliminate FIMM133814|FIMM003726
screen_table$Concentration[grepl("/", screen_table$Concentration)] = # for drug combi
  as.numeric(sapply(screen_table$Concentration[grepl("/", screen_table$Concentration)], function(i){mean(as.numeric(gsub(",", ".", (strsplit(i, "/")[[1]]))))}))


# create directories and dictionary
dir.create("./www/Results/Curve_fits/", recursive=T); dir.create("./www/Results/DSS/", recursive=T)
dictionary_ = c(); 
#browser();
# preparation of final tables
final_tbl_conc_drugs <- drug_annot_tbl_full[,c("ID_Drug", "Mechanism/Targets","Class.explained","DRUG_NAME")]
colnames(final_tbl_conc_drugs)[colnames(final_tbl_conc_drugs) == 'ID_Drug'] <- 'ID'
colnames(final_tbl_conc_drugs)[toupper(colnames(final_tbl_conc_drugs)) == 'DWELL'] <- 'Dwell'
final_tbl_conc_drugs$DRUG_NAME = gsub(":","-",final_tbl_conc_drugs$DRUG_NAME)

if(sDSS) final_tbl_sDSS <- c();

# create cluster and copy libs there
cl <- makeCluster(max(1, detectCores()), type = "SOCK",outfile = "./debug_outputs.txt")
registerDoSNOW(cl)
clusterExport(cl, c('dss'))
clusterEvalQ(cl, c(require(drc), require(caTools), require(ggplot2), require(gsubfn), require(data.table)))


for(screen in unique(screen_table$screen_id))
{
  #Load annotations files for this particular cell line.
  xpr_table <- screen_table[as.character(screen_table$screen_id) %in% screen,]; 
  readoutCTX = toupper(xpr_table$readout[[1]]) %in% c("CTX", "CTXG");   TEC50 = ifelse(readoutCTX, "TC50", "EC50");
  
  dir.create("./www/Results/Curve_fits/IC50", recursive = !0); dir.create(paste0("./www/Results/Curve_fits/", TEC50), recursive = !0);
  
  gc(T)
  
  ##average replicates
  #xpr_table <- xpr_table[, c("screen_id", "readout", "Plate", "ProductId", "ProductName", "Concentration", "inhibition_percent", "believe")]
  #cols_ <- colnames(xpr_table)[!grepl("inhibition_percent", colnames(xpr_table))] # columns which should be equal to average PI
  #X <- as.data.table(xpr_table)
  #xpr_table <- as.data.frame(X[,list(inhibition_percent = mean(inhibition_percent)),cols_], stringAsFactors = !1)
  
  #Use only wells with drugs 
  #drug_wells <- unique(as.character(xpr_table$ProductId)[!(as.character(xpr_table$ProductId) %in% c("","BzCl","empty","dmso", "DMSO","cells", NA, "NA"))])
  xpr_table <- xpr_table[!(toupper(as.character(xpr_table$ProductId)) %in% c("","BZCL","EMPTY","DMSO","CELLS", NA, "NA")),]
  drug_wells <- unique(xpr_table[c("ProductId", "ProductName")])
  
  print("drug_wells");
  print(drug_wells)
  
  saveRDS(xpr_table, file = paste0(dirCur, "/www/Results/Curve_fits/", screen, "XPR.RDS"))
  saveRDS(drug_annot_tbl_full, file = paste0(dirCur, "/www/Results/Curve_fits/", "drug_annot_tbl_full", "XPR.RDS"))
  
  
  
  ###########################
  # call parallel calculation of IC50/EC50/DSS
  #ic50_ec50_table =  mclapply(1:nrow(drug_wells),CALC_IC50_EC50_DSS, drug_wells_ = drug_wells, xpr_tbl = xpr_table, 
  #                            DSS_typ = DSS_typ, readoutCTX = readoutCTX, mc.cores = max(parallel::detectCores()-1,1))
  
  ic50_ec50_table = foreach(i = 1:nrow(drug_wells), .combine='rbind',  .multicombine=T) %dopar%
    CALC_IC50_EC50_DSS(i, drug_wells_ = drug_wells, xpr_tbl = xpr_table, DSS_typ = DSS_typ, readoutCTX = readoutCTX)
  
  
  saveRDS(ic50_ec50_table, file = paste0(dirCur, "/www/Results/Curve_fits/", screen, ".RDS"))
  # extract IC50, EC50 tables
  #ic50_ec50_table <- as.data.frame(ic50_ec50_table);   
  IC50_dataframe <- do.call(smartbind, sapply(seq_along(ic50_ec50_table[,1]), function(x) ic50_ec50_table[x, 1]))
  EC50_dataframe <- do.call(smartbind, sapply(seq_along(ic50_ec50_table[,2]), function(x) ic50_ec50_table[x, 2]))
  Base64img_ <- sapply(seq_along(ic50_ec50_table[,3]), function(x) ic50_ec50_table[x,3])
  believe_ <- sapply(seq_along(ic50_ec50_table[,4]), function(x) ic50_ec50_table[x,4])
  
  tomove_ <- c("GRAPH", "DSS", "sDSS", "SE_of_estimate"); # move these to the end. (so that D1..D9 are together)
  IC50_dataframe = IC50_dataframe[c(setdiff(names(IC50_dataframe), tomove_), tomove_)]; 
  EC50_dataframe = EC50_dataframe[c(setdiff(names(EC50_dataframe), tomove_), tomove_)]; 
  
  print("HIHI2");
  
  ICDSStable <- cbind(EC50_dataframe["ID"], rep(screen,nrow(EC50_dataframe)), 
                      EC50_dataframe[,c(TEC50, "DSS", "DRUG_NAME", "SLOPE","MAX", "Min.Conc.tested", "Max.Conc.tested", "TEC50_std_error")],
                      t(as.data.frame(Base64img_, stringsAsFactors = F)))
  
  colnames(ICDSStable)[2] <- "Experiment_id"; colnames(ICDSStable)[ncol(ICDSStable)] <- "GRAPH"; 
  final_tbl_conc_drugs = base::merge(final_tbl_conc_drugs, ICDSStable, by = c("ID", "DRUG_NAME"), all = T, suffixes=c("")); 
  colnames(final_tbl_conc_drugs) <- gsub("NA$", "", colnames(final_tbl_conc_drugs))
  
  
  #extract data from drug annot. table by ID
  drug_annot_tbl <- drug_annot_tbl_full[drug_annot_tbl_full$ID_Drug %in% IC50_dataframe$ID,]
  
   # get Plate and Well info from screen table
  info_ = merge(drug_annot_tbl[,c("ID_Drug","DRUG_NAME")], screen_table[,c("ProductId", "Plate", "DWell", "Concentration")], by.x = "ID_Drug", by.y = "ProductId", all = T)
  info_ = info_[!duplicated(info_), ]; 
  info_ = do.call(rbind, lapply(unique(info_$ID_Drug), function(dr_) {dr_ = info_[info_$ID_Drug == dr_,]; return(dr_[dr_$Concentration == max(dr_$Concentration),])}))
  info_$Concentration = NULL
  info_ = info_[match(drug_annot_tbl$ID_Drug, info_$ID_Drug),]
  drug_annot_tbl$Plate = info_$Plate; drug_annot_tbl$Dwell = info_$DWell;
  
  drug_annot_tblIC <- drug_annot_tbl[match(IC50_dataframe$ID, drug_annot_tbl$ID_Drug),c("Mechanism/Targets","Class.explained","High.phase/Approval.status",
                                                                                        "Res..code","Alias","activity.modifier","Active/inactive.in.clinic","Solvent","High.conc.(nM)","Plate","Dwell","InChI","ChEMBL.ID")]
  drug_annot_tblEC <- drug_annot_tbl[match(EC50_dataframe$ID, drug_annot_tbl$ID_Drug),c("Mechanism/Targets","Class.explained","High.phase/Approval.status",
                                                                                        "Res..code","Alias","activity.modifier","Active/inactive.in.clinic","Solvent","High.conc.(nM)","Plate","Dwell","InChI","ChEMBL.ID")]
  print("HIHI4");

  if(sDSS){
    # browser()
    #HealthyControls <- readRDS("final_tbl_conc_drugsHC_CTxG.rds");
    EC50_dataframe=merge(aaannoframe, EC50_dataframe, by=c("ID","DRUG_NAME"))
    EC50_dataframe$sDSS=(EC50_dataframe$DSS - EC50_dataframe[[xpo]])
    EC50_dataframe[[xpo]]<- NULL
    #EC50_dataframe=mergedoo
    newfile22=EC50_dataframe[,c("ID","DRUG_NAME","sDSS")]; colnames(newfile22)[3] = screen;
    if(is.null(final_tbl_sDSS)){ 
      final_tbl_sDSS = newfile22
    } else {
      final_tbl_sDSS = merge(final_tbl_sDSS, newfile22, by=c("ID","DRUG_NAME"), all=T)
    }
  }
  print("HIHI5");

  
  ################################
  # Check bad curves
  
#    load("/fs/projects/breeze/code/DSRT3/AutoFlag.RData"); library(ada);
#    dataOutCp = IC50_dataframe[,c("D1","D2","D3","D4","D5","SE_of_estimate","Min.Conc.tested","Max.Conc.tested","MAX","MIN","SLOPE","IC50")];
#   	dataOutCp$D1F = dataOutCp$D2F = dataOutCp$D3F = dataOutCp$D4F = dataOutCp$D5F = 0;
# 	for(i in 1:nrow(dataOutCp)){
# 	  aa = dataOutCp[i,]
# 	  fitted = aa$MAX + (aa$MIN - aa$MAX) / (1 + ((aa$Min.Conc.tested*(10**(0:4)))/aa$IC50)**(aa$SLOPE))
# 	  dataOutCp$D1F[i] = dataOutCp$D1[i]-fitted[[1]]; dataOutCp$D2F[i] = dataOutCp$D2[i]-fitted[[2]]; dataOutCp$D3F[i] = dataOutCp$D3[i]-fitted[[3]]; 
# 	  dataOutCp$D4F[i] = dataOutCp$D4[i]-fitted[[4]]; dataOutCp$D5F[i] = dataOutCp$D5[i]-fitted[[5]];
# 	}
# 	dataOutCp$Min.Conc.tested=dataOutCp$Max.Conc.tested=dataOutCp$MAX=dataOutCp$MIN=dataOutCp$IC50 = NULL
# 
# 	ada_ = rowMeans(cbind(
# 				   round(as.numeric(as.character(predict(model_, dataOutCp, type = "prob")[,2])),7),
# 				   round(as.numeric(as.character(predict(model2_, dataOutCp, type = "prob")[,2])),7),
# 				   round(as.numeric(as.character(predict(model3_, dataOutCp, type = "prob")[,2])),7),
# 				   round(as.numeric(as.character(predict(model4_, dataOutCp, type = "prob")[,2])),7),
# 				   round(as.numeric(as.character(predict(model5_, dataOutCp, type = "prob")[,2])),7),
# 				   round(as.numeric(as.character(predict(model6_, dataOutCp, type = "prob")[,2])),7),
# 				   round(as.numeric(as.character(predict(model7_, dataOutCp, type = "prob")[,2])),7)
# 				   ))
# 	ada_[ada_<.01]=0; ada_[ada_>=.01]=1;
# 	rm(list = c('model_','model2_','model3_','model4_','model5_','model6_','model7_'));
			  
  ############################# 
  #############    WRITE IC50/EC50 to .xlsx    (_DSRT_analysis_table_Rpipeline.xlsx)
  wb = openxlsx::createWorkbook();
  openxlsx::addWorksheet(wb = wb, sheetName = TEC50); openxlsx::addWorksheet(wb = wb, sheetName = "IC50"); 
  
  #write IC50 table and add images  
  #believe_ = c(unlist(believe_)); believe_ = as.numeric(ifelse(believe_==T, 1, 0));
  #believe_ = as.numeric(ada_);
  print("HIHI6");
  
  IC50_dataframe_full <- cbind(IC50_dataframe, drug_annot_tblIC, Dubious = 1)#believe_)
  print("HIHI7");
  openxlsx::writeDataTable(wb, sheet = "IC50", IC50_dataframe_full, colNames=T)
  openxlsx::conditionalFormatting(wb, "IC50", rows = 1:(nrow(IC50_dataframe_full)+1), cols = 1:35, rule="$AG1>0", type = "expression", style = openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"));
  print("HIHI8");
  G_ = which(colnames(IC50_dataframe_full)=="GRAPH")
  invisible(lapply(1:nrow(IC50_dataframe_full), function (i) {
    img_ <- paste0(dirCur,"/www/Results/Curve_fits/IC50/",IC50_dataframe_full$ID[i],gsub("/",".",IC50_dataframe_full$DRUG_NAME[i]), "_IC50_curve_drug.png");  
    openxlsx::insertImage(wb, sheet = "IC50", file = file.path(img_),
                          width = 700, height = 700, startRow = i+1, startCol = G_, units = "px", dpi = 360)}))
  #change style 
  openxlsx::setColWidths(wb, sheet = "IC50", cols = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, G_, G_+1, G_+2, G_+3), 
                         widths=c(16, 17, 11, 7.5, 10, 7, 7, 7, 7, 15.5, 6, 6, 6, 6, 6, 26.5, 8, 8, 8), ignoreMergedCells = F)
  openxlsx::setRowHeights(wb, sheet = "IC50", rows = 1:nrow(IC50_dataframe_full)+1, heights = 142.5)
  style_ = openxlsx::createStyle(halign = "center", valign = "center", wrapText = T)
  openxlsx::addStyle(wb, sheet = "IC50", style_, rows = 2:(nrow(IC50_dataframe_full)+1), cols = 1:45, gridExpand = T)
  #add drug annot. table to the right side after images 
  
  colnames(EC50_dataframe)[colnames(EC50_dataframe) == 'TEC50_std_error'] <- ifelse(readoutCTX, 'TC50_std_error', 'EC50_std_error');
  #write EC50 table and add images
  EC50_dataframe_full <- cbind(EC50_dataframe, drug_annot_tblEC)
  openxlsx::writeDataTable(wb, sheet = TEC50, EC50_dataframe_full, colNames =T)
  G_ = which(colnames(EC50_dataframe_full)=="GRAPH")
  invisible(lapply(1:nrow(EC50_dataframe_full), function (i) {
    img_ <- paste0(dirCur,"/www/Results/Curve_fits/",TEC50,"/",EC50_dataframe_full$ID[i],gsub("/",".",EC50_dataframe_full$DRUG_NAME[i]), "_", TEC50, "_curve_drug.png");  
    openxlsx::insertImage(wb, sheet = TEC50, file = file.path(img_),
                          width = 700, height = 700, startRow = i+1, startCol = G_, units = "px", dpi = 360)}))
  #change style 
  openxlsx::setColWidths(wb, sheet = TEC50, cols = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, G_, G_+1, G_+2, G_+3), 
                         widths=c(16, 17, 11, 7.5, 10, 7, 7, 7, 7, 15.5, 6, 6, 6, 6, 6, 26.5, 8, 8, 8), ignoreMergedCells = F)
  openxlsx::setRowHeights(wb, sheet = TEC50, rows = 1:nrow(EC50_dataframe_full)+1, heights = 142.5)
  style_ = openxlsx::createStyle(halign = "center", valign = "center",  wrapText = T)
  openxlsx::addStyle(wb, sheet = TEC50, style_, rows = 2:(nrow(IC50_dataframe_full)+1), cols = 1:45, gridExpand = T)
  

  
  openxlsx::saveWorkbook(wb, file = paste0(dirCur,"/www/Results/Curve_fits/", screen,"_DSRT_analysis_table_Rpipeline.xlsx"), overwrite = T)
  saveRDS(EC50_dataframe_full,file = paste0(dirCur,"/www/Results/Curve_fits/", screen,"_DSRT_analysis_table_Rpipeline.rds"))
  

    # EC50_dataframe_full$screen=screen
    # cpoxpo <<-EC50_dataframe_full
    # write.xlsx(cpoxpo,paste0(dirCur,"/www/Results/Curve_fits/", screen,"cpoxpo.xlsx"))
  
  unlink("./www/Results/Curve_fits/IC50", recursive=T, force = T)
  unlink(paste0("./www/Results/Curve_fits/",TEC50), recursive=T, force = T)
  
  dictionary_ = rbind(dictionary_, IC50_dataframe[,c("ID","DRUG_NAME")])
  
  dbPush <- function(){
    
    # connect to DB
    dbConnection <- function(){
      library(ROracle); drv <- dbDriver("Oracle"); Sys.setenv(TZ = "GMT"); Sys.setenv(ORA_SDTZ = "GMT")
      host <- "192.168.0.219"; port <- 1521; sid <- "rora"
      connect.string <- paste0("(DESCRIPTION=", "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))", "(CONNECT_DATA=(SID=", sid, ")))")
      dbConnect(drv, username = "breezeadmin", password = "fdsiQmR4u", dbname = connect.string)
    }
    
    tryCatch({
      
      con <<- dbConnection();
      
      #Extract drug ids for the current screen
      cv_drug_notation <- dbGetQuery(con,
                                     paste0("SELECT DRUG_NOTATION_ID,DESCRIPTION, NEW_FIMMID_AFTER_CHANGE FROM CV_DRUG_NOTATION_ID WHERE DRUG_NOTATION_STATUS_ID IN (1,2,3) AND DESCRIPTION IN ('",paste(unique(as.character(IC50_dataframe$ID)),collapse="','"),"')"))
      cv_product_drugs <- dbGetQuery(con, 
                                     paste0("SELECT PK_PRODUCT_NAME_ID,DRUG_NOTATION_ID,FIMM_PRODUCT_ID,DESCRIPTION FROM CV_PRODUCT_NAME WHERE DRUG_NOTATION_ID IN ('",paste(cv_drug_notation$DRUG_NOTATION_ID,collapse="','"),"')"))
      drug_annotation <- base::merge(cv_drug_notation,cv_product_drugs,by="DRUG_NOTATION_ID", all=F)[,c("DESCRIPTION.x","PK_PRODUCT_NAME_ID","FIMM_PRODUCT_ID","DESCRIPTION.y")]
      colnames(drug_annotation) <- c("ID","PRODUCT_NAME_ID","FIMM_PRODUCT_ID","DESCRIPTION")
      
      IC50_dataframe <- base::merge(drug_annotation,IC50_dataframe, by="ID", all = F);  print(IC50_dataframe[,c("ID","PRODUCT_NAME_ID","FIMM_PRODUCT_ID","DESCRIPTION")])
      IC50_dataframe$PK_CURVE_FITTING_ID <- ""; IC50_dataframe$ANALYSIS_ID <- 3; IC50_dataframe$STATUS_ID <- 1;
      IC50_dataframe$USER_STAMP=report.author;IC50_dataframe$TIME_STAMP=toupper(format(as.Date(Sys.Date(),"%d-%m-%Y"),format="%d-%b-%y"));IC50_dataframe$VERSION=1
      IC50_dataframe$SCREEN_ID=dbGetQuery(con, paste0("select PK_SCREEN_ID from BREEZEADMIN.SCREEN where IDENTIFIER = '",screen,"'"))$PK_SCREEN_ID
      EC50_dataframe <- base::merge(drug_annotation,EC50_dataframe, by="ID", all = F)	
      tc50 <- ifelse(any(grepl("TC50",colnames(EC50_dataframe))), !0, !1);
      EC50_dataframe$PK_CURVE_FITTING_ID <- ""; EC50_dataframe$ANALYSIS_ID <- ifelse(tc50,7,4); EC50_dataframe$STATUS_ID <- 1;
      EC50_dataframe$USER_STAMP=report.author;EC50_dataframe$TIME_STAMP=toupper(format(as.Date(Sys.Date(),"%d-%m-%Y"),format="%d-%b-%y"));EC50_dataframe$VERSION=1
      EC50_dataframe$SCREEN_ID=dbGetQuery(con, paste0("select PK_SCREEN_ID from BREEZEADMIN.SCREEN where IDENTIFIER = '",screen,"'"))$PK_SCREEN_ID
      DSS_dataframe <- IC50_dataframe; 
      
      IC50_dataframe <- IC50_dataframe[,c("PK_CURVE_FITTING_ID","SCREEN_ID","PRODUCT_NAME_ID","FIMM_PRODUCT_ID","ANALYSIS_ID","SLOPE","MIN","MAX","IC50","Min.Conc.tested","Max.Conc.tested","STATUS_ID","USER_STAMP","TIME_STAMP", "VERSION")]
      colnames(IC50_dataframe) <- c("PK_CURVE_FITTING_ID","SCREEN_ID","DRUG_ID","FIMM_DRUG_ID","ANALYSIS_TYPE_ID","SLOPE","ASYM_MIN","ASYM_MAX","RESULT_NUMERIC","CONC_TESTED_MIN","CONC_TESTED_MAX","STATUS_ID","USER_STAMP","TIME_STAMP", "VERSION")
      IC50_dataframe <- IC50_dataframe[!apply(IC50_dataframe[,c("SLOPE","ASYM_MIN","ASYM_MAX","RESULT_NUMERIC","CONC_TESTED_MIN","CONC_TESTED_MAX")],1,function(x)any(is.infinite(x))),]	
      
      EC50_dataframe <- EC50_dataframe[,c("PK_CURVE_FITTING_ID","SCREEN_ID","PRODUCT_NAME_ID","FIMM_PRODUCT_ID","ANALYSIS_ID","SLOPE","MIN","MAX",ifelse(tc50,"TC50","EC50"),"Min.Conc.tested","Max.Conc.tested","STATUS_ID","USER_STAMP","TIME_STAMP", "VERSION")]
      colnames(EC50_dataframe) <- c("PK_CURVE_FITTING_ID","SCREEN_ID","DRUG_ID","FIMM_DRUG_ID","ANALYSIS_TYPE_ID","SLOPE","ASYM_MIN","ASYM_MAX","RESULT_NUMERIC","CONC_TESTED_MIN","CONC_TESTED_MAX","STATUS_ID","USER_STAMP","TIME_STAMP", "VERSION")
      EC50_dataframe <- EC50_dataframe[!apply(EC50_dataframe[,c("SLOPE","ASYM_MIN","ASYM_MAX","RESULT_NUMERIC","CONC_TESTED_MIN","CONC_TESTED_MAX")],1,function(x)any(is.infinite(x))),]	
      
      DSS_dataframe$ANALYSIS_ID <- 1;
      DSS_dataframe <- DSS_dataframe[,c("PK_CURVE_FITTING_ID","SCREEN_ID","PRODUCT_NAME_ID","FIMM_PRODUCT_ID","ANALYSIS_ID","SLOPE","MIN","MAX","DSS","Min.Conc.tested","Max.Conc.tested","STATUS_ID","USER_STAMP","TIME_STAMP", "VERSION")]
      colnames(DSS_dataframe) <- c("PK_CURVE_FITTING_ID","SCREEN_ID","DRUG_ID","FIMM_DRUG_ID","ANALYSIS_TYPE_ID","SLOPE","ASYM_MIN","ASYM_MAX","RESULT_NUMERIC","CONC_TESTED_MIN","CONC_TESTED_MAX","STATUS_ID","USER_STAMP","TIME_STAMP", "VERSION")
      DSS_dataframe <- DSS_dataframe[!apply(DSS_dataframe[,c("SLOPE","ASYM_MIN","ASYM_MAX","RESULT_NUMERIC","CONC_TESTED_MIN","CONC_TESTED_MAX")],1,function(x)any(is.infinite(x))),]	
      
      dbWriteTable(con, "CURVE_FITTING", rbind(DSS_dataframe, IC50_dataframe, EC50_dataframe), 
                   row.names = F, overwrite = F, append = T, ora.number = T, schema = "BREEZEADMIN")
      
    }, 
    error = function(e) {message(e); print("I am failed at CF pushing")},
    finally = {dbDisconnect(con); print("CF push end")}
    )
  }
  if(exists("DBQCCF")) dbPush();
}

stopCluster(cl);

############################# 
#############    SAVE _dictionary.xlsx  (Merging_IC50_data_tables_dictionary.R in JP's code) and Final table (used for creation of DSS heatmaps and waterfall)

dictionary_ <- dictionary_[!duplicated(dictionary_[,1]),]
openxlsx::write.xlsx(dictionary_, file = paste0(dirCur,"/www/Results/Curve_fits/", make.names(paste0("IC50_table_DSS_format_",Sys.Date(),
                                                                                                 "_dictionary.xlsx"))), sheetName = "IC50", colNames = T)
final_tbl_conc_drugs = final_tbl_conc_drugs[apply(final_tbl_conc_drugs[,4:ncol(final_tbl_conc_drugs)], 1, function(x) !all(is.na(x))),];
DSS_cols = grep("DSS", colnames(final_tbl_conc_drugs)); i_ = 1:length(DSS_cols) # number of screens
final_tbl_conc_drugs = final_tbl_conc_drugs[!sapply(1:nrow(final_tbl_conc_drugs), function(i) all(is.na(final_tbl_conc_drugs[i,DSS_cols]))),]
save(final_tbl_conc_drugs, file = "final_tbl_conc_drugs.rds")
# write Original_DSS2
#screens_=na.omit(unique(final_tbl_conc_drugs[, grep("Experiment_id", colnames(final_tbl_conc_drugs))]))
screencols_ = final_tbl_conc_drugs[, grep("Experiment_id", colnames(final_tbl_conc_drugs))]
if(!is.null(dim(screencols_))) screens_ = as.character(apply(screencols_,2, function(x) na.omit(unique(x))[[1]])) else screens_ = as.character(na.omit(unique(screencols_))[[1]])
final_tbl_conc_drugs_DSS <- final_tbl_conc_drugs[,c(1,grep("DRUG_NAME", colnames(final_tbl_conc_drugs))[[1]],DSS_cols)]
colnames(final_tbl_conc_drugs_DSS) <- c("ID", "DRUG_NAME", screens_)
#final_tbl_conc_drugs_DSS <- final_tbl_conc_drugs_DSS[ !apply(final_tbl_conc_drugs_DSS[,c(2, 2+i_)], 1, function(x) all(is.na(x))), ]

openxlsx::write.xlsx(final_tbl_conc_drugs_DSS, file = paste0(dirCur,"/www/Results/DSS/","Original_DSS",as.character(DSS_typ),
                                                             "_",Sys.Date(),".xlsx"), sheetName = "DSS", startRow = 1)
if(sDSS) openxlsx::write.xlsx(final_tbl_sDSS, file = paste0(dirCur,"/www/Results/DSS/","Selective_DSS",as.character(DSS_typ),
                                                            "_",Sys.Date(),".xlsx"), sheetName = "DSS", startRow = 1) 

############################# 
#############    HTML report

if(HTMLreport)
{
  mclapply(i_, function(i){
    #final_tbl_conc_drugs[,DSS_cols] = gsub("\"","\'", final_tbl_conc_drugs[,DSS_cols])
    GRAPH_cols = grep("GRAPH", colnames(final_tbl_conc_drugs))[i]; DNAMES_cols = grep("DRUG_NAME", colnames(final_tbl_conc_drugs))[i];
    figure_table = final_tbl_conc_drugs[,c(1,DNAMES_cols, GRAPH_cols)]; # extract figure table for certain screen comprising figures in base64 format
    figure_table = unique(figure_table[!is.na(figure_table$GRAPH),])
    
    EC50_table <- readRDS(paste0(getwd(),"/www/Results/Curve_fits/", screens_[i], "_DSRT_analysis_table_Rpipeline.rds"))
    
    #exclude GRAPHs and get them from figure_table matching by id.
    EC50_table$GRAPH <- NULL; EC50_table = merge(EC50_table, figure_table,  all = T)
    readoutCTX = any(toupper(colnames(EC50_table)) %in% c("TC50"));  TEC50 = ifelse(readoutCTX, "TC50", "EC50");
    # reorder
    colsOrder_ <- c("ID", "DRUG_NAME", TEC50, "GRAPH")
    EC50_table = EC50_table[,c(colsOrder_, colnames(EC50_table)[!(colnames(EC50_table) %in% colsOrder_)])]; # colsorder + remain
    EC50_table = EC50_table[,-seq(ncol(EC50_table),by=-1,len=3)]
    
    #turning on/off columns...
    columnsEC = "<div class=\"popka\"><div id=\"colsec50\" class=\"col-sm-8\">Columns: <select class=\"selectpicker\" data-style=\"btn-primary\" multiple show-tick data-actions-box=\"T\">" 
    #...creating multiple selects for it
    namescolsEC <- colnames(EC50_table);
    for(j in 2:length(namescolsEC))
      columnsEC = paste0(columnsEC, " <option selected value = \"",j-1,"\">", namescolsEC[j], "</option>")
    columnsEC = paste0(columnsEC, "</select></div>")
    
    # create table and convert it to HTML (not using html.attributes here because removing spaces later and then classes will be merged.)
    htableEC <- print(xtable::xtable(EC50_table), include.rownames = F, caption.placement = "top", type = "html", print.results = F)
    
    # change table alignment, add classes, remove spaces.
    htableEC <- gsub("align=\"right\"", "", htableEC); 
    htableEC <- sub("<table", "<table id=\"ec50table\" class=\"table table-striped table-bordered nowrap hover\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%\" ", htableEC)
    
    htableEC <- gsub('<tr>  <th>', '<thead><th>', htableEC); htableEC <- gsub('</th>  </tr>', '</th></thead>', htableEC);
    htableEC <- gsub('<tr> <th>', '<thead><th>', htableEC); htableEC <- gsub('</th> </tr>', '</th></thead>', htableEC);
    htableEC <- gsub('.tested', '', htableEC)
    htableEC <- gsub("&lt;", "<", htableEC); htableEC <- gsub("&gt;", ">", htableEC)
    
    headercurve = base::readChar(headerCurvePath, file.info(headerCurvePath)$size);
    if(readoutCTX) {headercurve <- gsub('href="#EC50">EC50','href="#TC50">TC50',headercurve); headercurve <- gsub('id="EC50"','id="TC50"',headercurve);}
    
    htable <- paste0(headercurve, paste0("<div id=\"EC50\" class=\"tab-pane fade in active\"><br>", columnsEC, htableEC, "</div></div></div>"), 
                     '</body></html>')
    writeChar(htable, paste0(getwd(),"/www/Results/Curve_fits/",  screens_[i],"_DSRT_analysis_table_Rpipeline.html"),
              nchar(htable, type = "chars"))
    
  }, mc.cores = parallel::detectCores());
}


####
# JSON for final report, index.html
all_files = list.files('./www/Results/Curve_fits'); 
# for multiple cond. all_files[Reduce('&', lapply(c(".xlsx", "pipeline"), grepl, all_files))]
listToJSON <- c(doneCF = T, PipelineFiles = list(gsub(".xlsx", "", all_files[grepl("pipeline.xlsx",all_files)])))
jsonL <- rjson::toJSON(listToJSON)
write(jsonL, "./www/Results/HTMLreport/CF.json")


#remove all objects
# unloadNamespace('openxlsx')
# rm(list= ls()[!(ls() %in% c('report_name','writeReport','saving_libpath','DSS_typ','DSRT3','control_', 'controls_', 'files_', 'dmethod_'))])
# print("After removed...")

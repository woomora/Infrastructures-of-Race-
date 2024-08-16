# Functions ----

# Function to create a summary table from an RD model ----
table_rd <- 
  function(rdd = rdd){
    
    # Create a tibble with relevant statistics from the RD model
    foo <- 
      tibble(
        coef = (rdd$coef[3,]),     # Coefficient of the 3rd estimate (usually the treatment effect)
        se = rdd$se[3,],           # Standard error of the 3rd estimate
        lci = rdd$ci[3,1],         # Lower confidence interval
        uci = rdd$ci[3,2],         # Upper confidence interval
        pval = rdd$pv[3,],         # P-value of the 3rd estimate
        poly = as.integer(rdd$p),  # Polynomial order used in the model
        h = rdd$bws[1,1],          # Bandwidth for the left side of the cut-off
        b = rdd$bws[2,1],          # Bandwidth for the right side of the cut-off
        N = rdd$N_b[1] + rdd$N_b[2]# Total number of observations within the bandwidth
      ) |> 
      mutate_if(
        is.double,
        ~(format(round(., 3), nsmall = 3)) # Format numeric values to 3 decimal places
      )
    
    return(foo) # Return the tibble
  }

# Function to create a summary table from a local randomization RD ----
table_rdlr <- 
  function(model = rdlr){
    
    # Create a tibble with summary statistics from the linear regression model
    foo1 <- 
      tibble(
        mean_lftc = model$sumstats[3,1],  # Mean of the control group to the left of the cut-off
        mean_rgtc = model$sumstats[3,2],  # Mean of the control group to the right of the cut-off
        coef = model$obs.stat,            # Observed statistic (usually the treatment effect)
        lci = model$interf.ci[1],         # Lower confidence interval
        uci = model$interf.ci[2],         # Upper confidence interval
        asym_pvalue = model$asy.pvalue    # Asymptotic p-value
      ) |> 
      mutate_if(
        is.double, 
        funs(format(round(., 3), nsmall = 3)) # Format numeric values to 3 decimal places
      ) 
    
    return(foo1) # Return the tibble
    
  }

# Function to perform a linear combination test between two variables ----
linear_comb_test <- 
  function(x, var1, var2){
    
    require(fixest) # Load the fixest package for degrees of freedom calculation
    
    df <- fixest::degrees_freedom(x, "resid") # Calculate degrees of freedom
    num <- unname(x$coefficients[var2] - x$coefficients[var1]) # Difference in coefficients
    den <- unname(sqrt((x$se[var1])^2 + (x$se[var2])^2 - 2*x$cov.scaled[var1, var2])) # Standard error of the difference
    t <- abs(num/den) # Calculate the t-value
    p <- 2*pt(q=t, df=df, lower.tail=FALSE) # Calculate the p-value
    
    foo <- 
      tibble(
        diff = num, # Difference between the two coefficients
        pval = p    # P-value for the difference
      )
    
    return(foo) # Return the tibble
  }

# Function to compare two RD estimates (delta method) ----
delta <- 
  function(rd1 = rd1, rd2 = rd2, alpha = .05){
    
    # Diff-in-Disc estimate:
    diff <- (rd1$coef[3,] - rd2$coef[3,]) # Difference between the two RD estimates
    
    # Standard errors (robust)
    se1 <- rd1$se[3,] # Standard error of the first estimate
    se2 <- rd2$se[3,] # Standard error of the second estimate
    
    # Number of observations
    n1 <- sum(rd1$N_h) # Number of observations in the first RD model
    n2 <- sum(rd2$N_h) # Number of observations in the second RD model
    
    # Equal variance? Levene test
    # H0: se1 = se2
    # H1: se1 != se2
    levene_f = (se1/se2) # Ratio of the two standard errors
    
    # If p <= alpha (default 0.05), we reject H0 -> Variances are not equal
    levene_p <- pf(levene_f, n1-1, n2-2, lower.tail = FALSE) # Calculate the p-value for the Levene's test
    
    # Determine if variances are equal or not
    if(levene_p <= alpha){
      equal_var = F # Variances are not equal
    } else {
      equal_var = T # Variances are equal
    }
    
    # If variances are not equal, use the Welch's t-test
    if(equal_var == F){
      
      s1 <- (se1^2)/n1 # Variance of the first estimate divided by its sample size
      s2 <- (se2^2)/n2 # Variance of the second estimate divided by its sample size
      s <- sqrt(s1 + s2) # Combined standard error
      t <- diff/s # Calculate the t-value
      
      # Degrees of freedom using Welch-Satterthwaite equation
      v1 <- (((se1^2)/n1) + ((se2^2)/n2))^2
      v2 <- ((se1^4)/((n1^2)*(n1-1))) + ((se2^4)/((n2^2)*(n2-1)))
      v <-  v1/v2  # Effective degrees of freedom
      
      # P-value for the t-test
      p <- 2*pt(q=abs(t), df=v, lower.tail=F)
      
      # Confidence intervals for the difference
      lci <- diff - qt(p = alpha/2, df=v)*s
      uci <- diff + qt(p = alpha/2, df=v)*s
      
    } else { # If variances are equal, use the standard t-test
      
      sp1 <- (n1-1)*(se1^2) + (n2-1)*(se2^2) # Pooled variance numerator
      sp2 <- n1 + n2 - 2 # Pooled variance denominator
      sp <- sqrt(sp1/sp2) # Pooled standard deviation
      s <- sp*sqrt((1/n1) + (1/n2)) # Combined standard error
      t <- diff/sp # Calculate the t-value
      
      v <- n1 + n2 - 2 # Degrees of freedom
      
      p <- 2*pt(q=abs(t), df=v, lower.tail=F) # P-value for the t-test
      
      # Confidence intervals for the difference
      lci <- diff - qt(p = alpha/2, df=v)*s
      uci <- diff + qt(p = alpha/2, df=v)*s
      
    }
    
    # Return the results as a tibble
    return(
      tibble(
        diff = round(diff, 3),       # Difference in estimates, rounded
        pval = round(p, 3),          # P-value, rounded
        diff_lci = round(lci, 3),    # Lower confidence interval, rounded
        diff_uci = round(uci, 3),    # Upper confidence interval, rounded
        n_group1 = as.integer(n1),   # Number of observations in the first group
        n_group2 = as.integer(n2),   # Number of observations in the second group
        equal_var = equal_var,       # Indicator if variances are equal
        levene_p = round(levene_p, 3) # P-value from Levene's test, rounded
      )
    )
    
  }
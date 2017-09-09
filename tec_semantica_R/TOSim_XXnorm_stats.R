function (x) 
{
    sumTO <- array()
    df <- NULL
    k <- 1
    len <- length(x)
    x_go <- lapply(x, function(y) {
        accgo_list[[y]]
    })
    for (i in 1:(len - 1)) {
        for (j in (i + 1):len) {
            sumTO[k] <- TOSimGenes(x_go[[i]], x_go[[j]])$BPnorm
            df <- rbind(df, data.frame(term1 = x[i], term2 = x[j], 
                TO = sumTO[k]))
            k <- k + 1
        }
    }
    sumTOn <- as.numeric(na.exclude(sumTO))
    TOmean <- mean(sumTOn)
    TOlength <- length(sumTOn)
    TOSim_BPnorm_stats <- list(pairs = df, mean = TOmean, len = TOlength)
    return(TOSim_BPnorm_stats)
}

function (x) 
{
    sumTO <- array()
    df <- NULL
    k <- 1
    len <- length(x)
    x_go <- lapply(x, function(y) {
        accgo_list[[y]]
    })
    for (i in 1:(len - 1)) {
        for (j in (i + 1):len) {
            sumTO[k] <- TOSimGenes(x_go[[i]], x_go[[j]])$CCnorm
            df <- rbind(df, data.frame(term1 = x[i], term2 = x[j], 
                TO = sumTO[k]))
            k <- k + 1
        }
    }
    sumTOn <- as.numeric(na.exclude(sumTO))
    TOmean <- mean(sumTOn)
    TOlength <- length(sumTOn)
    TOSim_CCnorm_stats <- list(pairs = df, mean = TOmean, len = TOlength)
    return(TOSim_CCnorm_stats)
}

function (x) 
{
    sumTO <- array()
    df <- NULL
    k <- 1
    len <- length(x)
    x_go <- lapply(x, function(y) {
        accgo_list[[y]]
    })
    for (i in 1:(len - 1)) {
        for (j in (i + 1):len) {
            sumTO[k] <- TOSimGenes(x_go[[i]], x_go[[j]])$MFnorm
            df <- rbind(df, data.frame(term1 = x[i], term2 = x[j], 
                TO = sumTO[k]))
            k <- k + 1
        }
    }
    sumTOn <- as.numeric(na.exclude(sumTO))
    TOmean <- mean(sumTOn)
    TOlength <- length(sumTOn)
    TOSim_MFnorm_stats <- list(pairs = df, mean = TOmean, len = TOlength)
    return(TOSim_MFnorm_stats)
}
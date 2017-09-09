TOSimGenes <- function (annotSet1, annotSet2) 
{
    annotSet1_BP <- list()
    annotSet2_BP <- list()
    annotSet1_CC <- list()
    annotSet2_CC <- list()
    annotSet1_MF <- list()
    annotSet2_MF <- list()
    j <- 1
    for (i in 1:length(annotSet1)) {
        if (!is.null(annotSet1[[i]])) {
            if (!is.null(GOTERM[[annotSet1[i]]])) {
                ontologyClass <- Ontology(GOTERM[[annotSet1[i]]])
                if (ontologyClass == "BP") 
                  annotSet1_BP <- c(annotSet1[j], annotSet1_BP)
                if (ontologyClass == "MF") 
                  annotSet1_MF <- c(annotSet1[j], annotSet1_MF)
                if (ontologyClass == "CC") 
                  annotSet1_CC <- c(annotSet1[j], annotSet1_CC)
                j <- j + 1
            }
        }
    }
    j <- 1
    for (i in 1:length(annotSet2)) {
        if (!is.null(annotSet2[[i]])) {
            if (!is.null(GOTERM[[annotSet2[i]]])) {
                ontologyClass <- Ontology(GOTERM[[annotSet2[i]]])
                if (ontologyClass == "BP") 
                  annotSet2_BP <- c(annotSet2[j], annotSet2_BP)
                if (ontologyClass == "MF") 
                  annotSet2_MF <- c(annotSet2[j], annotSet2_MF)
                if (ontologyClass == "CC") 
                  annotSet2_CC <- c(annotSet2[j], annotSet2_CC)
                j <- j + 1
            }
        }
    }
    if (length(annotSet1_BP) == 0 | length(annotSet2_BP) == 0) {
        TO_BP = NA
        TO_BPnorm = NA
    }
    else {
        annotSet1_BP_ancestor <- list()
        annotSet2_BP_ancestor <- list()
        annotSet1_BP_ancestor <- annotSet1_BP
        annotSet2_BP_ancestor <- annotSet2_BP
        for (i in 1:length(annotSet1_BP)) annotSet1_BP_ancestor <- c(annotSet1_BP_ancestor, 
            GOBPANCESTOR[[annotSet1_BP[[i]]]])
        for (i in 1:length(annotSet2_BP)) annotSet2_BP_ancestor <- c(annotSet2_BP_ancestor, 
            GOBPANCESTOR[[annotSet2_BP[[i]]]])
        annotSet1_BP_ancestor <- unique(unlist(annotSet1_BP_ancestor))
        annotSet2_BP_ancestor <- unique(unlist(annotSet2_BP_ancestor))
        TO_BP <- length(intersect(annotSet1_BP_ancestor, annotSet2_BP_ancestor))
        if ("all" %in% TO_BP) 
            TO_BP <- TO_BP[-which(TO_BP == "all")]
        if ("GO:0008150" %in% TO_BP) 
            TO_BP <- TO_BP[-which(TO_BP == "GO:0008150")]
        TO_BPnorm <- TO_BP/min(length(annotSet1_BP_ancestor), 
            length(annotSet2_BP_ancestor))
    }
    if (length(annotSet1_MF) == 0 | length(annotSet2_MF) == 0) {
        TO_MF = NA
        TO_MFnorm = NA
    }
    else {
        annotSet1_MF_ancestor <- list()
        annotSet2_MF_ancestor <- list()
        annotSet1_MF_ancestor <- annotSet1_MF
        annotSet2_MF_ancestor <- annotSet2_MF
        for (i in 1:length(annotSet1_MF)) annotSet1_MF_ancestor <- c(annotSet1_MF_ancestor, 
            GOMFANCESTOR[[annotSet1_MF[[i]]]])
        for (i in 1:length(annotSet2_MF)) annotSet2_MF_ancestor <- c(annotSet2_MF_ancestor, 
            GOMFANCESTOR[[annotSet2_MF[[i]]]])
        annotSet1_MF_ancestor <- unique(unlist(annotSet1_MF_ancestor))
        annotSet2_MF_ancestor <- unique(unlist(annotSet2_MF_ancestor))
        TO_MF <- length(intersect(annotSet1_MF_ancestor, annotSet2_MF_ancestor))
        if ("all" %in% TO_MF) 
            TO_MF <- TO_MF[-which(TO_MF == "all")]
        if ("GO:0003674" %in% TO_MF) 
            TO_MF <- TO_MF[-which(TO_MF == "GO:0003674")]
        TO_MFnorm <- TO_MF/min(length(annotSet1_MF_ancestor), 
            length(annotSet2_MF_ancestor))
    }
    if (length(annotSet1_CC) == 0 | length(annotSet2_CC) == 0) {
        TO_CC = NA
        TO_CCnorm = NA
    }
    else {
        annotSet1_CC_ancestor <- list()
        annotSet2_CC_ancestor <- list()
        annotSet1_CC_ancestor <- annotSet1_CC
        annotSet2_CC_ancestor <- annotSet2_CC
        for (i in 1:length(annotSet1_CC)) annotSet1_CC_ancestor <- c(annotSet1_CC_ancestor, 
            GOCCANCESTOR[[annotSet1_CC[[i]]]])
        for (i in 1:length(annotSet2_CC)) annotSet2_CC_ancestor <- c(annotSet2_CC_ancestor, 
            GOCCANCESTOR[[annotSet2_CC[[i]]]])
        annotSet1_CC_ancestor <- unique(unlist(annotSet1_CC_ancestor))
        annotSet2_CC_ancestor <- unique(unlist(annotSet2_CC_ancestor))
        TO_CC <- length(intersect(annotSet1_CC_ancestor, annotSet2_CC_ancestor))
        if ("all" %in% TO_CC) 
            TO_CC <- TO_CC[-which(TO_CC == "all")]
        if ("GO:0005575" %in% TO_CC) 
            TO_CC <- TO_CC[-which(TO_CC == "GO:0003674")]
        TO_CCnorm <- TO_CC/min(length(annotSet1_CC_ancestor), 
            length(annotSet2_CC_ancestor))
    }
    TOSimGenes <- list(BP = TO_BP, BPnorm = TO_BPnorm, MF = TO_MF, 
        MFnorm = TO_MFnorm, CC = TO_CC, CCnorm = TO_CCnorm)
    return(TOSimGenes)
}

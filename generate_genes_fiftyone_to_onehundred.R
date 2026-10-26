library(SummarizedExperiment)
library(org.Hs.eg.db)
#grab the example data set
data(airway, package="airway")
#process the expression data
expression <- assays(airway)[[1]]
variances <- apply(expression, 1, var)
expression <- expression[order(variances, decreasing=TRUE)[51:100],]
#match the annnotation
annotation <- merge(toTable(org.Hs.egSYMBOL), toTable(org.Hs.egENSEMBL2EG))
gene_names <- annotation$symbol[match(rownames(expression), annotation$ensembl_id)]
rownames(expression)[! is.na(gene_names)] <- paste(gene_names[! is.na(gene_names)], rownames(expression)[! is.na(gene_names)], sep = ' / ')
#create the experiment data
experiment <- data.frame(colData(airway))
#save the data for use in the application
expression_fifty_one_to_one_hundred <- expression
experiment_fifty_one_to_one_hundred <- experiment
save(expression_fifty_one_to_one_hundred,file="expression_fifty_one_to_one_hundred.Rdata")
save(experiment_fifty_one_to_one_hundred,file="experiment_fifty_one_to_one_hundred.Rdata")






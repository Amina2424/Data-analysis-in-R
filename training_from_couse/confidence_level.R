library(Rsamtools)
library(GenomicRanges)
library(methylKit)
library(bayesmeta)
library(ggplot2)


bam_file <- "//c-files/HEAP-EPI/Diplome_gi_Imanalieyva_Amina/half_phased_t2t_27000380.bam"
bam <- BamFile(bam_file)
open(bam)

param <- ScanBamParam(tag = c("ML", "MM"))
bam_data <- scanBam(bam_file, param = param)

ml_values <- unlist(lapply(bam_data, function(x) x$tag$ML))
mm_values <- unlist(lapply(bam_data, function(x) x$tag$MM))

ml_values <- na.omit(as.numeric(ml_values))

# Функция для доверительного интервала
calculate_confidence_interval <- function(data, conf_level = 0.95) {
  n <- length(data)
  mean_val <- mean(data)
  stderr <- sd(data) / sqrt(n)
  error_margin <- qt(1 - (1 - conf_level) / 2, df = n - 1) * stderr
  c(lower = mean_val - error_margin, upper = mean_val + error_margin)
}

# Расчёт доверительного интервала
conf_interval <- calculate_confidence_interval(ml_values)
print(conf_interval)


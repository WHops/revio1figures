
load_paf <- function(inpaf_link){
  inpaf <- read.table(inpaf_link, sep = "\t")
  # Remove redundant rows
  inpaf <- unique(inpaf)
  
  colnames_paf <- c(
    "qname",
    "qlen",
    "qstart",
    "qend",
    "strand",
    "tname",
    "tlen",
    "tstart",
    "tend",
    "nmatch",
    "alen",
    "mapq"
  )
  colnames(inpaf)[1:length(colnames_paf)] <- colnames_paf
  
  inpaf <- transform(
    inpaf,
    qend = ifelse(strand == "-", qstart, qend),
    qstart = ifelse(strand == "-", qend, qstart)
  )
  
  inpaf <- inpaf[order(inpaf$qstart), ]
  
  return(inpaf)
}


# Function to replace continuous sequences
replace_sequences <- function(df) {
  df <- df[order(df$qrank), ]  # Ensure dataframe is sorted by qrank
  n <- nrow(df)
  new_qrank <- df$qrank  # Start with the original qrank
  
  i <- 1
  while (i <= n) {
    j <- i
    while (j < n && df$strand[j] == df$strand[j + 1] && df$qrank[j] + 1 == df$qrank[j + 1]) {
      j <- j + 1
    }
    if (j > i) {
      new_qrank[i:j] <- df$qrank[i]
    }
    i <- j + 1
  }
  
  df$qrank <- new_qrank
  return(df)
}

library(ggplot2)


inpaf_link = '../revio/stage1/paper/figure-factory/dotplots/CGR/chr6-118500000-119000000/diff/paf/out2.paf'

#inpaf_link = '../revio/stage1/paper/figure-factory/dotplots/CGR/chr6-118500000-119000000/diff/paf/hg38_P50-E5-h1_xy.paf'
paf = load_paf(inpaf_link)

ggplot(paf) + geom_segment(aes(x=tstart, xend=tend, y=qstart, yend=qend))

paf = paf[abs(paf$qend - paf$qstart) > 10000,]
ggplot(paf) + geom_segment(aes(x=tstart, xend=tend, y=qstart, yend=qend))

paf$qrank = rank(pmin(paf$qstart, paf$qend))



# Apply the function
paf <- replace_sequences(paf)
paf$qrank <- as.integer(factor(paf$qrank))
max_rank = max(paf$qrank)
print(paf)

paf <- transform(
  paf,
  tend = ifelse(strand == "-", tstart, tend),
  tstart = ifelse(strand == "-", tend, tstart)
)

p = ggplot() + geom_segment(data = paf, aes(x=tstart, xend=tend, y=max_rank - qrank, yend=max_rank - qrank),
                           linewidth = 3,
                           arrow = arrow(length = unit(0.5, "cm"),
                           )) +
  geom_segment(data= paf, aes(y=max_rank, yend=max_rank, x=min(tstart), xend=max(tend)), linewidth=3) + 
  ylim(c(-3,4)) + theme_void()

q = draw_snake(0,5e05,2,1,3, p)
q = draw_snake(0,10,0,1,3, ggplot())
  



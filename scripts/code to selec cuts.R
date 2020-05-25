seqs <- readxl::read_excel("/home/m/Dropbox/LBH data/Display-Interactions/Display sequence analyses/Display sequence analysis LBH nov-13with SUR2014.xlsx", sheet = "Sequence analysis")


head(seqs)

unique(seqs$`New cut name`)

seqs <- seqs[!is.na(seqs$`New cut name`), ]

hardcuts <- list.files(path = "/home/m/Desktop/Cuts from Grace")

hardcuts <- gsub(".mov", "", hardcuts, ignore.case = T)



seqs$`New cut name`[!seqs$`New cut name` %in% hardcuts]

hardcuts[!hardcuts %in% seqs$`New cut name` ]


seqs <- seqs[seqs$`New cut name` %in% hardcuts, ]
nrow(seqs)


seqs <- seqs[!is.na(seqs$`focal bird`),]


seqs$`Behavioral type`


file.copy(file.path("/home/m/Desktop/Cuts from Grace", paste0(seqs$`New cut name`, ".mov")), file.path("/home/m/Desktop/Cuts Kasia/", paste0(seqs$`New cut name`, ".mov")))


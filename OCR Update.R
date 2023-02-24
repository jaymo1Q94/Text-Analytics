#Install and load relevant packages ####
install.packages(c("tesseract", "magick", "tidyverse", 
                   "quanteda", "pdftools",
                   "stm", "quanteda.textmodels", "quanteda,textplots",
                   "quanteda.corpora", "quanteda.dictionaries", "flextable",
                   "textclean", "tm", "seededlda"))


ocr <- c("tesseract", "magick", "tidyverse", 
         "quanteda", "pdftools", 
         "stm", "quanteda.textmodels","quanteda.textplots",
         "flextable", "textclean", "tm", "seededlda")


#Set root directory 

root.dir <- paste0("~/GitLab/text-analytics")

lapply(ocr, require, character.only = T)

#Set tesseract engine ####

eng <- tesseract("eng")


#Import Health Department visuals 

HD.Images <- magick::image_read_pdf("HDVisuals.pdf")

#Batch uploading can speed up importation 

Hosp.Images <- magick::image_read_pdf("HospVisuals.pdf", pages = 2:60)


#image_read

#OCR Process #### 
image.text <- HD_Images %>% image_convert(type = "Grayscale") %>%
image_resize("2000x") %>%
tesseract::ocr()

image.text.2 <- Hosp.Images %>% image_convert(type = "Grayscale") %>%
image_resize("2000x") %>%
  tesseract::ocr()

#Print text output 

cat(image.text)
cat(image.text.2)


#Save text file 

fileConn <- file ("output.txt")
writeLines(image.text, sep = "\n", fileConn)
close(fileConn)


fileConn <- file("output2.txt")
writeLines(image.text.2, sep = "\n", fileConn)
close(fileConn)

#Corpus Creation #####

#Merge text files 

Albany_COVID_Text <- readtext::readtext("~/GitLab/text-analytics/Inputs/Text Files")

#Rename doc ids with their organizational titles 

Albany_COVID_Text$doc_id <- gsub("output.txt", "Southwest GADPH", Albany_COVID_Text$doc_id)
Albany_COVID_Text$doc_id <- gsub("output2.txt", "Phoebe-Putney Hospital", Albany_COVID_Text$doc_id)

corpus <- corpus(Albany_COVID_Text)

#Exploratory Data Analysis - KWIC ####

corpus.stats <- summary(corpus)
corpus.stats


#Tokenization for KWIC Searching 

tokens <- tokens(corpus, what = "word", remove_punct = T, remove_url = T, remove_numbers = T,
                 remove_symbols = T) %>% tokens_tolower() %>%                  tokens_remove(c(stopwords("english"))) %>%
                 replace_contraction() %>% replace_emoji() %>%
                 replace_hash(replacement = "") %>% replace_emoticon() %>%
                 str_squish() %>% str_trim() 

token_cleaner <- function(corpus) {
  tokens(corpus, what = "word", remove_punct = T, remove_url = T, remove_numbers = T,
         remove_symbols = T) %>% tokens_tolower() %>%                  tokens_remove(c(stopwords("english"))) %>%
    replace_contraction() %>% replace_emoji() %>%
    replace_hash(replacement = "") %>% replace_emoticon() %>%
    str_squish() %>% str_trim() 
} 

tokens <- token_cleaner(corpus)

class(tokens)


tokens <- tokenize(tokens)

tokens <- as.tokens(tokens)

tokens <- tokens_select(tokens, min_nchar = 3L)

#Will need to create a more involved string cleaning process


#Wow there are some 1-character values here that are leftovers
#So we'll subset for words that are at least 2 characters with the rationale
#of what if there are 2-character acronyms that we don't want to scrub out

#Upskill -- Words with repeated and trailing characters 

#Use Case 1 - KWIC Searching for Document Review via Term Concordance 

#KWIC Search 1 - What messages are being disseminated related to hand washing?

concordance_wash <- kwic(corpus, "wash", window = 20, valuetype = "regex") %>%
  as.data.frame() %>% flextable() %>% autofit()

#KWIC Search 2 - What messages are being disseminated related to masks? 

concordance_masks <- kwic(corpus, "mask", window = 20, valuetype = "regex") %>%
  as.data.frame() %>% flextable() %>% autofit()

#KWIC Search 3 - What messages are being disseminated related to social distancing?

concordance_distancing <- kwic(corpus, "distanc", window = 20, valuetype = "regex") %>%
  as.data.frame() %>% flextable() %>% autofit()

#KWIC Search 3 - What messages are being disseminated related to Covid-19? 

concordance_covid <- kwic(corpus, "covid", window = 20, valuetype = "regex") %>%
  as.data.frame() %>% flextable() %>% autofit()

#KWIC Search 4 - What messages are being disseminated related to Access?

#EDA1 - TextRay Plot #####

concordance_access <- kwic(corpus, "access", window = 20, valuetype = "regex") %>%
  as.data.frame() %>% flextable() %>% autofit()

quanteda.textplots::textplot_xray(kwic(corpus, pattern = "covid*"),
                                  kwic(corpus, pattern = "distanc*"))

#Use Case 3 - Topic Model Dictionary Methods

#Creating DFM - Comparable to Word2vec or BoW 

dfm <- dfm(tokens)

#Perform a TF_IDF Transformation 

tdf_idf <- quanteda::dfm_tfidf(dfm)


#Explore top features 

tdf_idf.topfeatures <- topfeatures(tdf_idf, 50)

tdf_idf.topfeatures

#Note that these frequency weatings 

unique_features <- tdf_idf[1,] %>% as.list()

#demonstration dictionary - essentially an agnostic approach to a topic model 
#but we can feed in our top 

dict <- dictionary(list(disease = c("covid", "covid19", "coronavirus", 
                                    "health", "covid-19", "cases", "spread"),
                        equity = c("community", "accessible", "available", "putney",
                                   "phoebe", "fair", "assistance", "testing"),
                        measures = c("wash", "distancing", "masks",
                                     "remote", "virtual", "spread"),
                        symptoms = c("cough", "fever","symptoms")))





#test it 

dfm_dict_test <- tokens(corpus) %>% 
  tokens_lookup(dictionary = dict) %>%
  dfm() %>% print()


#Compare with our structural topic model ####


#STM Workflow has changed -- Add this 

set.seed(100)

#Process 

processed <- textProcessor(Albany_COVID_Text$text, metadata = Albany_COVID_Text)
out  <- prepDocuments(processed$documents, processed$vocab,processed$meta)


albany.stm <- stm(documents = out$documents, vocab = out$vocab, K = 4,
                  data = out$meta, init.type = "Spectral")

#Visualize 

plot(albany.stm, type = "summary")

#No covariates so we can't decompose into multiple regression models
#we're essentially running a univariate analysis 

#Understanding 

topic.labels <- labelTopics(albany.stm, c(1,2,3,4))

topic.labels

#Seeded LDA #####

#Won't use a weighted dfm 

lda <- textmodel_seededlda(dfm, dict, residual = F)

terms(lda)


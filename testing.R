
library(tesseract)
library(ggplot2)
library(stringr)

tesseract_info()
engine <- tesseract('slk')
path_receipts <- '/home/user/Scanned stuff/Receipts/Billa'
path_receipts2 <- 'C:/Users/IBM_ADMIN/Desktop/MY STUFF/Billa'
shops <- c('Billa')

# create list of receipts separated by rows (vectors)

ocr_receipt_list <- str_split(ocr(image = paste(path_receipts, '/', list.files(path = path_receipts), sep = ''), 
                         engine = engine), '\n')

# pre kazdy vektor v liste, necham:
#   - 5. prevadzka
#   - 6. datum a cas
#   - 8 az po prvy vyskyt DPH -1, alebo REKAPITULACIA -1, alebo SADZBA -2
#   - tam kde je prvy vyskyt SUCET, alebo EUR, ciastku pre verifikaciu

addres_whole <- function(x){
  
  str_detect(x, '\\d{1,2},\\d{5}')

}

remove_un <- function(x){
  
  # removes unnecessary rows from receipt
  # returns returns only needed rows
  
  # x - vektor riadkov
  
  x <- x[!(x == '')] # odstran empty string
  index_to_keep <- 5 # piaty riadok ako prvy
  x_rem_punct <- gsub('[[:punct:]]', replacement = '', x)
  last_item_index <- grep('([0|D|O|L][P|F]H) ([[:alnum:]]{13})', x_rem_punct) - 1 # index riadku poslednej polozky
                          # riadok pred 'DPH REKAPITULACIA'
  #print(last_item_index)
 
  # adresa prevadzky, datum a cas
 
 if (addres_whole(x[5])){
   
   # ak je adresa rozdelena ciarkou v jednom riadku
   
   index_to_keep <- append(index_to_keep, 6)
   index_to_keep <- append(index_to_keep, 8:last_item_index)

   
 } else {
   
   # ak je adresa v dvoch riadkoch
   
   index_to_keep <- append(index_to_keep, 6:7)
   index_to_keep <- append(index_to_keep, 9:last_item_index)

   
 }
 
 total_index <- grep('EUR', x) # DOROB - [[11 a 12]] storno polozky medzisucet - aj tam je EUR
 index_to_keep <- append(index_to_keep, total_index)
 
 
 x <- x[index_to_keep]

}

receipt_list_entries <- lapply(ocr_receipt_list, remove_un)

# vytvorit list s named vektor s kategoriami
#   wholeAdress, alebo addressStreet a addressCity
#   dateTime
#   1 - 3
#   discountMark
#   discountAmount (toto je celkova suma zlavy, aj ked je viacero kusov)
#   stornoMark
#   stornoEntry
#   subtotal
#   total
#   payment

create_categories <- function(x){
  
  
    
    if (addres_whole(x[1])){
      
      names(x)[1] <- 'addressWhole'
      
    } else {
      
      names(x)[1] <- 'addressStreet'
      names(x)[2] <- 'addressCity'
      
    }
    
  return(x)
  
}

receipt_list_cats <- lapply(x, create_categories)

# potrebujem
# - prevadzka (ulica, cislo, mesto, smerove cislo)
# - datum a cas
# - zaznam  - typy    - 1.  - nazov polozky
#                           - mnozstvo
#                           - cena za jednotku
#                           - cena za vsetky kusy
#                           - 5. druh dane
#                     - 2.  - nazov polozky (balene veci co su uz navazene)
#                           - vaha/objem
#                           - mnozstvo
#                           - cena za jednotku
#                           - cena za vsetky kusy
#                           - 6. druh dane
#                     - 3.  - nazov polozky (veci co sa vazia)
#                           - vaha[kg]
#                           - cena za kg
#                           - cena za nakupene mnozstvo
#                           - 5. druh dane
#                     - 4.  - nazov zlavy (zaznam nad je 'ZLAVA NA POLOZKU'), pre polozku nad
#                           - percento zlavy (nemusi sa nachadzat)
#                           - suma zlavy za vsetky kusy nakupenej polozky
#                           - 4. dan (nepotrebujem) --- receipt8
# - sposob platby (Hotovost, Platobna kar)
# - na verifikaciu (sadzba dane - suma dane, SUCET)

# ako to bude fungovat

# sumar  1. - z novych blockov co este neboli spracovane
#           - co uz boli spracovane
#           - dohromady
#        2. - celkovy / kategorie
#           - podla mesiacov / kategorie
#           - podla tyzdnov / kategorie
#           - podla dni v tyzdni / kategorie
#           - podla poctu dni ostavajucich do konca mesiaca / kategorie
#           - kategorie
#           - lokacia na mape
#       3.  - porovnanie s minulym obdobim (rast/pokles)  - rok
#                                                         - mesiac
#                                                         - tyzden
#                                                         - den v tyzdni
#                                                         - pocet dni do konca mesiaca
#       4.  - vsetky polozky podla kategorii, mnozstvo, cena, % v ramci kategorie, % celkovo
#       5.  - sumar generovania reportu - pocet blockov, pocet poloziek, suma,
#               celkovy pocet blockov, celkovy pocet poloziek, celkova suma, cas generovania reportu a trvanie
# kategorie - sladkosti
#           - ovocie
#           - zelenina
#           - pecivo
#           - osobna drogeria
#           - domacnost
#           - maso
#           - fajcenie
#           - alkohol
#           - zabava
#           - oblecenie
#           - doprava
#           - sport
#           - ine


library(tesseract)
library(ggplot2)
library(stringr)

tesseract_info()
engine <- tesseract('slk')
path_receipts <- '/home/user/Scanned stuff/Receipts/Billa'
path_receipts2 <- 'C:/Users/IBM_ADMIN/Desktop/MY STUFF/Billa'
shops <- c('Billa')

ocr_receipt_list <- str_split(ocr(image = paste(path_receipts, '/', list.files(path = path_receipts), sep = ''), 
                         engine = engine), '\n')

# pre kazdy vektor v liste, necham:
#   - 5. prevadzka
#   - 6. datum a cas
#   - 8 az po prvy vyskyt DPH -1, alebo REKAPITULACIA -1, alebo SADZBA -2
#   - tam kde je prvy vyskyt SUCET, alebo EUR, ciastku pre verifikaciu

remove_un <- function(x){
  
 index_to_keep <- 5
 last_item_index <- grep('^([0|D|O]PH) ([[:alnum:]]{13})$', x) - 1 # index riadku poslednej polozky
 print(last_item_index)
 
 # adresa prevadzky, datum a cas
 
 if (str_detect(x[5], '\\d{1,2},\\d{5}')){
   
   # ak je adresa rozdelena ciarkou v jednom riadku
   
   index_to_keep[2] <-  6
   index_to_keep[3:(last_item_index - 5)] <- 8:last_item_index 
   
   
 } else {
   
   # ak je adresa v dvoch riadkoch
   
   index_to_keep[2] <- 6
   index_to_keep[3] <- 7
   
 }
 
 # polozky
 
 
 
 x <- x[index_to_keep]

}



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

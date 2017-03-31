
library(tesseract)
library(ggplot2)
library(stringr)

tesseract_info()
engine <- tesseract('slk')
path_receipts <- '/home/user/Scanned stuff/Receipts/Billa'
shops <- c('Billa')

# create list of receipts separated by rows (vectors)

ocr_receipt_list2 <- str_split(ocr(image = paste(path_receipts, '/', list.files(path = path_receipts), sep = ''), 
                         engine = engine), '\n')

# pre kazdy vektor v liste, necham:
#   - 5. prevadzka
#   - 6. datum a cas
#   - 8 az po prvy vyskyt DPH -1, alebo REKAPITULACIA -1, alebo SADZBA -2
#   - tam kde je prvy vyskyt SUCET, alebo EUR, ciastku pre verifikaciu

addres_whole <- function(x){
  
  str_detect(x, '\\d{1,2},\\d{5}')

}

d_platba <- function(x){
  
  str_detect(x , 'P(l|1|ł)atba.{5,}EUR')
  
}

d_sucet <- function(x){
  
  str_detect(x, '^.{0,3}S(U|Ú|ú)(C|Č|Ď|ć)ET.{5,}EUR')
  
}

d_medzisucet <- function(x){
  
  str_detect(x, '^.{0,2}M(E|B)(D|O)Z(I|1)S(U|Ú|ú)(C|Č|Ď|ć)ET.{5,}EUR')
  
}

d_discountMark <- function(x){
  
  str_detect(x, '^.{0,2}Z(L|Ľ|ĺ)AVA NA P(O|D|U)L(O|D|U)(Z|Ž)K(U|O).{0,16}$')
  
}

d_stornoMark <- function(x){
  
  str_detect(x, '^.{0,3}(S|B)T(O|U|D)(R|B)N(O|U|D).{7,11}$')
  
}

d_splitEntry <- function(x){
  
  str_length(x) < 15 # receipt194
  
}

remove_un <- function(x){
  
  # removes unnecessary rows from receipt
  # returns returns only needed rows
  
  # x - vektor riadkov
  
  x <- x[!(x == '')] # odstran empty string
  index_to_keep <- 5 # piaty riadok ako prvy
  x_rem_punct <- gsub('[[:punct:]]', replacement = '', x)
  last_item_index <- grep('([0|D|O|L][P|F]H)\\s?([[:alnum:]]{13})', x_rem_punct) - 1 # index riadku poslednej polozky
                          # riadok pred 'DPH REKAPITULACIA'
  
  # ak nenajde riadok 'DPH REKAPITULACIA'
  
  if (length(last_item_index) == 0){
    
    return(NULL)
    
  }
  
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

receipt_list_entries2 <- lapply(ocr_receipt_list2, remove_un)

# vytvorit list s named vektor s kategoriami
#   wholeAdress, alebo addressStreet a addressCity 
#   dateTime
#   1 - 3
#   discountMark
#   discountEntry (toto je celkova suma zlavy, aj ked je viacero kusov)
#   stornoMark
#   stornoEntry
#   subtotal
#   total
#   payment

create_categories <- function(x){
  
  
    
    if (addres_whole(x[1])){
      
      names(x)[1] <- 'addressWhole'
      names(x)[2] <- 'dateTime'
      
      i <- 3
      
    } else {
      
      names(x)[1] <- 'addressStreet'
      names(x)[2] <- 'addressCity'
      names(x)[3] <- 'dateTime'
      
      i <- 4
      
    }
  
  for (entry in x[i:length(x)]){
    
    if (d_platba(entry)){
      
      names(x)[i] <- 'payment'
      
    } else {
      
      if (d_sucet(entry)){
        
        names(x)[i] <- 'total'
        
      } else {
        
        if (d_medzisucet(entry)){
          
          names(x)[i] <- 'subtotal'
          
        } else {
          
          if (d_discountMark(entry)){
            
            names(x)[i] <- 'discountMark'
            names(x)[i + 1] <- 'discountEntry'
            
          } else {
            
            if (d_stornoMark(entry)){
              
              names(x)[i] <- 'stornoMark'
              names(x)[i + 1] <- 'stornoEntry'
              
            } else {
              
              if (names(x)[i - 1] %in% c('discountMark', 'stornoMark')){
                
                i <- i + 1
                next
              
              } else {
              
                if (d_splitEntry(entry)){
                  
                  names(x)[i] <- 'itemSplit2'
                  names(x)[i - 1] <- 'itemSplit1'
                  
                } else {
                  
                  names(x)[i] <- 'item'
                  
                }
              }
              
            }
            
          }
          
        }
        
      }
      
    }
    
    i <- i + 1
    
  }
  
  
    
  return(x)
  
}

receipt_list_cats2 <- lapply(receipt_list_entries2, create_categories)

# add filenames to list

receipt_list_cats2 <-
  mapply(c, receipt_list_cats2, lapply(as.list(list.files(path = path_receipts)), 'names<-', 'receiptFile'))

# format df
#   id
#   receiptFile
#   dateTime
#   streetName
#   streetNumber
#   cityName
#   postcode
#   itemName
#   itemAmount (NA when unit == 'weighted')
#   itemWeight
#   unit (pieces, weighted)
#   itemPricePerUnit
#   itemPrice
#   itemVAT

e_dateTime <- function(x){
  
  row_dateTime <- x[names(x) == 'dateTime']
  dateTime <- str_match(row_dateTime, '(\\d{2}\\.\\d{2}\\.\\d{4}) (\\d{2}:\\d{2}:\\d{2})')[1, 1]
  #dateTime <- as.POSIXct(strptime(dateTime, '%d.%m.%Y %H:%M:%S'))
  
  # returns string
  return(dateTime)
  
}

e_address <- function(x){
  
  
  if ('addressWhole' %in% names(x)){
    
    address <- str_match(x[names(x) == 'addressWhole'], 
                            '[prevádzka|prev.]:\\s(.{8,15})\\s(.{1,5}),(\\d{5})\\s(.{5,15})')[1,2:5]
    
  } else {
    
    address <- c(str_match(x[names(x) == 'addressStreet'],
                           'prevádzka:\\s(.{8,15})\\s(.{1,5})')[1, c(2,3)],
                 str_match(x[names(x) == 'addressCity'],
                           '(\\d{3}\\s\\d{2})\\s(.{5,15})')[1, c(2,3)])
    
  }
  
  names(address) <- c('streetName', 'streetNumber', 'postcode', 'cityName')
  
  return(address)
  
}


# [2] - receipt104 - extrakt adresy streetNumber vytiahne 'r'
# [59] - receipt3.jpg - ocr zle rozpozna mesto
# polozka moze byt aj v dvoch riadkoch [39] - receipt194!!!!

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

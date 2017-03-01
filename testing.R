
library(tesseract)
library(ggplot2)
library(stringr)

tesseract_info()
engine <- tesseract('slk')
path_receipts <- '/home/user/Scanned stuff/Receipts/'
path_receipts2 <- 'C:/Users/IBM_ADMIN/Desktop/MY STUFF/Billa'
shops <- c('Billa')


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

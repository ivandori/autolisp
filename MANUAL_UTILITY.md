# 📚 Manuale Funzioni UTILITY LISP

**Autore:** Ivano Dorigatti  
**Licenza:** MIT License  
**Ultima modifica:** 15/01/2026  
**Compatibilità:** ProgeCAD / DraftSight / AutoCAD (AutoLISP puro)

---

## 📋 Indice dei Moduli

1. [Gestione Dialog DCL (_dcl.lsp)](#gestione-dialog-dcl)
2. [Gestione Entità (_entita.lsp)](#gestione-entità)
3. [Gestione File (_file.lsp)](#gestione-file)
4. [Gestione Gruppi Selezione (_gruppi.lsp)](#gestione-gruppi-selezione)
5. [Gestione Layer (_layer.lsp)](#gestione-layer)
6. [Gestione Linee (_linee.lsp)](#gestione-linee)
7. [Gestione Liste (_liste.lsp)](#gestione-liste)
8. [Funzioni Matematiche (_mat.lsp)](#funzioni-matematiche)
9. [Gestione Polilinee (_polilinee.lsp)](#gestione-polilinee)
10. [Gestione Punti (_punti.lsp)](#gestione-punti)
11. [Gestione Record (_record.lsp)](#gestione-record)
12. [Gestione Stringhe (_stringhe.lsp)](#gestione-stringhe)
13. [Gestione XDATA (_xdata.lsp)](#gestione-xdata)

---

## Gestione Dialog DCL

### File: `_dcl.lsp`

#### 1. GESTIONE FILE INI

##### `LOAD-DCL-INI`
Carica valori da file INI e li imposta nei controlli DCL  
**Argomenti:**
- `ini-file` - Percorso file INI da leggere

**Ritorna:** T se successo, nil se errore  
**Formato file INI:**
```
[Sezione Commento]
nome_controllo=valore
altro_controllo=altro_valore
```

---

##### `SAVE-DCL-INI`
Salva valori dei controlli DCL in file INI  
**Argomenti:**
- `ini-file` - Percorso file INI da creare/sovrascrivere
- `var-list` - Lista di nomi controlli DCL (stringhe)

**Ritorna:** T  
**Esempio:**
```lisp
(save-dcl-ini "C:\\config\\impostazioni.ini"
  '("[Testo]" "edit_nome" "edit_cognome" 
    "[Opzioni]" "check_opzione1" "radio_scelta"))
```

---

##### `LOAD-VARIABLES-INI`
Carica variabili da file INI e le setta in memoria AutoLISP  
**Argomenti:**
- `ini-file` - Percorso file INI

**Ritorna:** T se successo, nil se errore  
**Formato variabili:**
- `nome_var@i=123` → Integer
- `nome_var@r=45.67` → Real
- `nome_var@b=T` → Boolean
- `nome_var=testo` → String

**Esempio:**
```
altezza@r=2.5
contatore@i=10
attivo@b=T
nome=Mario
```

---

##### `SAVE-VARIABLES-INI`
Salva variabili AutoLISP in file INI  
**Argomenti:**
- `ini-file` - Percorso file INI da creare
- `var-list` - Lista di nomi variabili (stringhe con suffisso tipo)

**Ritorna:** T  
**Esempio:**
```lisp
(save-variables-ini "C:\\config.ini"
  '("[Dimensioni]" "altezza@r" "larghezza@r"
    "[Contatori]" "numero@i" "attivo@b"
    "[Testo]" "nome" "cognome"))
```

---

#### 2. GESTIONE DIALOG BOX DCL

##### `SHOW-DIALOG`
Visualizza un dialog DCL base (solo visualizzazione)  
**Argomenti:**
- `dialog-name` - Nome del dialog

**Esempio:**
```lisp
(show-dialog "info_dialog")
```

---

##### `ENABLE-CONTROLS`
Abilita una lista di controlli DCL  
**Argomenti:**
- `control-list` - Lista di nomi controlli (stringhe)

**Esempio:**
```lisp
(enable-controls '("edit1" "button1" "check1"))
```

---

##### `DISABLE-CONTROLS`
Disabilita una lista di controlli DCL  
**Argomenti:**
- `control-list` - Lista di nomi controlli

**Esempio:**
```lisp
(disable-controls '("edit2" "button2"))
```

---

##### `SELECT-CONTROLS`
Seleziona (check) una lista di controlli  
**Argomenti:**
- `control-list` - Lista di nomi controlli checkbox/radio

**Esempio:**
```lisp
(select-controls '("check1" "check2" "radio1"))
```

---

##### `DESELECT-CONTROLS`
Deseleziona (uncheck) una lista di controlli  
**Argomenti:**
- `control-list` - Lista di nomi controlli

**Esempio:**
```lisp
(deselect-controls '("check1" "check2"))
```

---

#### 3. LISTE E SELEZIONI DCL

##### `POPULATE-LISTS`
Popola più list_box contemporaneamente  
**Argomenti:**
- `key-list` - Lista di nomi list_box
- `data-lists` - Lista di liste (una per ogni list_box)

**Esempio:**
```lisp
(populate-lists '("lista1" "lista2")
  '(("Item1" "Item2") ("OpzioneA" "OpzioneB")))
```

---

##### `GET-LIST-SELECTION`
Ottiene elemento selezionato da list_box  
**Argomenti:**
- `key` - Nome list_box
- `item-list` - Lista originale di item

**Ritorna:** Elemento selezionato o nil  
**Esempio:**
```lisp
(setq scelta (get-list-selection "lista_citta" 
  '("Milano" "Roma" "Torino")))
```

---

##### `SET-LIST-SELECTION`
Imposta selezione in list_box  
**Argomenti:**
- `key` - Nome list_box
- `value` - Valore da selezionare (o indice numerico)
- `item-list` - Lista originale di item

---

##### `INSERT-LIST-ITEMS`
Inserisce item in list_box (con clear)  
**Argomenti:**
- `key` - Nome list_box
- `item-list` - Lista di item da inserire

**Esempio:**
```lisp
(insert-list-items "lista_prodotti" 
  '("Prodotto A" "Prodotto B" "Prodotto C"))
```

---

##### `GET-MULTI-SELECTION`
Ottiene elementi selezionati da list_box multipla  
**Argomenti:**
- `key` - Nome list_box
- `item-list` - Lista originale di item

**Ritorna:** Lista di elementi selezionati

---

##### `SET-MULTI-SELECTION`
Imposta selezione multipla in list_box  
**Argomenti:**
- `key` - Nome list_box
- `indices` - Lista di indici da selezionare

---

#### 4. GESTIONE IMMAGINI E SLIDE

##### `DISPLAY-SLIDE`
Visualizza una slide in un controllo immagine DCL  
**Argomenti:**
- `slide-name` - Percorso file .sld

**Ritorna:** T  
**Nota:** Richiede controllo immagine con key "slide" nel DCL

**Esempio:**
```lisp
(display-slide "C:\\slides\\schema01.sld")
```

---

##### `CLEAR-SLIDE`
Cancella/riempie area slide con colore  
**Argomenti:**
- `color` - Numero colore AutoCAD (default 3 = verde)

**Esempio:**
```lisp
(clear-slide 250)  ; Bianco
(clear-slide 3)    ; Verde
```

---

##### `TOGGLE-IMAGE`
Accende/spegne un controllo immagine  
**Argomenti:**
- `image-key` - Nome controllo immagine
- `on` - T per accendere, nil per spegnere

**Esempio:**
```lisp
(toggle-image "immagine1" T)   ; Accende (verde)
(toggle-image "immagine1" nil) ; Spegne (nero)
```

---

## Gestione Entità

### File: `_entita.lsp`

#### 1. LETTURA ATTRIBUTI ENTITÀ

##### `GET-ENTITY-ATTRIBUTE`
Ottiene valore attributo di un'entità  
**Argomenti:**
- `entity` - Nome entità
- `attrib-code` - Codice attributo DXF

**Ritorna:** Valore attributo o nil  
**Esempi:**
```lisp
(get-entity-attribute ent 0)  => Tipo entità ("LINE", "CIRCLE")
(get-entity-attribute ent 8)  => Layer
(get-entity-attribute ent 62) => Colore
```

---

##### `GET-ENTITY-ATTRIBUTES-LIST`
Ottiene lista di tutti i valori per attributo ripetuto  
**Argomenti:**
- `entity` - Nome entità
- `attrib-code` - Codice attributo

**Ritorna:** Lista di valori  
**Nota:** Utile per LWPOLYLINE con vertici multipli  
**Esempio:**
```lisp
(get-entity-attributes-list ent 10) => Lista di punti vertici
```

---

##### `GET-ENTITY-HANDLE`
Ottiene handle di un'entità  
**Argomenti:**
- `entity` - Nome entità

**Ritorna:** Handle come stringa

---

##### `GET-ENTITY-NAME`
Ottiene nome di un'entità (per blocchi)  
**Argomenti:**
- `entity` - Nome entità

**Ritorna:** Nome blocco o nil

---

##### `GET-ENTITY-TYPE`
Ottiene tipo di un'entità  
**Argomenti:**
- `entity` - Nome entità

**Ritorna:** Tipo entità ("LINE", "CIRCLE", "INSERT", ecc.)

---

##### `GET-ENTITY-WIDTH`
Restituisce larghezza costante di polyline (codice DXF 43)  
**Argomenti:**
- `ent` - Entità polyline

**Ritorna:** Larghezza o nil

---

#### 2. MODIFICA ATTRIBUTI ENTITÀ

##### `SET-ENTITY-ATTRIBUTE`
Modifica valore attributo di un'entità  
**Argomenti:**
- `entity` - Nome entità
- `attrib-code` - Codice attributo DXF
- `value` - Nuovo valore

**Ritorna:** T se successo  
**Esempi:**
```lisp
(set-entity-attribute ent 8 "NUOVO_LAYER")
(set-entity-attribute ent 62 1)  ; Rosso
```

---

##### `REMOVE-ENTITY-ATTRIBUTE`
Rimuove un attributo da un'entità  
**Argomenti:**
- `entity` - Nome entità
- `attrib-code` - Codice attributo da rimuovere

**Ritorna:** T se successo  
**Esempio:**
```lisp
(remove-entity-attribute ent 62) ; Rimuove colore
```

---

##### `SET-ENTITY-TO-BYLAYER`
Imposta entità su layer 0 con colore BYBLOCK  
**Argomenti:**
- `entity` - Nome entità

**Ritorna:** T

---

#### 3. GESTIONE BLOCCHI RICORSIVA

##### `MODIFY-BLOCK-RECURSIVE`
Modifica attributo di tutte le entità in un blocco (ricorsivo)  
**Argomenti:**
- `entity` - Entità blocco
- `attrib-code` - Codice attributo
- `value` - Nuovo valore

**Nota:** Gestisce blocchi annidati e attributi  
**Esempio:**
```lisp
(modify-block-recursive ent 62 3) ; Imposta colore rosso
```

---

##### `MODIFY-ALL-BLOCKS-BY-NAME`
Modifica attributo di tutti i blocchi con nome specifico  
**Argomenti:**
- `block-name` - Nome blocco da cercare
- `attrib-code` - Codice attributo
- `value` - Nuovo valore

**Ritorna:** Numero blocchi modificati  
**Esempio:**
```lisp
(modify-all-blocks-by-name "MIO_BLOCCO" 62 5) ; Imposta colore blu
```

---

## Gestione File

### File: `_file.lsp`

#### 1. CONTROLLI DISEGNO / STATO

##### `IS-DRAWING-MODIFIED`
Verifica se il disegno corrente è modificato (DBMOD bit0)  
**Ritorna:** T o nil

---

#### 2. DATA / ORA E CONFRONTI

##### `CAD-DATE-NUMBER-TO-TIME-LIST`
Converte numero CAD YYYYMMDD.HHMM in lista (year month day hour minute second)  
**Argomenti:**
- `cad-date` - numero CAD

**Ritorna:** lista time o nil  
**Esempio:**
```lisp
(cad-date-number-to-time-list 20240115.1030)
=> (2024 1 15 10 30 0)
```

---

##### `GET-FILE-DATE`
Ottiene data/ora file come stringa "YYYY-MM-DD HH:MM:SS"  
**Argomenti:**
- `filename` - percorso file

**Ritorna:** stringa data o nil

---

##### `GET-FILE-DATE-NUMBER`
Data in formato numerico CAD YYYYMMDD.HHMM  
**Argomenti:**
- `filename` - percorso file

**Ritorna:** numero o nil

---

##### `FORMAT-DATE-STRING`
Formatta lista time in "GG/MM/AAAA   HH:MM"  
**Argomenti:**
- `time-list` - lista (year month day hour minute second)

**Ritorna:** stringa formattata o nil  
**Esempio:**
```lisp
(format-date-string '(2024 1 15 10 30 0)) => "15/01/2024   10:30"
```

---

##### `FORMAT-CAD-DATE-NUMBER`
Formatta numero CAD YYYYMMDD.HHMM in "GG/MM/AAAA   HH:MM"  
**Argomenti:**
- `data` - numero CAD

**Ritorna:** stringa formattata o nil  
**Esempio:**
```lisp
(format-cad-date-number 20240115.1030) => "15/01/2024   10:30"
```

---

##### `COMPARE-FILE-TIMES`
Confronta due liste time  
**Argomenti:**
- `time1` - lista (year month day hour minute second)
- `time2` - lista (year month day hour minute second)

**Ritorna:** 1 se time1 > time2, -1 se <, 0 se =  
**Esempio:**
```lisp
(compare-file-times '(2024 1 15 10 30 0) '(2024 1 14 12 0 0)) => 1
```

---

##### `GET-LATEST-FILE-DATE`
Trova la data più recente in lista file  
**Argomenti:**
- `file-list` - lista percorsi file

**Ritorna:** lista time o nil

---

##### `GET-LATEST-FILE-DATE-NUMBER`
Data più recente come numero CAD  
**Argomenti:**
- `file-list` - lista percorsi file

**Ritorna:** numero CAD o 0

---

##### `IS-FILE-NEWER`
Confronta le date di modifica di due file  
**Argomenti:**
- `file1` - Percorso del primo file (stringa)
- `file2` - Percorso del secondo file (stringa)

**Ritorna:** T se file1 è più recente, nil altrimenti  
**Esempio:**
```lisp
(is-file-newer "C:/temp/new.dwg" "C:/temp/old.dwg") => T
```

---

## Gestione Gruppi Selezione

### File: `_gruppi.lsp`

#### 1. MODIFICA ATTRIBUTI GRUPPI

##### `MODIFY-SS-ATTRIBUTE`
Modifica attributo per tutte le entità in un selection set  
**Argomenti:**
- `ss` - Selection set
- `attrib-code` - Codice attributo DXF
- `value` - Nuovo valore
- `recursive` - T per modificare anche blocchi annidati

**Ritorna:** Numero entità modificate  
**Esempio:**
```lisp
(modify-ss-attribute ss 62 1 T) ; Imposta colore rosso a tutte le entità in ss
```

---

##### `SET-SS-TO-LAYER`
Sposta tutte le entità di un SS su un layer  
**Argomenti:**
- `ss` - Selection set
- `layer-name` - Nome layer destinazione

**Ritorna:** Numero entità modificate

---

##### `SET-SS-COLOR`
Imposta colore per tutte le entità di un SS  
**Argomenti:**
- `ss` - Selection set
- `color` - Numero colore (0=BYBLOCK, 256=BYLAYER)

**Ritorna:** Numero entità modificate

---

##### `SET-SS-TO-BYLAYER`
Imposta tutte le entità su layer 0 e BYBLOCK  
**Argomenti:**
- `ss` - Selection set

**Ritorna:** Numero entità modificate

---

#### 2. RICERCA IN GRUPPI SELEZIONE

##### `FIND-RIGHTMOST-ENTITY`
Trova entità con punto inserzione più a destra  
**Argomenti:**
- `ss` - Selection set

**Ritorna:** Entità più a destra o nil

---

##### `FIND-LEFTMOST-ENTITY`
Trova entità con punto inserzione più a sinistra  
**Argomenti:**
- `ss` - Selection set

**Ritorna:** Entità più a sinistra o nil

---

##### `FIND-TOPMOST-ENTITY`
Trova entità con punto inserzione più in alto  
**Argomenti:**
- `ss` - Selection set

**Ritorna:** Entità più in alto o nil

---

##### `FIND-BOTTOMMOST-ENTITY`
Trova entità con punto inserzione più in basso  
**Argomenti:**
- `ss` - Selection set

**Ritorna:** Entità più in basso o nil

---

#### 3. BOUNDING BOX GRUPPI (SENZA VLA)

##### `GET-SS-BOUNDING-BOX`
Calcola bounding box di un selection set (senza VLA)  
**Argomenti:**
- `ss` - Selection set

**Ritorna:** Lista (punto-min punto-max) o nil

---

##### `GET-SS-CENTER`
Calcola punto centrale di un selection set  
**Argomenti:**
- `ss` - Selection set

**Ritorna:** Punto centrale (x y) o nil

---

## Gestione Layer

### File: `_layer.lsp`

#### 1. VERIFICA E INTERROGAZIONE LAYER

##### `LAYER-EXISTS`
Verifica se un layer esiste nel disegno  
**Argomenti:**
- `layer-name` - Nome layer da verificare

**Ritorna:** T se esiste, nil altrimenti

---

##### `IS-LAYER-ON`
Verifica se un layer è acceso  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** T se acceso, nil se spento

---

##### `IS-LAYER-LOCKED`
Verifica se un layer è bloccato  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** T se bloccato, nil se sbloccato

---

##### `IS-LAYER-FROZEN`
Verifica se un layer è congelato  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** T se congelato, nil se scongelato

---

##### `GET-LAYER-COLOR`
Ottiene colore di un layer  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** Numero colore o nil

---

##### `GET-LAYER-LINETYPE`
Ottiene tipo linea di un layer  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** Nome tipo linea o nil

---

#### 2. CREAZIONE E MODIFICA LAYER

##### `CREATE-LAYER`
Crea un nuovo layer o lo imposta come corrente  
**Argomenti:**
- `layer-name` - Nome layer
- `color` - Colore (opzionale)
- `linetype` - Tipo linea (opzionale)

**Ritorna:** T se successo

---

##### `SET-CURRENT-LAYER`
Imposta layer corrente  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** T se successo

---

##### `GET-CURRENT-LAYER`
Ottiene nome layer corrente  
**Ritorna:** Nome layer corrente

---

#### 3. STATO LAYER (ON/OFF/LOCK/FREEZE)

##### `TURN-LAYER-ON`
Accende un layer  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** T se successo

---

##### `TURN-LAYER-OFF`
Spegne un layer (passando prima su layer 0)  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** T se successo

---

##### `LOCK-LAYER`
Blocca un layer (può essere lista separata da virgole)  
**Argomenti:**
- `layer-spec` - Nome layer o lista separata da virgole

**Ritorna:** T

---

##### `UNLOCK-LAYER`
Sblocca un layer (può essere lista separata da virgole)  
**Argomenti:**
- `layer-spec` - Nome layer o lista separata da virgole

**Ritorna:** T

---

##### `FREEZE-LAYER`
Congela un layer  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** T

---

##### `THAW-LAYER`
Scongela un layer  
**Argomenti:**
- `layer-name` - Nome layer

**Ritorna:** T

---

##### `SET-LAYER-PLOTTABLE`
Imposta layer come stampabile  
**Argomenti:**
- `layer-name` - Nome layer
- `plottable` - T per stampabile, nil per non stampabile

**Ritorna:** T

---

## Gestione Linee

### File: `_linee.lsp`

#### 1. IMPOSTAZIONI E DEFAULT LINEE

##### `SET-LINE-DEFAULTS`
Imposta i valori predefiniti per linee (tipo, colore, peso, layer)  
**Restituisce:** T se eseguito correttamente  
**Nota:** Non apre finestre di dialogo - usa solo variabili di sistema

---

##### `GET-LINE-PROPERTIES`
Ottiene tutte le proprietà di una linea  
**Argomenti:**
- `line-ent` - Entità linea

**Ritorna:** Lista associativa con proprietà

---

#### 2. CREAZIONE LINEE

##### `INSERT-LINE-INTERACTIVE`
Inserisce linea interattivamente  
**Ritorna:** Entità linea creata

---

##### `CREATE-LINE`
Crea linea tra due punti  
**Argomenti:**
- `pt1` - Punto iniziale
- `pt2` - Punto finale
- `layer` - Layer (opzionale)
- `color` - Colore (opzionale)

**Ritorna:** Entità linea creata

---

#### 3. CALCOLO INTERSEZIONI

##### `CALC-LINE-INTERSECTION`
Calcola punto di intersezione tra due linee  
**Argomenti:**
- `line1` - Lista (pt1 pt2) prima linea
- `line2` - Lista (pt3 pt4) seconda linea

**Ritorna:** Punto intersezione o nil se non si intersecano  
**Nota:** Algoritmo robusto che gestisce tutti i casi

---

## Gestione Liste

### File: `_liste.lsp`

#### 1. CONVERSIONE LISTE/STRINGHE

##### `LIST-TO-STRING`
Converte lista di stringhe in stringa con delimitatore  
**Argomenti:**
- `lst` - Lista di stringhe
- `delimiter` - Carattere separatore

**Ritorna:** Stringa unica  
**Esempio:**
```lisp
(list-to-string '("A" "B" "C") ",") => "A,B,C"
```

---

##### `LISTS-TO-STRING-LIST`
Converte lista di liste in lista di stringhe  
**Argomenti:**
- `lst` - Lista di liste di stringhe
- `delimiter` - Carattere separatore

**Ritorna:** Lista di stringhe

---

#### 2. AGGIUNTA E INSERIMENTO ELEMENTI

##### `APPEND-ELEMENT`
Aggiunge elemento alla fine della lista (opposto di cons)  
**Argomenti:**
- `element` - Elemento da aggiungere
- `lst` - Lista

**Ritorna:** Nuova lista con elemento in coda

---

##### `APPEND-UNIQUE`
Aggiunge elemento solo se non esiste già  
**Argomenti:**
- `element` - Elemento da aggiungere
- `lst` - Lista

**Ritorna:** Nuova lista

---

##### `INSERT-AT-POSITION`
Inserisce elemento in posizione specifica  
**Argomenti:**
- `element` - Elemento da inserire
- `lst` - Lista
- `position` - Posizione (0-based)

**Ritorna:** Nuova lista

---

#### 3. RICERCA ELEMENTI

##### `FIND-POSITION`
Trova posizione di un elemento in lista (ottimizzata)  
**Argomenti:**
- `element` - Elemento da cercare
- `lst` - Lista in cui cercare

**Ritorna:** Posizione (0-based) o nil

---

##### `FIND-POSITION-REAL`
Trova posizione numero reale con tolleranza  
**Argomenti:**
- `element` - Numero da cercare
- `lst` - Lista numeri
- `tolerance` - Tolleranza (default 1e-6)

**Ritorna:** Posizione o nil

---

##### `FIND-MIN`
Trova valore minimo in lista numerica  
**Argomenti:**
- `lst` - Lista numeri

**Ritorna:** Valore minimo

---

##### `FIND-MAX`
Trova valore massimo in lista numerica  
**Argomenti:**
- `lst` - Lista numeri

**Ritorna:** Valore massimo

---

##### `FIND-MAX-POSITIVE`
Trova massimo tra numeri positivi  
**Argomenti:**
- `lst` - Lista numeri

**Ritorna:** Massimo positivo o 0

---

#### 4. RIMOZIONE ELEMENTI

##### `REMOVE-AT-POSITION`
Rimuove elemento in posizione specifica  
**Argomenti:**
- `position` - Posizione da rimuovere
- `lst` - Lista

**Ritorna:** Nuova lista senza elemento

---

##### `REMOVE-ELEMENT`
Rimuove ultima occorrenza elemento  
**Argomenti:**
- `element` - Elemento da rimuovere
- `lst` - Lista

**Ritorna:** Nuova lista

---

##### `REMOVE-ELEMENT-ALL`
Rimuove tutte le occorrenze elemento  
**Argomenti:**
- `element` - Elemento da rimuovere
- `lst` - Lista

**Ritorna:** Nuova lista

---

##### `REMOVE-DUPLICATES`
Rimuove duplicati dalla lista  
**Argomenti:**
- `lst` - Lista con possibili duplicati

**Ritorna:** Lista senza duplicati

---

#### 5. ESTRAZIONE SOTTOLISTE

##### `EXTRACT-SUBLIST`
Estrae sottolista da posizioni specifiche  
**Argomenti:**
- `positions` - Lista posizioni (come stringhe)
- `lst` - Lista sorgente

**Ritorna:** Sottolista

---

##### `EXTRACT-BY-CONDITION`
Estrae elementi che soddisfano condizione  
**Argomenti:**
- `test-str` - Stringa da cercare
- `lst` - Lista stringhe

**Ritorna:** Lista elementi che contengono test-str

---

##### `CUT-LIST-AT-ELEMENT`
Taglia lista dall'elemento in poi  
**Argomenti:**
- `element` - Elemento da cui tagliare
- `lst` - Lista

**Ritorna:** Sottolista da element in poi

---

##### `EXTRACT-NTH-FROM-SUBLISTS`
Estrae n-esimo elemento da lista di liste  
**Argomenti:**
- `position` - Posizione da estrarre
- `lst` - Lista di liste

**Ritorna:** Lista di elementi estratti

---

##### `EXTRACT-CARS`
Estrae primi elementi da lista di coppie  
**Argomenti:**
- `lst` - Lista di coppie (a . b)

**Ritorna:** Lista dei CAR

---

##### `EXTRACT-CDRS`
Estrae secondi elementi da lista di coppie  
**Argomenti:**
- `lst` - Lista di coppie (a . b)

**Ritorna:** Lista dei CDR

---

#### 6. ORDINAMENTO

##### `SORT-STRING-LIST`
Ordina lista di stringhe alfabeticamente  
**Argomenti:**
- `lst` - Lista stringhe

**Ritorna:** Lista ordinata

---

##### `SORT-NUMBER-LIST`
Ordina lista di numeri  
**Argomenti:**
- `lst` - Lista numeri

**Ritorna:** Lista ordinata

---

##### `SORT-STRING-NUMBER-LIST`
Ordina lista di stringhe numeriche  
**Argomenti:**
- `lst` - Lista stringhe rappresentanti numeri

**Ritorna:** Lista ordinata

---

##### `SORT-PAIR-LIST`
Ordina lista di coppie (numero . valore)  
**Argomenti:**
- `lst` - Lista di coppie

**Ritorna:** Lista ordinata per numero

---

#### 7. CONTEGGIO E STATISTICHE

##### `COUNT-OCCURRENCES`
Conta occorrenze elementi  
**Argomenti:**
- `lst` - Lista elementi

**Ritorna:** Lista associativa ((elemento . conteggio) ...)  
**Esempio:**
```lisp
(count-occurrences '("A" "B" "A" "C")) => (("A" . 2) ("B" . 1) ("C" . 1))
```

---

##### `COUNT-OCCURRENCES-PAIRS`
Conta e somma occorrenze per coppie (stringa . numero)  
**Argomenti:**
- `lst` - Lista coppie (stringa . numero)

**Ritorna:** Lista associativa con somme  
**Esempio:**
```lisp
(count-occurrences-pairs '(("A" . 2) ("B" . 3) ("A" . 4)))
=> (("A" . 6) ("B" . 3))
```

---

#### 8. CONVERSIONE TIPI

##### `STRING-LIST-TO-NUMBER-LIST`
Converte lista stringhe in lista numeri  
**Argomenti:**
- `lst` - Lista stringhe numeriche

**Ritorna:** Lista numeri (int o real)  
**Esempio:**
```lisp
(string-list-to-number-list '("10" "20.5" "30")) => (10 20.5 30.0)
```

---

##### `NUMBER-LIST-TO-STRING-LIST`
Converte lista numeri in lista stringhe  
**Argomenti:**
- `lst` - Lista numeri

**Ritorna:** Lista stringhe

---

## Funzioni Matematiche

### File: `_mat.lsp`

#### 1. CONTROLLO SEGNO E TIPO

##### `SAME-SIGN`
Verifica se due numeri hanno stesso segno  
**Argomenti:**
- `a` - Primo numero
- `b` - Secondo numero

**Ritorna:** T se stesso segno, nil altrimenti

---

##### `IS-POSITIVE`
Verifica se numero è positivo (>0)  
**Argomenti:**
- `n` - Numero da verificare

**Ritorna:** T se positivo, nil altrimenti

---

##### `IS-REAL-TYPE`
Verifica se numero è tipo REAL (ha decimali)  
**Argomenti:**
- `n` - Numero da verificare

**Ritorna:** T se ha decimali, nil se intero

---

##### `HAS-DECIMAL-PART`
Verifica se numero ha parte decimale non zero  
**Argomenti:**
- `n` - Numero da verificare

**Ritorna:** T se ha decimali, nil altrimenti

---

#### 2. ARROTONDAMENTO

##### `ROUND-NUMBER`
Arrotonda numero reale all'intero più vicino  
**Argomenti:**
- `r` - Numero reale

**Ritorna:** Numero intero arrotondato  
**Esempio:**
```lisp
(round-number 3.4) => 3
(round-number 3.6) => 4
```

---

##### `ROUND-UP`
Arrotonda sempre per eccesso  
**Argomenti:**
- `r` - Numero reale

**Ritorna:** Intero superiore  
**Esempio:**
```lisp
(round-up 3.1) => 4
(round-up 3.0) => 3
```

---

##### `ROUND-DOWN`
Arrotonda sempre per difetto  
**Argomenti:**
- `r` - Numero reale

**Ritorna:** Intero inferiore

---

##### `ROUND-TO-DECIMAL`
Arrotonda a N decimali  
**Argomenti:**
- `num` - Numero
- `decimals` - Numero decimali

**Ritorna:** Numero arrotondato  
**Esempio:**
```lisp
(round-to-decimal 3.14159 2) => 3.14
```

---

#### 3. ESTRAZIONE PARTI NUMERICHE

##### `GET-DECIMAL-PART`
Estrae parte decimale di un numero  
**Argomenti:**
- `r` - Numero reale

**Ritorna:** Parte decimale  
**Esempio:**
```lisp
(get-decimal-part 3.75) => 0.75
```

---

##### `GET-INTEGER-PART`
Estrae parte intera  
**Argomenti:**
- `r` - Numero reale

**Ritorna:** Parte intera

---

#### 4. PARITÀ E MODULO

##### `IS-EVEN`
Verifica se numero è pari  
**Argomenti:**
- `n` - Numero intero

**Ritorna:** T se pari, nil se dispari

---

##### `IS-ODD`
Verifica se numero è dispari  
**Argomenti:**
- `n` - Numero intero

**Ritorna:** T se dispari, nil se pari

---

#### 5. INTERPOLAZIONE

##### `LINEAR-INTERPOLATE`
Interpolazione lineare tra due punti  
**Argomenti:**
- `x` - Valore X da interpolare
- `x1` - X punto 1
- `y1` - Y punto 1
- `x2` - X punto 2
- `y2` - Y punto 2

**Ritorna:** Valore Y interpolato  
**Esempio:**
```lisp
(linear-interpolate 5 0 0 10 100) => 50.0
```

---

##### `INTERPOLATE-LIST`
Interpola valore in lista di coppie (x . y)  
**Argomenti:**
- `x` - Valore da interpolare
- `pairs` - Lista coppie ((x1 . y1) (x2 . y2) ...)

**Ritorna:** Y interpolato

---

#### 6. CONFRONTO E SELEZIONE

##### `ROUND-SYM`
Arrotondamento simmetrico (away from zero)  
**Argomenti:**
- `num` - Numero

**Ritorna:** Numero arrotondato

---

##### `ROUND-FLOOR`
Arrotondamento verso l'infinito negativo (floor)  
**Argomenti:**
- `num` - Numero

**Ritorna:** Numero arrotondato

---

## Gestione Polilinee

### File: `_polilinee.lsp`

#### 1. GESTIONE VERTICI - ESTRAZIONE E INFO

##### `GET-POLY-VERTICES`
Estrae vertici da entità POLYLINE o LWPOLYLINE (lista di punti)  
**Argomenti:**
- `ent` - Entità

**Ritorna:** lista punti  
**Nota:** Funziona anche con POLYLINE 3D ma non con POLYLINE 2D  
**Esempio:**
```lisp
(get-poly-vertices ent)
```

---

##### `GET-LWPOLYLINE-VERTICES`
Alias più esplicito su LWPOLYLINE  
**Argomenti:**
- `ent` - Entità

**Ritorna:** lista punti  
**Nota:** Funziona solo con LWPOLYLINE

---

##### `GET-NUMBERED-LWPOLYLINE-VERTICES`
Restituisce ((0 . pt) (1 . pt) ...)  
**Argomenti:**
- `ent` - Entità

**Ritorna:** lista associativa

---

##### `GET-VERTEX-BY-INDEX`
Estrae vertice per indice (0-based)  
**Argomenti:**
- `ent` - Entità
- `index` - Indice del vertice

**Ritorna:** punto o nil  
**Esempio:**
```lisp
(get-vertex-by-index ent 2)
```

---

##### `GET-VERTEX-COUNT`
Conta i vertici di una polilinea  
**Argomenti:**
- `ent` - Entità

**Ritorna:** intero

---

##### `GET-FIRST-VERTEX`
Ottiene il primo vertice  
**Argomenti:**
- `ent` - Entità

**Ritorna:** punto o nil

---

##### `GET-LAST-VERTEX`
Ottiene l'ultimo vertice  
**Argomenti:**
- `ent` - Entità

**Ritorna:** punto o nil

---

##### `IS-POLYLINE-CLOSED`
Verifica se polilinea è chiusa  
**Argomenti:**
- `ent` - Entità

**Ritorna:** T se chiusa, nil altrimenti

---

##### `GET-POLYLINE-PROPERTIES`
Restituisce proprietà base della polilinea  
**Argomenti:**
- `ent` - Entità

**Ritorna:** lista associativa  
**Esempio di output:**
```lisp
(get-polyline-properties ent) => ((layer . "0") (color . 7) ...)
```

---

#### 2. RICERCA E INDICIZZAZIONE

##### `FIND-VERTEX-POSITION`
Cerca punto nella lista, ritorna indice 0-based o -1  
**Argomenti:**
- `vertex` - Punto da cercare
- `vertex-list` - Lista di punti

**Ritorna:** indice o -1  
**Nota:** usa tolleranza 1e-6 per confronto punti  
**Esempio:**
```lisp
(find-vertex-position '(10 20) '((0 0) (10 20) (30 40)))
```

---

##### `FIND-SEGMENT-AFTER-POINT`
Trova indice segmento successivo al punto (1-based)  
**Argomenti:**
- `point` - Punto di riferimento
- `ent` - Entità

**Ritorna:** indice segmento o nil  
**Nota:** Funziona solo con LWPOLYLINE

---

##### `FIND-SEGMENT-AT-POINT`
Ritorna (v1 v2 pos)  
**Argomenti:**
- `point` - Punto di riferimento
- `ent` - Entità

**Ritorna:** lista o nil

---

## Gestione Punti

### File: `_punti.lsp`

#### 1. FUNZIONI FONDAMENTALI

##### `CALC-DISTANCE`
Calcola la distanza euclidea tra due punti (2D o 3D)  
**Argomenti:**
- `pt1` - Primo punto come lista (x y) o (x y z)
- `pt2` - Secondo punto come lista (x y) o (x y z)

**Ritorna:** Distanza come numero reale, o nil se input non valido  
**Esempi:**
```lisp
(calc-distance '(0 0) '(3 4)) => 5.0
(calc-distance '(0 0 0) '(1 1 1)) => 1.73205
```

---

##### `CALC-MIDPOINT`
Calcola il punto medio tra due punti (2D o 3D)  
**Argomenti:**
- `pt1` - Primo punto
- `pt2` - Secondo punto

**Ritorna:** Punto medio come lista, o nil se input non valido  
**Esempio:**
```lisp
(calc-midpoint '(0 0) '(10 10)) => (5.0 5.0)
```

---

##### `GET-X-COORDS`
Estrae le coordinate X da una lista di punti  
**Argomenti:**
- `pt-list` - Lista di punti

**Ritorna:** Lista di coordinate X  
**Esempio:**
```lisp
(get-x-coords '((1 2) (3 4) (5 6))) => (1 3 5)
```

---

##### `GET-MAX-X`
Trova la coordinata X massima in una lista di punti  
**Argomenti:**
- `pt-list` - Lista di punti

**Ritorna:** Valore X massimo come reale, o nil se lista vuota  
**Esempio:**
```lisp
(get-max-x '((1 2) (5 3) (3 4))) => 5
```

---

#### 2. OPERAZIONI SU LISTE DI PUNTI

##### `CALC-CENTROID`
Calcola il centro aritmetico (baricentro) di una lista di punti  
**Argomenti:**
- `pt-list` - Lista di punti (2D)

**Ritorna:** Punto baricentro come lista (x y), o nil se lista vuota  
**Esempio:**
```lisp
(calc-centroid '((0 0) (10 0) (5 10))) => (5.0 3.33333)
```

---

##### `CALC-CUMULATIVE-DISTANCE`
Calcola distanza cumulativa dall'inizio lista fino a punto specifico  
**Argomenti:**
- `target-pt` - Punto target da raggiungere (deve essere nella lista)
- `pt-list` - Lista di punti consecutivi

**Ritorna:** Distanza cumulativa come reale, o nil se punto non trovato  
**Esempio:**
```lisp
(calc-cumulative-distance '(5 0) '((0 0) (5 0) (10 0))) => 5.0
```

---

##### `FIND-NEAREST-POINT`
Trova il punto più vicino in una lista rispetto a un punto di riferimento  
**Argomenti:**
- `ref-pt` - Punto di riferimento
- `pt-list` - Lista di punti candidati

**Ritorna:** Punto più vicino dalla lista, o nil se lista vuota  
**Esempio:**
```lisp
(find-nearest-point '(5 5) '((0 0) (10 10) (6 6))) => (6 6)
```

---

##### `FIND-NEAREST-PAIR`
Trova il punto più vicino e il suo vicino in una lista  
**Argomenti:**
- `ref-pt` - Punto di riferimento
- `pt-list` - Lista di punti consecutivi

**Ritorna:** Lista di due punti (più vicino e suo vicino più prossimo)

---

## Gestione Record

### File: `_record.lsp`

**Struttura Record:** Un record è una lista associativa del tipo:
```lisp
'((campo1 . valore1) (campo2 . valore2) (campo3 . valore3))
Esempio: '((nome . "Mario") (eta . 30) (citta . "Milano"))
```

#### 1. MODIFICA CAMPI RECORD

##### `REPLACE-FIELD`
Sostituisce il valore di un campo in un record  
**Argomenti:**
- `record` - Record (lista associativa)
- `field` - Nome del campo (simbolo o stringa)
- `value` - Nuovo valore da assegnare

**Ritorna:** Nuovo record con campo modificato  
**Esempio:**
```lisp
(replace-field '((nome . "Mario") (eta . 30)) 'eta 31)
=> ((nome . "Mario") (eta . 31))
```

---

##### `UPDATE-FIELD`
Aggiorna il valore di un campo esistente in un record  
**Argomenti:**
- `record` - Record (lista associativa)
- `field` - Nome del campo da aggiornare
- `value` - Nuovo valore

**Ritorna:** Record aggiornato  
**Nota:** Se il campo non esiste, viene aggiunto  
**Esempio:**
```lisp
(update-field '((x . 10) (y . 20)) 'x 15)
=> ((x . 15) (y . 20))
```

---

#### 2. QUERY E RICERCA

##### `QUERY-RECORDS`
Cerca tutti i record in una lista che hanno un campo con valore specifico  
**Argomenti:**
- `field` - Nome del campo da verificare
- `value` - Valore da cercare
- `record-list` - Lista di record da interrogare

**Ritorna:** Lista di record che soddisfano la condizione, o 'ERRORE se nessuno  
**Esempio:**
```lisp
(query-records 'citta "Milano" 
  '(((nome . "Mario") (citta . "Milano"))
    ((nome . "Luca") (citta . "Roma"))
    ((nome . "Anna") (citta . "Milano"))))
=> (((nome . "Mario") (citta . "Milano")) 
    ((nome . "Anna") (citta . "Milano")))
```

---

##### `FIND-RECORDS-BY-FIELD`
Alias per query-records con nome più descrittivo

---

##### `GET-FIELD-VALUE`
Estrae il valore di un campo da un record  
**Argomenti:**
- `record` - Record da cui estrarre
- `field` - Nome del campo

**Ritorna:** Valore del campo, o nil se non esiste  
**Esempio:**
```lisp
(get-field-value '((nome . "Mario") (eta . 30)) 'eta) => 30
```

---

#### 3. ESTRAZIONE DATI

##### `EXTRACT-FIELD-VALUES`
Estrae tutti i valori unici di un campo da una lista di record  
**Argomenti:**
- `record-list` - Lista di record
- `field` - Nome del campo da estrarre

**Ritorna:** Lista di valori unici (senza duplicati)  
**Esempio:**
```lisp
(extract-field-values 
  '(((nome . "Mario") (citta . "Milano"))
    ((nome . "Luca") (citta . "Roma"))
    ((nome . "Anna") (citta . "Milano")))
  'citta)
=> ("Milano" "Roma")
```

---

##### `EXTRACT-ALL-FIELD-VALUES`
Estrae tutti i valori di un campo (inclusi duplicati)  
**Argomenti:**
- `record-list` - Lista di record
- `field` - Nome del campo da estrarre

**Ritorna:** Lista di tutti i valori (con duplicati)  
**Esempio:**
```lisp
(extract-all-field-values 
  '(((x . 1) (y . 2)) ((x . 1) (y . 3)) ((x . 2) (y . 2)))
  'x)
=> (1 1 2)
```

---

##### `COUNT-RECORDS`
Conta quanti record soddisfano una condizione  
**Argomenti:**
- `field` - Nome del campo
- `value` - Valore da cercare
- `record-list` - Lista di record

**Ritorna:** Numero di record trovati

---

##### `FILTER-RECORDS`
Filtra record basandosi su una funzione di test  
**Argomenti:**
- `test-func` - Funzione che ritorna T o nil per ogni record
- `record-list` - Lista di record da filtrare

**Ritorna:** Lista di record che passano il test  
**Esempio:**
```lisp
(filter-records 
  (function (lambda (r) (> (cdr (assoc 'eta r)) 25)))
  '(((nome . "Mario") (eta . 30)) ((nome . "Luca") (eta . 20))))
=> (((nome . "Mario") (eta . 30)))
```

---

##### `ADD-FIELD`
Aggiunge un nuovo campo a un record  
**Argomenti:**
- `record` - Record originale
- `field` - Nome nuovo campo
- `value` - Valore del nuovo campo

**Ritorna:** Record con campo aggiunto

---

##### `REMOVE-FIELD`
Rimuove un campo da un record  
**Argomenti:**
- `record` - Record originale
- `field` - Nome campo da rimuovere

**Ritorna:** Record senza il campo specificato

---

## Gestione Stringhe

### File: `_stringhe.lsp`

#### 1. DIVISIONE E PARSING

##### `SPLIT-STRING`
Divide stringa in lista usando delimitatore  
**Argomenti:**
- `str` - Stringa da dividere
- `delimiter` - Carattere separatore

**Ritorna:** Lista di sottostringhe  
**Esempio:**
```lisp
(split-string "A,B,C" ",") => ("A" "B" "C")
```

---

##### `SPLIT-STRING-TO-NUMBERS`
Divide stringa di numeri in lista numerica  
**Argomenti:**
- `str` - Stringa numeri separati
- `delimiter` - Carattere separatore

**Ritorna:** Lista di numeri  
**Esempio:**
```lisp
(split-string-to-numbers "1.5,2.3,4.0" ",") => (1.5 2.3 4.0)
```

---

#### 2. ESTRAZIONE PARTI

##### `STRING-LEFT-OF`
Estrae parte sinistra di stringa rispetto a delimitatore  
**Argomenti:**
- `str` - Stringa da processare
- `delimiter` - Delimitatore

**Ritorna:** Parte sinistra  
**Esempio:**
```lisp
(string-left-of "nome=valore" "=") => "nome"
```

---

##### `STRING-RIGHT-OF`
Estrae parte destra di stringa rispetto a delimitatore  
**Argomenti:**
- `str` - Stringa da processare
- `delimiter` - Delimitatore

**Ritorna:** Parte destra  
**Esempio:**
```lisp
(string-right-of "nome=valore" "=") => "valore"
```

---

##### `STRING-PART`
Estrae parte sinistra o destra rispetto a delimitatore  
**Argomenti:**
- `str` - Stringa da processare
- `delimiter` - Delimitatore
- `side` - "l" per sinistra, "r" per destra

**Ritorna:** Parte richiesta  
**Esempio:**
```lisp
(string-part "a=b=c" "=" "l") => "a"
```

---

##### `STRING-LEFT`
Estrae parte sinistra fino a delimitatore  
**Argomenti:**
- `str` - Stringa
- `delimiter` - Delimitatore

**Ritorna:** Parte sinistra  
**Esempio:**
```lisp
(string-left "nome=valore" "=") => "nome"
```

---

##### `STRING-RIGHT`
Estrae parte destra da delimitatore  
**Argomenti:**
- `str` - Stringa
- `delimiter` - Delimitatore

**Ritorna:** Parte destra  
**Esempio:**
```lisp
(string-right "nome=valore" "=") => "valore"
```

---

##### `STRING-MID`
Estrae sottostringhe da posizione specifica dopo delimitatore  
**Argomenti:**
- `str` - Stringa
- `delimiter` - Delimitatore
- `position` - Posizione nella lista risultante

**Ritorna:** Sottostringa  
**Esempio:**
```lisp
(string-mid "a:b:c" ":" 1) => "b"
```

---

##### `STRING-TAIL`
Mantiene ultimi N caratteri  
**Argomenti:**
- `n` - Numero caratteri
- `str` - Stringa

**Ritorna:** Ultimi N caratteri  
**Esempio:**
```lisp
(string-tail 3 "abcdef") => "def"
```

---

##### `STRING-HEAD`
Mantiene primi N caratteri  
**Argomenti:**
- `n` - Numero caratteri
- `str` - Stringa

**Ritorna:** Primi N caratteri  
**Esempio:**
```lisp
(string-head 3 "abcdef") => "abc"
```

---

##### `STRING-SKIP`
Salta primi N caratteri  
**Argomenti:**
- `n` - Numero caratteri da saltare
- `str` - Stringa

**Ritorna:** Stringa rimanente  
**Esempio:**
```lisp
(string-skip 2 "abcdef") => "cdef"
```

---

##### `STRING-LAST-CHAR`
Ottiene ultimo carattere  
**Argomenti:**
- `str` - Stringa

**Ritorna:** Ultimo carattere  
**Esempio:**
```lisp
(string-last-char "abc") => "c"
```

---

#### 3. RICERCA E POSIZIONAMENTO

##### `FIND-CHAR-POSITION`
Trova ultima posizione carattere in stringa (ricorsiva)  
**Argomenti:**
- `char` - Carattere da cercare
- `str` - Stringa

**Ritorna:** Posizione o 0  
**Esempio:**
```lisp
(find-char-position "." "a.b.c") => 3
```

---

##### `FIND-FIRST-SUBSTRING`
Trova prima posizione di una sottostringa in una stringa (sostituisce vl-string-search)  
**Argomenti:**
- `pattern` - Sottostringa da cercare
- `str` - Stringa in cui cercare

**Ritorna:** Posizione (1-based) o nil  
**Esempio:**
```lisp
(find-first-substring "test" "questo è un test") => 13
(find-first-substring "xyz" "questa stringa") => nil
```

---

##### `FIND-LAST-SUBSTRING`
Cerca l'ULTIMA occorrenza di una sottostringa in una stringa  
**Argomenti:**
- `pattern` - Sottostringa da cercare
- `str` - Stringa in cui cercare

**Ritorna:** Posizione o nil  
**Esempio:**
```lisp
(find-last-substring "ab" "abxxab") => 5
```

---

##### `FIND-SUBSTRING-FROM`
Trova la prima occorrenza della sottostringa a partire da una posizione  
**Argomenti:**
- `substring` - Sottostringa da cercare
- `str` - Stringa in cui effettuare la ricerca
- `start-pos` - Posizione iniziale di ricerca (1-based)

**Ritorna:** Posizione (1-based) della prima occorrenza trovata, oppure nil  
**Esempio:**
```lisp
(find-substring-from "test" "this is a test test" 10) => 15
```

---

##### `STRING-CONTAINS`
Verifica se stringa contiene sottostringa  
**Argomenti:**
- `substring` - Sottostringa da cercare
- `str` - Stringa

**Ritorna:** T se contiene, nil altrimenti  
**Esempio:**
```lisp
(string-contains "test" "this is a test") => T
```

---

#### 4. RIMOZIONE CARATTERI

##### `REMOVE-CHAR`
Rimuove tutti i caratteri specifici  
**Argomenti:**
- `char` - Carattere da rimuovere
- `str` - Stringa

**Ritorna:** Stringa pulita  
**Esempio:**
```lisp
(remove-char "-" "a-b-c") => "abc"
```

---

##### `REMOVE-SPACES`
Rimuove tutti gli spazi  
**Argomenti:**
- `str` - Stringa

**Ritorna:** Stringa senza spazi  
**Esempio:**
```lisp
(remove-spaces "a b c") => "abc"
```

---

## Gestione XDATA

### File: `_xdata.lsp`

**Codici XDATA Comuni:**
- `1000` - Stringa ASCII
- `1001` - Nome applicazione registrata
- `1002` - Parentesi apertura/chiusura { }
- `1003` - Nome layer
- `1005` - Handle database
- `1010` - 3 numeri reali (punto 3D)
- `1011` - Posizione world space 3D
- `1040` - Numero reale
- `1070` - Integer 16-bit
- `1071` - Long integer 32-bit

#### 1. RICERCA ENTITÀ CON XDATA

##### `SEARCH-ENTITIES-BY-XDATA`
Trova entità collegate ad un'entità padre tramite XDATA  
**Argomenti:**
- `parent-ent` - Entità padre di riferimento
- `entity-type` - Tipo entità da cercare (es. "LWPOLYLINE,HATCH")
- `block-name` - Nome blocco (solo per INSERT), nil per altri
- `xdata-name` - Nome applicazione XDATA ("*" per tutte)

**Ritorna:** Lista di entità trovate  
**Esempio:**
```lisp
(search-entities-by-xdata ent-principale "LWPOLYLINE" nil "MIA_APP")
```

---

##### `FIND-ENTITIES-WITH-XDATA`
Trova tutte le entità che contengono una specifica applicazione XDATA  
**Argomenti:**
- `xdata-name` - Nome applicazione XDATA (stringa)
- `entity-type` - Tipo entità (stringa, opzionale; nil = tutti i tipi)

**Ritorna:** Lista di ename delle entità trovate  
**Esempio:**
```lisp
(find-entities-with-xdata "MIA_APP" "LWPOLYLINE")
```

---

#### 2. LETTURA VALORI XDATA

##### `GET-XDATA-APP-NAME`
Ottiene il nome della prima applicazione XDATA di un'entità  
**Argomenti:**
- `entity` - Entità da interrogare

**Ritorna:** Nome applicazione o nil  
**Esempio:**
```lisp
(get-xdata-app-name ent)
```

---

##### `GET-XDATA-STRING`
Ottiene valore stringa da XDATA  
**Argomenti:**
- `entity` - Entità da interrogare
- `app-name` - Nome applicazione XDATA

**Ritorna:** Valore stringa (ultimo inserito) o nil  
**Esempio:**
```lisp
(get-xdata-string ent "MIA_APP")
```

---

##### `GET-XDATA-INTEGER`
Ottiene valore integer da XDATA  
**Argomenti:**
- `entity` - Entità da interrogare
- `app-name` - Nome applicazione XDATA

**Ritorna:** Valore integer (ultimo inserito) o nil  
**Esempio:**
```lisp
(get-xdata-integer ent "MIA_APP")
```

---

##### `GET-XDATA-REAL`
Ottiene valore real da XDATA  
**Argomenti:**
- `entity` - Entità da interrogare
- `app-name` - Nome applicazione XDATA

**Ritorna:** Valore real (ultimo inserito) o nil  
**Esempio:**
```lisp
(get-xdata-real ent "MIA_APP")
```

---

##### `GET-XDATA-POINT`
Ottiene valore punto 3D da XDATA  
**Argomenti:**
- `entity` - Entità da interrogare
- `app-name` - Nome applicazione XDATA

**Ritorna:** Punto (x y z) o nil  
**Esempio:**
```lisp
(get-xdata-point ent "MIA_APP")
```

---

##### `HAS-XDATA`
Verifica se un'entità ha XDATA per un'applicazione  
**Argomenti:**
- `entity` - Entità da verificare
- `app-name` - Nome applicazione XDATA

**Ritorna:** T se ha XDATA, nil altrimenti

---

## Note Finali

- **Compatibilità:** Tutte le funzioni sono scritte in AutoLISP puro e compatibili con ProgeCAD, DraftSight e AutoCAD
- **Evitare funzioni VL:** Non utilizzare funzioni VL (Visual LISP) poiché non sono compatibili con ProgeCAD
- **Licenza:** MIT License - Libero utilizzo e modifica
- **Dipendenze:** Verificare le dipendenze esterne indicate in ogni file LISP

---

**Fine Manuale - Generato il 29/01/2026**

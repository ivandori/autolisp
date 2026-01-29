demo_dcl : dialog {
  label = "Demo Gestione DCL / INI - Con Controlli Test";

  : column {
    : boxed_row {
      label = "Dati Anagrafici";

      : edit_box { key = "edit_nome"; label = "Nome:"; }
      : edit_box { key = "edit_cognome"; label = "Cognome:"; }
    }

    : boxed_row {
      label = "Opzioni";

      : toggle { key = "check_attivo"; label = "Attivo"; }
      : radio_button { key = "radio_a"; label = "Scelta A"; }
      : radio_button { key = "radio_b"; label = "Scelta B"; }
    }

    : boxed_row {
      label = "Liste";

      : list_box {
        key = "lista_citta";
        height = 4;
      }

      : list_box {
        key = "lista_opzioni";
        multiple_select = true;
        height = 4;
      }
    }

    : image {
      key = "slide";
      width = 40;
      height = 8;
    }

    : image {
        key = "simple_img";
        width = 40;
        height = 20;
        color = 1;
        fixed_width = true;
        fixed_height = true;
    }

    : boxed_row {
      label = "Test Funzioni Controlli";
      
      : column {
        : row {
          : button { 
            key = "btn_enable"; 
            label = "Abilita Edit"; 
            width = 12;
          }
          : button { 
            key = "btn_disable"; 
            label = "Disabilita Edit"; 
            width = 12;
          }
        }
       : row {
          : button { 
            key = "img_enable"; 
            label = "Abilita Immagine"; 
            width = 12;
          }
          : button { 
            key = "img_disable"; 
            label = "Disabilita Immagine"; 
            width = 12;
          }
        }
         
        : row {
          : button { 
            key = "btn_select"; 
            label = "Seleziona Check"; 
            width = 12;
          }
          : button { 
            key = "btn_deselect"; 
            label = "Deseleziona Check"; 
            width = 12;
          }
        }
        
        : row {
          : button { 
            key = "btn_sel_radio_a"; 
            label = "Radio A"; 
            width = 12;
          }
          : button { 
            key = "btn_sel_radio_b"; 
            label = "Radio B"; 
            width = 12;
          }
        }
      }
      
      : column {
        : row {
          : button { 
            key = "btn_fill_citta"; 
            label = "Riemp. Città"; 
            width = 12;
          }
          : button { 
            key = "btn_sel_roma"; 
            label = "Sel. Roma"; 
            width = 12;
          }
        }
        
        : row {
          : button { 
            key = "btn_fill_opzioni"; 
            label = "Riemp. Opzioni"; 
            width = 12;
          }
          : button { 
            key = "btn_sel_multi"; 
            label = "Sel. Multipla"; 
            width = 12;
          }
        }
        
        : row {
          : button { 
            key = "btn_clear_citta"; 
            label = "Pulisci Città"; 
            width = 12;
          }
          : button { 
            key = "btn_clear_opzioni"; 
            label = "Pulisci Opzioni"; 
            width = 12;
          }
        }
      }
    }

    : row {
      : button { key = "accept"; label = "OK"; is_default = true; }
      : button { key = "cancel"; label = "Annulla"; }
    }
  }
}
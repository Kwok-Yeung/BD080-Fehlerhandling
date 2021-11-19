REPORT zca_bd080_smsg_00l.

START-OF-SELECTION.

  DATA lf_ident TYPE sy-uzeit.
  DATA lf_identification TYPE sy-uzeit.
  DATA lf_line TYPE n LENGTH 3.
  DATA lf_string TYPE string.

  "Meldungssammlung starten
  GET TIME.
  lf_ident = sy-uzeit.

  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXPORTING
      i_identification = lf_ident
    IMPORTING
      e_identification = lf_identification
    EXCEPTIONS
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Meldung speichern
  ADD 1 TO lf_line.
  MESSAGE e248(29) WITH '100' 'M1' '1000' '000200' INTO lf_string.
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      arbgb  = sy-msgid
      msgty  = sy-msgty
      msgv1  = sy-msgv1
      msgv2  = sy-msgv2
      msgv3  = sy-msgv3
      msgv4  = sy-msgv4
      txtnr  = sy-msgno
      zeile  = lf_line
    EXCEPTIONS
      OTHERS = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Meldung speichern
  ADD 1 TO lf_line.
  MESSAGE i886(0d) WITH '2020' '8410005000' '12055,50' 'EUR' INTO lf_string.
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      arbgb  = sy-msgid
      msgty  = sy-msgty
      msgv1  = sy-msgv1
      msgv2  = sy-msgv2
      msgv3  = sy-msgv3
      msgv4  = sy-msgv4
      txtnr  = sy-msgno
      zeile  = lf_line
    EXCEPTIONS
      OTHERS = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Meldung speichern
  ADD 1 TO lf_line.
  MESSAGE w130(06) INTO lf_string.
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      arbgb  = sy-msgid
      msgty  = sy-msgty
      txtnr  = sy-msgno
      zeile  = lf_line
    EXCEPTIONS
      OTHERS = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Meldungssammlung stoppen
  CALL FUNCTION 'MESSAGES_STOP'
    EXPORTING
      i_identification  = lf_identification
    EXCEPTIONS
      a_message         = 1
      e_message         = 2
      w_message         = 3
      i_message         = 4
      s_message         = 5
      deactivated_by_md = 6
      OTHERS            = 7.
  CASE sy-subrc.
    WHEN 6 OR 7.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.

  "Anzahl Meldungen ermitteln
  DATA lf_count TYPE sy-tabix.

  CLEAR lf_count.
  CALL FUNCTION 'MESSAGES_COUNT'
    IMPORTING
      count  = lf_count
    EXCEPTIONS
      OTHERS = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Meldungen ausgeben
  IF lf_count > 0.
    CALL FUNCTION 'MESSAGES_SHOW'
      EXCEPTIONS
        OTHERS = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

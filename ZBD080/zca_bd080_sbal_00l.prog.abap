REPORT zca_bd080_sbal_00l.

START-OF-SELECTION.

  DATA lf_counter      TYPE balcntcum.
  DATA lf_log_handle   TYPE balloghndl.
  DATA ls_s_log        TYPE bal_s_log.
  DATA ls_s_msg        TYPE bal_s_msg.
  DATA lt_log_handle   TYPE bal_t_logh.

  "--- Meldungskopf anlegen
  CLEAR ls_s_log.

  ls_s_log-object    = 'BTC'. "Objekt
  ls_s_log-subobject = 'TEST'. "Unterobjekt
  ls_s_log-aldate    = sy-datum.
  ls_s_log-aluser    = sy-uname.
  ls_s_log-alprog    = sy-repid.
  ls_s_log-extnumber = 'ZBD080_' && sy-uname && '_' && sy-datum && '_' && sy-uzeit. "Externe Ident.
  ls_s_log-alprog    = sy-repid.

  CLEAR lf_log_handle.
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = ls_s_log
    IMPORTING
      e_log_handle            = lf_log_handle
    EXCEPTIONS
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE e305(cpcl) WITH TEXT-opr lf_log_handle.
  ENDIF.

  lf_counter = 0.

  "Meldung hinzufügen
  ADD 1 TO lf_counter.
  CLEAR ls_s_msg.
  ls_s_msg-msgty = 'I'.
  ls_s_msg-msgid = '29'.
  ls_s_msg-msgno = '248'.
  ls_s_msg-msgv1 = '100'.
  ls_s_msg-msgv2 = 'M1'.
  ls_s_msg-msgv3 = '1000'.
  ls_s_msg-msgv4 = '000200'.
  ls_s_msg-detlevel = '1'.
  ls_s_msg-msg_count = lf_counter.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle     = lf_log_handle
      i_s_msg          = ls_s_msg
    EXCEPTIONS
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Meldung hinzufügen
  ADD 1 TO lf_counter.
  CLEAR ls_s_msg.
  MESSAGE i886(0d) WITH '2020' '8410005000' '12055,50' 'EUR' INTO ls_s_msg-msgv1.
  MOVE-CORRESPONDING syst TO ls_s_msg.
  ls_s_msg-detlevel = '1'.
  ls_s_msg-msg_count = lf_counter.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle     = lf_log_handle
      i_s_msg          = ls_s_msg
    EXCEPTIONS
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Meldung hinzufügen
  ADD 1 TO lf_counter.
  CLEAR ls_s_msg.
  MESSAGE w130(06) INTO ls_s_msg-msgv1.
  MOVE-CORRESPONDING syst TO ls_s_msg.
  ls_s_msg-detlevel = '1'.
  ls_s_msg-msg_count = lf_counter.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle     = lf_log_handle
      i_s_msg          = ls_s_msg
    EXCEPTIONS
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Protokolle sichern
  CLEAR lt_log_handle[].
  APPEND lf_log_handle TO lt_log_handle.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  WRITE / 'Fertig!'.

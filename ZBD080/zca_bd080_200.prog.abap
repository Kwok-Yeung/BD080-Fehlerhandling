REPORT zca_bd080_200.

*C01  Kapitel 1: Debugging
*C02  Kapitel 2: Checkpoint-Gruppe
*C03  Kapitel 3: Nachrichtenklasse
*C04  Kapitel 4: Klassische Ausnahme
*C05  Kapitel 5: Ausnahmeklasse
*C06  Kapitel 6: Nachrichten sammeln
*C07  Kapitel 7: Protokoll speichern


*P_BALEXT	Ext. Identifikation
*P_BALOBJ	Objekt
*P_BALPAR	Parameter mitgeben
*P_BALSOB	Unterobjekt
*P_CLEBPI	BREAK-POINT ID
*P_CLELPI	LOG-POINT ID
*P_DEXBC1	Ü: Breakpoint mit Bedingung
*P_DEXDV1	Ü: Variablen anz. Fehlersuche
*P_DEXUW1	Ü: Watchpoint
*P_DLEBC1	Breakpoint mit Bedingung
*P_DLECOV	Variablen vergleichen
*P_DLEDV1	Variablen anzeigen Fehlersuche
*P_DLEGTS	Springe zu Anweisung
*P_DLENAV	Navigation
*P_DLESOC	Variablen anzeigen/ändern
*P_DLESYS	System Debugging
*P_DLEWP1	Watchpoint mit Bedingung
*P_DLEWP2	Watchpoint ohne Bedingung
*P_ELECEX	Klassische Ausnahme abfangen
*P_MLECSH	Nachr. mit Überschrift sammeln
*P_MLECSM	Nachr.ohne Überschrift sammeln
*P_SLERPD	Protokoll von Datenbank lesen
*P_SLESPR	Protokoll speichern
*P_SPRAS  Sprache
*P_XLECEC	Klassenbas. Ausnahme abfangen
*P_XLEDCE	Datenkonv. Ausnahme abfangen


"! --- Debugging ---
SELECTION-SCREEN COMMENT /1(70) TEXT-c01.
"! Navigation.
"! Navigation.
PARAMETERS p_dlenav RADIOBUTTON GROUP tas.
"! Show or change variable's value.
"! Variablen anzeigen/ändern
PARAMETERS p_dlesoc RADIOBUTTON GROUP tas.
"! Display variables to find error
"! Variablen anzeigen lassen zur Fehlersuche
PARAMETERS p_dledv1 RADIOBUTTON GROUP tas.
"! Compare variables
"! Variablen vergleichen
PARAMETERS p_dlecov RADIOBUTTON GROUP tas.
"! Exercise: Display variables to find error
"! Ü: Variablen anzeigen lassen zur Fehlersuche
PARAMETERS p_dexdv1 RADIOBUTTON GROUP tas.

"! Use break point with condition.
"! Verwende Breakpoint mit Bedingung.
PARAMETERS p_dlebc1 RADIOBUTTON GROUP tas.
"! Excercise: Use break point with condition.
"! Übung: Verwende Breakpoint mit Bedingung.
PARAMETERS p_dexbc1 RADIOBUTTON GROUP tas.

"! Use Watchpoint with condition.
"! Watchpoint mit Bedingung
PARAMETERS p_dlewp1 RADIOBUTTON GROUP tas.
"! Use Watchpoint without condition.
"! Watchpoint ohne Bedingung
PARAMETERS p_dlewp2 RADIOBUTTON GROUP tas.
"! Exercise: Watchpoint
"! Übung: Watchpoint
PARAMETERS p_dexuw1 RADIOBUTTON GROUP tas.

"! Go to statement.
"! Springe zu Anweisung
PARAMETERS p_dlegts RADIOBUTTON GROUP tas.

"! System debugging
"! System Debugging
PARAMETERS p_dlesys RADIOBUTTON GROUP tas.

"! Language
"! Sprache
PARAMETERS p_spras TYPE sy-langu DEFAULT 'S'.
SELECTION-SCREEN SKIP 1.


"! --- Checkpoint-Gruppe ---
SELECTION-SCREEN COMMENT /1(70) TEXT-c02.
"! BREAK-POINT ID
PARAMETERS p_clebpi RADIOBUTTON GROUP tas.
"! LOG-POINT ID
PARAMETERS p_clelpi RADIOBUTTON GROUP tas.
SELECTION-SCREEN SKIP 1.


" --- Nachrichtenklasse ---


"! --- Klassische Ausnahme ---
SELECTION-SCREEN COMMENT /1(70) TEXT-c04.
"! Catch classic exception
"! Klassische Ausnahme abfangen
PARAMETERS p_elecex RADIOBUTTON GROUP tas.
SELECTION-SCREEN SKIP 1.


"! --- Ausnahmeklasse ---
SELECTION-SCREEN COMMENT /1(70) TEXT-c05.
"! Catch class based exception
"! Klassenbas. Ausnahme abfangen
PARAMETERS p_xlecec RADIOBUTTON GROUP tas.
"! Data conversion. Catch exception.
"! Datenkonvertierung. Ausnahme abfangen
PARAMETERS p_xledce RADIOBUTTON GROUP tas.
SELECTION-SCREEN SKIP 1.


"! --- Nachrichten sammeln und ausgeben, MESSAGE_STORE
SELECTION-SCREEN COMMENT /1(70) TEXT-c06.
"! Collect and show messages.
"! Nachr.ohne Überschrift sammeln
PARAMETERS p_mlecsm RADIOBUTTON GROUP tas.
"! Collect and show messages with headlines.
"! Nachr. mit Überschrift sammeln
PARAMETERS p_mlecsh RADIOBUTTON GROUP tas.
SELECTION-SCREEN SKIP 1.


"! --- Protokoll speichern, SLG1
SELECTION-SCREEN COMMENT /1(70) TEXT-c07.
PARAMETERS p_balobj TYPE bal_s_log-object OBLIGATORY.
PARAMETERS p_balsob TYPE bal_s_log-subobject OBLIGATORY.
PARAMETERS p_balext TYPE bal_s_log-extnumber OBLIGATORY.
PARAMETERS p_balpar AS CHECKBOX. "Parameter mitgeben
"! Save protocoll.
"! Protokoll speichern.
PARAMETERS p_slespr RADIOBUTTON GROUP tas.
"! Read protocoll from data base.
"! Protokoll von Datenbank lesen.
PARAMETERS p_slerpd RADIOBUTTON GROUP tas.


CLASS lcl_main DEFINITION DEFERRED.
CLASS lcl_checkpointgroup DEFINITION DEFERRED.
CLASS lcl_classic_exception DEFINITION DEFERRED.
CLASS lcl_debugging DEFINITION DEFERRED.
CLASS lcx_exception_class DEFINITION DEFERRED.
CLASS lcl_exception_class DEFINITION DEFERRED.
CLASS lcl_show_collect_mess DEFINITION DEFERRED.
DATA go_main TYPE REF TO lcl_main.

*----------------------------------------------------------------------*
*       CLASS lcl_main DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS constructor.
    METHODS ev_start_of_selection.


  PROTECTED SECTION.
    CLASS-DATA gf_inst TYPE i.
    DATA mf_datum TYPE sy-datum.
    DATA mf_inst TYPE i.
ENDCLASS.                    "lcl_main DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_checkpointgroup DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_checkpointgroup DEFINITION FINAL.
  PUBLIC SECTION.

    "Lessons:
    METHODS le_break_point_id.
    METHODS le_log_point_id.

    "Exercises:

ENDCLASS.                    "lcl_checkpointgroup DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_classic_exception DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_classic_exception DEFINITION FINAL.
  PUBLIC SECTION.

    "Lessons:
    METHODS le_catch_exception.

    "Exercises:

ENDCLASS.                    "lcl_classic_exception DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_exception_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception_class DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    "INTERFACES if_t100_dyn_msg .

    "Exception C_MATERIAL_NOT_EXISTS
    CONSTANTS:
      BEGIN OF c_material_not_exists,
        msgid TYPE symsgid VALUE '09',
        msgno TYPE symsgno VALUE '419',
        attr1 TYPE scx_attrname VALUE 'MF_MATNR',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_material_not_exists .

    "Exception C_WRONG_PLANT
    CONSTANTS:
      BEGIN OF c_wrong_plant,
        msgid TYPE symsgid VALUE '29',
        msgno TYPE symsgno VALUE '887',
        attr1 TYPE scx_attrname VALUE 'MF_WERKS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF c_wrong_plant .

    "Attributes to be used as dynamic values in messages.
    DATA mf_matnr TYPE mara-matnr .
    DATA mf_werks TYPE t001w-werks .

    "Constructor
    "- Regarding itelligence naming conventions
    "- With parameters for dynamic values in messages.
    METHODS constructor
      IMPORTING
        !is_textid   LIKE if_t100_message=>t100key OPTIONAL
        !io_previous LIKE previous OPTIONAL
        !if_matnr    TYPE mara-matnr OPTIONAL
        !if_werks    TYPE t001w-werks OPTIONAL.

ENDCLASS.                    "lcx_exception_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcx_exception_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception_class_dyn DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    "Constructor
    "- Regarding itelligence naming conventions
    "- With parameters for dynamic values in messages.
    METHODS constructor
      IMPORTING
        !is_textid   LIKE if_t100_message=>t100key OPTIONAL
        !io_previous LIKE previous OPTIONAL.

ENDCLASS.                    "lcx_exception_class_dyn DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_debugging DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_debugging DEFINITION FINAL.
  PUBLIC SECTION.

    "Lessons:
    METHODS le_break_point_condition.
    METHODS le_compare_variables.
    METHODS le_display_variable_1.
    METHODS le_go_to_statement.
    METHODS le_navigation.
    METHODS le_show_or_change_variables.
    METHODS le_system_debugging.
    METHODS le_use_watchpoints1.
    METHODS le_use_watchpoints2.

    "Exercises:
    METHODS ex_break_point_condition.
    METHODS ex_display_variable_1.
    METHODS ex_wp_find_error_1.


  PRIVATE SECTION.

    TYPES: BEGIN OF ts_datum,
             datum TYPE sy-datum,
           END OF ts_datum.
    TYPES tt_datum TYPE STANDARD TABLE OF ts_datum.
    TYPES: BEGIN OF ts_material,
             matnr  TYPE mara-matnr,
             meins  TYPE mara-meins,
             matkl  TYPE mara-matkl,
             s_makt TYPE makt,
           END OF ts_material.
    TYPES tt_material TYPE STANDARD TABLE OF ts_material.
    TYPES tt_t005 TYPE STANDARD TABLE OF t005.

    "Help methods, e.g. to generate test data
    METHODS help_fill_compare_data
      EXPORTING
        es_mara_1 TYPE mara
        es_mara_2 TYPE mara
        et_t005_1 TYPE tt_t005
        et_t005_2 TYPE tt_t005.
    METHODS help_fill_t_datum
      EXPORTING
        et_datum TYPE tt_datum.
    METHODS help_fill_t_material
      EXPORTING
        et_material TYPE tt_material.
    METHODS help_navigation_1.
    METHODS help_navigation_2.
    METHODS help_navigation_3.

    METHODS help_print_datum
      IMPORTING
        if_datum TYPE sy-datum.
ENDCLASS.                    "lcl_debugging DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_exception_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_exception_class DEFINITION FINAL.
  PUBLIC SECTION.

    "Lessons:
    METHODS le_catch_class_exception.
    METHODS le_data_conv_catch_ex.

    "Exercises:


  PRIVATE SECTION.

    "Help methods.
    METHODS help_raise_exception
      RAISING
        lcx_exception_class.

    METHODS help_raise_exception_dyn
      RAISING
        lcx_exception_class_dyn.

ENDCLASS.                    "lcl_exception_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_save_protocol DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_save_protocol DEFINITION FINAL.
  PUBLIC SECTION.

    "Lessons:
    METHODS le_save_protocol.
    METHODS le_read_protocol_from_db.


  PRIVATE SECTION.

    TYPES: tt_bal_s_msg TYPE STANDARD TABLE OF bal_s_msg.

    "Help methods.
    METHODS help_get_temp_messages
      IMPORTING
        if_loghan   TYPE balloghndl
      EXPORTING
        et_messages TYPE tt_bal_s_msg.
ENDCLASS.                    "lcl_save_protocol DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_show_collect_mess DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_show_collect_mess DEFINITION FINAL.
  PUBLIC SECTION.

    "Lessons:
    METHODS le_show_collect_mess. "without headlines
    METHODS le_show_collect_mess2."with headlines


  PRIVATE SECTION.

    TYPES tf_line TYPE n LENGTH 3 .

    "Help methods.
    METHODS help_store_message_syst
      IMPORTING
        if_line TYPE tf_line.
    METHODS set_default_line_syst
      IMPORTING
        if_line TYPE tf_line.
ENDCLASS.                    "lcl_show_collect_mess DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_main IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD constructor.
    ADD 1 TO gf_inst.
    mf_datum = sy-datum.
    mf_inst = gf_inst.

    p_balobj = 'BTC'.
    p_balsob = 'TEST'.
    p_balext = 'CSU_' && sy-uname && '_' && sy-datum && '_' && sy-uzeit.
  ENDMETHOD.                    "constructor

  METHOD ev_start_of_selection.
    DATA lo_checkpointgroup TYPE REF TO lcl_checkpointgroup.
    DATA lo_classic_exeption TYPE REF TO lcl_classic_exception.
    DATA lo_exeption_class TYPE REF TO lcl_exception_class.
    DATA lo_debugging TYPE REF TO lcl_debugging.
    DATA lo_save_protocol TYPE REF TO lcl_save_protocol.
    DATA lo_show_collect_mess TYPE REF TO lcl_show_collect_mess.

    CASE abap_true.


        "--- Debugging

        "Lessons:
      WHEN p_dlebc1.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_break_point_condition( ).
      WHEN p_dlecov.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_compare_variables( ).
      WHEN p_dledv1.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_display_variable_1( ).
      WHEN p_dlegts.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_go_to_statement( ).
      WHEN p_dlenav.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_navigation( ).
        BREAK-POINT.
      WHEN p_dlesoc.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_show_or_change_variables( ).
      WHEN p_dlesys.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_system_debugging( ).
      WHEN p_dlewp1.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_use_watchpoints1( ).
      WHEN p_dlewp2.
        CREATE OBJECT lo_debugging.
        lo_debugging->le_use_watchpoints2( ).

        "Exercises:
      WHEN p_dexbc1.
        CREATE OBJECT lo_debugging.
        lo_debugging->ex_break_point_condition( ).
      WHEN p_dexdv1.
        CREATE OBJECT lo_debugging.
        lo_debugging->ex_display_variable_1( ).
      WHEN p_dexuw1.
        CREATE OBJECT lo_debugging.
        lo_debugging->ex_wp_find_error_1( ).


        "---Checkpoint group

      WHEN p_clebpi.
        CREATE OBJECT lo_checkpointgroup.
        lo_checkpointgroup->le_break_point_id( ).
      WHEN p_clelpi.
        CREATE OBJECT lo_checkpointgroup.
        lo_checkpointgroup->le_log_point_id( ).


        "---Classical exteptions

      WHEN p_elecex.
        CREATE OBJECT lo_classic_exeption.
        lo_classic_exeption->le_catch_exception( ).


        "---Class bases exceptions, TRY

      WHEN p_xlecec.
        CREATE OBJECT lo_exeption_class.
        lo_exeption_class->le_catch_class_exception( ).
      WHEN p_xledce.
        CREATE OBJECT lo_exeption_class.
        lo_exeption_class->le_data_conv_catch_ex( ).


        "--- Collect and show messages, MESSAGE_STORE

      WHEN p_mlecsm.
        CREATE OBJECT lo_show_collect_mess.
        lo_show_collect_mess->le_show_collect_mess( ).
      WHEN p_mlecsh.
        CREATE OBJECT lo_show_collect_mess.
        lo_show_collect_mess->le_show_collect_mess2( ).


        "--- Save protocol, SLG1

      WHEN p_slespr.
        CREATE OBJECT lo_save_protocol.
        lo_save_protocol->le_save_protocol( ).
      WHEN p_slerpd.
        CREATE OBJECT lo_save_protocol.
        lo_save_protocol->le_read_protocol_from_db( ).

    ENDCASE.
  ENDMETHOD.                    "ev_START_OF_SELECTION
ENDCLASS.                    "lcl_main IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_checkpointgroup IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_checkpointgroup IMPLEMENTATION.
  METHOD le_break_point_id.
    DATA lf_i TYPE i.

    lf_i = 1.
    lf_i = 2.
    BREAK-POINT ID zbd080.
    lf_i = 3.
    lf_i = 4.
  ENDMETHOD.                    "le_break_point_id

  METHOD le_log_point_id.
    DATA ls_t001 TYPE t001.
    DATA lt_t001 TYPE STANDARD TABLE OF t001.

    SELECT SINGLE * FROM t001 INTO ls_t001.

    SELECT * FROM t001 INTO TABLE lt_t001 UP TO 3 ROWS.

    LOG-POINT ID zbd080.

    "LS_T001
    LOG-POINT ID zbd080
      SUBKEY sy-uname && '_' && sy-datum
             && '_V0'
      FIELDS ls_t001-bukrs ls_t001-butxt ls_t001-ktopl
             ls_t001-land1.

    LOG-POINT ID zbd080
      SUBKEY sy-uname && '_' && sy-datum
             && '_V0'
      FIELDS ls_t001-bukrs ls_t001-butxt ls_t001-ktopl
             ls_t001-land1.

    "LT_T001, Versuch 1:
    "  Nur 1 Datensatz erscheint im Protokoll,
    "  weil der Subkey immer der gleich ist.
    LOOP AT lt_t001 INTO ls_t001.
      LOG-POINT ID zbd080
        SUBKEY sy-uname && '_' && sy-datum
               && '_V1'
        FIELDS ls_t001-bukrs ls_t001-butxt ls_t001-ktopl
               ls_t001-land1.
    ENDLOOP.

    "LT_T001, Versuch 2:
    "  Alle 3 Datensätze erscheinen im Protokoll,
    "  weil der Subkey immer unterschidlich ist.
    LOOP AT lt_t001 INTO ls_t001.
      LOG-POINT ID zbd080
        SUBKEY sy-uname && '_' && sy-datum
               && '_V2_' && ls_t001-bukrs
        FIELDS ls_t001-bukrs ls_t001-butxt ls_t001-ktopl
               ls_t001-land1.
    ENDLOOP.

    "Ganze Struktur übergeben (am Besten alleine)
    LOG-POINT ID zbd080 SUBKEY 'LS' FIELDS ls_t001.

    "Ganze interne Tabelle übergeben (am Besten alleine)
    LOG-POINT ID zbd080 SUBKEY 'LT' FIELDS lt_t001.


    WRITE: 'Ende'.
  ENDMETHOD.                    "le_log_point
ENDCLASS.                    "lcl_checkpointgroup IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_classic_exception IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_classic_exception IMPLEMENTATION.
  METHOD le_catch_exception.
    DATA lt_tline TYPE tline_tab.

    BREAK-POINT.

    "With all exceptions.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = sy-langu
        name                    = 'ZCSU'
        object                  = '-.,ß'
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    WRITE: / sy-subrc. "= 5, because the object is not existing

    "With less exceptions.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = sy-langu
        name                    = 'ZCSU'
        object                  = '-.,ß'
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 11
        language                = 32
*       name                    = 3
*       not_found               = 4
*       object                  = 5
*       reference_check         = 6
        wrong_access_to_archive = 700
        OTHERS                  = 18.
    IF sy-subrc <> 0.
      WRITE: / sy-subrc. "=18, because exception 'OBJECT' is not catched.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Without exceptions OBJECT and OTHERS.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = sy-langu
        name                    = 'ZCSU'
        object                  = '-.,ß'
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 11
        language                = 32
*       name                    = 3
*       not_found               = 4
*       object                  = 5
*       reference_check         = 6
        wrong_access_to_archive = 700.
*        OTHERS                  = 18.

    "=> The program ends with an error,
    "   because OBJECT and OTHERS are not catched.
    "=> The following lines will not be procesed.

    IF sy-subrc <> 0.
      WRITE: / sy-subrc.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "LE_CATCH_EXCEPTION
ENDCLASS.                    "lcl_classic_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_main IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_debugging IMPLEMENTATION.

  METHOD help_navigation_1.
    DATA lf_c TYPE c LENGTH 1.

    "Use F5 or F6!
    lf_c = 'a'.
    lf_c = 'b'.
    lf_c = 'c'.
  ENDMETHOD.                    "help_navigation_1

  METHOD help_navigation_2.
    DATA lf_c TYPE c LENGTH 1.

    "Use F7!
    lf_c = 'd'.
    lf_c = 'e'.
    lf_c = 'f'.
  ENDMETHOD.                    "help_navigation_2

  METHOD help_navigation_3.
    DATA lf_c TYPE c LENGTH 1.

    "Use F8!
    lf_c = 'g'.
    lf_c = 'h'.
    lf_c = 'i'.
  ENDMETHOD.                    "help_navigation_2

  METHOD help_print_datum.
    CASE if_datum+4(4).
      WHEN '0101'.
        WRITE: / 'Neujahr', if_datum+0(4).
      WHEN '1225' OR '1226'.
        WRITE: / 'Weihnachten', if_datum+0(4).
      WHEN OTHERS.
        WRITE: / if_datum.
    ENDCASE.
  ENDMETHOD.                    "help_print_datum

  METHOD le_break_point_condition.
    DATA ls_datum TYPE ts_datum.
    DATA lt_datum TYPE tt_datum.

    "Get test data.
    me->help_fill_t_datum(
      IMPORTING
        et_datum = lt_datum[] ).


    BREAK-POINT.
    "Debugge die folgende Methode,
    "aber nicht für jeden Datensatz,
    "sondern nur bei Datum '16001225'.


    LOOP AT lt_datum INTO ls_datum.
      me->help_print_datum( if_datum = ls_datum-datum ).
    ENDLOOP.
  ENDMETHOD.                    "le_break_point_condition

  METHOD ex_break_point_condition.
    DATA lt_datum TYPE tt_datum.
    FIELD-SYMBOLS <ls_datum> TYPE ts_datum.

    "Get test data.
    me->help_fill_t_datum(
      IMPORTING
        et_datum = lt_datum[] ).


    BREAK-POINT.
    "Debugge die folgende Methode,
    "aber nicht für jeden Datensatz,
    "sondern nur wenn das Datum mit '0101' endet.


    LOOP AT lt_datum ASSIGNING <ls_datum>.
      me->help_print_datum( if_datum = <ls_datum>-datum ).
    ENDLOOP.
  ENDMETHOD.                    "ex_break_point_condition

  METHOD ex_display_variable_1.
    DATA lt_t005t TYPE STANDARD TABLE OF t005t.
    FIELD-SYMBOLS <ls_t005t> TYPE t005t.

    SELECT spras land1 landx natio landx50
    FROM t005t INTO TABLE lt_t005t
    WHERE spras = 'D'
    AND   land1 LIKE 'D%'.


    BREAK-POINT.
    "Warum werden hier unsinnige Daten ausgegeben?


    LOOP AT lt_t005t ASSIGNING <ls_t005t>.
      WRITE: / 'Länderkürzel', <ls_t005t>-land1, 'steht für das Land', <ls_t005t>-landx.
    ENDLOOP.
  ENDMETHOD.                    "find_error_1

  METHOD ex_wp_find_error_1.
    DATA lf_datum TYPE sy-datum.
    DATA ls_datum TYPE ts_datum.
    DATA lt_datum TYPE tt_datum.

    "Get test data.
    me->help_fill_t_datum(
      IMPORTING
        et_datum = lt_datum[] ).


    BREAK-POINT.
    "Bitte nur diese Methode debuggen, nicht den Funktionsbaustein.
    "Welches Datum ist ungültig?
    "Der Funktionsbaustein setzt SY-SUBRC bei ungültigem Datum auf den Wert 1.


    sy-subrc = 0.

    LOOP AT lt_datum INTO ls_datum.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = ls_datum-datum
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.
    ENDLOOP.
  ENDMETHOD.                    "ex_wp_find_error_1

  METHOD help_fill_compare_data.
    DATA lo_descr_ref TYPE REF TO cl_abap_structdescr.
    DATA lr_t005 TYPE REF TO t005.
    FIELD-SYMBOLS <lf_any> TYPE any.
    FIELD-SYMBOLS <ls_comp>  TYPE abap_compdescr.

    CLEAR: es_mara_1, es_mara_2, et_t005_1[], et_t005_2[].

    "ES_MARA
    lo_descr_ref ?= cl_abap_typedescr=>describe_by_data( es_mara_1 ).
    LOOP AT lo_descr_ref->components ASSIGNING <ls_comp>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE es_mara_1 TO <lf_any>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      CASE <ls_comp>-type_kind.
        WHEN 'C'.
          <lf_any> = 'aBcDeFgHiJkLmNoPqRsTuVwXyZ'.
        WHEN 'D'.
          <lf_any> = '87654321'.
        WHEN 'N'.
          <lf_any> = '1'.
        WHEN 'P'.
          <lf_any> = '3.145192'.
      ENDCASE.
    ENDLOOP.
    es_mara_2 = es_mara_1.
    es_mara_2-zeinr+12(1) = 'n'.
    es_mara_2-mfrpn+18(1) = 't'.

    "ET_T005
    SELECT * FROM t005 UP TO 18 ROWS INTO TABLE et_t005_1.

    et_t005_2 = et_t005_1[].

    READ TABLE et_t005_2 REFERENCE INTO lr_t005 INDEX 3.
    IF sy-subrc = 0.
      lr_t005->intcn3 = 13.
      lr_t005->lnst4 = 33.
    ENDIF.
    READ TABLE et_t005_2 REFERENCE INTO lr_t005 INDEX 11.
    IF sy-subrc = 0.
      lr_t005->curin = 'CSU§!'.
    ENDIF.
    READ TABLE et_t005_2 REFERENCE INTO lr_t005 INDEX 15.
    IF sy-subrc = 0.
      lr_t005->sureg = 'Himmel'.
    ENDIF.
    DELETE et_t005_2 INDEX 17.
  ENDMETHOD.                    "help_fill_compare_data

  METHOD help_fill_t_datum.
    DATA ls_datum TYPE ts_datum.

    CLEAR et_datum[].

    DEFINE macro_append_datum.
      ls_datum-datum = &1.
      APPEND ls_datum TO et_datum.
    END-OF-DEFINITION.

    macro_append_datum:
    `20201225`, `20201226`, `20201229`, `20210102`, `20200104`, `20210225`, `20211227`,
    `20201125`, `20201226`, `20201229`, `20210125`, `20200111`, `20210214`, `20191225`,
    `23201225`, `20200101`, `20201029`, `24210805`, `20200721`, `17210223`, `20211230`,
    `20201225`, `20601226`, `20201229`, `20210105`, `20200115`, `20210225`, `20211225`,
    `20501225`, `20201204`, `20200729`, `20210105`, `16200111`, `20210205`, `18211025`,
    `20201214`, `20201226`, `20201229`, `22210130`, `20200218`, `20210225`, `20211217`,
    `20201225`, `20201226`, `20201203`, `20210105`, `20200111`, `20210215`, `20211225`,

    `20201221`, `20201226`, `20201224`, `20210102`, `20200104`, `20210225`, `20211227`,
    `20301125`, `20271226`, `20201229`, `20210125`, `90200111`, `20210214`, `00191225`,
    `23201224`, `20201225`, `20251029`, `24210805`, `20200121`, `17210223`, `20211230`,
    `20231225`, `20601213`, `20201226`, `20210108`, `20200115`, `20210225`, `20211225`,
    `20501225`, `20201204`, `20200729`, `20210105`, `16200511`, `10210101`, `18211025`,
    `20201214`, `20241226`, `20201229`, `82210130`, `20200118`, `20210225`, `20211217`,
    `20201225`, `20201226`, `20201203`, `20210105`, `20200111`, `20210215`, `20211225`,

    `20201225`, `20201226`, `20201229`, `20210102`, `20200104`, `20210225`, `20211227`,
    `20201125`, `20201226`, `20201229`, `20210125`, `20200111`, `20210214`, `20191225`,
    `23201225`, `20201226`, `20201029`, `24210805`, `20200721`, `17210223`, `20211230`,
    `20201225`, `20601226`, `20201229`, `20210105`, `20200115`, `20210225`, `20211225`,
    `20501225`, `20201204`, `20200729`, `20210105`, `16200111`, `20210205`, `18211025`,
    `20201214`, `20201226`, `20201229`, `22210130`, `20200218`, `20210225`, `20211217`,
    `20201225`, `20201226`, `20201203`, `20210105`, `20200111`, `20210215`, `20211225`,

    `20201223`, `20201226`, `20201223`, `20210102`, `20200104`, `20210225`, `20211227`,
    `20201125`, `20201229`, `20201229`, `20210125`, `20200111`, `20210214`, `19191225`,
    `23201228`, `20201226`, `20201029`, `24210805`, `20200321`, `17210223`, `20211230`,
    `20201225`, `20601226`, `20201131`, `20210102`, `20200115`, `16001225`, `20211225`,
    `20501225`, `20201204`, `20200729`, `20210105`, `16200111`, `20210205`, `18211025`,
    `20201214`, `20201226`, `20201229`, `22210130`, `20200118`, `20210225`, `20211217`,
    `20201225`, `20201226`, `20201203`, `20210105`, `20200111`, `20210215`, `20211225`.
  ENDMETHOD.                    "help_fill_t_datum

  METHOD help_fill_t_material.
    DATA ls_material TYPE ts_material.

    CLEAR et_material.

    CLEAR ls_material.
    ls_material-matnr = 'A4711'.
    ls_material-meins = 'ST'.
    ls_material-matkl = 'FERT'.
    ls_material-s_makt-mandt = sy-mandt.
    ls_material-s_makt-matnr = ls_material-matnr.
    ls_material-s_makt-spras = 'D'.
    ls_material-s_makt-maktx = 'Atlas 4711'.
    ls_material-s_makt-maktg = 'ATLAS 4711'.
    APPEND ls_material TO et_material.

    CLEAR ls_material.
    ls_material-matnr = 'A4712'.
    ls_material-meins = 'ST'.
    ls_material-matkl = 'FERT'.
    ls_material-s_makt-mandt = sy-mandt.
    ls_material-s_makt-matnr = ls_material-matnr.
    ls_material-s_makt-spras = 'D'.
    ls_material-s_makt-maktx = 'Atlas 4712'.
    ls_material-s_makt-maktg = 'ATLAS 4712'.
    APPEND ls_material TO et_material.
  ENDMETHOD.                    "fill_t_material

  METHOD le_compare_variables.
    DATA lo_main_1 TYPE REF TO lcl_main.
    DATA lo_main_2 TYPE REF TO lcl_main.
    DATA ls_mara_1 TYPE mara.
    DATA ls_mara_2 TYPE mara.
    DATA lt_t005_1 TYPE tt_t005.
    DATA lt_t005_2 TYPE tt_t005.

    "Testdaten generieren
    CREATE OBJECT lo_main_1.
    CREATE OBJECT lo_main_2.
    me->help_fill_compare_data(
      IMPORTING
        es_mara_1 = ls_mara_1
        es_mara_2 = ls_mara_2
        et_t005_1 = lt_t005_1[]
        et_t005_2 = lt_t005_2[] ).

    BREAK-POINT.
    "- Aufgabe 1
    "  - Welche Unterschiede gibt es zwischen LS_MARA_1 und LS_MARA_2?
    "  - Welche Unterschiede gibt es zwischen LT_T005_1 und LT_T005_2?
    "  - Welche Unterschiede gibt es zwischen LO_MAIN_1 und LO_MAIN_2?

  ENDMETHOD.                    "le_compare_variables

  METHOD le_display_variable_1.
    BREAK-POINT.

    IF p_spras = 'ES'.
      WRITE: / 'Spanisch ist eine tolle Sprache.' COLOR COL_POSITIVE.
    ELSE.
      WRITE: / 'FEHLER !' COLOR COL_NEGATIVE.
    ENDIF.

    BREAK-POINT. "Warum kommt die Fehlermeldung?
  ENDMETHOD.                    "find_error_1

  METHOD le_go_to_statement.
    WRITE '1'.
    WRITE '2'.

    BREAK-POINT.
    "Now go to statement "WRITE '5'."
    "Than press <F8>.

    WRITE '3'.
    WRITE '4'.
    WRITE '5'.   "<<<
    WRITE '6'.
  ENDMETHOD.                    "go_to_statement

  METHOD le_navigation.
    DATA lf_i TYPE i.

    BREAK-POINT.

    "--- Use <F5> !

    lf_i = 1.
    lf_i = 2.
    lf_i = 3.
    me->help_navigation_1( ).
    lf_i = 4.

    "--- Use <F6> !

    lf_i = 5.
    lf_i = 6.
    me->help_navigation_1( ).
    lf_i = 7.

    "--- Use <F5> !

    me->help_navigation_2( ).

    lf_i = 8.

    "--- Use <F5> !

    me->help_navigation_3( ).

    lf_i = 9.
    lf_i = 10.
    lf_i = 11.
  ENDMETHOD.                    "le_navigation

  METHOD le_show_or_change_variables.
    DATA lf_kunnr TYPE kunnr.
    DATA lr_makt TYPE REF TO makt.
    DATA ls_makt TYPE makt.
    DATA lt_makt TYPE STANDARD TABLE OF makt.
    DATA lt_makt_s TYPE SORTED TABLE OF makt WITH UNIQUE KEY matnr spras.
    DATA lt_material TYPE tt_material.

    ls_makt-mandt = sy-mandt.
    ls_makt-matnr = 'A4711'.
    ls_makt-spras = 'D'.
    ls_makt-maktx = 'Atlas 4711'.
    ls_makt-maktg = 'ATLAS 4711'.
    INSERT ls_makt INTO TABLE lt_makt_s.

    READ TABLE lt_makt_s REFERENCE INTO lr_makt INDEX 1.

    help_fill_t_material(
      IMPORTING
        et_material = lt_material[] ).

    BREAK-POINT.

    "Aufgabe 1: Inhalt des Feldes LF_KUNNR anzeigen lassen.
    "Aufgabe 2: Inhalt des Feldes LF_KUNNR ändern.

    "Aufgabe 3: Inhalt der Struktur LS_MAKT anzeigen lassen.
    "Aufgabe 4: Inhalt der Struktur LS_MAKT ändern.

    "Aufgabe 5: Inhalt der internen Tabelle LT_MAKT anzeigen lassen.
    "Aufgabe 6: Inhalt der internen Tabelle LT_MAKT ändern.
    "           - Zeilen hinzufügen
    "           - Zeilen ändern
    "           - Zeilen löschen

    "Aufgabe 7: Schlüssel der internen Tabelle LT_MAKT_S anzeigen lassen.

    "Aufgabe 8: Nur 2 Variablen in der Feldliste speichern.
    "           Degugging beenden. Debugger neu starten.
  ENDMETHOD.                    "show_or_change_variables_values

  METHOD le_system_debugging.
    DATA lo_poc TYPE REF TO cb_show_command_poc.


    BREAK-POINT.
    "Debugge Methode LO_POC->CONSTRUCTOR.
    "Hinweise:
    "  - Klasse CB_SHOW_COMMAND_POC ist als Systemprogramm gekennzeichnet.
    "  - Debugging ist nur bei eingeschaltetem Systemdebugging möglich.


    CREATE OBJECT lo_poc.
  ENDMETHOD.                    "le_system_debugging

  METHOD le_use_watchpoints1.
    TYPES: BEGIN OF ts_1,
             zahl        TYPE i,
             quadratzahl TYPE i,
           END OF ts_1.
    DATA lr_1 TYPE REF TO ts_1.
    DATA ls_1 TYPE ts_1.
    DATA lt_1 TYPE STANDARD TABLE OF ts_1.
    FIELD-SYMBOLS <ls_1> TYPE ts_1.

    "Testdaten erzeigen.
    DO 100000 TIMES.
      ls_1-zahl = sy-index.
      APPEND ls_1 TO lt_1.
    ENDDO.

    BREAK-POINT. "Watchpoint für LS_1-ZAHL mit Bedingung anlegen

    "Zu prüfender Quelltext.
    LOOP AT lt_1 INTO ls_1.
      TRY.
          ls_1-quadratzahl = ls_1-zahl ** 2.
        CATCH cx_root.
          FORMAT COLOR COL_NEGATIVE.
          WRITE: / 'Bei Zahl = ', ls_1-zahl, 'kommt es zu einem Fehler.'.
          FORMAT COLOR OFF.
          EXIT.
      ENDTRY.
    ENDLOOP.
    "Fazit: Watchpoint für Variable (Feld, Struktur, Tabelle) verwendbar.

    BREAK-POINT. "Watchpoint für <LS_1>-ZAHL mit Bedingung anlegen

    LOOP AT lt_1 ASSIGNING <ls_1>.
      TRY.
          <ls_1>-quadratzahl = <ls_1>-zahl ** 2.
        CATCH cx_root.
          FORMAT COLOR COL_NEGATIVE.
          WRITE: / 'Bei Zahl = ', <ls_1>-zahl, 'kommt es zu einem Fehler.'.
          FORMAT COLOR OFF.
          EXIT.
      ENDTRY.
    ENDLOOP.
    "Fazit: Watchpoint für Feldsymbol NICHT verwendbar.

    BREAK-POINT. "Watchpoint für LR_1->ZAHL mit Bedingung anlegen

    LOOP AT lt_1 REFERENCE INTO lr_1.
      TRY.
          lr_1->quadratzahl = lr_1->zahl ** 2.
        CATCH cx_root.
          FORMAT COLOR COL_NEGATIVE.
          WRITE: / 'Bei Zahl = ', lr_1->zahl, 'kommt es zu einem Fehler.'.
          FORMAT COLOR OFF.
          EXIT.
      ENDTRY.
    ENDLOOP.
    "Fazit: Watchpoint für Datenreferenz NICHT verwendbar.

    WRITE / 'Fertig.'.
  ENDMETHOD.                    "use_watchpoints1

  METHOD le_use_watchpoints2.
    TYPES: BEGIN OF ts_1,
             fieldname TYPE dd03l-fieldname,
           END OF ts_1.
    DATA lf_strlen TYPE i.
    DATA lt_1 TYPE STANDARD TABLE OF ts_1.
    FIELD-SYMBOLS <ls_1> TYPE ts_1.

    SELECT fieldname FROM dd03l INTO TABLE lt_1
    WHERE tabname = 'MARA'.

    BREAK-POINT.
    "Aufgabe 1: Watchpoint für LF_STRLEN ohne Bedingung anlegen.
    "Aufgabe 2: Watchpoint wieder löschen.

    LOOP AT lt_1 ASSIGNING <ls_1>.
      lf_strlen = strlen( <ls_1>-fieldname ).
    ENDLOOP.

    "HINWEIS: Watchpoints können nicht gespeichert werden.

  ENDMETHOD.                    "use_watchpoints2


ENDCLASS.                    "lcl_main IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_exception_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_exception_class IMPLEMENTATION.
  METHOD le_catch_class_exception.

    BREAK-POINT.

    "Call method which raises a static exception.
    TRY.
        me->help_raise_exception( ).
      CATCH lcx_exception_class INTO DATA(lo_x).
        "Get message text.
        sy-lisel = lo_x->get_text( ).
    ENDTRY.

    "Call method which raises a dynamic exception.
    TRY.
        me->help_raise_exception_dyn( ).
      CATCH lcx_exception_class_dyn INTO DATA(lo_x2).
        "Get message text.
        sy-lisel = lo_x->get_text( ).
    ENDTRY.

  ENDMETHOD.                    "le_catch_class_exception

  METHOD le_data_conv_catch_ex.
    DATA lf_char10 TYPE c LENGTH 10.
    DATA lf_i TYPE i.
    DATA lf_string TYPE string.
    DATA lo_x_root TYPE REF TO cx_root.

    BREAK-POINT.

    lf_char10 = 'Lieferant'.
    TRY.
        lf_i = lf_char10. "invalid value
      CATCH cx_root INTO lo_x_root.
        "Ausnahme CX_SY_CONVERSION_NO_NUMBER wurde ausgelöst.
        lf_string = lo_x_root->get_text( ).
        WRITE / lf_string.
    ENDTRY.

    TRY.
        lf_i = 1000000 * 1000000. "value to large
      CATCH cx_root INTO lo_x_root.
        "SAPgui: Ausnahme CX_SY_ARITHMETIC_OVERFLOW wird ausgelöst.
        "Debugging in Eclipse: Parse error
        lf_string = lo_x_root->get_text( ).
        WRITE / lf_string.
    ENDTRY.
  ENDMETHOD.                    "le_data_conv_catch_ex

  METHOD help_raise_exception.
    "Raise class based exception.

    RAISE EXCEPTION TYPE lcx_exception_class
      EXPORTING
        is_textid = lcx_exception_class=>c_material_not_exists
        if_matnr  = '4711'.
  ENDMETHOD.                    "help_raise_exception

  METHOD help_raise_exception_dyn.
    "Raise class based exception.

    RAISE EXCEPTION TYPE lcx_exception_class_dyn
      MESSAGE e305(m3) WITH '4711'.

  ENDMETHOD.                    "help_raise_exception_2

ENDCLASS.                    "lcl_exception_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcx_exception_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception_class IMPLEMENTATION.
  METHOD constructor ##adt_suppress_generation.
    CALL METHOD super->constructor
      EXPORTING
        previous = io_previous.

    "Fill attributes for static messages.
    me->mf_matnr = if_matnr.
    me->mf_werks = if_werks.

*      CLEAR me->ms_textid.
    "Fill key.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.                    "constructor

ENDCLASS.                    "lcx_exception_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcx_exception_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception_class_dyn IMPLEMENTATION.
  METHOD constructor ##adt_suppress_generation.
    CALL METHOD super->constructor
      EXPORTING
        previous = io_previous.

*      CLEAR me->ms_textid.
    "Fill key.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcx_exception_class_dyn IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_save_protocol IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_save_protocol IMPLEMENTATION.
  METHOD help_get_temp_messages.
    "Bisher gesammelte Meldungen ermitteln
    DATA ls_log_filter TYPE bal_s_lfil.
    DATA ls_s_msg TYPE bal_s_msg.
    DATA ls_r_extnumber TYPE bal_s_extn.
    DATA ls_r_object TYPE bal_s_obj.
    DATA ls_r_subobject TYPE bal_s_sub.
    DATA ls_bal_s_logh TYPE bal_s_logh.
    DATA lt_bal_r_logh TYPE bal_r_logh.
    DATA lt_i_t_log_handle TYPE bal_t_logh.
    DATA ls_msg_handle TYPE balmsghndl.
    DATA lt_e_t_log_handle TYPE bal_t_logh.
    DATA lt_e_t_msg_handle TYPE bal_t_msgh.
    FIELD-SYMBOLS <ls_msg_handle> TYPE balmsghndl.

    IF if_loghan IS INITIAL.
      RETURN.
    ENDIF.

    "Loghandle is known.

    "Fill selection criteria.
    CLEAR lt_i_t_log_handle.
    APPEND if_loghan TO lt_i_t_log_handle.

    "Get message numbers.
    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = lt_i_t_log_handle
      IMPORTING
        e_t_log_handle = lt_e_t_log_handle
        e_t_msg_handle = lt_e_t_msg_handle
      EXCEPTIONS
        msg_not_found  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Get messages.
    LOOP AT lt_e_t_msg_handle ASSIGNING <ls_msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <ls_msg_handle>
          i_langu        = sy-langu
        IMPORTING
          e_s_msg        = ls_s_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      "APPEND messages.
      APPEND ls_s_msg TO et_messages.
    ENDLOOP.
  ENDMETHOD.                    "help_get_temp_messages

  METHOD le_read_protocol_from_db.
    "Protokolle von der Datenbank ermitteln und anzeigen.
    DATA ls_r_extnumber TYPE bal_s_extn.
    DATA ls_r_object TYPE bal_s_obj.
    DATA ls_r_subobject TYPE bal_s_sub.
    DATA ls_log_filter TYPE bal_s_lfil.
    DATA ls_log_header   TYPE balhdr.
    DATA lt_log_header   TYPE balhdr_t.
    DATA lt_msg_handle   TYPE bal_t_msgh.
    DATA ls_s_msg TYPE bal_s_msg.
    DATA lt_log_handle   TYPE bal_t_logh.

    "Parameter füllen
    CLEAR ls_log_filter.

    CLEAR ls_r_object.
    ls_r_object-sign   = 'I'.
    ls_r_object-option = 'EQ'.
    ls_r_object-low    = p_balobj.
    APPEND ls_r_object TO ls_log_filter-object.

    CLEAR ls_r_subobject.
    ls_r_subobject-sign   = 'I'.
    ls_r_subobject-option = 'EQ'.
    ls_r_subobject-low    = p_balsob.
    APPEND ls_r_subobject TO ls_log_filter-subobject.

    CLEAR ls_r_extnumber.
    ls_r_extnumber-sign   = 'I'.
    ls_r_extnumber-option = 'EQ'.
    ls_r_extnumber-low    = p_balext.
    APPEND ls_r_extnumber TO ls_log_filter-extnumber.

    BREAK-POINT.

    "Protokolle suchen
    CLEAR lt_log_header[].
    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter     = ls_log_filter
      IMPORTING
        e_t_log_header     = lt_log_header
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Load protocoll from data base.
    CLEAR: lt_msg_handle[], lt_log_handle[].
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = lt_log_header
      IMPORTING
        e_t_log_handle     = lt_log_handle
        e_t_msg_handle     = lt_msg_handle
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Get messages.
    DATA lr_msg_handle TYPE REF TO balmsghndl.
    DATA lr_par TYPE REF TO bal_s_par.

    LOOP AT lt_msg_handle REFERENCE INTO lr_msg_handle.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = lr_msg_handle->*
          i_langu        = sy-langu
        IMPORTING
          e_s_msg        = ls_s_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      "Print messages.
      WRITE: / ls_s_msg-msgty, ls_s_msg-msgid, ls_s_msg-msgno,
               ls_s_msg-msgv1, ls_s_msg-msgv2, ls_s_msg-msgv3, ls_s_msg-msgv4.
      IF ls_s_msg-params-altext IS NOT INITIAL.
        WRITE: /3 ls_s_msg-params-altext.
      ENDIF.
      LOOP AT ls_s_msg-params-t_par REFERENCE INTO lr_par.
        WRITE: /5 lr_par->parname, lr_par->parvalue.
      ENDLOOP.
    ENDLOOP.

    "Refresh function group. Dequeue protocoll.
    DATA lr_log_header TYPE REF TO balhdr.
    LOOP AT lt_log_header REFERENCE INTO lr_log_header.
      CALL FUNCTION 'BAL_LOG_REFRESH'
        EXPORTING
          i_log_handle = lr_log_header->log_handle
        EXCEPTIONS
          OTHERS       = 0.
    ENDLOOP.
  ENDMETHOD.                    "le_read_protocol_from_db

  METHOD le_save_protocol.
    "Protokoll anlegen und auf der Datenbank sichern.
    DATA lf_counter      TYPE balcntcum.
    DATA lf_log_handle   TYPE balloghndl.
    DATA ls_s_log        TYPE bal_s_log.
    DATA ls_s_msg        TYPE bal_s_msg.
    DATA ls_par          TYPE bal_s_par.
    DATA lt_log_handle   TYPE bal_t_logh.

    BREAK-POINT.

    "Meldungskopf anlegen

    "Parameter für Funktionsbaustein 'BAL_LOG_CREATE' füllen
    CLEAR ls_s_log.

    ls_s_log-object    = p_balobj. "Objekt in Transaktion SLG0 anlegen
    ls_s_log-subobject = p_balsob."Unterobjekt in Transaktion SLG0 anlegen
    ls_s_log-aldate    = sy-datum.
    ls_s_log-aluser    = sy-uname.
    ls_s_log-alprog    = sy-repid.
    ls_s_log-extnumber = p_balext.
    ls_s_log-alprog    = sy-repid.

    "FB 'BAL_LOG_CREATE': Meldungskopf anlegen
    CLEAR lf_log_handle.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_s_log
      IMPORTING
        e_log_handle            = lf_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE e305(cpcl) WITH TEXT-opr lf_log_handle.
    ENDIF.

    WRITE: / 'Log handle:', lf_log_handle.
    WRITE: / 'Extnumber :', ls_s_log-extnumber.

    "Meldungszeilen hinzufügen
    lf_counter = 0.

    "1. Meldungszeile füllen
    ADD 1 TO lf_counter.
    CLEAR ls_s_msg.
    ls_s_msg-msgty = 'I'.
    ls_s_msg-msgid = '00'.
    ls_s_msg-msgno = '398'.
    ls_s_msg-msgv1 = 'ZCO_INVBEWERTUNG-PREIS alt'.
    ls_s_msg-msgv2 = '12.00'.
    ls_s_msg-msgv3 = 'ZCO_INVBEWERTUNG-PREIS neu'.
    ls_s_msg-msgv4 = '16.00'.
    ls_s_msg-detlevel = '1'.
    ls_s_msg-msg_count = lf_counter.

    IF p_balpar = abap_true.
      "OPTIONAL: Parameter hinzufügen.
      ls_s_msg-params-altext = 'Aufgabe'.
      ls_par-parname  = 'TASK_ICON'.
      ls_par-parvalue = 'ICON_DELETE'.
      APPEND ls_par TO ls_s_msg-params-t_par.
    ENDIF.

    "Meldungszeile hinzufügen
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
*       i_log_handle     = lf_log_handle
        i_s_msg          = ls_s_msg
*        IMPORTING
*       E_S_MSG_HANDLE   =
*       E_MSG_WAS_LOGGED =
*       E_MSG_WAS_DISPLAYED =
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "2. Meldungszeile füllen
    ADD 1 TO lf_counter.
    CLEAR ls_s_msg.
    ls_s_msg-msgty = 'I'.
    ls_s_msg-msgid = '00'.
    ls_s_msg-msgno = '398'.
    ls_s_msg-msgv1 = 'blubba Meldung 2'.
    ls_s_msg-msg_count = lf_counter.

    IF p_balpar = abap_true.
      "OPTIONAL: Parameter hinzufügen.
      ls_s_msg-params-altext = 'Aufgabe'.
      ls_par-parname  = 'TASK_ICON'.
      ls_par-parvalue = 'ICON_CREATE'.
      APPEND ls_par TO ls_s_msg-params-t_par.
      ls_par-parname  = 'KUNNR'.
      ls_par-parvalue = '4711'.
      APPEND ls_par TO ls_s_msg-params-t_par.
      ls_par-parname  = 'MATNR'.
      ls_par-parvalue = 'Ford KA'.
      APPEND ls_par TO ls_s_msg-params-t_par.
    ENDIF.

    "Meldungszeile hinzufügen
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
*       i_log_handle     = lf_log_handle
        i_s_msg          = ls_s_msg
*        IMPORTING
*       E_S_MSG_HANDLE   =
*       E_MSG_WAS_LOGGED =
*       E_MSG_WAS_DISPLAYED =
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    "3. Meldungszeile füllen: freier Text
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
*       I_LOG_HANDLE     = lf_log_handle
        i_msgty          = 'I'
        i_text           = 'Das ist ein sehr freier Text in Meldung 3.'
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.



    "OPTIONAL: Bisher gesammelte Meldungen ermitteln.
    DATA lt_messages_tmp TYPE tt_bal_s_msg.

    BREAK-POINT.

    me->help_get_temp_messages(
      EXPORTING
        if_loghan   = lf_log_handle
      IMPORTING
        et_messages = lt_messages_tmp[] ).



    "Protokolle sichern
    CLEAR lt_log_handle[].
    APPEND lf_log_handle TO lt_log_handle.

    "WICHTIG: Bei Funktionsbaustein 'BAL_DB_SAVE' muss entweder
    "         Parameter I_SAVE_ALL mit 'X'     oder
    "         Parameter I_T_LOG_HANDLE mit den Loghandles gefüllt werden.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
*       I_IN_UPDATE_TASK = ' '
*       I_SAVE_ALL       = ' '
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "le_save_protocoll
ENDCLASS.                    "lcl_save_protocol IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_show_collect_mess IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_show_collect_mess IMPLEMENTATION.
  METHOD help_store_message_syst.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                   = sy-msgid
        exception_if_not_active = ' '
        msgty                   = sy-msgty
        msgv1                   = sy-msgv1
        msgv2                   = sy-msgv2
        msgv3                   = sy-msgv3
        msgv4                   = sy-msgv4
        txtnr                   = sy-msgno
        zeile                   = if_line
      EXCEPTIONS
        message_type_not_valid  = 1
        not_active              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "help_store_message_syst

  METHOD set_default_line_syst.
    CALL FUNCTION 'MESSAGE_SET_DEFAULTLINE'
      EXPORTING
        arbgb      = sy-msgid
        msgty      = sy-msgty
        msgv1      = sy-msgv1
        msgv2      = sy-msgv2
        msgv3      = sy-msgv3
        msgv4      = sy-msgv4
        txtnr      = sy-msgno
        zeile      = if_line
      EXCEPTIONS
        not_active = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "set_default_line_syst

  METHOD le_show_collect_mess.
    DATA lf_ident TYPE sy-uzeit.
    DATA lf_identification TYPE sy-uzeit.
    DATA lf_line TYPE tf_line.

    "Initialize messages
    GET TIME.
    lf_ident = sy-uzeit.

    BREAK-POINT.

    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXPORTING
        i_identification     = lf_ident
      IMPORTING
        e_identification     = lf_identification
      EXCEPTIONS
        log_not_active       = 1
        wrong_identification = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.



    "Store a message.
    ADD 1 TO lf_line.
    MESSAGE w447(0d) WITH '0053027720' '1001001001' '4479000000' 'KA4' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).

    "Store a message.
    ADD 1 TO lf_line.
    MESSAGE e450(06) WITH '0000020020' 'M4711' 13 'ST' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).

    ADD 1 TO lf_line.
    MESSAGE w450(06) WITH '0000020020' 'M4711' 13 'ST' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).

    "Without changing LF_LINE one message
    "can only be stored one time with the same variables.
    "That's why this message will NOT be added a 3rd time.
*   ADD 1 TO lf_line.
    MESSAGE w450(06) WITH '0000020020' 'M4711' 13 'ST' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).

    "Store a message.
    ADD 1 TO lf_line.
    MESSAGE i208(00) WITH 'Ganz wichtige Info. Heute ist ... .' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).

    "This message will be stored.
*   ADD 1 TO lf_line.
    MESSAGE w450(06) WITH '0000020020' 'M4711' 13 'ST' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).



    "--- Stop and show.

    "Stop collecting messages.
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

    "How many messages are stored?
    DATA lf_count TYPE sy-tabix.

    CLEAR lf_count.
    CALL FUNCTION 'MESSAGES_COUNT'
      IMPORTING
        count                       = lf_count
      EXCEPTIONS
        inconsistent_range          = 1
        inconsistent_range_severity = 2
        OTHERS                      = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "If some messages are stored
    IF lf_count > 0.
      "Show messages
      CALL FUNCTION 'MESSAGES_SHOW'
        EXCEPTIONS
          inconsistent_range = 1
          no_messages        = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "le_show_collect_mess

  METHOD le_show_collect_mess2.
    DATA lf_ident TYPE sy-uzeit.
    DATA lf_identification TYPE sy-uzeit.
    DATA lf_line TYPE tf_line.

    "Initialize messages
    GET TIME.
    lf_ident = sy-uzeit.

    BREAK-POINT.

    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXPORTING
        i_identification     = lf_ident
      IMPORTING
        e_identification     = lf_identification
      EXCEPTIONS
        log_not_active       = 1
        wrong_identification = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    "---Store a headline. Headline without messages (equal LF_LINE) will not be shown.
    ADD 1 TO lf_line.
    MESSAGE i208(00) WITH '--- Überschrift 1, Auftrag 531 ---' INTO sy-lisel.
    me->set_default_line_syst( if_line = lf_line ).



    "---Store a headline. Headline with messages will not be shown.
    ADD 1 TO lf_line.
    MESSAGE i208(00) WITH '--- Überschrift 2, Auftrag 532 ---' INTO sy-lisel.
    me->set_default_line_syst( if_line = lf_line ).

    "Store a message: Do not increment LF_LINE if message belongs to a headline.
    MESSAGE i208(00) WITH 'Ganz wichtige Info. Heute ist ... .' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).

    "Store a message: Do not increment LF_LINE if message belongs to a headline.
    MESSAGE w029(m3) WITH '4711' '1000' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).



    "---Store a headline. Headline with messages will not be shown.
    ADD 1 TO lf_line.
    MESSAGE i208(00) WITH '--- Überschrift 3, Auftrag 533 ---' INTO sy-lisel.
    me->set_default_line_syst( if_line = lf_line ).

    "Store a message: Do not increment LF_LINE if message belongs to a headline.
    MESSAGE e450(06) WITH '0000020020' 'M4711' 13 'ST' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).



    "---Store a headline.
    ADD 1 TO lf_line.
    MESSAGE i208(00) WITH '--- Überschrift 4, Auftrag 534 ---' INTO sy-lisel.
    me->set_default_line_syst( if_line = lf_line ).



    "---Store a headline.
    ADD 1 TO lf_line.
    MESSAGE s208(00) WITH '--- Überschrift 5, Auftrag 535 ---' INTO sy-lisel.
    me->set_default_line_syst( if_line = lf_line ).

    "Store a message: Do not increment LF_LINE if message belongs to a headline.
    MESSAGE s447(0d) WITH '0053027720' '1001001001' '4479000000' 'KA4' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).

    "Store a message: Do not increment LF_LINE if message belongs to a headline.
    MESSAGE w450(06) WITH '0000020020' 'M4711' 13 'ST' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).

    "Store a message: Do not increment LF_LINE if message belongs to a headline.
    MESSAGE i208(00) WITH 'Ganz wichtige Info. Heute ist ... .' INTO sy-lisel.
    me->help_store_message_syst( if_line = lf_line ).



    "--- Stop and show.

    "Stop collecting messages.
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

    "How many messages are stored?
    DATA lf_count TYPE sy-tabix.

    CLEAR lf_count.
    CALL FUNCTION 'MESSAGES_COUNT'
      IMPORTING
        count                       = lf_count
      EXCEPTIONS
        inconsistent_range          = 1
        inconsistent_range_severity = 2
        OTHERS                      = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "If some messages are stored
    IF lf_count > 0.
      "Show messages
      CALL FUNCTION 'MESSAGES_SHOW'
        EXCEPTIONS
          inconsistent_range = 1
          no_messages        = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "le_show_collect_mess2
ENDCLASS.                    "lcl_show_collect_mess IMPLEMENTATION


INITIALIZATION.
  IF go_main IS NOT BOUND.
    CREATE OBJECT go_main.
  ENDIF.

START-OF-SELECTION.
  go_main->ev_start_of_selection( ).

REPORT zca_bd080_try2_00.

CLASS lcl_main DEFINITION DEFERRED.
DATA go_main TYPE REF TO lcl_main.

PARAMETERS p_rolln TYPE dd03l-rollname DEFAULT 'KUNNR'.

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS ev_start_of_selection
      IMPORTING
        if_rollname TYPE dd03l-rollname.

  PRIVATE SECTION.

    METHODS process_rollname
      IMPORTING
        if_rollname TYPE dd03l-rollname.

    METHODS get_print_rollname_properties
      IMPORTING
        if_rollname TYPE dd03l-rollname
      EXPORTING
        ef_domname  TYPE dd03l-domname.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD ev_start_of_selection.
    me->process_rollname(
      if_rollname = if_rollname ).
  ENDMETHOD.

  METHOD process_rollname.
    DATA lf_domname TYPE dd03l-domname.

    WRITE: / 'Datenelement', 20 if_rollname.

    me->get_print_rollname_properties(
      EXPORTING
        if_rollname = if_rollname
      IMPORTING
        ef_domname  = lf_domname ).

    WRITE: / 'Domäne:', 20 lf_domname.
  ENDMETHOD.

  METHOD get_print_rollname_properties.
    DATA: BEGIN OF ls_prop,
            domname  TYPE dd04l-domname,
            memoryid TYPE dd04l-memoryid,
            shlpname TYPE dd04l-shlpname,
          END OF ls_prop.

    "Select rollname's properties.
    SELECT SINGLE domname memoryid shlpname FROM dd04l INTO ls_prop
    WHERE rollname = if_rollname
    AND   as4local = 'A'
    AND   as4vers  = `0000`.

    WRITE: / 'Memory-ID:', 20 ls_prop-memoryid,
           / 'Suchhilfe:', 20 ls_prop-shlpname.

    ef_domname = ls_prop-domname.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  IF go_main IS NOT BOUND.
    go_main = NEW #( ).
  ENDIF.

  go_main->ev_start_of_selection(
    if_rollname = p_rolln ).

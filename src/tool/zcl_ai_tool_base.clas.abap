CLASS zcl_ai_tool_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_ai_tool.

    METHODS constructor
      IMPORTING
        iv_name        TYPE string
        iv_description TYPE string.

    METHODS get_name
      RETURNING VALUE(rv_name) TYPE string.


    METHODS get_description
      RETURNING VALUE(rv_description) TYPE string.


  PROTECTED SECTION.

    METHODS:

      do_execute
        IMPORTING
                  iv_input  TYPE string
        EXPORTING
                  ev_output TYPE string
        RAISING   cx_root,


      log_call_stub
        IMPORTING
          iv_input  TYPE string
          iv_output TYPE string
          iv_status TYPE string
          iv_error  TYPE string OPTIONAL.


    DATA mv_name TYPE string.
    DATA mv_description TYPE string.


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ai_tool_base IMPLEMENTATION.

  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD get_name.
    rv_name = mv_name.
  ENDMETHOD.

  METHOD get_description.
    rv_description = mv_description.
  ENDMETHOD.


  METHOD do_execute.
    " ------------------------------------------------------------
    " Default implementation – concrete tools should redefine this
    " ------------------------------------------------------------
    ev_output = |[Tool { mv_name } has no implementation yet].|.
  ENDMETHOD.

  METHOD zif_ai_tool~execute.
    " ------------------------------------------------------------
    " wraps do_execute with added logging and error handling
    " ------------------------------------------------------------

    DATA lv_output TYPE string.
    DATA lv_status TYPE string VALUE 'SUCCESS'.
    DATA lv_error  TYPE string.

    TRY.
        do_execute(
          EXPORTING
            iv_input  = iv_input
          IMPORTING
            ev_output = lv_output ).
      CATCH cx_root INTO DATA(lx).
        lv_status = 'ERROR'.
        lv_error  = lx->get_text( ).
    ENDTRY.

    log_call_stub(
     iv_input  = iv_input
     iv_output = lv_output
     iv_status = lv_status
     iv_error  = lv_error ).

    ev_output = lv_output.

  ENDMETHOD.


  METHOD log_call_stub.
    " ------------------------------------------------------------
    " Logging stub for PoC:
    " - Called after every tool execution (success or error)
    " - Later:
    "   - use https://help.sap.com/docs/sap-btp-abap-environment/abap-environment/create-new-application-log
    "   - attach RUN_STEP_ID / AGENT_RUN_ID
    " ------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.

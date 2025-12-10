CLASS zcl_ai_tool_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_ai_tool.

    METHODS constructor
      IMPORTING
        name        TYPE string
        description TYPE string.

    METHODS get_name
      RETURNING VALUE(name) TYPE string.

    METHODS get_description
      RETURNING VALUE(description) TYPE string.


  PROTECTED SECTION.
    METHODS:

      do_execute
        IMPORTING
                  input  TYPE string
        EXPORTING
                  output TYPE string
        RAISING   cx_root,

      log_call_stub
        IMPORTING
          input  TYPE string
          output TYPE string
          status TYPE string
          error  TYPE string OPTIONAL.

    DATA name TYPE string.
    DATA description TYPE string.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_ai_tool_base IMPLEMENTATION.

  METHOD constructor.
    me->name = name.
    me->description = description.
  ENDMETHOD.

  METHOD get_name.
    name = me->name.
  ENDMETHOD.

  METHOD get_description.
    description = me->description.
  ENDMETHOD.

  METHOD do_execute.
    " ------------------------------------------------------------
    " Default implementation – concrete tools should redefine this
    " ------------------------------------------------------------
    output = |[Tool { me->name } has no implementation yet].|.
  ENDMETHOD.

  METHOD zif_ai_tool~execute.
    " ------------------------------------------------------------
    " wraps do_execute with added logging and error handling
    " ------------------------------------------------------------
    " No need to declare another local variable to save tool output
    " DATA tool_output TYPE string.
    DATA exit_status TYPE string VALUE 'SUCCESS'.
    DATA error  TYPE string.

    TRY.
        do_execute(
          EXPORTING
            input  = input
          IMPORTING
            output = output ).
      CATCH cx_root INTO DATA(lx).
        exit_status = 'ERROR'.
        error  = lx->get_text( ).
        " return status 1 on error
        status = 1.
    ENDTRY.

    log_call_stub(
     input  = input
     output = output
     status = exit_status
     error  = error ).

    " tool ouput is already assigned when executing the do_execute method
    "output = tool_output.
    " return status 0 on success
    status = 0.

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

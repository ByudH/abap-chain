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

      METHODS define_argument_metadata
        RETURNING VALUE(arguments) TYPE zcl_tool_schema=>tt_tool_arguments.

    DATA name TYPE string.
    DATA description TYPE string.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_ai_tool_base IMPLEMENTATION.

  METHOD constructor.
    me->name = name.
    me->description = description.
  ENDMETHOD.

  METHOD zif_ai_tool~get_name.
    name = me->name.
  ENDMETHOD.

  METHOD zif_ai_tool~get_description.
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


    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    TRY.
        logger->log_tool(
          node_id   = '00000000000000000000000000000000' "unknown here
          node_name = '(tool)'
          tool_name = me->name  " or me->name
          message   = |Tool execute start. input_len={ strlen( input ) }.|
          severity  = if_bali_constants=>c_severity_information ).
      CATCH cx_root.
    ENDTRY.

    TRY.
      do_execute(
        EXPORTING
          input  = input
        IMPORTING
          output = output ).
    CATCH cx_root INTO DATA(ex).
      TRY.
          logger->log_error(
            message = |Tool "{ me->name }" failed: { ex->get_text( ) }| ).
        CATCH cx_root.
      ENDTRY.
      RAISE EXCEPTION ex.
  ENDTRY.

  TRY.
      logger->log_tool(
        node_id   = '00000000000000000000000000000000'
        node_name = '(tool)'
        tool_name = me->name
        message   = |Tool execute end. output_len={ strlen( output ) }.|
        severity  = if_bali_constants=>c_severity_information ).
    CATCH cx_root.
  ENDTRY.
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

  METHOD zif_ai_tool~get_argument_metadata.
    " Delegate to protected method that subclasses can override
    arguments = define_argument_metadata( ).
  ENDMETHOD.

  METHOD define_argument_metadata.
    " Default: no arguments
    " Subclasses override this to define their arguments
    CLEAR arguments.
  ENDMETHOD.

ENDCLASS.

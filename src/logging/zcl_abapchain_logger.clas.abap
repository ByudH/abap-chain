CLASS zcl_abapchain_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    " Singleton access
    CLASS-METHODS get_instance
      RETURNING
        VALUE(logger) TYPE REF TO zcl_abapchain_logger.

    " Enable/disable debug mode and optionally set ADT console
    CLASS-METHODS set_debug_mode
      IMPORTING
        debug TYPE abap_bool
        out   TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

    " Start a new agent run log
    METHODS start_run
      IMPORTING
        agent_name TYPE string
        agent_id   TYPE zif_ai_types=>ty_agent_id OPTIONAL
      RAISING
        cx_bali_runtime
        cx_uuid_error.

    " Log node execution
    METHODS log_node
      IMPORTING
        node_id  TYPE zif_ai_types=>ty_node_id
        " node_name TYPE string
        message  TYPE string
        severity TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_status
      RAISING
        cx_bali_runtime.

    " Log tool call
    METHODS log_tool
      IMPORTING
        node_id   TYPE zif_ai_types=>ty_node_id
        node_name TYPE string
        tool_name TYPE string
        message   TYPE string
        severity  TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_information
      RAISING
        cx_bali_runtime.

    " Log orchestrator step
    METHODS log_orchestrator
      IMPORTING
        step         TYPE i
        current_node TYPE zif_ai_types=>ty_node_id
        next_node    TYPE zif_ai_types=>ty_node_id
        branch_label TYPE string
        message      TYPE string
        severity     TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_information
      RAISING
        cx_bali_runtime.

    " Simple helpers
    METHODS log_debug
      IMPORTING
        message TYPE string
      RAISING
        cx_bali_runtime.

    METHODS log_error
      IMPORTING
        message TYPE string
      RAISING
        cx_bali_runtime.

    " Save log and get handle (for later navigation)
    METHODS save_and_get_handle
      RETURNING
        VALUE(handle) TYPE if_bali_log=>ty_handle
      RAISING
        cx_bali_runtime.

    " Helper function for Text conversion
    TYPES bali_text TYPE c LENGTH 200.
    METHODS to_bali_text
      IMPORTING
        text          TYPE string
      RETURNING
        VALUE(result) TYPE bali_text.


  PRIVATE SECTION.

    CLASS-DATA instance    TYPE REF TO zcl_abapchain_logger.
    CLASS-DATA debug_mode  TYPE abap_bool.
    CLASS-DATA debug_out   TYPE REF TO if_oo_adt_classrun_out.

    DATA log            TYPE REF TO if_bali_log.
    DATA current_run_id TYPE string.
    DATA current_agent  TYPE string.

    METHODS write_console
      IMPORTING
        text TYPE string.

ENDCLASS.


CLASS zcl_abapchain_logger IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS INITIAL.
      CREATE OBJECT instance.
    ENDIF.
    logger = instance.
  ENDMETHOD.


  METHOD set_debug_mode.
    debug_mode = debug.

    IF out IS BOUND.
      debug_out = out.
    ENDIF.

    instance = get_instance( ). "ensure instance exists
    " Use write_console so it also respects debug_mode and handles missing out safely
    IF debug = abap_true.
      instance->write_console( |[ABAPCHAIN-LOGGER] Debug mode ENABLED.| ).
    ELSE.
      instance->write_console( |[ABAPCHAIN-LOGGER] Debug mode DISABLED.| ).
    ENDIF.
  ENDMETHOD.


  METHOD start_run.

    DATA ts TYPE timestampl.
    GET TIME STAMP FIELD ts.

    " Create new log instance
    log = cl_bali_log=>create( ).

    " For grouping in Application Log
    current_run_id = cl_system_uuid=>create_uuid_c32_static( ).
    current_agent  = agent_name.

    DATA external_id TYPE string.
*    external_id = |{ agent_name }| && '|' && current_run_id.
    external_id = |{ agent_name }| && '|' && |{ ts TIMESTAMP = ISO }| && current_run_id.

    TRY.
        " Header: Object/Subobject/External ID
        DATA(header) = cl_bali_header_setter=>create(
                         object      = 'ZABAPCHAIN'
                         subobject   = 'AGENT_RUN'
                         external_id = CONV #( external_id )
                       )->set_expiry(
                         expiry_date       = CONV d( cl_abap_context_info=>get_system_date( ) + 7 )
                         keep_until_expiry = abap_true ).

        log->set_header( header ).

      CATCH cx_bali_runtime INTO DATA(l_runtime_exception).
        write_console( l_runtime_exception->get_text(  ) ).
    ENDTRY.
    DATA text TYPE string.

    IF agent_id IS INITIAL.
      text = |[AGENT RUN START] Agent="{ agent_name }" RunID={ current_run_id }|.
    ELSE.
      text = |[AGENT RUN START] Agent="{ agent_name }" AgentID={ agent_id } RunID={ current_run_id }|.
    ENDIF.

    DATA(item) = cl_bali_free_text_setter=>create(
                    severity = if_bali_constants=>c_severity_status
                    text     = to_bali_text( text ) ).

    log->add_item( item ).

    write_console( text ).
  ENDMETHOD.


  METHOD log_node.
    CHECK log IS BOUND.

    DATA(text) =
      |[NODE] id={ node_id } msg="{ message }"|.
    " |[NODE] id={ node_id } name="{ node_name }" msg="{ message }"|.

    DATA(item) = cl_bali_free_text_setter=>create(
                    severity = severity
                    text     = to_bali_text( text ) ).

    log->add_item( item ).

    write_console( text ).
  ENDMETHOD.


  METHOD log_tool.
    CHECK log IS BOUND.

    DATA(text) =
      |[TOOL] node_id={ node_id } node="{ node_name }" tool="{ tool_name }" msg="{ message }"|.

    DATA(item) = cl_bali_free_text_setter=>create(
                    severity = severity
                    text     = to_bali_text( text ) ).

    log->add_item( item ).

    write_console( text ).
  ENDMETHOD.


  METHOD log_orchestrator.
    CHECK log IS BOUND.

    DATA(text) =
      |[ORCHESTRATOR] step={ step } current={ current_node } next={ next_node } | &&
      |branch="{ branch_label }" msg="{ message }"|.

    DATA(item) = cl_bali_free_text_setter=>create(
                    severity = severity
                    text     = to_bali_text( text ) ).

    log->add_item( item ).

    write_console( text ).
  ENDMETHOD.


  METHOD log_debug.
    DATA(text) = |[DEBUG] { message }|.

    " Always write to console in debug mode
    write_console( text ).

    " Only write to BALI if a log exists
    IF log IS BOUND.
      DATA(item) = cl_bali_free_text_setter=>create(
                      severity = if_bali_constants=>c_severity_information
                      text     = to_bali_text( text ) ).
      log->add_item( item ).
    ENDIF.
  ENDMETHOD.


  METHOD log_error.

    DATA(text) = |[ERROR] { message }|.

    write_console( text ).

    IF log IS BOUND.
      DATA(item) = cl_bali_free_text_setter=>create(
                      severity = if_bali_constants=>c_severity_error
                      text     = to_bali_text( text ) ).

      log->add_item( item ).
    ENDIF.

  ENDMETHOD.


  METHOD save_and_get_handle.
    CHECK log IS BOUND.

    DATA(log_db) = cl_bali_log_db=>get_instance( ).

    log_db->save_log( log = log ).

    handle = log->get_handle( ).

    COMMIT WORK AND WAIT.

    TRY.
        DATA(log2) = cl_bali_log_db=>get_instance( )->load_log( handle = handle ).
        write_console( |Reload OK by handle: { log2->get_handle( ) }| ).
      CATCH cx_bali_runtime INTO DATA(lx).
        write_console( |Reload FAILED by handle: { lx->get_text( ) }| ).
    ENDTRY.

*    DATA(hint) =
*      |[ABAPCHAIN-LOGGER] Log saved with handle { handle }. | &&
*      |Object="ZABAPCHAIN", Subobject="AGENT_RUN", External ID="{ current_agent }\|{ current_run_id }".| &&
*      |\nView via Fiori app "Application Logs" (ID F1487) | &&
*      |or via Analyze Application Log (SLG1) in on-prem systems.|.
*
*    write_console( hint ).
  ENDMETHOD.


  METHOD to_bali_text.
    DATA flat TYPE string.
    flat = text.

    " Optional: flatten newlines
*    flat = replace(
*             val  = flat
*             sub  = cl_abap_char_utilities=>newline
*             with = ' '
*             occ  = 0 ).

    " Safe truncation to 200 chars
    IF strlen( flat ) > 200.
      result = flat+0(200). " 200 characters from index 0
    ELSE.
      " Assigning string -> c(200) is fine; it will pad with blanks
      result = flat.
    ENDIF.
  ENDMETHOD.

  METHOD write_console.
    CHECK debug_mode = abap_true.

    IF debug_out IS BOUND.
      debug_out->write( text ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

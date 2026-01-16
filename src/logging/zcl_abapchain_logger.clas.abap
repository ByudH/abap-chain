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
        agent_id   TYPE zif_ai_types=>ty_agent_id OPTIONAL.

    " Log node execution
    METHODS log_node
      IMPORTING
        node_id   TYPE zif_ai_types=>ty_node_id
        node_name TYPE string
        message   TYPE string
        severity  TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_status.

    METHODS log_node_ref
      IMPORTING
        node     TYPE REF TO zif_ai_node
        message  TYPE string
        severity TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_status.

    " Log tool call
    METHODS log_tool
      IMPORTING
        node_id   TYPE zif_ai_types=>ty_node_id
        node_name TYPE string
        tool_name TYPE string
        message   TYPE string
        severity  TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_information.

    METHODS log_tool_ref
      IMPORTING
        tool     TYPE REF TO zif_ai_tool
        message  TYPE string
        severity TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_information.

    METHODS log_tool_ctx
      IMPORTING
        node     TYPE REF TO zif_ai_node OPTIONAL
        tool     TYPE REF TO zif_ai_tool
        message  TYPE string
        severity TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_information.

    " Log orchestrator step
    METHODS log_orchestrator
      IMPORTING
        step         TYPE i
        current_node TYPE zif_ai_types=>ty_node_id
        next_node    TYPE zif_ai_types=>ty_node_id
        branch_label TYPE string
        message      TYPE string
        severity     TYPE if_bali_constants=>ty_severity
          DEFAULT if_bali_constants=>c_severity_information.

    " Simple helpers
    METHODS log_debug
      IMPORTING
        message TYPE string.

    METHODS log_error
      IMPORTING
        message TYPE string.

    " Save log and get handle (for later navigation)
    METHODS save_and_get_handle
      RETURNING
        VALUE(handle) TYPE if_bali_log=>ty_handle.

    " Helper function for Text conversion
    TYPES bali_text TYPE c LENGTH 200.
    METHODS to_bali_text
      IMPORTING
        text          TYPE string
      RETURNING
        VALUE(result) TYPE bali_text.


  PRIVATE SECTION.

    CONSTANTS:
      gc_object    TYPE if_bali_header_setter=>ty_object    VALUE 'ZABAPCHAIN',
      gc_subobject TYPE if_bali_header_setter=>ty_subobject VALUE 'AGENT_RUN'.

    CLASS-DATA instance    TYPE REF TO zcl_abapchain_logger.
    CLASS-DATA debug_mode  TYPE abap_bool.
    CLASS-DATA debug_out   TYPE REF TO if_oo_adt_classrun_out.

    DATA log            TYPE REF TO if_bali_log.
    DATA current_run_id TYPE string.
    DATA current_agent  TYPE string.


    METHODS add_text_item_safe
      IMPORTING
        severity TYPE if_bali_constants=>ty_severity
        text     TYPE string.

    METHODS safe
      IMPORTING
        action_name TYPE string
        details     TYPE string OPTIONAL.

    METHODS write_console
      IMPORTING
        text TYPE string.

    " Helpers to build consistent message formats
    METHODS fmt_node
      IMPORTING
                node_id     TYPE zif_ai_types=>ty_node_id
                node_name   TYPE string OPTIONAL
                message     TYPE string
      RETURNING VALUE(text) TYPE string.

    METHODS fmt_tool
      IMPORTING
                node_id     TYPE zif_ai_types=>ty_node_id OPTIONAL
                node_name   TYPE string OPTIONAL
                tool_name   TYPE string
                message     TYPE string
      RETURNING VALUE(text) TYPE string.

    METHODS fmt_orchestrator
      IMPORTING
                step         TYPE i
                current_node TYPE zif_ai_types=>ty_node_id
                next_node    TYPE zif_ai_types=>ty_node_id
                branch_label TYPE string
                message      TYPE string
      RETURNING VALUE(text)  TYPE string.

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


  METHOD add_text_item_safe.
    " Always: best-effort console
    write_console( text ).

    " Best-effort BALI
    IF log IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(item) = cl_bali_free_text_setter=>create(
                       severity = severity
                       text     = to_bali_text( text ) ).
        log->add_item( item ).
      CATCH cx_root INTO DATA(ex).
        safe( action_name = 'add_item' details = ex->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD safe.
    CHECK debug_mode = abap_true.
    IF debug_out IS BOUND.
      DATA(msg) = |[ABAPCHAIN-LOGGER_ERROR] { action_name } { details }|.
      debug_out->write( msg ).
    ENDIF.
  ENDMETHOD.

  METHOD write_console.
    CHECK debug_mode = abap_true.
    CHECK debug_out IS BOUND.
    debug_out->write( text ).
  ENDMETHOD.

  METHOD fmt_node.
    text = COND string(
      WHEN node_name IS INITIAL
        THEN |[NODE] id={ node_id } msg="{ message }"|
      ELSE |[NODE] id={ node_id } name="{ node_name }" msg="{ message }"| ).
  ENDMETHOD.

  METHOD fmt_tool.
    text = COND string(
      WHEN node_id IS INITIAL AND node_name IS INITIAL
        THEN |[TOOL] tool="{ tool_name }" msg="{ message }"|
      WHEN node_name IS INITIAL
        THEN |[TOOL] node_id={ node_id } tool="{ tool_name }" msg="{ message }"|
      ELSE |[TOOL] node_id={ node_id } node="{ node_name }" tool="{ tool_name }" msg="{ message }"| ).
  ENDMETHOD.

  METHOD fmt_orchestrator.
    text =
      |[ORCHESTRATOR] step={ step } current={ current_node } next={ next_node } | &&
      |branch="{ branch_label }" msg="{ message }"|.
  ENDMETHOD.


  METHOD start_run.

    TRY.
        DATA ts TYPE timestampl.
        GET TIME STAMP FIELD ts.

        " Create new log instance
        log = cl_bali_log=>create( ).

        " For grouping in Application Log
        current_run_id = cl_system_uuid=>create_uuid_c32_static( ).
        current_agent  = agent_name.

        DATA external_id TYPE string.
        external_id = |{ agent_name }| && '|' && |{ ts TIMESTAMP = ISO }| && current_run_id.


        " Header: Object/Subobject/External ID
        DATA(header) = cl_bali_header_setter=>create(
                         object    = gc_object
                         subobject = gc_subobject
                         external_id = CONV #( external_id )
                       )->set_expiry(
                         expiry_date       = CONV d( cl_abap_context_info=>get_system_date( ) + 7 )
                         keep_until_expiry = abap_true ).

        log->set_header( header ).



        DATA(text) = COND string(
          WHEN agent_id IS INITIAL
            THEN |[AGENT RUN START] Agent="{ agent_name }" RunID={ current_run_id }|
          ELSE |[AGENT RUN START] Agent="{ agent_name }" AgentID={ agent_id } RunID={ current_run_id }| ).

        add_text_item_safe(
          severity = if_bali_constants=>c_severity_status
          text     = text ).


      CATCH cx_root INTO DATA(ex).
        CLEAR log.
        safe( action_name = 'start_run' details = ex->get_text( ) ).
        write_console( |[AGENT RUN START FAILED] Agent="{ agent_name }" { ex->get_text( ) }| ).

    ENDTRY.
  ENDMETHOD.


  METHOD log_node.
    add_text_item_safe(
      severity = severity
      text     = fmt_node(
                  node_id   = node_id
                  node_name = node_name
                  message   = message ) ).
  ENDMETHOD.

  METHOD log_node_ref.
    IF node IS NOT BOUND.
      log_error( |log_node_ref called with node INITIAL. msg="{ message }"| ).
      RETURN.
    ENDIF.

    DATA(node_id)   = node->get_node_id( ).
    DATA(node_name) = node->get_node_name( ).

    add_text_item_safe(
      severity = severity
      text     = fmt_node(
                  node_id   = node_id
                  node_name = node_name
                  message   = message ) ).
  ENDMETHOD.

  METHOD log_tool.
    add_text_item_safe(
      severity = severity
      text     = fmt_tool(
                  node_id   = node_id
                  node_name = node_name
                  tool_name = tool_name
                  message   = message ) ).
  ENDMETHOD.

  METHOD log_tool_ref.
    IF tool IS NOT BOUND.
      log_error( |log_tool_ref called with tool INITIAL. msg="{ message }"| ).
      RETURN.
    ENDIF.

    DATA(tool_name) = tool->get_name( ).

    add_text_item_safe(
      severity = severity
      text     = fmt_tool(
                  tool_name = tool_name
                  message   = message ) ).
  ENDMETHOD.

  METHOD log_tool_ctx.
    IF tool IS NOT BOUND.
      log_error( |log_tool_ctx called with tool INITIAL. msg="{ message }"| ).
      RETURN.
    ENDIF.

    DATA(tool_name) = tool->get_name( ).

    DATA node_id  TYPE zif_ai_types=>ty_node_id.
    DATA node_name TYPE string.

    IF node IS BOUND.
      node_id   = node->get_node_id( ).
      node_name = node->get_node_name( ).
    ENDIF.

    add_text_item_safe(
      severity = severity
      text     = fmt_tool(
                  node_id   = node_id
                  node_name = node_name
                  tool_name = tool_name
                  message   = message ) ).
  ENDMETHOD.

  METHOD log_orchestrator.
    add_text_item_safe(
      severity = severity
      text     = fmt_orchestrator(
                  step         = step
                  current_node = current_node
                  next_node    = next_node
                  branch_label = branch_label
                  message      = message ) ).
  ENDMETHOD.


  METHOD log_debug.
    add_text_item_safe(
      severity = if_bali_constants=>c_severity_information
      text     = |[DEBUG] { message }| ).
  ENDMETHOD.


  METHOD log_error.
    add_text_item_safe(
      severity = if_bali_constants=>c_severity_error
      text     = |[ERROR] { message }| ).
  ENDMETHOD.


  METHOD save_and_get_handle.

    CLEAR handle.
    IF log IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(log_db) = cl_bali_log_db=>get_instance( ).
        log_db->save_log( log = log ).
        handle = log->get_handle( ).


        COMMIT WORK AND WAIT.

        IF debug_mode = abap_true.
          TRY.
              DATA(log2) = cl_bali_log_db=>get_instance( )->load_log( handle = handle ).
              write_console( |Reload OK by handle: { log2->get_handle( ) }| ).
            CATCH cx_root INTO DATA(ex2).
              safe( action_name = 'reload_after_save' details = ex2->get_text( ) ).
          ENDTRY.
        ENDIF.

      CATCH cx_root INTO DATA(ex).
        safe( action_name = 'save_log' details = ex->get_text( ) ).
        CLEAR handle.
    ENDTRY.

  ENDMETHOD.



  METHOD to_bali_text.
    DATA flat TYPE string.
    flat = text.

    " Flatten newlines for compact log entries
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN flat WITH ' '.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf   IN flat WITH ' '.

    " Safe truncation to 200 chars
    IF strlen( flat ) > 200.
      result = flat+0(200). " 200 characters from index 0
    ELSE.
      " Assigning string -> c(200) is fine; it will pad with blanks
      result = flat.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

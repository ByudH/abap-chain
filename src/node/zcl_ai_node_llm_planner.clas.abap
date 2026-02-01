CLASS zcl_ai_node_llm_planner DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ai_tool_catalog_aware.

    METHODS constructor
      IMPORTING
        name TYPE string.

    METHODS zif_ai_node~get_configuration REDEFINITION.
    METHODS zif_ai_node~set_configuration REDEFINITION.

  PROTECTED SECTION.
    DATA llm_client TYPE REF TO zcl_ai_llm_client.

    " Catalog injected by builder (all tools known to agent)
    DATA tool_catalog TYPE zif_ai_types=>th_tool_registry_map.

    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_llm_planner_node_config,
             name TYPE string,
           END OF ts_llm_planner_node_config.

    METHODS catalog_to_text
      RETURNING VALUE(text) TYPE string.

    METHODS pick_tool_heuristic
      IMPORTING
        state            TYPE zif_ai_types=>ts_graph_state
      RETURNING
        VALUE(tool_name) TYPE string.

    METHODS should_request_hitl
      IMPORTING
        state             TYPE zif_ai_types=>ts_graph_state
      RETURNING
        VALUE(needs_hitl) TYPE abap_bool.

    METHODS get_last_tool_message_text
      IMPORTING
        state        TYPE zif_ai_types=>ts_graph_state
      RETURNING
        VALUE(text)  TYPE string.
ENDCLASS.



CLASS zcl_ai_node_llm_planner IMPLEMENTATION.

  METHOD constructor.
    super->constructor( node_name = name ).
    me->llm_client = NEW zcl_ai_llm_client( ).
  ENDMETHOD.

  METHOD zif_ai_tool_catalog_aware~set_tool_catalog.
    me->tool_catalog = tool_catalog.
  ENDMETHOD.

  METHOD catalog_to_text.
    text = ``.
    LOOP AT tool_catalog INTO DATA(entry).
      text = text &&
             | - { entry-tool_name }: { entry-tool_description }| &&
             cl_abap_char_utilities=>newline.
    ENDLOOP.
  ENDMETHOD.

  METHOD pick_tool_heuristic.
    DATA messages_upper TYPE string.
    messages_upper = zcl_ai_utils=>messages_to_string( state-messages ).
    TRANSLATE messages_upper TO UPPER CASE.

    IF messages_upper CS 'VBAK' OR messages_upper CS 'TABLE' OR messages_upper CS 'RISK'.
      IF line_exists( tool_catalog[ tool_name = 'FAKE_TABLE_INFO' ] ).
        tool_name = 'FAKE_TABLE_INFO'.
        RETURN.
      ENDIF.
    ENDIF.

    IF messages_upper CS 'WEATHER'
       OR messages_upper CS 'MUNICH'
       OR messages_upper CS 'FORECAST'
       OR messages_upper CS 'TEMPERATURE'.

      IF line_exists( tool_catalog[ tool_name = 'WEATHER_INFO' ] ).
        tool_name = 'WEATHER_INFO'.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT tool_catalog INTO DATA(entry).
      tool_name = entry-tool_name.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_last_tool_message_text.
    text = ``.

    DATA idx TYPE i.
    idx = lines( state-messages ).

    WHILE idx > 0.
      READ TABLE state-messages INDEX idx INTO DATA(msg).
      IF sy-subrc = 0 AND msg-role = zif_ai_types=>gc_role_tool.
        text = msg-content.
        RETURN.
      ENDIF.
      idx = idx - 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD do_execute.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    TRY.
        logger->log_node(
          node_name = me->node_name
          node_id   = me->node_id
          message   = |Planner step. Catalog tools={ lines( tool_catalog ) } last_tool="{ state-last_tool_name }" branch="{ state-branch_label }".|
          severity  = if_bali_constants=>c_severity_status ).
      CATCH cx_root.
    ENDTRY.

    IF tool_catalog IS INITIAL.
      APPEND VALUE #(
        role    = zif_ai_types=>gc_role_error
        content = |[Planner { node_name }] No tool catalog injected. Ending.| ) TO state-messages.
      state-branch_label = 'END'.
      RETURN.
    ENDIF.

    DATA msg_upper TYPE string.
    msg_upper = zcl_ai_utils=>messages_to_string( state-messages ).
    TRANSLATE msg_upper TO UPPER CASE.

    DATA is_table_demo TYPE abap_bool VALUE abap_false.
    IF msg_upper CS 'VBAK' OR msg_upper CS 'TABLE' OR msg_upper CS 'RISK'.
      is_table_demo = abap_true.
    ENDIF.

    IF is_table_demo = abap_true.

      IF state-branch_label = 'APPROVED'.

        IF state-last_tool_name = 'WEATHER_INFO'.
          state-branch_label = 'FINAL'.
          APPEND VALUE #(
            role    = zif_ai_types=>gc_role_assistant
            content = |[Planner { node_name }] Demo(Table): Weather done -> FINAL.| ) TO state-messages.
          RETURN.
        ENDIF.

        IF line_exists( tool_catalog[ tool_name = 'WEATHER_INFO' ] ).
          state-branch_label   = 'TOOL'.
          state-last_tool_name = 'WEATHER_INFO'.
          state-tool_arguments = '{ "latitude": 48.137154, "longtitude": 11.576124 }'.

          APPEND VALUE #(
            role    = zif_ai_types=>gc_role_assistant
            content = |[Planner { node_name }] Demo(Table): Approved -> call WEATHER_INFO (Munich).| ) TO state-messages.
          RETURN.
        ENDIF.

        state-branch_label = 'FINAL'.
        APPEND VALUE #(
          role    = zif_ai_types=>gc_role_assistant
          content = |[Planner { node_name }] Demo(Table): Approved but WEATHER_INFO missing -> FINAL.| ) TO state-messages.
        RETURN.
      ENDIF.

      IF state-branch_label = 'REJECTED'.
        state-branch_label = 'FINAL'.
        APPEND VALUE #(
          role    = zif_ai_types=>gc_role_assistant
          content = |[Planner { node_name }] Demo(Table): Rejected -> FINAL (no external call).| ) TO state-messages.
        RETURN.
      ENDIF.

      IF state-last_tool_name = 'WEATHER_INFO'.
        state-branch_label = 'FINAL'.
        APPEND VALUE #(
          role    = zif_ai_types=>gc_role_assistant
          content = |[Planner { node_name }] Demo(Table): Weather done -> FINAL.| ) TO state-messages.
        RETURN.
      ENDIF.

      IF state-last_tool_name IS INITIAL.
        IF line_exists( tool_catalog[ tool_name = 'FAKE_TABLE_INFO' ] ).
          state-branch_label   = 'TOOL'.
          state-last_tool_name = 'FAKE_TABLE_INFO'.
          state-tool_arguments = '{ "table_name": "VBAK"}'.

          APPEND VALUE #(
            role    = zif_ai_types=>gc_role_assistant
            content = |[Planner { node_name }] Demo(Table): Request tool "FAKE_TABLE_INFO".| ) TO state-messages.
          RETURN.
        ENDIF.

        DATA(fallback_tool_0) = pick_tool_heuristic( state ).
        IF fallback_tool_0 IS INITIAL.
          APPEND VALUE #(
            role    = zif_ai_types=>gc_role_error
            content = |[Planner { node_name }] No tools available. Ending.| ) TO state-messages.
          state-branch_label = 'END'.
          RETURN.
        ENDIF.

        state-branch_label   = 'TOOL'.
        state-last_tool_name = fallback_tool_0.
        state-tool_arguments = '{}'.

        APPEND VALUE #(
          role    = zif_ai_types=>gc_role_assistant
          content = |[Planner { node_name }] Demo(Table): Fallback tool "{ fallback_tool_0 }".| ) TO state-messages.
        RETURN.
      ENDIF.

      IF state-last_tool_name = 'FAKE_TABLE_INFO'.
        IF NOT line_exists( tool_catalog[ tool_name = 'FAKE_RISK_CHECK' ] ).
          APPEND VALUE #(
            role    = zif_ai_types=>gc_role_error
            content = |[Planner { node_name }] FAKE_RISK_CHECK not in catalog. Ending.| ) TO state-messages.
          state-branch_label = 'END'.
          RETURN.
        ENDIF.

        state-branch_label   = 'TOOL'.
        state-last_tool_name = 'FAKE_RISK_CHECK'.
        state-tool_arguments = '{}'.

        APPEND VALUE #(
          role    = zif_ai_types=>gc_role_assistant
          content = |[Planner { node_name }] Demo(Table): Next tool "FAKE_RISK_CHECK".| ) TO state-messages.
        RETURN.
      ENDIF.

      IF state-last_tool_name = 'FAKE_RISK_CHECK'.
        DATA(needs_hitl) = should_request_hitl( state ).

        IF needs_hitl = abap_true.
          state-branch_label = 'HITL'.

          state-hitl_topic           = 'ABAPCHAIN'.
          state-hitl_reason          = 'High or sensitive risk identified; approval required before external enrichment.'.
          state-hitl_prompt          = 'Approve continuing and calling an external service to enrich the assessment?'.
          state-hitl_primary_field   = 'approved'.
          state-hitl_response_schema =
            '{ "type":"object", "properties": { "approved": { "type":"boolean" }, "comment": { "type":"string" } }, "required": ["approved"] }'.

          APPEND VALUE #(
            role    = zif_ai_types=>gc_role_assistant
            content = |[Planner { node_name }] Demo(Table): Risk HIGH -> route to HITL.| ) TO state-messages.
          RETURN.
        ENDIF.

        IF line_exists( tool_catalog[ tool_name = 'WEATHER_INFO' ] ).
          state-branch_label   = 'TOOL'.
          state-last_tool_name = 'WEATHER_INFO'.
          state-tool_arguments = '{ "latitude": 48.137154, "longtitude": 11.576124 }'.

          APPEND VALUE #(
            role    = zif_ai_types=>gc_role_assistant
            content = |[Planner { node_name }] Demo(Table): Risk not HIGH -> call WEATHER_INFO (Munich).| ) TO state-messages.
          RETURN.
        ENDIF.

        state-branch_label = 'FINAL'.
        APPEND VALUE #(
          role    = zif_ai_types=>gc_role_assistant
          content = |[Planner { node_name }] Demo(Table): Risk not HIGH -> FINAL.| ) TO state-messages.
        RETURN.
      ENDIF.

      state-branch_label = 'FINAL'.
      APPEND VALUE #(
        role    = zif_ai_types=>gc_role_assistant
        content = |[Planner { node_name }] Demo(Table): Default -> FINAL.| ) TO state-messages.
      RETURN.
    ENDIF.

    DATA(next_tool) = pick_tool_heuristic( state ).
    IF next_tool IS INITIAL.
      APPEND VALUE #(
        role    = zif_ai_types=>gc_role_error
        content = |[Planner { node_name }] Could not pick a tool. Ending.| ) TO state-messages.
      state-branch_label = 'END'.
      RETURN.
    ENDIF.

    state-branch_label   = 'TOOL'.
    state-last_tool_name = next_tool.
    state-tool_arguments = '{}'.

    APPEND VALUE #(
      role    = zif_ai_types=>gc_role_assistant
      content = |[Planner { node_name }] Fallback: Request tool "{ next_tool }".| ) TO state-messages.
  ENDMETHOD.


  METHOD should_request_hitl.
    needs_hitl = abap_false.

    DATA last_tool_text TYPE string.
    last_tool_text = get_last_tool_message_text( state ).
    TRANSLATE last_tool_text TO UPPER CASE.

    IF last_tool_text CS 'RISK LEVEL HIGH'
       OR last_tool_text CS 'RISK: HIGH'
       OR last_tool_text CS 'HIGH RISK'
       OR last_tool_text CS 'FORCE_HITL'.
      needs_hitl = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ai_node~get_configuration.
    DATA(config) = VALUE ts_llm_planner_node_config( name = me->node_name ).
    configuration = xco_cp_json=>data->from_abap( config )->to_string( ).
  ENDMETHOD.

  METHOD zif_ai_node~set_configuration.
    DATA(config) = VALUE ts_llm_planner_node_config( ).
    xco_cp_json=>data->from_string( configuration )->write_to( REF #( config ) ).
    me->node_name = config-name.
  ENDMETHOD.

ENDCLASS.

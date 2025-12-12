CLASS zcl_ai_node_llm_planner DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ai_tool_catalog_aware.

    METHODS constructor
      IMPORTING
        node_id     TYPE zif_ai_types=>ty_node_id
        agent_id    TYPE zif_ai_types=>ty_agent_id
        name        TYPE string
        llm_client  TYPE REF TO zcl_ai_llm_client_stub OPTIONAL.

  PROTECTED SECTION.
    DATA name       TYPE string.
    DATA llm_client TYPE REF TO zcl_ai_llm_client_stub.

    " Catalog injected by builder (all tools known to agent)
    DATA tool_catalog TYPE zif_ai_types=>th_tool_registry_map.

    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.
    METHODS catalog_to_text
      RETURNING VALUE(text) TYPE string.

    METHODS pick_tool_heuristic
      IMPORTING
        state TYPE zif_ai_types=>ts_graph_state
      RETURNING
        VALUE(tool_name) TYPE string.
ENDCLASS.



CLASS zcl_ai_node_llm_planner IMPLEMENTATION.

  METHOD constructor.
    super->constructor( node_id = node_id agent_id = agent_id ).
    me->name = name.
    me->llm_client = llm_client.
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
    " Simple deterministic planner:
    " - If user mentions 'risk' -> try risk_check_tool if exists
    " - Else if user mentions 'table' or an SAP table name-like token -> try table_info_tool if exists
    " - Else pick first tool from catalog (if any)

    DATA messages_upper TYPE string.
    messages_upper = to_upper( state-messages ).


    " TODO: TODO: Use LLM to determine tool to use
    " TODO: Use LLM to determine tool parameters to use

    " If user mentions 'risk' -> try risk_check_tool if exists
    IF messages_upper CS 'RISK'.
      IF line_exists( tool_catalog[ tool_name = 'risk_check_tool' ] ).
        tool_name = 'risk_check_tool'.
        RETURN.
      ENDIF.
    ENDIF.

    IF messages_upper CS 'TABLE' OR messages_upper CS 'VBAK'.
      IF line_exists( tool_catalog[ tool_name = 'table_info_tool' ] ).
        tool_name = 'table_info_tool'.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT tool_catalog INTO DATA(entry).
      tool_name = entry-tool_name.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD do_execute.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    TRY.
        logger->log_node(
          node_id   = me->node_id
          message   = |Planner step. Catalog tools={ lines( tool_catalog ) }.|
          severity  = if_bali_constants=>c_severity_status ).
      CATCH cx_root.
    ENDTRY.

    " If no catalog -> cannot plan tool usage
    IF tool_catalog IS INITIAL.
      state-messages = state-messages &&
        cl_abap_char_utilities=>newline &&
        |[Planner { name }] No tool catalog injected. Ending.|.

      state-branch_label = 'END'.
      RETURN.
    ENDIF.

    " If a tool was just executed, decide whether to finish.
    " Minimal policy: after one tool call, end the run (for now).
    IF state-last_tool_name IS NOT INITIAL.
      state-messages = state-messages &&
        cl_abap_char_utilities=>newline &&
        |[Planner { name }] Tool "{ state-last_tool_name }" already used. Ending.|.

      state-branch_label = 'END'.
      RETURN.
    ENDIF.

    " Decide next tool:
    DATA(next_tool) = pick_tool_heuristic( state ).

    IF next_tool IS INITIAL.
      state-messages = state-messages &&
        cl_abap_char_utilities=>newline &&
        |[Planner { name }] Could not pick a tool. Ending.|.

      state-branch_label = 'END'.
      RETURN.
    ENDIF.

    " Route to Tool Executor node
    state-branch_label   = 'TOOL'.
    state-last_tool_name = next_tool.

    state-messages = state-messages &&
      cl_abap_char_utilities=>newline &&
      |[Planner { name }] Request tool "{ next_tool }".|.

    TRY.
        logger->log_node(
          node_id   = me->node_id
          message   = |Route TOOL. requested="{ next_tool }".|
          severity  = if_bali_constants=>c_severity_information ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_ai_node_planner_llm DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ai_tool_catalog_aware.

    METHODS constructor
      IMPORTING
        name             TYPE string DEFAULT 'Planner_LLM'
        prompt      TYPE string DEFAULT ''
        max_context_msgs TYPE i      DEFAULT 12.

    METHODS zif_ai_node~get_configuration REDEFINITION.
    METHODS zif_ai_node~set_configuration REDEFINITION.

    METHODS set_user_prompt
      IMPORTING prompt TYPE string.

  PROTECTED SECTION.
    DATA llm_client        TYPE REF TO zcl_ai_llm_client.
    DATA tool_catalog      TYPE zif_ai_types=>th_tool_registry_map.
    DATA user_prompt       TYPE string.
    DATA max_context_msgs_history  TYPE i.

    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_planner_config,
             name             TYPE string,
             user_prompt       TYPE string,
             max_context_msgs  TYPE i,
           END OF ts_planner_config.

    "--- helpers
    METHODS catalog_to_text
      RETURNING VALUE(text) TYPE string.

    METHODS build_planner_system_prompt
      IMPORTING state TYPE zif_ai_types=>ts_graph_state
      RETURNING VALUE(prompt) TYPE string.

    METHODS get_recent_messages_text
      IMPORTING state TYPE zif_ai_types=>ts_graph_state
      RETURNING VALUE(text) TYPE string.

ENDCLASS.


CLASS zcl_ai_node_planner_llm IMPLEMENTATION.

  METHOD constructor.
    super->constructor( node_name = name ).
    llm_client = NEW zcl_ai_llm_client( ).
    user_prompt = COND string(
      WHEN prompt IS INITIAL
      THEN `You are a routing planner. Decide the next step as JSON only, following the given schema.`
      ELSE prompt ).
    max_context_msgs_history = max_context_msgs.
  ENDMETHOD.

  METHOD zif_ai_tool_catalog_aware~set_tool_catalog.
    me->tool_catalog = tool_catalog.
  ENDMETHOD.

  METHOD catalog_to_text.
    DATA(text_acc) = ``.
    LOOP AT tool_catalog INTO DATA(entry).
      text_acc = text_acc &&
        | - { entry-tool_name }: { entry-tool_description }| &&
        cl_abap_char_utilities=>newline.
    ENDLOOP.
    text = text_acc.
  ENDMETHOD.

  METHOD get_recent_messages_text.
    "Take last max_context_msgs messages to avoid huge prompts
    DATA(total) = lines( state-messages ).
    DATA(start_idx) = total - max_context_msgs_history + 1.
    IF start_idx < 1. start_idx = 1. ENDIF.

    DATA(acc) = ``.
    DO total TIMES.
      DATA(idx) = sy-index.
      IF idx < start_idx.
        CONTINUE.
      ENDIF.
      READ TABLE state-messages INDEX idx INTO DATA(msg).
      IF sy-subrc = 0.
        acc = acc && |[{ msg-role }] { msg-content }| && cl_abap_char_utilities=>newline.
      ENDIF.
    ENDDO.

    text = acc.
  ENDMETHOD.

  METHOD build_planner_system_prompt.
    DATA(catalog_txt) = catalog_to_text( ).
    DATA(msgs_txt)    = get_recent_messages_text( state ).

    prompt =
      |You are the planner of an agentic workflow. You MUST respond with JSON only.| &&
      cl_abap_char_utilities=>newline &&
      |Decision schema (JSON Schema):| && cl_abap_char_utilities=>newline &&
      zcl_abap_types_lh=>get_planner_schema( ) && cl_abap_char_utilities=>newline &&
      |Available tools:| && cl_abap_char_utilities=>newline &&
      catalog_txt && cl_abap_char_utilities=>newline &&
      |Current state: last_tool_name="{ state-last_tool_name }" branch_label="{ state-branch_label }" status="{ state-status }"| &&
      cl_abap_char_utilities=>newline &&
      |Conversation context (recent messages):| && cl_abap_char_utilities=>newline &&
      msgs_txt && cl_abap_char_utilities=>newline &&
      |Rules:| && cl_abap_char_utilities=>newline &&
      |- If you want to call a tool, set decision="TOOL" and provide tool_name and tool_arguments.| && cl_abap_char_utilities=>newline &&
      |- If you need approval or approval was requested, set decision="HITL" and fill hitl.* fields.| && cl_abap_char_utilities=>newline &&
      |- If done, set decision="FINAL". If unrecoverable, set decision="END".| && cl_abap_char_utilities=>newline &&
      |- Do NOT invent tools that are not in the catalog.| && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD do_execute.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    IF tool_catalog IS INITIAL.
      APPEND VALUE #( role = zif_ai_types=>gc_role_error
                      content = |[Planner { node_name }] No tool catalog injected. Ending.| ) TO state-messages.
      state-branch_label = 'END'.
      RETURN.
    ENDIF.

    DATA(system_prompt) = build_planner_system_prompt( state ).
    DATA(local_user_prompt) = user_prompt.

    TRY.
        DATA(llm_output) = llm_client->generate_completion(
          iv_system_prompt = system_prompt
          iv_user_prompt   = local_user_prompt ).

        "Parse+validate planner JSON
        DATA(clean_json) = zcl_ai_planner_resp_parser=>clean_json_string( llm_output ).
        DATA(decision)   = zcl_ai_planner_resp_val=>parse_and_validate( clean_json ).

        "Apply decision to state
        zcl_ai_planner_apply=>apply(
          EXPORTING is_decision = decision
          CHANGING  cs_state    = state
                    ct_messages = state-messages ).

        TRY.
            logger->log_node(
              node_name = me->node_name
              node_id   = me->node_id
              message   = |Planner LLM decision="{ state-branch_label }" tool="{ state-last_tool_name }".|
              severity  = if_bali_constants=>c_severity_status ).
          CATCH cx_root.
        ENDTRY.

      CATCH zcx_ai_agent_error INTO DATA(agent_error).
        state-status = zif_ai_types=>gc_workflow_status_error.
        APPEND VALUE #( role = zif_ai_types=>gc_role_error
                        content = |[Planner { node_name }] LLM error: { agent_error->message }| ) TO state-messages.
        state-branch_label = 'END'.

      CATCH cx_root INTO DATA(root_error).
        state-status = zif_ai_types=>gc_workflow_status_error.
        APPEND VALUE #( role = zif_ai_types=>gc_role_error
                        content = |[Planner { node_name }] Unhandled error: { root_error->get_text( ) }| ) TO state-messages.
        state-branch_label = 'END'.
    ENDTRY.
  ENDMETHOD.

  METHOD set_user_prompt.
    user_prompt = prompt.
  ENDMETHOD.

  METHOD zif_ai_node~get_configuration.
    DATA(config) = VALUE ts_planner_config(
      name             = me->node_name
      user_prompt      = user_prompt
      max_context_msgs = max_context_msgs_history ).
    configuration = xco_cp_json=>data->from_abap( config )->to_string( ).
  ENDMETHOD.

  METHOD zif_ai_node~set_configuration.
    DATA(config) = VALUE ts_planner_config( ).
    xco_cp_json=>data->from_string( configuration )->write_to( REF #( config ) ).
    me->node_name = config-name.
    user_prompt = config-user_prompt.
    max_context_msgs_history = config-max_context_msgs.
  ENDMETHOD.

ENDCLASS.

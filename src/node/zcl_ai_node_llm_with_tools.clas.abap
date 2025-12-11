CLASS zcl_ai_node_llm_with_tools DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base_lh
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " this is derived through base class INTERFACES zif_ai_node.
    INTERFACES zif_ai_node_tool_aware.

    METHODS constructor
      IMPORTING
        iv_node_id    TYPE zif_ai_types=>ty_node_id
        iv_agent_id   TYPE zif_ai_types=>ty_agent_id
        iv_name       TYPE string
        io_llm_client TYPE REF TO zcl_ai_llm_client_stub.

  PROTECTED SECTION.

    DATA mv_name       TYPE string.
    DATA mo_llm_client TYPE REF TO zcl_ai_llm_client_stub.

    " Local tool registry for this node (what the LLM can choose from)
    DATA mt_tools TYPE zif_ai_types=>th_tool_registry_map.

    METHODS do_execute REDEFINITION.

ENDCLASS.


CLASS zcl_ai_node_llm_with_tools IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      node_id  = iv_node_id
      agent_id = iv_agent_id ).

    mv_name       = iv_name.
    mo_llm_client = io_llm_client.
  ENDMETHOD.


  METHOD zif_ai_node_tool_aware~add_tool.
    DATA ls_tool TYPE zif_ai_types=>ts_tool_registry.

    ls_tool-tool_name        = iv_tool_name.
    ls_tool-tool_description = iv_description.
    ls_tool-tool_endpoint    = io_tool.

    INSERT ls_tool INTO TABLE mt_tools.
  ENDMETHOD.

  METHOD zif_ai_node_tool_aware~get_tools.
    rt_tools = mt_tools.
  ENDMETHOD.

  METHOD zif_ai_node_tool_aware~get_tool.
    READ TABLE mt_tools INTO DATA(ls_tool)
         WITH KEY tool_name = iv_tool_name.
    IF sy-subrc = 0.
      ro_tool = ls_tool-tool_endpoint.
    ENDIF.
  ENDMETHOD.


  METHOD do_execute.

    DATA ls_state TYPE zif_ai_types=>ts_graph_state.
    ls_state = state_input.


    " No tools
    IF mt_tools IS INITIAL.
      ls_state-messages =
          ls_state-messages &&
          |[LLM-With-Tools Node { mv_name }] No tools available.| &&
          |\n|.

      state_output = ls_state.
      RETURN.
    ENDIF.


    " Pick an arbitrary tool (first in hashed table) for PoC
    DATA ls_tool TYPE zif_ai_types=>ts_tool_registry.

    LOOP AT mt_tools INTO ls_tool.
      EXIT.
    ENDLOOP.

    IF ls_tool-tool_name IS INITIAL.
      ls_state-messages =
          ls_state-messages &&
          |[LLM-With-Tools Node { mv_name }] No tools found in registry.| &&
          |\n|.
      state_output = ls_state.
      RETURN.
    ENDIF.

    DATA(lo_tool) = ls_tool-tool_endpoint.


    " Execute tool
    DATA lv_tool_output TYPE string.

    lo_tool->execute(
      EXPORTING
        iv_input  = ls_state-messages
      IMPORTING
        ev_output = lv_tool_output ).

    ls_state-last_tool_name = ls_tool-tool_name.


    DATA lv_prompt TYPE string.

    lv_prompt =
        |[Node { mv_name }] executed tool { ls_tool-tool_name }| && |\n\n| &&
        |User context:| && |\n| &&
        |{ ls_state-messages }| && |\n\n| &&
        |Tool output:| && |\n| &&
        |{ lv_tool_output }| && |\n\n| &&
        |Task:| && |\n| &&
        |Summarize the result in 1–3 sentences.|.

    " Invoke the LLM client
    DATA lv_llm_result TYPE string.

    IF mo_llm_client IS BOUND.
      lv_llm_result = mo_llm_client->generate_completion( prompt = lv_prompt ).
    ELSE.
      lv_llm_result = |[LLM Stub] Would summarize: { lv_tool_output }|.
    ENDIF.

    " Update Graph State
    ls_state-messages =
        ls_state-messages &&
        |\n[Node { mv_name } used tool { ls_tool-tool_name }]\n| &&
        lv_llm_result.

    ls_state-result_json  = lv_llm_result.
    ls_state-branch_label = ls_tool-tool_name. " last tool used?

    state_output = ls_state.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_ai_node_llm_with_tools DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ai_node_tool_aware.

    METHODS constructor
      IMPORTING
        node_id    TYPE zif_ai_types=>ty_node_id
        agent_id   TYPE zif_ai_types=>ty_agent_id
        name       TYPE string
        llm_client TYPE REF TO zcl_ai_llm_client_stub.

  PROTECTED SECTION.
    DATA name       TYPE string.
    DATA llm_client TYPE REF TO zcl_ai_llm_client_stub.

    " Local tool registry for this node (what the LLM can choose from)
    DATA tools TYPE zif_ai_types=>th_tool_registry_map.

    METHODS do_execute REDEFINITION.

ENDCLASS.



CLASS zcl_ai_node_llm_with_tools IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      node_id  = node_id
      agent_id = agent_id ).

    me->name       = name.
    me->llm_client = llm_client.
  ENDMETHOD.


  METHOD zif_ai_node_tool_aware~add_tool.
    DATA tool_entry TYPE zif_ai_types=>ts_tool_registry.

    tool_entry-tool_name        = tool_name.
    tool_entry-tool_description = description.
    tool_entry-tool_endpoint    = tool.

    INSERT tool_entry INTO TABLE me->tools.
  ENDMETHOD.


  METHOD zif_ai_node_tool_aware~get_tools.
    tools = tools.
  ENDMETHOD.


  METHOD zif_ai_node_tool_aware~get_tool.
    READ TABLE tools INTO DATA(tool_entry)
         WITH KEY tool_name = tool_name.
    IF sy-subrc = 0.
      tool = tool_entry-tool_endpoint.
    ENDIF.
  ENDMETHOD.


  METHOD do_execute.
    " Work directly on CHANGING parameter `state` from base class

    " No tools registered
    IF tools IS INITIAL.
      state-messages =
          state-messages &&
          |[LLM-With-Tools Node { name }] No tools available.| &&
          |\n|.
      RETURN.
    ENDIF.

    " Pick an arbitrary tool (first in hashed table) for PoC
    DATA tool_entry TYPE zif_ai_types=>ts_tool_registry.

    LOOP AT tools INTO tool_entry.
      EXIT.
    ENDLOOP.

    IF tool_entry-tool_name IS INITIAL.
      state-messages =
          state-messages &&
          |[LLM-With-Tools Node { name }] No tools found in registry.| &&
          |\n|.
      RETURN.
    ENDIF.

    DATA(tool_instance) = tool_entry-tool_endpoint.

    " Execute tool
    DATA tool_output TYPE string.

    tool_instance->execute(
      EXPORTING
        input  = state-messages
      IMPORTING
        output = tool_output ).

    state-last_tool_name = tool_entry-tool_name.

    " Build LLM prompt
    DATA prompt TYPE string.

    prompt =
        |[Node { name }] executed tool { tool_entry-tool_name }| && |\n\n| &&
        |User context:| && |\n| &&
        |{ state-messages }| && |\n\n| &&
        |Tool output:| && |\n| &&
        |{ tool_output }| && |\n\n| &&
        |Task:| && |\n| &&
        |Summarize the result in 1–3 sentences.|.

    " Invoke the LLM client
    DATA llm_result TYPE string.

    IF llm_client IS BOUND.
      llm_result = llm_client->generate_completion( prompt = prompt ).
    ELSE.
      llm_result = |[LLM Stub] Would summarize: { tool_output }|.
    ENDIF.

    " Update Graph State
    state-messages =
        state-messages &&
        |\n[Node { name } used tool { tool_entry-tool_name }]\n| &&
        llm_result.

    state-result_json  = llm_result.
    state-branch_label = tool_entry-tool_name. " e.g. route on last tool name

  ENDMETHOD.

ENDCLASS.

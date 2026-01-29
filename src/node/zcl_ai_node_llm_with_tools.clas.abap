CLASS zcl_ai_node_llm_with_tools DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ai_node_tool_aware.

    METHODS constructor
      IMPORTING
       " node_id    TYPE zif_ai_types=>ty_node_id
        "agent_id   TYPE zif_ai_types=>ty_agent_id
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
       node_name = name
      "node_id  = node_id
      "agent_id = agent_id
      ).

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
    APPEND VALUE #(
      role = zif_ai_types=>gc_role_error
      content = |[LLM-With-Tools Node { name }] No tools available.|
    ) TO state-messages.
    RETURN.
  ENDIF.

    " Pick an arbitrary tool (first in hashed table) for PoC
    DATA tool_entry TYPE zif_ai_types=>ts_tool_registry.

    LOOP AT tools INTO tool_entry.
      EXIT.
    ENDLOOP.

 IF tool_entry-tool_name IS INITIAL.
    APPEND VALUE #(
      role = zif_ai_types=>gc_role_error
      content = |[LLM-With-Tools Node { name }] No tools found in registry.|
    ) TO state-messages.
    RETURN.
  ENDIF.

    DATA(tool_instance) = tool_entry-tool_endpoint.

    " Execute tool
    DATA tool_output TYPE string.

    DATA(messages_string) = zcl_ai_utils=>messages_to_string( state-messages ).

    tool_instance->execute(
      EXPORTING
        input  = messages_string
      IMPORTING
        output = tool_output ).

    state-last_tool_name = tool_entry-tool_name.

    " Build LLM prompt
    DATA prompt TYPE string.

    prompt =
        |[Node { name }] executed tool { tool_entry-tool_name }| && |\n\n| &&
        |User context:| && |\n| &&
        |{ messages_string }| && |\n\n| &&
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

  " VALIDATE THE LLM RESPONSE
  TRY.
      DATA(validated_response) = zcl_llm_response_validator=>parse_and_validate(
        json_response = llm_result ).

      " Store reasoning as message
      APPEND VALUE #(
        role = zif_ai_types=>gc_role_assistant
        content = |[Reasoning] { validated_response-reasoning }|
      ) TO state-messages.

      " Check what LLM decided
      IF validated_response-tool IS NOT INITIAL.
        " LLM wants to call another tool
        state-last_tool_name = validated_response-tool.

        " Serialize arguments
        IF validated_response-arguments IS BOUND.
          /ui2/cl_json=>serialize(
            EXPORTING data = validated_response-arguments
                      pretty_name = /ui2/cl_json=>pretty_mode-none
            RECEIVING r_json = state-tool_arguments ).
        ELSE.
          state-tool_arguments = '{}'.
        ENDIF.

        " Add tool call message
        APPEND VALUE #(
          role = zif_ai_types=>gc_role_assistant
          content = |[Tool Call] Requesting tool: { validated_response-tool }|
        ) TO state-messages.

        state-branch_label = 'EXECUTE_TOOL'.

      ELSEIF validated_response-final_answer IS NOT INITIAL.
        " LLM provided final answer

        " Add tool execution summary
        APPEND VALUE #(
          role = zif_ai_types=>gc_role_tool
          content = |[Node { name } used tool { tool_entry-tool_name }]|
        ) TO state-messages.

        " Add final answer
        APPEND VALUE #(
          role = zif_ai_types=>gc_role_assistant
          content = validated_response-final_answer
        ) TO state-messages.

        "  Check if state has final_response field
        " If your state structure doesn't have this field, comment out this line
        " state-final_response = validated_response-final_answer.

        state-branch_label = 'COMPLETE'.
      ENDIF.

    CATCH zcx_schema_validation INTO DATA(validation_ex).
      " Validation failed - append error
      APPEND VALUE #(
        role = zif_ai_types=>gc_role_error
        content = |[Validation Error] { validation_ex->error_message }|
      ) TO state-messages.
      state-branch_label = 'END'.
  ENDTRY.

  state-result_json = llm_result.

  ENDMETHOD.

ENDCLASS.

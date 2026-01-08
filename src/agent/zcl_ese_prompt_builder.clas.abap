CLASS zcl_ese_prompt_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS build_system_prompt
      IMPORTING available_tools TYPE  zif_ai_types=>th_tool_registry_map
      RETURNING VALUE(prompt) TYPE string.

  PRIVATE SECTION.
    METHODS format_tool_list
      IMPORTING tools TYPE zif_ai_types=>th_tool_registry_map
      RETURNING VALUE(formatted) TYPE string.

    METHODS format_single_tool
      IMPORTING
        tool_entry TYPE zif_ai_types=>ts_tool_registry
        index      TYPE i
      RETURNING
        VALUE(formatted) TYPE string.

ENDCLASS.

CLASS zcl_ese_prompt_builder IMPLEMENTATION.
  METHOD build_system_prompt.
    " 1. Get the response JSON schema
    DATA(response_schema) = zcl_ese_llm_response_schema=>get_schema_definition( ).

    " 2. Format available tools from registry
    DATA(tool_catalog) = format_tool_list( available_tools ).

    " 3. Build complete system prompt (Tool Catalog FIRST, then Schema, then Rules)
    prompt =
      |Available Tools:\n\n| &&
      |{ tool_catalog }\n| &&
      |---\n\n| &&
      |Response Format:\n| &&
      |You must respond with valid JSON matching this schema:\n| &&
      |{ response_schema }\n\n| &&
      |Execution Rules:\n| &&
      |1. Tool call: Set "tool" to the exact tool name from the catalog above, | &&
      |"arguments" to a JSON object with required parameters, and "final_answer" to null\n| &&
      |2. Final answer: Set "tool" to null, "arguments" to \{\}, and "final_answer" to your response\n| &&
      |3. Always include detailed "reasoning" explaining your thought process\n| &&
      |4. Only use tools listed in the Available Tools section above\n| &&
      |5. Provide all required arguments when calling a tool\n| &&
      |6. Do NOT use markdown code blocks or ``` json|.

ENDMETHOD.

  METHOD format_tool_list.
    " Handle case when no tools are available
    IF tools IS INITIAL.
      formatted = '(No tools currently available - provide direct answers based on your knowledge)'.
      RETURN.
    ENDIF.

    " Build formatted catalog
    DATA lv_catalog TYPE string.
    DATA lv_index TYPE i VALUE 1.

    LOOP AT tools INTO DATA(tool_entry).
      lv_catalog = lv_catalog && format_single_tool(
        tool_entry = tool_entry
        index      = lv_index ).
      lv_index = lv_index + 1.
    ENDLOOP.

    formatted = lv_catalog.
  ENDMETHOD.

  METHOD format_single_tool.
    " Get tool metadata from the tool object
    DATA(tool_name) = tool_entry-tool_endpoint->get_name( ).
    DATA(tool_desc) = tool_entry-tool_endpoint->get_description( ).
    DATA(tool_args) = tool_entry-tool_endpoint->get_argument_metadata( ).

    " Build complete tool metadata structure
    DATA(tool_metadata) = VALUE zcl_tool_schema=>ty_tool_metadata(
      name        = tool_name
      description = tool_desc
      arguments   = tool_args
    ).

    " Use the schema class to format (consistent with response schema pattern)
    formatted =
      |{ index }. | &&
      zcl_tool_schema=>get_tool_description_text( tool_metadata ) &&
      |\n|.
  ENDMETHOD.
ENDCLASS.



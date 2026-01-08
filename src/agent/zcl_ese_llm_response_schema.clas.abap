CLASS zcl_ese_llm_response_schema DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_llm_response,
             reasoning     TYPE string,
             tool          TYPE string,
             arguments     TYPE REF TO data,
             final_answer  TYPE string,
           END OF ty_llm_response.

    CLASS-METHODS get_schema_definition
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS get_example_tool_call
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS get_example_final_answer
      RETURNING VALUE(result) TYPE string.

ENDCLASS.

CLASS zcl_ese_llm_response_schema IMPLEMENTATION.
  METHOD get_schema_definition.
    result = '{ "type": "object", ' &&
             '"required": ["reasoning", "tool", "arguments", "final_answer"], ' &&
             '"properties": { ' &&
             '"reasoning": { ' &&
             '"type": "string", ' &&
             '"description": "Step-by-step thought process" ' &&
             '}, ' &&
             '"tool": { ' &&
             '"type": ["string", "null"], ' &&
             '"description": "Tool name or null" ' &&
             '}, ' &&
             '"arguments": { ' &&
             '"type": "object", ' &&
             '"description": "Tool parameters or empty object" ' &&
             '}, ' &&
             '"final_answer": { ' &&
             '"type": ["string", "null"], ' &&
             '"description": "User response or null" ' &&
             '} ' &&
             '} }'.
  ENDMETHOD.

  METHOD get_example_tool_call.
    result = '{ "reasoning": "Need to call tool", ' &&
             '"tool": "get_weather", ' &&
             '"arguments": { "location": "Berlin" }, ' &&
             '"final_answer": null }'.
  ENDMETHOD.

  METHOD get_example_final_answer.
    result = '{ "reasoning": "Direct answer available", ' &&
             '"tool": null, ' &&
             '"arguments": {}, ' &&
             '"final_answer": "SAP was founded in 1972" }'.
  ENDMETHOD.
ENDCLASS.

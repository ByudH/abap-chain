CLASS zcl_llm_response_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Response structure (moved from zcl_ese_llm_response_schema)
    TYPES: BEGIN OF ty_llm_response,
             reasoning     TYPE string,
             tool          TYPE string,
             arguments     TYPE REF TO data,
             final_answer  TYPE string,
           END OF ty_llm_response.

    " Main validation method
    CLASS-METHODS parse_and_validate
      IMPORTING
        json_response       TYPE string
      RETURNING
        VALUE(response)     TYPE ty_llm_response
      RAISING
        zcx_schema_validation.

    " Schema methods (moved from zcl_ese_llm_response_schema)
    CLASS-METHODS get_schema_definition
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS get_example_tool_call
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS get_example_final_answer
      RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    " Business rule validation (ReAct pattern)
    CLASS-METHODS validate_react_pattern
      IMPORTING
        response TYPE ty_llm_response
        has_arguments_field TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_schema_validation.

ENDCLASS.

CLASS zcl_llm_response_validator IMPLEMENTATION.

  METHOD parse_and_validate.
    "Clean JSON
    DATA(lv_clean_json) = zcl_schema_validator=>clean_json_string( json_response ).

    "Check if arguments field exists before deserialization

    DATA(lv_has_arguments_field) = abap_false.

    "Look for argument field in JSON, checking with quotes to avoid false positives in string values

    IF lv_clean_json CS '"arguments"'.
        lv_has_arguments_field = abap_true.
    ENDIF.

    "Deserialize
    TRY.
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = lv_clean_json
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data = response ).

      CATCH cx_sy_move_cast_error
            cx_sy_conversion_no_number
            INTO DATA(lx_parse_error).
        RAISE EXCEPTION TYPE zcx_schema_validation
          EXPORTING
            error_message = |JSON parsing failed: { lx_parse_error->get_text( ) }|
            previous = lx_parse_error.
    ENDTRY.

    "Validate ReAct pattern business rules
    validate_react_pattern(
    response = response
    has_arguments_field = lv_has_arguments_field ).

  ENDMETHOD.

  METHOD validate_react_pattern.
    " Rule 1: Reasoning must always be present
    IF response-reasoning IS INITIAL.
      RAISE EXCEPTION TYPE zcx_schema_validation
        EXPORTING
            error_message = 'Missing required field: reasoning'
            field_name = 'reasoning'
            validation_type = 'REQUIRED_FIELD'.

    ENDIF.

    " Rule 2: Tool and final_answer are mutually exclusive
    IF response-tool IS NOT INITIAL AND
       response-final_answer IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_schema_validation
        EXPORTING
            error_message = 'Invalid response: Both tool and final_answer are set. Only one should have a value.'
            validation_type = 'REACT_PATTERN'.

    ENDIF.

    " Rule 3: At least one must be set
    IF response-tool IS INITIAL AND
       response-final_answer IS INITIAL.
      RAISE EXCEPTION TYPE zcx_schema_validation
        EXPORTING
            error_message = 'Invalid response: Either tool or final_answer must be set.'
            validation_type = 'REACT_PATTERN'.
    ENDIF.

    " Rule 4: If tool is set, arguments should be bound
    IF response-tool IS NOT INITIAL AND
       has_arguments_field = abap_false.
      RAISE EXCEPTION TYPE zcx_schema_validation
        EXPORTING
            error_message = 'Invalid response: Tool is specified but arguments are missing.'
            field_name = 'arguments'
            validation_type = 'REQUIRED_FIELD'.
    ENDIF.

    " Rule 5: If arguments are set, tool should be bound/ its completely unconditional so needs to be deleted
*    IF response-arguments IS NOT BOUND.
*      RAISE EXCEPTION TYPE zcx_schema_validation
*        EXPORTING
*            error_message = 'Invalid response: Tool is specified but arguments are missing.'
*            field_name = 'arguments'
*            validation_type = 'REQUIRED_FIELD'.
*    ENDIF.
  ENDMETHOD.

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



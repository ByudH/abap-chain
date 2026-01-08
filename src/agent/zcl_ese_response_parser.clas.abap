CLASS zcl_ese_response_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS parse_llm_response
      IMPORTING iv_json TYPE string
      RETURNING VALUE(rs_response) TYPE zcl_ese_llm_response_schema=>ty_llm_response
      RAISING zcx_parse_error.

  PRIVATE SECTION.
    METHODS validate_response
      IMPORTING is_response TYPE zcl_ese_llm_response_schema=>ty_llm_response
      RAISING zcx_parse_error.

    METHODS clean_json_string
      IMPORTING raw TYPE string
      RETURNING VALUE(clean) TYPE string.
ENDCLASS.

CLASS zcl_ese_response_parser IMPLEMENTATION.
  METHOD clean_json_string.
  clean = raw.
  " Remove markdown code blocks if LLM ignores instructions
  clean = replace( val = clean regex = '```' with = '').
  " Trim whitespace
  clean = condense( clean ).
  ENDMETHOD.

  METHOD parse_llm_response.
   " Clean the JSON first
    DATA(lv_clean_json) = clean_json_string( iv_json ).

    " Parse using standard ABAP JSON deserializer
    TRY.
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = lv_clean_json
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data = rs_response ).

        " Validate the parsed response
        validate_response( rs_response ).

      CATCH cx_sy_move_cast_error
            cx_sy_conversion_no_number
            INTO DATA(lx_parse_error).
        RAISE EXCEPTION TYPE zcx_parse_error
          EXPORTING
            error_text = |JSON parsing failed: { lx_parse_error->get_text( ) }|
            previous = lx_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_response.
      " Rule 1: Reasoning must always be present
    IF is_response-reasoning IS INITIAL.
      RAISE EXCEPTION TYPE zcx_parse_error
        EXPORTING error_text = 'Missing required field: reasoning'.
    ENDIF.

    " Rule 2: Tool and final_answer are mutually exclusive
    IF is_response-tool IS NOT INITIAL AND
       is_response-final_answer IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_parse_error
        EXPORTING error_text =
          'Invalid response: Both tool and final_answer are set. Only one should have a value.'.
    ENDIF.

    " Rule 3: At least one must be set
    IF is_response-tool IS INITIAL AND
       is_response-final_answer IS INITIAL.
      RAISE EXCEPTION TYPE zcx_parse_error
        EXPORTING error_text =
          'Invalid response: Either tool or final_answer must be set.'.
    ENDIF.

    " Rule 4: If tool is set, arguments should be bound
    IF is_response-tool IS NOT INITIAL AND
       is_response-arguments IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_parse_error
        EXPORTING error_text =
          'Invalid response: Tool is specified but arguments are missing.'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

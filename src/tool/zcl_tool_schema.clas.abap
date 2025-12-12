CLASS zcl_tool_schema DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Structure for a single tool argument
    TYPES: BEGIN OF ty_tool_argument,
             name         TYPE string,
             type         TYPE string,
             description  TYPE string,
             required     TYPE abap_bool,
             default      TYPE string,
           END OF ty_tool_argument,
           tt_tool_arguments TYPE STANDARD TABLE OF ty_tool_argument WITH KEY name.

    " Structure for complete tool metadata
    TYPES: BEGIN OF ty_tool_metadata,
             name        TYPE string,
             description TYPE string,
             arguments   TYPE tt_tool_arguments,
           END OF ty_tool_metadata.

    " Get JSON schema for a tool (matching response schema)
    CLASS-METHODS get_tool_schema_json
      IMPORTING
        tool_metadata     TYPE ty_tool_metadata
      RETURNING
        VALUE(json_schema) TYPE string.

    " Get human-readable format for LLM prompt
    CLASS-METHODS get_tool_description_text
      IMPORTING
        tool_metadata TYPE ty_tool_metadata
      RETURNING
        VALUE(text)   TYPE string.

    " Validate a tool input structure against this schema
    CLASS-METHODS validate_input
      IMPORTING
        tool_metadata TYPE ty_tool_metadata
        is_input      TYPE tt_tool_arguments
      RAISING
        zcx_ai_tool_error.

ENDCLASS.

CLASS zcl_tool_schema IMPLEMENTATION.

  METHOD get_tool_schema_json.
    " Build JSON schema for tool arguments (similar to your response schema)
    DATA lv_json TYPE string.

    IF tool_metadata-arguments IS INITIAL.
      json_schema = '{ "type": "object", "properties": {}, "required": [] }'.
      RETURN.
    ENDIF.

    lv_json = '{ "type": "object", "properties": {'.

    LOOP AT tool_metadata-arguments INTO DATA(arg).
      IF sy-tabix > 1.
        lv_json = lv_json && ', '.
      ENDIF.

      lv_json = lv_json &&
                |"{ arg-name }": \{ | &&
                |"type": "{ arg-type }", | &&
                |"description": "{ arg-description }"| &&
                COND #( WHEN arg-default IS NOT INITIAL
                        THEN |, "default": "{ arg-default }"| ) &&
                | \}|.
    ENDLOOP.

    lv_json = lv_json && ' }, "required": ['.

    DATA lv_first TYPE abap_bool VALUE abap_true.
    LOOP AT tool_metadata-arguments INTO arg WHERE required = abap_true.
      IF lv_first = abap_false.
        lv_json = lv_json && ', '.
      ENDIF.
      lv_json = lv_json && |"{ arg-name }"|.
      lv_first = abap_false.
    ENDLOOP.

    lv_json = lv_json && '] }'.
    json_schema = lv_json.
  ENDMETHOD.

  METHOD get_tool_description_text.
    " Human-readable format for LLM system prompt
    text = |Tool: { tool_metadata-name }\n| &&
           |Description: { tool_metadata-description }\n|.

    IF tool_metadata-arguments IS NOT INITIAL.
      text = text && |Arguments:\n|.

      LOOP AT tool_metadata-arguments INTO DATA(arg).
        DATA(req_text) = COND string( WHEN arg-required = abap_true
                                      THEN 'required'
                                      ELSE 'optional' ).

        text = text &&
               |  - { arg-name } ({ arg-type }, { req_text }): { arg-description }|.

        IF arg-default IS NOT INITIAL.
          text = text && | [default: { arg-default }]|.
        ENDIF.

        text = text && |\n|.
      ENDLOOP.
    ELSE.
      text = text && |Arguments: None\n|.
    ENDIF.
  ENDMETHOD.

  METHOD validate_input.
    DATA: ls_expected TYPE ty_tool_argument,
          ls_provided TYPE ty_tool_argument,
          lv_found    TYPE abap_bool.

    " 1. Check REQUIRED parameters are provided or not
    LOOP AT tool_metadata-arguments INTO ls_expected WHERE required = abap_true.
      READ TABLE is_input WITH KEY name = ls_expected-name
        INTO ls_provided.
      IF sy-subrc <> 0 AND ls_expected-default IS INITIAL.
        RAISE EXCEPTION NEW zcx_ai_tool_error(
          error_message = |Missing required argument: { ls_expected-name }|
        ).
      ENDIF.
    ENDLOOP.

    " 2. Check if there is parameter that doesn't exist in schema
    LOOP AT is_input INTO ls_provided.
      READ TABLE tool_metadata-arguments
        WITH KEY name = ls_provided-name
        INTO ls_expected.
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_ai_tool_error(
          error_message = |Unknown argument provided: { ls_provided-name }|
        ).
      ENDIF.

    " 3. Check types
    CASE ls_expected-type.

      WHEN 'string'.
        " string doesn't need check

      WHEN 'number'.
        " convert string to number
        DATA lv_num TYPE decfloat34.
        TRY.
            lv_num = ls_provided-description.
          CATCH cx_sy_conversion_no_number.
            RAISE EXCEPTION NEW zcx_ai_tool_error(
              error_message = |Argument { ls_provided-name } must be a number|
            ).
        ENDTRY.

      WHEN 'boolean'.
        IF ls_provided-description <> 'true'
        AND ls_provided-description <> 'false'.
          RAISE EXCEPTION NEW zcx_ai_tool_error(
            error_message = |Argument { ls_provided-name } must be boolean (true/false)|
          ).
        ENDIF.

      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_ai_tool_error(
          error_message = |Unsupported argument type: { ls_expected-type }|
        ).
    ENDCASE.

  ENDLOOP.

ENDMETHOD.

ENDCLASS.



CLASS zcl_tool_argument_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Compatibility with existing tool interface
    TYPES: ty_tool_argument     TYPE zcl_schema_validator=>ty_field_definition,
           tt_tool_arguments    TYPE zcl_schema_validator=>tt_field_definitions,
           ty_tool_metadata     TYPE zcl_schema_validator=>ty_schema_metadata.

    " Main validation method
    CLASS-METHODS validate_tool_input
      IMPORTING
        tool_metadata TYPE ty_tool_metadata
        tool_arguments TYPE string  " JSON string from LLM
      RETURNING
        VALUE(parsed_args) TYPE tt_tool_arguments
      RAISING
        zcx_schema_validation.

    " Schema generation (delegates to base class)
    CLASS-METHODS get_tool_schema_json
      IMPORTING
        tool_metadata     TYPE ty_tool_metadata
      RETURNING
        VALUE(json_schema) TYPE string.

    CLASS-METHODS get_tool_description_text
      IMPORTING
        tool_metadata TYPE ty_tool_metadata
      RETURNING
        VALUE(text)   TYPE string.

ENDCLASS.

CLASS zcl_tool_argument_validator IMPLEMENTATION.

  METHOD validate_tool_input.
    " 1. Parse JSON arguments from LLM
    DATA(lv_clean_json) = zcl_schema_validator=>clean_json_string( tool_arguments ).

    TRY.
        " Deserialize JSON to field table
        DATA(provided_args) = zcl_schema_validator=>parse_json_to_fields( lv_clean_json ).

      CATCH zcx_schema_validation INTO DATA(parse_ex).
        RAISE EXCEPTION parse_ex.
    ENDTRY.

    " 2. Validate required fields
    zcl_schema_validator=>validate_required_fields(
      schema_fields = tool_metadata-fields
      provided_data = provided_args ).

    " 3. Validate no unknown fields
    zcl_schema_validator=>validate_no_unknown_fields(
      schema_fields = tool_metadata-fields
      provided_data = provided_args ).

    " 4. Validate field types
    LOOP AT provided_args INTO DATA(provided_field).
      " Find schema definition for this field
      READ TABLE tool_metadata-fields INTO DATA(schema_field)
        WITH KEY name = provided_field-name.

      IF sy-subrc = 0.
        zcl_schema_validator=>validate_field_type(
          field_name  = provided_field-name
          field_type  = schema_field-type
          field_value = provided_field-description ).  " Value stored in description field
      ENDIF.
    ENDLOOP.

    parsed_args = provided_args.
  ENDMETHOD.

  METHOD get_tool_schema_json.
    " Delegate to base class
    json_schema = zcl_schema_validator=>get_json_schema( tool_metadata ).
  ENDMETHOD.

  METHOD get_tool_description_text.
    " Delegate to base class with tool-specific formatting
    DATA(base_text) = zcl_schema_validator=>get_text_description( tool_metadata ).

    " Replace "Schema:" with "Tool:" for consistency
    text = replace( val = base_text sub = 'Schema:' with = 'Tool:' occ = 1 ).
    text = replace( val = text sub = 'Fields:' with = 'Arguments:' occ = 1 ).
  ENDMETHOD.

ENDCLASS.



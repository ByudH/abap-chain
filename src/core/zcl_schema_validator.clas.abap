CLASS zcl_schema_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Shared type definitions
    TYPES: BEGIN OF ty_field_definition,
             name        TYPE string,
             type        TYPE string,      " 'string', 'number', 'boolean', 'object'
             description TYPE string,
             required    TYPE abap_bool,
             default     TYPE string,
           END OF ty_field_definition,
           tt_field_definitions TYPE STANDARD TABLE OF ty_field_definition WITH KEY name.

    TYPES: BEGIN OF ty_schema_metadata,
             name        TYPE string,
             description TYPE string,
             fields      TYPE tt_field_definitions,
           END OF ty_schema_metadata.

    " Core validation methods (shared by both tool and LLM validators)
    CLASS-METHODS validate_required_fields
      IMPORTING
        schema_fields TYPE tt_field_definitions
        provided_data TYPE tt_field_definitions
      RAISING
        zcx_schema_validation.

    CLASS-METHODS validate_no_unknown_fields
      IMPORTING
        schema_fields TYPE tt_field_definitions
        provided_data TYPE tt_field_definitions
      RAISING
        zcx_schema_validation.

    CLASS-METHODS validate_field_type
      IMPORTING
        field_name  TYPE string
        field_type  TYPE string
        field_value TYPE string
      RAISING
        zcx_schema_validation.

    " JSON utilities (shared)
    CLASS-METHODS clean_json_string
      IMPORTING
        raw          TYPE string
      RETURNING
        VALUE(clean) TYPE string.

    CLASS-METHODS parse_json_to_fields
      IMPORTING
        json_string   TYPE string
      RETURNING
        VALUE(fields) TYPE tt_field_definitions
      RAISING
        zcx_schema_validation.

    " Schema generation (shared)
    CLASS-METHODS get_json_schema
      IMPORTING
        schema_metadata    TYPE ty_schema_metadata
      RETURNING
        VALUE(json_schema) TYPE string.

    CLASS-METHODS get_text_description
      IMPORTING
        schema_metadata TYPE ty_schema_metadata
      RETURNING
        VALUE(text)     TYPE string.

ENDCLASS.

CLASS zcl_schema_validator IMPLEMENTATION.

  METHOD clean_json_string.
    clean = raw.
    " Remove markdown code blocks (```json ... ```)
    clean = replace( val = clean regex = '```json' with = '' ).
    clean = replace( val = clean regex = '```' with = '' ).
    " Trim whitespace
    clean = condense( clean ).
  ENDMETHOD.

  METHOD validate_required_fields.
    DATA: ls_schema_field TYPE ty_field_definition,
          ls_provided     TYPE ty_field_definition.

    LOOP AT schema_fields INTO ls_schema_field WHERE required = abap_true.
      READ TABLE provided_data WITH KEY name = ls_schema_field-name
        INTO ls_provided.

      " Check if field exists or has default value
      IF sy-subrc <> 0 AND ls_schema_field-default IS INITIAL.
        RAISE EXCEPTION TYPE zcx_schema_validation
          EXPORTING
            error_message = |Missing required field: { ls_schema_field-name }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_no_unknown_fields.
    DATA: ls_provided     TYPE ty_field_definition,
          ls_schema_field TYPE ty_field_definition.

    LOOP AT provided_data INTO ls_provided.
      READ TABLE schema_fields WITH KEY name = ls_provided-name
        INTO ls_schema_field.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_schema_validation
          EXPORTING
            error_message = |Unknown field provided: { ls_provided-name }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_field_type.
    DATA: lv_num TYPE decfloat34.

    CASE field_type.
      WHEN 'string'.
        " Always valid - no type check needed

      WHEN 'number'.
        TRY.
            lv_num = CONV decfloat34( field_value ).
          CATCH cx_sy_conversion_no_number.
            RAISE EXCEPTION TYPE zcx_schema_validation
              EXPORTING
                error_message = |Field { field_name } must be a number, got: { field_value }|.
        ENDTRY.

      WHEN 'boolean'.
        IF field_value <> 'true' AND field_value <> 'false'.
          RAISE EXCEPTION TYPE zcx_schema_validation
            EXPORTING
              error_message = |Field { field_name } must be boolean (true/false), got: { field_value }|.
        ENDIF.

      WHEN 'object'.
        " Object type - check if valid JSON
        IF field_value IS INITIAL OR
           ( field_value(1) <> '{' AND field_value(1) <> '[' ).
          RAISE EXCEPTION TYPE zcx_schema_validation
            EXPORTING
              error_message = |Field { field_name } must be a valid JSON object|.
        ENDIF.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_schema_validation
          EXPORTING
            error_message = |Unsupported field type: { field_type } for field { field_name }|.
    ENDCASE.
  ENDMETHOD.

  METHOD parse_json_to_fields.
    " Parse JSON string into field definitions table
    " This is a helper for converting {"key": "value"} to tt_field_definitions
    DATA: lv_clean_json TYPE string.

    lv_clean_json = clean_json_string( json_string ).

    TRY.
        " Use ABAP JSON parser to extract key-value pairs
        DATA(json_obj) = /ui2/cl_json=>generate( json = lv_clean_json ).

        " Convert to field definitions
        FIELD-SYMBOLS: <ls_root>  TYPE any,
                       <lv_value> TYPE any.

        " 1. Dereference the generic data object to access the structure
        ASSIGN json_obj->* TO <ls_root>.

        IF <ls_root> IS ASSIGNED.
          " 2. Use RTTI to describe the dynamic structure
          DATA(lo_struct_descr) = CAST cl_abap_structdescr(
            cl_abap_typedescr=>describe_by_data( <ls_root> )
          ).

          " 3. Loop through the components (JSON Keys)
          LOOP AT lo_struct_descr->components ASSIGNING FIELD-SYMBOL(<ls_component>).

            " Create a new entry in the output table
            APPEND INITIAL LINE TO fields ASSIGNING FIELD-SYMBOL(<ls_field_def>).

            " Map JSON Key -> Name
            <ls_field_def>-name = to_lower( <ls_component>-name ).

            " Map JSON Value -> Description
            " We assign the specific component of the structure to a field symbol
            ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_root> TO <lv_value>.

            IF <lv_value> IS ASSIGNED.
              FIELD-SYMBOLS: <lv_actual_content> TYPE any.

              " 2. Check if it is a reference and dereference it
              IF <lv_value> IS BOUND AND
                 cl_abap_typedescr=>describe_by_data( <lv_value> )->type_kind = cl_abap_typedescr=>typekind_dref.

                ASSIGN <lv_value>->* TO <lv_actual_content>.
                IF <lv_actual_content> IS ASSIGNED.
                  DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <lv_actual_content> ).

                  CASE lo_type->type_kind.
                      " Case 1: It is a Structure (Nested JSON Object) or Table (Array)
                    WHEN cl_abap_typedescr=>typekind_struct1
                      OR cl_abap_typedescr=>typekind_struct2
                      OR cl_abap_typedescr=>typekind_table.

                      " Serialize the ABAP structure back to a JSON string
                      <ls_field_def>-description = /ui2/cl_json=>serialize(
                        data     = <lv_actual_content>
                        compress = abap_true
                      ).

                      " Case 2: It is a Simple Type (String, Integer, Boolean, etc.)
                    WHEN OTHERS.
                      " Safe to convert directly to string
                      <ls_field_def>-description = |{ <lv_actual_content> }|.
                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDIF.


          ENDLOOP.
        ENDIF.

      CATCH cx_root INTO DATA(lx_json_error).
        RAISE EXCEPTION TYPE zcx_schema_validation
          EXPORTING
            error_message = |JSON parsing failed: { lx_json_error->get_text( ) }|
            previous      = lx_json_error.
    ENDTRY.
  ENDMETHOD.

  METHOD get_json_schema.
    DATA: lv_json  TYPE string,
          ls_field TYPE ty_field_definition,  " ← Explicit declaration
          lv_first TYPE abap_bool.

    IF schema_metadata-fields IS INITIAL.
      json_schema = '{ "type": "object", "properties": {}, "required": [] }'.
      RETURN.
    ENDIF.

    " Build JSON schema structure
    lv_json = '{ "type": "object", "properties": { '.

    LOOP AT schema_metadata-fields INTO ls_field.
      IF sy-tabix > 1.
        lv_json = lv_json && ', '.
      ENDIF.

      lv_json = lv_json &&
                |"{ ls_field-name }": \{ | &&
                |"type": "{ ls_field-type }", | &&
                |"description": "{ ls_field-description }"| &&
                COND #( WHEN ls_field-default IS NOT INITIAL
                        THEN |, "default": "{ ls_field-default }"| ) &&
                | \}|.
    ENDLOOP.

    " Add required fields array
    lv_json = lv_json && ' }, "required": ['.

    lv_first = abap_true.
    LOOP AT schema_metadata-fields INTO ls_field WHERE required = abap_true.
      IF lv_first = abap_false.
        lv_json = lv_json && ', '.
      ENDIF.
      lv_json = lv_json && |"{ ls_field-name }"|.
      lv_first = abap_false.
    ENDLOOP.

    lv_json = lv_json && '] }'.
    json_schema = lv_json.
  ENDMETHOD.


  METHOD get_text_description.
    " Human-readable schema description
    text = |Schema: { schema_metadata-name }\n| &&
           |Description: { schema_metadata-description }\n|.

    IF schema_metadata-fields IS NOT INITIAL.
      text = text && |Fields:\n|.

      LOOP AT schema_metadata-fields INTO DATA(field).
        DATA(req_text) = COND string( WHEN field-required = abap_true
                                      THEN 'required'
                                      ELSE 'optional' ).

        text = text &&
               |  - { field-name } ({ field-type }, { req_text }): { field-description }|.

        IF field-default IS NOT INITIAL.
          text = text && | [default: { field-default }]|.
        ENDIF.

        text = text && |\n|.
      ENDLOOP.
    ELSE.
      text = text && |Fields: None\n|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.



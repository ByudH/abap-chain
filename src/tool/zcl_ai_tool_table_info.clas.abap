CLASS zcl_ai_tool_table_info DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM zcl_ai_tool_base.

  PUBLIC SECTION.
    METHODS constructior
      IMPORTING
        name        TYPE string
        description TYPE string.
  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.
    METHODS define_argument_metadata REDEFINITION.
  PRIVATE SECTION..

ENDCLASS.



CLASS zcl_ai_tool_table_info IMPLEMENTATION.
  METHOD constructior.
    me->name        = name.
    IF description IS NOT INITIAL.
      me->description = description.
    ELSE.
      me->description = |Retrieves the schema, column names, and field descriptions for a specified SAP database table or CDS view. Use this tool to understand the table structure before generating SQL queries.|.
    ENDIF.
  ENDMETHOD.

  METHOD define_argument_metadata.
    arguments = VALUE #(
      ( name        = 'table_name'
        type        = 'string'
        description = 'Name of the SAP table or CDS view (max 16 characters, uppercase)'
        required    = abap_true
        default     = '' )
    ).
  ENDMETHOD.

  METHOD do_execute.
    " 1. Sanitize Input
    DATA(object_name) = to_upper( input ).
    " check if the name is longer then 16 characters (max for table names)
    IF strlen( object_name ) > 16.
      output = |Error: Object name '{ object_name }' is too long.|.
      RAISE EXCEPTION NEW zcx_ai_tool_error( output ).
      RETURN.
    ENDIF.

    " 2. Initialize Variables
    DATA(schema_text) = |Schema Definition for { object_name }:\n|.
    DATA(object_found) = abap_false.

    " Variables to hold the final readable schema info
    DATA type_in_string TYPE string.
    DATA type_length  TYPE i.
    DATA built_in_type TYPE REF TO cl_xco_ad_built_in_type.
    DATA data_element_name TYPE sxco_ad_object_name.
    DATA data_element_content TYPE REF TO if_xco_dtel_content.
    DATA short_description TYPE string.
    DATA(no_built_in_type) = 'Data Element without underlying built-in type'.
    " -----------------------------------------------------------------
    " Strategy A: Check if it is a Transparent Database Table
    " -----------------------------------------------------------------
    DATA(database_table) = xco_cp_abap_dictionary=>database_table( iv_name = CONV #( object_name ) ).

    IF database_table->exists( ).
      object_found = abap_true.

      " Retrieve all fields
      DATA(table_fields) = database_table->fields->all->get( ).

      LOOP AT table_fields INTO DATA(table_field).
        DATA(table_field_name) = table_field->name.
        DATA(table_field_content) = table_field->content( ).

        " 1. Get the Type Object (This is an OBJECT, not a string)
        DATA(table_field_type) = table_field_content->get_type( ).

        " 2. Determine if it is a Data Element or Built-in Type
        IF table_field_type->is_data_element( ).
          " --- SCENARIO: Data Element (e.g., MATNR) ---
          data_element_name = table_field_type->get_data_element( )->name.

          " We must go deeper: DE -> Domain -> Format to get the technical type (CHAR, DEC, etc.)
          data_element_content = xco_cp_abap_dictionary=>data_element( data_element_name )->content( ).

          IF data_element_content->has_underlying_built_in_type( ).
            built_in_type = data_element_content->get_underlying_built_in_type( ).
            type_in_string = built_in_type->type.
            type_length  = built_in_type->length.
          ELSE.
            " Fallback if no domain (rare in standard tables, possible in custom types)
            type_in_string = no_built_in_type.
            type_length  = 0.
          ENDIF.

        ELSEIF table_field_type->is_built_in_type( ).
          " --- SCENARIO: Built-in (e.g., CHAR 10 hardcoded) ---
          built_in_type = table_field_type->get_built_in_type( ).
          type_in_string = built_in_type->type.
          type_length  = built_in_type->length.
        ENDIF.

        " 3. Get Description safely
        " get_short_description returns a Translation Object. We need to ask for a specific language.
        " If no description exists on the field, this might return empty.
        short_description = table_field_content->get_short_description( ).
        IF short_description IS INITIAL.
          short_description = table_field_name.
        ENDIF.
          " 4. Append to Schema String
          schema_text = schema_text && | - Field Name: { table_field_name } ({ type_in_string } { type_length }), Field Description: { short_description }\n|.
        ENDLOOP.

      ELSE.

        " !!!!! Functionality of getting types not work for cds views. !!!!
        " -----------------------------------------------------------------
        " Strategy B: Check if it is a CDS View Entity (I_View)
        " -----------------------------------------------------------------

        DATA(cds_view) = xco_cp_cds=>view_entity( CONV #( object_name ) ).
        " --- B1. Get Metadata (Type & Description) via Data Element ---

        IF cds_view->exists( ).
          object_found = abap_true.
          DATA(view_descr) = cds_view->content( ).


          " Retrieve all fields
          DATA(cds_view_fields) = cds_view->fields->all->get( ).

          DATA struct_descr TYPE REF TO cl_abap_structdescr.
          TRY.
              struct_descr ?= cl_abap_typedescr=>describe_by_name( object_name ).
            CATCH cx_sy_move_cast_error.
              RETURN. " Handle error
          ENDTRY.

          DATA(components) = struct_descr->get_components( ).

          LOOP AT cds_view_fields INTO DATA(cds_view_field).
            DATA(cds_view_field_name) = cds_view_field->name.

            " --- B1. Get Metadata (Type & Description) via Data Element ---
            " We assume the field uses a Data Element (Common for standard views)
            DATA(cds_view_field_content) = cds_view_field->content( ).
            DATA(header) = cds_view_field_content->get( ).
            DATA(cds_view_field_type)    = cds_view_field_content->get_type( ).

            " For cds view field which is directly selected from a raw data table, the type is initial
            IF cds_view_field_type IS INITIAL.
              " if type is initial, we cannot determine anything, just print the field name
              " schema_text = schema_text && | - { cds_view_field_name } \n|.
              READ TABLE components INTO DATA(component) WITH KEY name = cds_view_field_name.
              type_in_string = component-type->type_kind.
              type_length = component-type->length.
              short_description = cds_view_field_name.
            ELSE.
              IF cds_view_field_type->is_data_element( ).
                " 1. Get Data Element Name
                data_element_name = cds_view_field_type->get_data_element( )->name.

                " 2. Get Data Element Content
                data_element_content = xco_cp_abap_dictionary=>data_element( data_element_name )->content( ).

                " 3. Get Description (Logon Language)
                DATA(data_element_description) = data_element_content->get_short_description( ).
                IF data_element_description IS NOT INITIAL.
                  short_description = data_element_description.
                ELSE.
                  short_description = cds_view_field_name.
                ENDIF.

                " 4. Get Technical Type (Length/Type)
                IF data_element_content->has_underlying_built_in_type( ).
                  built_in_type = data_element_content->get_underlying_built_in_type( ).
                  type_in_string = built_in_type->type.
                  type_length      = built_in_type->length.
                ELSE.
                  short_description = no_built_in_type.
                ENDIF.

              ELSEIF cds_view_field_type->is_built_in_type( ).
                " Field is defined directly (e.g. cast( ... as abap.char(10) ))
                built_in_type = cds_view_field_type->get_built_in_type( ).
                type_in_string = built_in_type->type.
                type_length      = built_in_type->length.
                short_description = cds_view_field_name. " No DE description available for built-ins
              ELSE.
                type_in_string = 'OTHER'.
                short_description     = cds_view_field_name.
              ENDIF.
            ENDIF.
            " Append to Schema
            schema_text = schema_text && | - Field Name: { cds_view_field_name } ({ type_in_string } { type_length }), Field Description: { short_description } \n|.
          ENDLOOP.
        ENDIF.
      ENDIF.

      " -----------------------------------------------------------------
      " Final Output
      " -----------------------------------------------------------------
      IF object_found = abap_true.
        output = schema_text.

      ELSE.
        output = |Error: Object '{ object_name }' not found. It is neither a released Table nor a CDS View.|.
        RAISE EXCEPTION NEW zcx_ai_tool_error( output ).
        RETURN.
      ENDIF.

    ENDMETHOD.

ENDCLASS.

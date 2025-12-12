CLASS ltc_tool_schema_test DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_empty_arguments FOR TESTING,
      test_single_required_arg FOR TESTING,
      test_multiple_args_mixed FOR TESTING,
      test_json_schema_format FOR TESTING.

ENDCLASS.

CLASS ltc_tool_schema_test IMPLEMENTATION.

  METHOD test_empty_arguments.
    " Test tool with no arguments
    DATA(tool_metadata) = VALUE zcl_tool_schema=>ty_tool_metadata(
      name        = 'NO_ARGS_TOOL'
      description = 'A tool with no arguments'
      arguments   = VALUE #( ) ).

    DATA(json_schema) = zcl_tool_schema=>get_tool_schema_json( tool_metadata ).

    cl_abap_unit_assert=>assert_equals(
      act = json_schema
      exp = '{ "type": "object", "properties": {}, "required": [] }'
      msg = 'Empty arguments should produce empty schema' ).
  ENDMETHOD.

  METHOD test_single_required_arg.
    " Test tool with one required argument
    DATA(tool_metadata) = VALUE zcl_tool_schema=>ty_tool_metadata(
      name        = 'SINGLE_ARG_TOOL'
      description = 'Tool with one argument'
      arguments   = VALUE #(
        ( name = 'param1' type = 'string' description = 'Test param' required = abap_true )
      ) ).

    DATA(json) = zcl_tool_schema=>get_tool_schema_json( tool_metadata ).

    " Verify required field is in JSON
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( find( val = json sub = '"required": ["param1"]' ) >= 0 )
      msg = 'Required parameter should be in required array' ).
  ENDMETHOD.

  METHOD test_multiple_args_mixed.
    " Test tool with required and optional arguments
    DATA(tool_metadata) = VALUE zcl_tool_schema=>ty_tool_metadata(
      name        = 'MIXED_ARGS_TOOL'
      description = 'Tool with mixed arguments'
      arguments   = VALUE #(
        ( name = 'required_param' type = 'string' description = 'Required' required = abap_true )
        ( name = 'optional_param' type = 'integer' description = 'Optional' required = abap_false default = '10' )
      ) ).

    DATA(text) = zcl_tool_schema=>get_tool_description_text( tool_metadata ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( find( val = text sub = 'required_param (string, required)' ) >= 0 )
      msg = 'Should show required parameter correctly' ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( find( val = text sub = 'optional_param (integer, optional)' ) >= 0 )
      msg = 'Should show optional parameter correctly' ).
  ENDMETHOD.

  METHOD test_json_schema_format.
    " Verify JSON schema is valid JSON structure
    DATA(tool_metadata) = VALUE zcl_tool_schema=>ty_tool_metadata(
      name = 'TEST_TOOL'
      arguments = VALUE #(
        ( name = 'test' type = 'string' description = 'Test' required = abap_true )
      ) ).

    DATA(json) = zcl_tool_schema=>get_tool_schema_json( tool_metadata ).

    " Verify it contains expected JSON structure elements
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( find( val = json sub = '"type": "object"' ) >= 0 )
      msg = 'Should be an object type' ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( find( val = json sub = '"properties":' ) >= 0 )
      msg = 'Should have properties field' ).
  ENDMETHOD.

ENDCLASS.

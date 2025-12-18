CLASS ltc_table_info_tool_test DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO zcl_ai_tool_table_info.

    METHODS:
      setup,
      test_tool_name          FOR TESTING,
      test_tool_description   FOR TESTING,
      test_argument_metadata  FOR TESTING,
      test_argument_structure FOR TESTING.

ENDCLASS.

CLASS ltc_table_info_tool_test IMPLEMENTATION.

  METHOD setup.
    cut = NEW zcl_ai_tool_table_info( name = 'TABLE_INFO' description = 'Table info test.' ).
  ENDMETHOD.

  METHOD test_tool_name.
    DATA(name) = cut->zif_ai_tool~get_name( ).

    cl_abap_unit_assert=>assert_equals(
      act = name
      exp = 'TABLE_INFO'
      msg = 'Tool name should be TABLE_INFO' ).
  ENDMETHOD.

  METHOD test_tool_description.
    DATA(desc) = cut->zif_ai_tool~get_description( ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( desc IS NOT INITIAL )
      msg = 'Description should not be empty' ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( find( val = to_lower( desc ) sub = 'table' ) >= 0 )
      msg = 'Description should mention table' ).
  ENDMETHOD.

  METHOD test_argument_metadata.
    DATA(args) = cut->zif_ai_tool~get_argument_metadata( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( args )
      exp = 1
      msg = 'TABLE_INFO should have exactly 1 argument' ).
  ENDMETHOD.

  METHOD test_argument_structure.
    DATA(args) = cut->zif_ai_tool~get_argument_metadata( ).

    READ TABLE args INDEX 1 INTO DATA(arg).

    cl_abap_unit_assert=>assert_equals(
      act = arg-name
      exp = 'table_name'
      msg = 'Argument name should be table_name' ).

    cl_abap_unit_assert=>assert_equals(
      act = arg-type
      exp = 'string'
      msg = 'Argument type should be string' ).

    cl_abap_unit_assert=>assert_equals(
      act = arg-required
      exp = abap_true
      msg = 'table_name should be required' ).
  ENDMETHOD.

ENDCLASS.

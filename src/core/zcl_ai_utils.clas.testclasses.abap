*"* use this source file for your ABAP unit test classes

CLASS ltcl_serialization_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    " Define the structure types locally or reference the global ZIF types
    " For this example, I assume your types are in zif_ai_types

    METHODS:
      setup,    " Runs before EACH test method (optional)
      teardown. " Runs after EACH test method (optional)

    METHODS:
      test_round_trip     FOR TESTING,
      test_empty_state    FOR TESTING,
      test_special_chars  FOR TESTING,
      test_partial_json   FOR TESTING,
      test_malformed_json FOR TESTING.

ENDCLASS.


CLASS ltcl_serialization_test IMPLEMENTATION.

  METHOD setup.
    " Usually used to reset static data or create dummy objects.
    " Not strictly needed for pure static method testing, but good practice.
  ENDMETHOD.

  METHOD teardown.
    " Cleanup code if needed.
  ENDMETHOD.

  METHOD test_round_trip.
    " 1. ARRANGE (Prepare data)
    DATA(input_state) = VALUE zif_ai_types=>ts_graph_state(
      messages       = value #( ( role = 'system' content = 'test message' ) )
      last_tool_name = `tool_A`
      branch_label   = `success`
      result_json    = `{"key":1}`
      status         = `RUNNING`
    ).

    " 2. ACT (Execute logic)
    " Replace 'zcl_hager_serializestate' with the actual class name containing the static methods
    DATA(json_output) = zcl_ai_utils=>state_to_json( input_state ).
    DATA(result_state) = zcl_ai_utils=>json_to_state( json_output ).

    " 3. ASSERT (Verify results)
    cl_abap_unit_assert=>assert_equals(
      act = result_state
      exp = input_state
      msg = 'The deserialized state should match the original input exactly'
    ).
  ENDMETHOD.

  METHOD test_empty_state.
    " 1. ARRANGE
    DATA(empty_state) = VALUE zif_ai_types=>ts_graph_state( ).

    " 2. ACT
    DATA(json) = zcl_ai_utils=>state_to_json( empty_state ).
    DATA(result) = zcl_ai_utils=>json_to_state( json ).

    " 3. ASSERT
    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = empty_state
      msg = 'Empty state should survive a round-trip without creating garbage data'
    ).
  ENDMETHOD.

  METHOD test_special_chars.
    " 1. ARRANGE
    DATA(nasty_state) = VALUE zif_ai_types=>ts_graph_state(
      messages = value #( ( role = 'system' content = |Line 1\nLine 2 with "Quotes" and \\ backslash| ) )
    ).

    " 2. ACT
    DATA(json) = zcl_ai_utils=>state_to_json( nasty_state ).
    DATA(result) = zcl_ai_utils=>json_to_state( json ).

    " 3. ASSERT
    cl_abap_unit_assert=>assert_equals(
      act = result-messages
      exp = nasty_state-messages
      msg = 'Special characters (newlines, quotes) must be preserved'
    ).
  ENDMETHOD.

  METHOD test_partial_json.
    " 1. ARRANGE
    " Manually create JSON that is missing fields (simulating an old version)
    DATA(old_json) = `{"status": "DONE"}`.

    " 2. ACT
    DATA(result) = zcl_ai_utils=>json_to_state( old_json ).

    " 3. ASSERT
    cl_abap_unit_assert=>assert_equals(
      act = result-status
      exp = 'DONE'
      msg = 'Existing fields should be parsed'
    ).

    cl_abap_unit_assert=>assert_initial(
      act = result-messages
      msg = 'Missing fields (messages) should be initial/empty'
    ).
  ENDMETHOD.

  METHOD test_malformed_json.
    " 1. ARRANGE
    DATA(bad_json) = `{"status": "broken...`.

    " 2. ACT & ASSERT STRATEGY
    TRY.
        zcl_ai_utils=>json_to_state( bad_json ).

        " If we reach this line, the test FAILED (because no exception was raised)
        cl_abap_unit_assert=>fail( msg = 'Deserializing bad JSON should raise an exception' ).

      CATCH cx_root.
        " If we catch an exception, the test PASSED.
        " We can optionally assert details about the exception here.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

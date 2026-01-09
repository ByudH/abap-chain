*"* use this source file for your ABAP unit test classes
CLASS ltcl_node_llm DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      verify_node_type FOR TESTING.
ENDCLASS.

CLASS ltcl_node_llm IMPLEMENTATION.

  METHOD verify_node_type.
    " 1. GIVEN: Create the instance
    DATA(lo_cut) = NEW zcl_ai_node_llm( node_id = zcl_ai_utils=>generate_uuid( ) agent_id = zcl_ai_utils=>generate_uuid( ) ).

    " 2. WHEN: Call the method
    DATA(lv_actual_type) = lo_cut->zif_ai_node~get_node_type( ).

    " 3. THEN: Verify it matches expectation
    " Note: Adjust 'ZCL_AI_NODE_LLM' if you implemented the logic to return just 'LLM'
    cl_abap_unit_assert=>assert_equals(
      act = lv_actual_type
      exp = 'ZCL_AI_NODE_LLM'
      msg = 'Node type retrieval should return the correct class name'
    ).
  ENDMETHOD.

ENDCLASS.

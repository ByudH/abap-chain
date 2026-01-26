" --- 1. Local Mock Class Definition ---
" We create a concrete class inheriting from the abstract base to simulate a real node
CLASS lcl_mock_node DEFINITION INHERITING FROM zcl_ai_node_base.
  PROTECTED SECTION.
    " Redefining the protected execution method as required by zcl_ai_node_base
    METHODS do_execute REDEFINITION.
ENDCLASS.

CLASS lcl_mock_node IMPLEMENTATION.
  METHOD do_execute.
    " Simulate node business logic: Append execution trace to messages
    state-messages = VALUE zif_ai_types=>tt_messages(
      BASE state-messages
      ( role    = zif_ai_types=>gc_role_assistant
        content = '[Node Executed]' )
    ).
    " Set a branch label for the orchestrator routing logic
    state-branch_label = 'SUCCESS'.
  ENDMETHOD.
ENDCLASS.

" --- 2. Orchestrator Test Class ---
CLASS lcl_test_orchestrator DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    " Test method to verify orchestration logic and DB persistence
    METHODS test_orchestration_persistence FOR TESTING.
ENDCLASS.

CLASS lcl_test_orchestrator IMPLEMENTATION.

  METHOD test_orchestration_persistence.
    " A. Prepare compatible ID types (SYSUUID_X16)
    DATA: lv_test_node_id  TYPE zif_ai_types=>ty_node_id,
          lv_test_agent_id TYPE zif_ai_types=>ty_agent_id.

    " Generate valid 16-byte raw UUIDs to avoid type conflicts
    lv_test_node_id  = cl_system_uuid=>create_uuid_x16_static( ).
    lv_test_agent_id = cl_system_uuid=>create_uuid_x16_static( ).

    DATA(lt_graph) = VALUE zif_ai_types=>th_graph_map( ).

    data agent_name type string value 'Test Agent'.

    " B. Instantiate the mock node with valid IDs
    DATA(lo_node) = NEW lcl_mock_node(

      node_name = 'Test'
    ).

    " C. Build the Graph structure: START -> END
    INSERT VALUE #(
      source_node_id = lv_test_node_id
      source_node    = lo_node
      next_nodes     = VALUE #( ) " No outgoing edges means termination
    ) INTO TABLE lt_graph.

    " D. Execute the Orchestrator
*    DATA(ls_final_state) = zcl_ai_orchestrator=>run(
*      agent_id     = lv_test_agent_id
*      node_edge_graph = lt_graph
*      start_node_id   = lv_test_node_id
*      initial_state   = VALUE #( messages = 'Start: ' )
*    ).
    DATA(ls_final_state) = zcl_ai_orchestrator=>run(
      agent_id        = lv_test_agent_id
      agent_name      = agent_name
      node_edge_graph = lt_graph
      tools = value #( )
      start_node_id   = lv_test_node_id
      initial_state   = VALUE zif_ai_types=>ts_graph_state(
        messages = VALUE zif_ai_types=>tt_messages(
          ( role = zif_ai_types=>gc_role_user content = 'Start' )
        )
        status   = zif_ai_types=>gc_workflow_status_new
      )
    ).

    " E. Result Assertions

    " 1. Verify if the node was actually executed (checking the message string)
    " We use CS (Contains String) operator for maximum compatibility across SAP versions
*    cl_abap_unit_assert=>assert_true(
*      act = boolc( ls_final_state-messages CS '[Node Executed]' )
*      msg = 'Execution trace missing in final state'
*    ).

    DATA(found) = abap_false.
    LOOP AT ls_final_state-messages ASSIGNING FIELD-SYMBOL(<m>).
      IF <m>-content = '[Node Executed]'.
        found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true(
      act = found
      msg = 'Execution trace missing in final state messages'
    ).



    " 2. Verify Database Persistence (Checkpoint)
    " The orchestrator should have saved exactly 1 record for this node execution
    DATA lv_db_count TYPE i.
    SELECT COUNT(*) FROM zai_checkpoint
      WHERE agent_id = @lv_test_agent_id
      INTO @lv_db_count.

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_db_count
      msg = 'Orchestrator failed to save the checkpoint in ZAI_CHECKPOINT table'
    ).
  ENDMETHOD.

ENDCLASS.

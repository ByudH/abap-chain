CLASS zcl_abapchain_demo DEFINITION
  PUBLIC
    " INHERITING FROM cl_demo_classrun
  FINAL

  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    " METHODS main REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: out TYPE REF TO if_oo_adt_classrun_out.
    METHODS: abapchaintest.
ENDCLASS.


CLASS zcl_abapchain_demo IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    me->out = out.

    abapchaintest( ).

  ENDMETHOD.

  METHOD abapchaintest.
    DATA ls_initial_state TYPE zif_ai_types=>ty_graph_state.
    ls_initial_state-messages =
      |User: Please analyze table VBAK and check if there is any risk.|.

    " Create LLM client
    DATA(lo_llm_client) = NEW zcl_ai_llm_client_stub( ).

    " Generate UUIDs
    DATA(lv_agent_id) = zcl_ai_utils=>generate_uuid( ).
    DATA(lv_node1_id) = zcl_ai_utils=>generate_uuid( ).
    DATA(lv_node2_id) = zcl_ai_utils=>generate_uuid( ).

    " Create nodes
    DATA(lo_node1) =
      NEW zcl_ai_node_llm_with_tools(
        iv_node_id    = lv_node1_id
        iv_agent_id   = lv_agent_id
        iv_name       = 'Node_Analyze_Request'
        io_llm_client = lo_llm_client ).

    DATA(lo_node2) =
      NEW zcl_ai_node_llm_with_tools(
        iv_node_id    = lv_node2_id
        iv_agent_id   = lv_agent_id
        iv_name       = 'Node_LLMAgent_With_Tools'
        io_llm_client = lo_llm_client ).

    " Create tools
    DATA(lo_tool_table_info) =
      NEW zcl_ai_tool_fake_table_info( iv_name = 'TableInfoTool' ).

    DATA(lo_tool_risk_check) =
      NEW zcl_ai_tool_fake_risk_check( iv_name = 'RiskCheckTool' ).

    " Build agent
    DATA(lo_builder) =
      zcl_ai_agent_builder=>new( iv_agent_name = 'ABAPCHAIN_DEMO_AGENT' ).

    lo_builder = lo_builder->add_node( lo_node1 ).
    lo_builder = lo_builder->add_node( lo_node2 ).

    lo_builder = lo_builder->connect_always(
                   iv_from_node_id = lv_node1_id
                   iv_to_node_id   = lv_node2_id ).

    lo_builder = lo_builder->add_tool(
                   iv_tool_name        = 'table_info_tool'
                   iv_tool_description = 'Retrieve ABAP table metadata'
                   io_tool             = lo_tool_table_info ).

    lo_builder = lo_builder->add_tool(
                   iv_tool_name        = 'risk_check_tool'
                   iv_tool_description = 'Mock risk check'
                   io_tool             = lo_tool_risk_check ).

    lo_builder = lo_builder->attach_tool_to_node(
                   io_node      = lo_node2
                   iv_tool_name = 'table_info_tool' ).

    lo_builder = lo_builder->attach_tool_to_node(
                   io_node      = lo_node2
                   iv_tool_name = 'risk_check_tool' ).

    lo_builder = lo_builder->set_start_node( iv_node_id = lv_node1_id ).

    " Build final agent
    DATA(lo_agent) = lo_builder->build( ).

    " Run agent
    DATA(ls_final_state) =
      lo_agent->run( is_initial_state = ls_initial_state ).



    DATA(rv_output) = |=== ABAPCHAIN DEMO RUN ===| && |\n|
               && |Final messages: { ls_final_state-messages }| && |\n|
               && |Last tool: { ls_final_state-last_tool_name }| && |\n|
               && |Result: { ls_final_state-result_json }|.


    " cl_abap_unit_assert=>fail( msg = rv_output ).
    out->write( rv_output ).
  ENDMETHOD.

ENDCLASS.

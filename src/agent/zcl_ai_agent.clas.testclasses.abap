*"* use this source file for your ABAP unit test classes
CLASS ltzcl_ai_agent_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      serialize_agent_to_blueprint   FOR TESTING RAISING cx_static_check,
      deserialize_blueprint_to_agent FOR TESTING RAISING cx_static_check.
    DATA:
      blueprint    TYPE zif_ai_types=>ts_agent_blueprint,
      agent        TYPE REF TO zcl_ai_agent,
      tool_table   TYPE REF TO zif_ai_tool,
      tool_risk    TYPE REF TO zif_ai_tool,
      llm_node     TYPE REF TO zcl_ai_node_llm,
      tool_node    TYPE REF TO zcl_ai_node_tool.
    DATA builder TYPE REF TO zcl_ai_agent_builder.
ENDCLASS.

CLASS ltzcl_ai_agent_test IMPLEMENTATION.
  METHOD setup.
    " A. Create Mock Tools
    " (Assuming you have a simple tool class or mock available)
    tool_table = NEW zcl_ai_tool_fake_table_info( ).
    tool_risk  = NEW zcl_ai_tool_fake_risk_check( ).

    " B. Create Nodes
    " --- LLM Node ---
    llm_node = NEW zcl_ai_node_llm(
      name = 'LLM-Node'
    ).

    " --- Tool Node ---
    tool_node = NEW zcl_ai_node_tool(
      name = 'Tool Executor'
    ).

    builder = zcl_ai_agent_builder=>new( name = 'ABAPCHAIN_DEMO' ).

    builder->add_node(
                tool_node

            )->add_node(
            llm_node

            )->add_tool(
                alias       = 'table_info'
                description = 'Mock: table metadata'
                tool        = tool_table

            )->add_tool(
                alias       = 'risk_check'
                description = 'Mock: risk rating'
                tool        = tool_risk

            )->connect_on_control(
                from_node_id  = llm_node->node_id
                to_node_id    = tool_node->node_id
                control_value = 'TOOL'
                priority      = 1

            )->connect_always(
                from_node_id  = tool_node->node_id
                to_node_id    = llm_node->node_id
                priority      = 1

            )->attach_tool_to_node(
                node  = tool_node
                tool_name  = 'table_info'

            )->attach_tool_to_node(
                node  = tool_node
                tool_name  = 'risk_check'
            )->set_start_node(
                llm_node->node_id
            ).

    TRY.
        agent = builder->build( ).
      CATCH zcx_ai_agent_error INTO DATA(e).
        cl_abap_unit_assert=>fail( msg = |Agent build failed: { e->message }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD serialize_agent_to_blueprint.
    " ==================================================================
    " 1. GIVEN: An Agent with 1 LLM Node and 1 Tool Node (w/ 2 Tools)
    " ==================================================================

    " A. Create Mock Tools
    " (Assuming you have a simple tool class or mock available)
*    DATA(tool_table) = NEW zcl_ai_tool_fake_table_info( ).
*    DATA(tool_risk)  = NEW zcl_ai_tool_fake_risk_check( ).
*    DATA(agent_id) = zcl_ai_utils=>generate_uuid( ).
*    DATA(tool_node_id)    = zcl_ai_utils=>generate_uuid( ).
*    DATA(llm_node_id) = zcl_ai_utils=>generate_uuid( ).
*
*    " B. Create Nodes
*    " --- LLM Node ---
*    DATA(llm_node) = NEW zcl_ai_node_llm(
*      node_id  = llm_node_id
*      agent_id = agent_id
*    ).
*
*    " --- Tool Node ---
*    DATA(tool_node) = NEW zcl_ai_node_tool(
*      node_id  = tool_node_id
*      agent_id = agent_id
*      name     = 'Tool Executor'
*    ).
*
*    DATA(builder) = zcl_ai_agent_builder=>new( name = 'ABAPCHAIN_DEMO' ).
*
*    builder->add_node(
*                tool_node
*
*            )->add_node(
*            llm_node
*
*            )->add_tool(
*                alias       = 'table_info'
*                description = 'Mock: table metadata'
*                tool        = tool_table
*
*            )->add_tool(
*                alias       = 'risk_check'
*                description = 'Mock: risk rating'
*                tool        = tool_risk
*
*            )->connect_on_control(
*                from_node_id  = llm_node_id
*                to_node_id    = tool_node_id
*                control_value = 'TOOL'
*                priority      = 1
*
*            )->connect_always(
*                from_node_id  = tool_node_id
*                to_node_id    = llm_node_id
*                priority      = 1
*
*            )->attach_tool_to_node(
*                node  = tool_node
*                tool_name  = 'table_info'
*
*            )->attach_tool_to_node(
*                node  = tool_node
*                tool_name  = 'risk_check'
*            )->set_start_node(
*                llm_node_id
*            ).
*
*    DATA(agent) = builder->build( ).


    " ==================================================================
    " 2. WHEN: We serialize the Agent to a Blueprint
    " ==================================================================
    blueprint = agent->get_agent_blueprint( ).
    " ==================================================================
    " 3. THEN: Assert the Blueprint matches the structure
    " ==================================================================

    " A. Check Header
    cl_abap_unit_assert=>assert_equals(
      act = blueprint-agent_name
      exp = 'ABAPCHAIN_DEMO'
      msg = 'Agent name should match' ).

      cl_abap_unit_assert=>assert_equals(
      act = blueprint-agent_id
      exp = agent->agent_id
      msg = 'Agent id should match' ).

    cl_abap_unit_assert=>assert_equals(
      act = blueprint-start_node_id
      exp = llm_node->node_id
      msg = 'Start node ID should match' ).

    " B. Check Nodes (Should be 2)
    cl_abap_unit_assert=>assert_equals(
      act = lines( blueprint-graph_blueprint )
      exp = 2
      msg = 'Should have exactly 2 nodes serialized' ).

    " Verify Node Types & Config
    READ TABLE blueprint-graph_blueprint INTO DATA(bp_llm) WITH KEY node_id = llm_node->node_id.
    cl_abap_unit_assert=>assert_subrc( msg = 'LLM Node must exist in blueprint' ).

    " Check if JSON config was generated (Simple check: not empty)
    cl_abap_unit_assert=>assert_not_initial(
      act = bp_llm-config
      msg = 'LLM Node configuration JSON should not be empty' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = bp_llm-class_name
      exp = 'zcl_ai_node_llm'
      msg = 'Tool Node config should contain tool executor name' ).

    READ TABLE blueprint-graph_blueprint INTO DATA(bp_tool) WITH KEY node_id = tool_node->node_id.
    cl_abap_unit_assert=>assert_subrc( msg = 'LLM Node must exist in blueprint' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = bp_llm-config
      msg = 'Tool Node configuration JSON should not be empty' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = bp_tool-class_name
      exp = 'zcl_ai_node_tool'
      msg = 'Tool Node config should contain tool executor name' ).

    " C. Check Edges
    " LLM->Tool and Tool->LLM = 2 edges
    cl_abap_unit_assert=>assert_equals(
      act = lines( blueprint-graph_blueprint[ node_id = llm_node->node_id ]-next_nodes ) + lines( blueprint-graph_blueprint[ node_id = tool_node->node_id ]-next_nodes )
      exp = 2
      msg = 'Should have 2 edges defined' ).

    " D. Check Tools Registry (Should be 2)
    cl_abap_unit_assert=>assert_equals(
      act = lines( blueprint-tool_registry_blueprint )
      exp = 2
      msg = 'Blueprint should contain 2 tool definitions' ).

    READ TABLE blueprint-tool_registry_blueprint INTO DATA(bp_tool_search) WITH KEY tool_name = 'table_info'.
    cl_abap_unit_assert=>assert_subrc( msg = 'Search tool must be in blueprint' ).
    cl_abap_unit_assert=>assert_equals(
      act = bp_tool_search-tool_description
      exp = 'Mock: table metadata'
      msg = 'Tool description should be preserved' ).

    READ TABLE blueprint-tool_registry_blueprint INTO DATA(bp_tool_risk) WITH KEY tool_name = 'risk_check'.
    cl_abap_unit_assert=>assert_subrc( msg = 'Risk check tool must be in blueprint' ).
    cl_abap_unit_assert=>assert_equals(
      act = bp_tool_risk-tool_description
      exp = 'Mock: risk rating'
      msg = 'Tool description should be preserved' ).

  ENDMETHOD.

  METHOD deserialize_blueprint_to_agent.
    blueprint = agent->get_agent_blueprint( ).
    DATA(actual_agent) = builder->build_from_blueprint( blueprint ).
    cl_abap_unit_assert=>assert_equals(
      act = actual_agent->agent_id
      exp = agent->agent_id
      msg = 'Agent Id should be the same' ).

    cl_abap_unit_assert=>assert_equals(
      act = actual_agent->agent_id
      exp = agent->agent_id
      msg = 'Agent name should be the same' ).

    cl_abap_unit_assert=>assert_equals(
      act = actual_agent->start_node_id
      exp = agent->start_node_id
      msg = 'Start node id should be the same' ).

    LOOP AT agent->tools INTO DATA(original_tool).
      DATA(actual_tool) = actual_agent->tools[ tool_name = original_tool-tool_name ].
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>fail( msg = |Tool { original_tool-tool_name } should exist in deserialized agent| ).
      ENDIF.
      cl_abap_unit_assert=>assert_equals(
        act = actual_tool-tool_name
        exp = original_tool-tool_name
        msg = |Tool name { original_tool-tool_name } should match| ).
      cl_abap_unit_assert=>assert_equals(
        act = actual_tool-tool_description
        exp = original_tool-tool_description
        msg = |Tool description { original_tool-tool_description } should match| ).
      cl_abap_unit_assert=>assert_equals(
        act = actual_tool-tool_endpoint->get_tool_type( )
        exp = original_tool-tool_endpoint->get_tool_type( )
        msg = |Tool type should match| ).
      cl_abap_unit_assert=>assert_equals(
        act = actual_tool-tool_endpoint->get_name( )
        exp = original_tool-tool_endpoint->get_name( )
        msg = |Tool name should match| ).
      cl_abap_unit_assert=>assert_equals(
        act = actual_tool-tool_endpoint->get_description( )
        exp = original_tool-tool_endpoint->get_description( )
        msg = |Tool description should match| ).
    ENDLOOP.

    " assert the graph structure
    LOOP AT agent->node_edge_graph INTO DATA(original_node).
      DATA(actual_node) = actual_agent->node_edge_graph[ source_node_id = original_node-source_node_id ].
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>fail( msg = |Node { original_node-source_node_id } should exist in deserialized agent| ).
      ENDIF.
      cl_abap_unit_assert=>assert_equals(
        act = actual_node-source_node_id
        exp = original_node-source_node_id
        msg = |Node id { original_node-source_node_id } should match| ).
      cl_abap_unit_assert=>assert_equals(
        act = actual_node-source_node->get_configuration( )
        exp = original_node-source_node->get_configuration( )
        msg = |Node configuration should match| ).
      " if the node is tool node, check the tool endpoint in the tool node
      " tool registry points to the tool endpoint in the agent tool registry
      IF actual_node-source_node->get_node_type( ) = to_upper('zcl_ai_node_tool').
        DATA(actual_tool_node) = CAST zif_ai_node_tool_aware( actual_node-source_node ).
        DATA(actual_tool_node_tools) = actual_tool_node->get_tools( ).
        LOOP AT actual_tool_node_tools INTO DATA(actual_tool_node_tool).
          cl_abap_unit_assert=>assert_equals(
            act = actual_tool_node_tool-tool_endpoint
            exp = actual_agent->tools[ tool_name = actual_tool_node_tool-tool_name ]-tool_endpoint
            msg = |Tool endpoint in tool node should point to the tool in agent tool registry|
          ).
        ENDLOOP.
      ENDIF.

      LOOP AT original_node-next_nodes INTO DATA(original_edge).
        "DATA(actual_edge) = actual_node-next_nodes[ target_node_id = original_edge-target_node_id ]. " failure causing line

        DATA(actual_edge) = actual_node-next_nodes[ sy-tabix ].
*        IF sy-subrc <> 0.
*          cl_abap_unit_assert=>fail( msg = |Edge to node { original_edge-target_node_id } should exist in deserialized agent| ).
*        ENDIF.
        cl_abap_unit_assert=>assert_equals(
          act = actual_edge-target_node_id
          exp = original_edge-target_node_id
          msg = |Edge to node { original_edge-target_node_id } should match| ).

        cl_abap_unit_assert=>assert_equals(
          act = actual_edge-condition
          exp = original_edge-condition
          msg = |Edge condition to node { original_edge-target_node_id } should match| ).
        cl_abap_unit_assert=>assert_equals(
          act = actual_edge-condition_value
          exp = original_edge-condition_value
          msg = |Edge condition value to node { original_edge-target_node_id } should match| ).
        cl_abap_unit_assert=>assert_equals(
          act = actual_edge-priority
          exp = original_edge-priority
          msg = |Edge priority to node { original_edge-target_node_id } should match| ).
        " check the target node points to the correct node in the node_edge_graph
        cl_abap_unit_assert=>assert_equals(
          act = actual_edge-target_node
          exp = actual_agent->node_edge_graph[ source_node_id = actual_edge-target_node_id ]-source_node ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

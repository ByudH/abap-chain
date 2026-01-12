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
      agent        TYPE REF TO zcl_ai_agent_lh,
      tool_table   TYPE REF TO zif_ai_tool,
      tool_risk    TYPE REF TO zif_ai_tool,
      agent_id     TYPE zif_ai_types=>ty_agent_id,
      llm_node     TYPE REF TO zif_ai_node,
      tool_node    TYPE REF TO zif_ai_node,
      tool_node_id TYPE zif_ai_types=>ty_node_id,
      llm_node_id  TYPE zif_ai_types=>ty_node_id.
    DATA builder TYPE REF TO zcl_ai_agent_builder.
ENDCLASS.

CLASS ltzcl_ai_agent_test IMPLEMENTATION.
  METHOD setup.
    " A. Create Mock Tools
    " (Assuming you have a simple tool class or mock available)
    tool_table = NEW zcl_ai_tool_fake_table_info( ).
    tool_risk  = NEW zcl_ai_tool_fake_risk_check( ).
    agent_id = zcl_ai_utils=>generate_uuid( ).
    tool_node_id    = zcl_ai_utils=>generate_uuid( ).
    llm_node_id = zcl_ai_utils=>generate_uuid( ).

    " B. Create Nodes
    " --- LLM Node ---
    llm_node = NEW zcl_ai_node_llm(
      name = 'LLM-Node'
      node_id  = llm_node_id
      agent_id = agent_id
    ).

    " --- Tool Node ---
    tool_node = NEW zcl_ai_node_tool(
      node_id  = tool_node_id
      agent_id = agent_id
      name     = 'Tool Executor'
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
                from_node_id  = llm_node_id
                to_node_id    = tool_node_id
                control_value = 'TOOL'
                priority      = 1

            )->connect_always(
                from_node_id  = tool_node_id
                to_node_id    = llm_node_id
                priority      = 1

            )->attach_tool_to_node(
                node  = tool_node
                tool_name  = 'table_info'

            )->attach_tool_to_node(
                node  = tool_node
                tool_name  = 'risk_check'
            )->set_start_node(
                llm_node_id
            ).

    agent = builder->build( ).


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
      act = blueprint-start_node_id
      exp = llm_node_id
      msg = 'Start node ID should match' ).

    " B. Check Nodes (Should be 2)
    cl_abap_unit_assert=>assert_equals(
      act = lines( blueprint-graph_blueprint )
      exp = 2
      msg = 'Should have exactly 2 nodes serialized' ).

    " Verify Node Types & Config
    READ TABLE blueprint-graph_blueprint INTO DATA(bp_llm) WITH KEY node_id = llm_node_id.
    cl_abap_unit_assert=>assert_subrc( msg = 'LLM Node must exist in blueprint' ).

    " Check if JSON config was generated (Simple check: not empty)
    cl_abap_unit_assert=>assert_not_initial(
      act = bp_llm-config
      msg = 'LLM Node configuration JSON should not be empty' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = bp_llm-class_name
      exp = 'zcl_ai_node_llm'
      msg = 'Tool Node config should contain tool executor name' ).

    READ TABLE blueprint-graph_blueprint INTO DATA(bp_tool) WITH KEY node_id = tool_node_id.
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
      act = lines( blueprint-graph_blueprint[ node_id = llm_node_id ]-next_nodes ) + lines( blueprint-graph_blueprint[ node_id = tool_node_id ]-next_nodes )
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

  ENDMETHOD.

ENDCLASS.

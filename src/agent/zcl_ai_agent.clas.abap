CLASS zcl_ai_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    DATA agent_name TYPE string READ-ONLY.
    DATA agent_id TYPE zif_ai_types=>ty_agent_id READ-ONLY.
    " design-time graph structures
    DATA graph_structure TYPE zif_ai_types=>th_graph_map READ-ONLY.
    DATA graph_state TYPE zif_ai_types=>ts_graph_state READ-ONLY.
    DATA start_node TYPE REF TO zif_ai_node READ-ONLY.
    " add tool registry map
    DATA tools TYPE zif_ai_types=>th_tool_registry_map READ-ONLY.

    CLASS-METHODS create
      IMPORTING
                agent_name      TYPE string
                graph_structure TYPE zif_ai_types=>th_graph_map
                graph_state     TYPE zif_ai_types=>ts_graph_state
                start_node      TYPE REF TO zif_ai_node
                tools           TYPE zif_ai_types=>th_tool_registry_map OPTIONAL

      RETURNING VALUE(agent)    TYPE REF TO zcl_ai_agent.

    " Firstly use addNode and addEdge to build the graph structure.
    " Will be integrated with agent builder in the future.
    METHODS add_node
      IMPORTING
        node TYPE REF TO zcl_ai_node_base.
    " deprecated -- use graph builder instead
    METHODS add_edge
      IMPORTING
        source_node TYPE REF TO zcl_ai_node_base
        target_node TYPE REF TO zcl_ai_node_base
        condition   TYPE string.
    " deprecated use run instead
    METHODS execute_graph.
    METHODS run
      CHANGING
        state TYPE zif_ai_types=>ts_graph_state.

  PRIVATE SECTION.
    METHODS constructor
      IMPORTING
        agent_name      TYPE string
        graph_structure TYPE zif_ai_types=>th_graph_map
        graph_state     TYPE zif_ai_types=>ts_graph_state
        start_node      TYPE REF TO zif_ai_node
        tools           TYPE zif_ai_types=>th_tool_registry_map.

ENDCLASS.



CLASS zcl_ai_agent IMPLEMENTATION.
  METHOD constructor.
    me->agent_name = agent_name.
    me->agent_id = zcl_ai_utils=>generate_uuid( ).
    " Insert the tool stubs into the tool registry map
    me->tools = tools.
*    me->tools = VALUE #(
*    ( tool_name        = 'risk_check_tool'
*      tool_endpoint    = NEW zcl_ai_tool_fake_risk_check( iv_name = 'RiskCheckTool' )
*      tool_description = 'A tool to perform risk checks on given data.' )
*    ( tool_name        = 'table_info_tool'
*      tool_endpoint    = NEW zcl_ai_tool_fake_table_info( iv_name = 'TableInfoTool' )
*      tool_description = 'A tool to retrieve information about database tables.' )
*    ).
    me->graph_structure = graph_structure.
    me->graph_state = graph_state.
    me->start_node = start_node.

    " TODO: initialize the graph state to have a system prompt or other necessary info
  ENDMETHOD.
  METHOD run.
    " add orchestrator implementation here
    zcl_ai_orchestrator=>run(
                    node_edge_graph = me->graph_structure
                    start_node_id   = me->start_node->get_node_id( )
                    initial_state   = graph_state ).
  ENDMETHOD.
  METHOD create.
    agent = NEW zcl_ai_agent(
      agent_name      = agent_name
      graph_structure = graph_structure
      graph_state     = graph_state
      start_node      = start_node
      tools           = tools
    ).
  ENDMETHOD.
  METHOD add_node.
    INSERT VALUE #(
      source_node_id = node->node_id
      source_node    = node
      next_nodes     = VALUE #( )
    ) INTO TABLE me->graph_structure.
    IF me->start_node IS NOT BOUND.
      me->start_node = node.
    ENDIF.
  ENDMETHOD.
  METHOD add_edge.
    " need to make sure the source_node already exists
    ASSIGN me->graph_structure[ source_node_id = source_node->node_id ]-next_nodes TO FIELD-SYMBOL(<edge_list>).
    INSERT VALUE #(
      target_node_id = target_node->node_id
      target_node    = target_node
      condition      = condition
    ) INTO TABLE <edge_list>.

  ENDMETHOD.

  METHOD execute_graph.
    " TODO: exception handling for empty graph

    " Execute from the start node
    DATA(next_node) = me->start_node.
    WHILE next_node IS BOUND.
      next_node->execute( CHANGING state = me->graph_state ).
      " Determine the next node based on conditions
      DATA(edge_list) = me->graph_structure[ source_node_id = next_node->get_node_id( ) ]-next_nodes.
      DATA(found_next) = abap_false.
      LOOP AT edge_list INTO DATA(edge).
        " Here we would evaluate the condition against the current graph state
        " For simplicity, we assume conditions are always met in this example
        next_node = edge-target_node.
        found_next = abap_true.
        EXIT.
      ENDLOOP.
      IF found_next = abap_false.
        " No valid next node found, end execution
        EXIT.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.

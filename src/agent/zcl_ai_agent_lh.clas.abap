
CLASS zcl_ai_agent_lh DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE. " enforce factory usage

  PUBLIC SECTION.

    DATA agent_name      TYPE string                    READ-ONLY.
    DATA agent_id        TYPE zif_ai_types=>ty_agent_id READ-ONLY.

    " Runtime graph representation
    DATA node_edge_graph TYPE zif_ai_types=>th_graph_map      READ-ONLY.
    DATA start_node_id   TYPE zif_ai_types=>ty_node_id        READ-ONLY.

    " Agent-level tool registry
    DATA tools           TYPE zif_ai_types=>th_tool_registry_map READ-ONLY.

    " Factory method – called by the builder
    CLASS-METHODS create
      IMPORTING
        agent_name      TYPE string
        node_edge_graph TYPE zif_ai_types=>th_graph_map
        start_node_id   TYPE zif_ai_types=>ty_node_id
        tools           TYPE zif_ai_types=>th_tool_registry_map OPTIONAL
      RETURNING
        VALUE(agent)    TYPE REF TO zcl_ai_agent_lh.

    METHODS run
      IMPORTING
        initial_state      TYPE zif_ai_types=>ts_graph_state OPTIONAL
      RETURNING
        VALUE(final_state) TYPE zif_ai_types=>ts_graph_state.

  PRIVATE SECTION.

    METHODS constructor
      IMPORTING
        agent_name      TYPE string
        node_edge_graph TYPE zif_ai_types=>th_graph_map
        start_node_id   TYPE zif_ai_types=>ty_node_id
        tools           TYPE zif_ai_types=>th_tool_registry_map.

ENDCLASS.

CLASS zcl_ai_agent_lh IMPLEMENTATION.

  METHOD run.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    TRY.
        logger->start_run(
          agent_name = me->agent_name
          agent_id   = me->agent_id ).
      CATCH cx_root INTO DATA(err).
        logger->log_error( err->get_text( ) ).
        " Logging must never break execution
    ENDTRY.

    final_state = zcl_ai_orchestrator=>run(
      agent_id        = me->agent_id
      node_edge_graph = me->node_edge_graph
      start_node_id   = me->start_node_id
      initial_state   = initial_state ).

    TRY.
        logger->save_and_get_handle( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    " Basic metadata
    me->agent_name = agent_name.

    " Create a new agent_id
    me->agent_id = zcl_ai_utils=>generate_uuid( ).

    " Graph structure
    me->node_edge_graph = node_edge_graph.
    me->start_node_id   = start_node_id.

    " Tools
    me->tools = tools.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT agent
      EXPORTING
        agent_name      = agent_name
        node_edge_graph = node_edge_graph
        start_node_id   = start_node_id
        tools           = tools.
  ENDMETHOD.

ENDCLASS.

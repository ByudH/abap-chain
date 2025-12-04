CLASS zcl_ai_agent_lh DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE. " enforce factory usage

  PUBLIC SECTION.

    DATA agent_name     TYPE string                        READ-ONLY.
    DATA agent_id       TYPE zif_ai_types=>ty_agent_id     READ-ONLY.

    " Design-time graph representation
    DATA nodes          TYPE zif_ai_types=>tt_node_registry_lh READ-ONLY.
    DATA edges          TYPE zif_ai_types=>tt_edge_lh         READ-ONLY.
    DATA start_node_id  TYPE zif_ai_types=>ty_node_id      READ-ONLY.


    DATA tools TYPE zif_ai_types=>tool_registry_map READ-ONLY.

    " Factory method – this is what the builder will call
    CLASS-METHODS create
      IMPORTING
        iv_agent_name    TYPE string
        it_nodes         TYPE zif_ai_types=>tt_node_registry_lh
        it_edges         TYPE zif_ai_types=>tt_edge_lh
        iv_start_node_id TYPE zif_ai_types=>ty_node_id
        it_tools         TYPE zif_ai_types=>tool_registry_map OPTIONAL
      RETURNING
        VALUE(ro_agent)  TYPE REF TO zcl_ai_agent_lh.


    METHODS run
      IMPORTING
        is_initial_state      TYPE zif_ai_types=>ty_graph_state OPTIONAL
      RETURNING
        VALUE(rs_final_state) TYPE zif_ai_types=>ty_graph_state.

  PRIVATE SECTION.

    METHODS constructor
      IMPORTING
        iv_agent_name    TYPE string
        it_nodes         TYPE zif_ai_types=>tt_node_registry_lh
        it_edges         TYPE zif_ai_types=>tt_edge_lh
        iv_start_node_id TYPE zif_ai_types=>ty_node_id
        it_tools         TYPE zif_ai_types=>tool_registry_map.

ENDCLASS.

CLASS zcl_ai_agent_lh IMPLEMENTATION.

   METHOD run.

    rs_final_state = zcl_ai_orchestrator=>run(
                       it_nodes         = me->nodes
                       it_edges         = me->edges
                       iv_start_node_id = me->start_node_id
                       is_initial_state = is_initial_state ).

  ENDMETHOD.


   METHOD constructor.

    " Basic metadata
    me->agent_name = iv_agent_name.

    " Create a new agent_id – adjust if you have a different util
    me->agent_id = zcl_ai_utils=>generate_uuid( ).

    " Graph structure
    me->nodes         = it_nodes.
    me->edges         = it_edges.
    me->start_node_id = iv_start_node_id.

    " Tools
    me->tools = it_tools.

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_agent
      EXPORTING
        iv_agent_name    = iv_agent_name
        it_nodes         = it_nodes
        it_edges         = it_edges
        iv_start_node_id = iv_start_node_id
        it_tools         = it_tools.
  ENDMETHOD.

ENDCLASS.

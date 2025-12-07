CLASS zcl_ai_agent_builder DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Entry point – start defining a new agent
    CLASS-METHODS new
      IMPORTING
        iv_agent_name TYPE string
      RETURNING
        VALUE(ro_builder) TYPE REF TO zcl_ai_agent_builder.

    " Register a node in the graph
    " First added node becomes start node by default (unless overridden)
    METHODS add_node
      IMPORTING
        io_node TYPE REF TO zif_ai_node
      RETURNING
        VALUE(ro_builder) TYPE REF TO zcl_ai_agent_builder.

    " Connect nodes with an ALWAYS edge
    METHODS connect_always
      IMPORTING
        iv_from_node_id TYPE zif_ai_types=>ty_node_id
        iv_to_node_id   TYPE zif_ai_types=>ty_node_id
        iv_priority     TYPE i OPTIONAL
      RETURNING
        VALUE(ro_builder) TYPE REF TO zcl_ai_agent_builder.

    " Connect nodes with ON_CONTROL condition
    METHODS connect_on_control
      IMPORTING
        iv_from_node_id  TYPE zif_ai_types=>ty_node_id
        iv_to_node_id    TYPE zif_ai_types=>ty_node_id
        iv_control_value TYPE string
        iv_priority      TYPE i OPTIONAL
      RETURNING
        VALUE(ro_builder) TYPE REF TO zcl_ai_agent_builder.

    " Explicitly override the start node (optional)
    METHODS set_start_node
      IMPORTING
        iv_node_id TYPE zif_ai_types=>ty_node_id
      RETURNING
        VALUE(ro_builder) TYPE REF TO zcl_ai_agent_builder.

    " Register a tool at agent level (name + description + implementation)
    METHODS add_tool
      IMPORTING
        iv_tool_name        TYPE string
        iv_tool_description TYPE string
        io_tool             TYPE REF TO zif_ai_tool
      RETURNING
        VALUE(ro_builder)   TYPE REF TO zcl_ai_agent_builder.

    " Attach a registered tool to a specific node (if node supports tools)
    METHODS attach_tool_to_node
      IMPORTING
        io_node      TYPE REF TO zif_ai_node
        iv_tool_name TYPE string
      RETURNING
        VALUE(ro_builder) TYPE REF TO zcl_ai_agent_builder.

    " Finalize – create the zcl_ai_agent with nodes, edges, tools
    METHODS build
      RETURNING
        VALUE(ro_agent) TYPE REF TO zcl_ai_agent_lh
      RAISING zcx_ai_agent_error.

  PRIVATE SECTION.

    DATA mv_agent_name    TYPE string.
    DATA mv_start_node_id TYPE zif_ai_types=>ty_node_id.

    " Design-time registry of nodes (id + node ref)
    DATA mt_nodes TYPE zif_ai_types=>tt_node_registry_lh.

    " Design-time list of edges (source/target IDs + condition)
    DATA mt_edges TYPE zif_ai_types=>tt_edge_lh.

    " Agent-level tool registry (HASHED TABLE by tool_name)
    DATA mt_tools TYPE zif_ai_types=>tool_registry_map.

ENDCLASS.


CLASS zcl_ai_agent_builder IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_builder.
    ro_builder->mv_agent_name = iv_agent_name.
  ENDMETHOD.

  METHOD add_node.
    DATA ls_entry TYPE zif_ai_types=>ty_node_entry_lh.

    " Get the node's ID from the base class
    ls_entry-node_id = io_node->get_node_id( ).
    ls_entry-node    = io_node.

    APPEND ls_entry TO mt_nodes.

    " If no explicit start node yet, use the first added node
    IF mv_start_node_id IS INITIAL.
      mv_start_node_id = ls_entry-node_id.
    ENDIF.

    ro_builder = me.
  ENDMETHOD.

  METHOD connect_always.
    DATA ls_edge TYPE zif_ai_types=>ty_edge_lh.

    ls_edge-source_node_id  = iv_from_node_id.
    ls_edge-target_node_id  = iv_to_node_id.
    ls_edge-condition  = zif_ai_types=>gc_cond_always.
    ls_edge-condition_value = ''.
    ls_edge-priority        =
      COND #( WHEN iv_priority IS INITIAL THEN 1 ELSE iv_priority ).

    APPEND ls_edge TO mt_edges.

    ro_builder = me.
  ENDMETHOD.

  METHOD connect_on_control.
    DATA ls_edge TYPE zif_ai_types=>ty_edge_lh.

    ls_edge-source_node_id  = iv_from_node_id.
    ls_edge-target_node_id  = iv_to_node_id.
    ls_edge-condition  = zif_ai_types=>gc_cond_on_control.
    ls_edge-condition_value = iv_control_value.
    ls_edge-priority        =
      COND #( WHEN iv_priority IS INITIAL THEN 1 ELSE iv_priority ).

    APPEND ls_edge TO mt_edges.

    ro_builder = me.
  ENDMETHOD.

  METHOD set_start_node.
    mv_start_node_id = iv_node_id.
    ro_builder       = me.
  ENDMETHOD.

  METHOD add_tool.
    DATA ls_tool TYPE zif_ai_types=>ty_tool_registry.

    ls_tool-tool_name        = iv_tool_name.
    ls_tool-tool_description = iv_tool_description.
    ls_tool-tool_endpoint    = io_tool.

    " tool_registry_map is HASHED TABLE OF ty_tool_registry
    INSERT ls_tool INTO TABLE mt_tools.

    ro_builder = me.
  ENDMETHOD.

  METHOD attach_tool_to_node.
    " Find tool definition by name in builder’s registry
    READ TABLE mt_tools INTO DATA(ls_tool)
         WITH KEY tool_name = iv_tool_name.
    IF sy-subrc <> 0.
      " Tool not found – silently ignore for now or later: raise exception
      ro_builder = me.
      RETURN.
    ENDIF.

    " Try to cast node to tool-aware interface
    DATA lo_tool_aware TYPE REF TO zif_ai_node_tool_aware.

    TRY.
        lo_tool_aware ?= io_node.
      CATCH cx_sy_move_cast_error.
        " Node does not support tools – ignore for now
        ro_builder = me.
        RETURN.
    ENDTRY.

    " Attach the tool implementation to the node
    lo_tool_aware->add_tool(
      iv_tool_name  = ls_tool-tool_name
      io_tool       = ls_tool-tool_endpoint
      iv_description = ls_tool-tool_description ).

    ro_builder = me.
  ENDMETHOD.

  METHOD build.
    " TODO: (later) validation:
    "  - mt_nodes is not initial
    "  - mv_start_node_id exists in mt_nodes
    "  - each edge source/target exists in mt_nodes
    "
    " Situation 1: Must have at least one node
    IF mt_nodes IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ai_agent_error
        EXPORTING text = 'Agent must contain at least one node.'.
    ENDIF.

    " Situation 2: Check that start node exists
    IF mv_start_node_id IS INITIAL OR
       NOT line_exists( mt_nodes[ node_id = mv_start_node_id ] ).
      RAISE EXCEPTION TYPE zcx_ai_agent_error
        EXPORTING text = |Start node ID "{ mv_start_node_id }" does not exist in nodes|.
    ENDIF.

    " Situation 3: Verify edges have valid source and target nodes
    LOOP AT mt_edges INTO DATA(ls_edge).
      IF NOT line_exists( mt_nodes[ node_id = ls_edge-source_node_id ] ).
        RAISE EXCEPTION TYPE zcx_ai_agent_error
          EXPORTING text = |Edge source node "{ ls_edge-source_node_id }" not found in nodes|.
      ENDIF.

      IF NOT line_exists( mt_nodes[ node_id = ls_edge-target_node_id ] ).
        RAISE EXCEPTION TYPE zcx_ai_agent_error
          EXPORTING text = |Edge target node "{ ls_edge-target_node_id }" not found in nodes|.
      ENDIF.
    ENDLOOP.

    ro_agent = zcl_ai_agent_lh=>create(
                 iv_agent_name    = mv_agent_name
                 it_nodes         = mt_nodes
                 it_edges         = mt_edges
                 iv_start_node_id = mv_start_node_id
                 it_tools         = mt_tools ).

  ENDMETHOD.

ENDCLASS.


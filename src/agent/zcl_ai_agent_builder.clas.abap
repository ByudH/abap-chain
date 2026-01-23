CLASS zcl_ai_agent_builder DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Entry point – start defining a new agent
    CLASS-METHODS new
      IMPORTING
        name           TYPE string
      RETURNING
        VALUE(builder) TYPE REF TO zcl_ai_agent_builder.

    " Register a node in the graph
    " First added node becomes start node by default (unless overridden)
    METHODS add_node
      IMPORTING
        node           TYPE REF TO zcl_ai_node_base
      RETURNING
        VALUE(builder) TYPE REF TO zcl_ai_agent_builder.

    " Connect nodes with an ALWAYS edge
    METHODS connect_always
      IMPORTING
        from_node_id   TYPE zif_ai_types=>ty_node_id
        to_node_id     TYPE zif_ai_types=>ty_node_id
        priority       TYPE i OPTIONAL
      RETURNING
        VALUE(builder) TYPE REF TO zcl_ai_agent_builder.

    " Connect nodes with ON_CONTROL condition
    METHODS connect_on_control
      IMPORTING
        from_node_id   TYPE zif_ai_types=>ty_node_id
        to_node_id     TYPE zif_ai_types=>ty_node_id
        control_value  TYPE string
        priority       TYPE i OPTIONAL
      RETURNING
        VALUE(builder) TYPE REF TO zcl_ai_agent_builder.

    " Explicitly override the start node (optional)
    METHODS set_start_node
      IMPORTING
        node_id        TYPE zif_ai_types=>ty_node_id
      RETURNING
        VALUE(builder) TYPE REF TO zcl_ai_agent_builder.

    " Register a tool at agent level
    METHODS add_tool
      IMPORTING
        alias          TYPE string OPTIONAL
        description    TYPE string OPTIONAL
        tool           TYPE REF TO zif_ai_tool
      RETURNING
        VALUE(builder) TYPE REF TO zcl_ai_agent_builder.

    " Attach a registered tool to a specific node (if node supports tools)
    METHODS attach_tool_to_node
      IMPORTING
        node           TYPE REF TO zif_ai_node
        tool_name      TYPE string
      RETURNING
        VALUE(builder) TYPE REF TO zcl_ai_agent_builder.

    " Finalize – create the zcl_ai_agent with nodes, edges, tools
    METHODS build
      RETURNING
                VALUE(agent) TYPE REF TO zcl_ai_agent
      RAISING   zcx_ai_agent_error.

    METHODS build_from_blueprint
      IMPORTING
                agent_blueprint TYPE zif_ai_types=>ts_agent_blueprint
      RETURNING
                VALUE(agent)    TYPE REF TO zcl_ai_agent
      RAISING   zcx_ai_agent_error.

  PRIVATE SECTION.
    DATA agent_id      TYPE zif_ai_types=>ty_agent_id.
    DATA agent_name    TYPE string.
    DATA start_node_id TYPE zif_ai_types=>ty_node_id.

    " Runtime graph that the orchestrator will consume
    DATA node_edge_graph TYPE zif_ai_types=>th_graph_map.

    " Agent-level tool registry
    DATA tools TYPE zif_ai_types=>th_tool_registry_map.

    METHODS restore_graph_from_blueprint
      IMPORTING
        graph_blueprint        TYPE zif_ai_types=>tt_graph_blueprint
        agent_id               TYPE zif_ai_types=>ty_agent_id
      RETURNING
        VALUE(node_edge_graph) TYPE zif_ai_types=>th_graph_map.

    METHODS restore_tools_from_blueprint
      IMPORTING
        tool_registry_blueprint TYPE zif_ai_types=>tt_tool_blueprints
        agent_id                TYPE zif_ai_types=>ty_agent_id
      RETURNING
        VALUE(tools)            TYPE zif_ai_types=>th_tool_registry_map.

ENDCLASS.


CLASS zcl_ai_agent_builder IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT builder.
    builder->agent_name = name.
    builder->agent_id = zcl_ai_utils=>generate_uuid( ).
  ENDMETHOD.

  METHOD add_node.
    DATA node_id TYPE zif_ai_types=>ty_node_id.

    node_id = node->node_id.

    " set the agent_id of the node
    node->agent_id = agent_id.
    " check if an entry for this node already exists
    READ TABLE node_edge_graph ASSIGNING FIELD-SYMBOL(<entry>)
         WITH KEY source_node_id = node_id.

    IF sy-subrc <> 0.
      " Create new graph entry for this node (no edges yet)
      DATA(new_entry) = VALUE zif_ai_types=>ts_graph_entry(
                          source_node_id = node_id
                          source_node    = node
                          next_nodes     = VALUE zif_ai_types=>tt_edge_list( ) ).

      INSERT new_entry INTO TABLE node_edge_graph.
    ELSE.
      " Update existing entry with node reference (edges may already exist)
      <entry>-source_node = node.
    ENDIF.

    " If no explicit start node yet, use the first added node
    IF start_node_id IS INITIAL.
      start_node_id = node_id.
    ENDIF.

    builder = me.
  ENDMETHOD.

  METHOD connect_always.
    " Ensure an entry for from_node_id exists
    READ TABLE node_edge_graph ASSIGNING FIELD-SYMBOL(<entry>)
         WITH KEY source_node_id = from_node_id.

    IF sy-subrc <> 0.
      DATA(new_entry) = VALUE zif_ai_types=>ts_graph_entry(
                          source_node_id = from_node_id
                          "source_node   is left initial, will be set by add_node
                          "next_nodes    is left INITIAL (empty table)
      ).
      INSERT new_entry INTO TABLE node_edge_graph.
      READ TABLE node_edge_graph ASSIGNING <entry>
           WITH KEY source_node_id = from_node_id.
    ENDIF.

    DATA(edge) = VALUE zif_ai_types=>ts_edge(
                   target_node_id  = to_node_id
                   " target_node    is left INITIAL, resolved in build( )
                   condition       = zif_ai_types=>gc_cond_always
                   condition_value = ''
                   priority        =
                     COND #( WHEN priority IS INITIAL THEN 1 ELSE priority ) ).

    APPEND edge TO <entry>-next_nodes.

    builder = me.
  ENDMETHOD.

  METHOD connect_on_control.
    READ TABLE node_edge_graph ASSIGNING FIELD-SYMBOL(<entry>)
         WITH KEY source_node_id = from_node_id.

    IF sy-subrc <> 0.
      DATA(new_entry) = VALUE zif_ai_types=>ts_graph_entry(
                          source_node_id = from_node_id
                          " source_node initial
                          " next_nodes  initial
      ).
      INSERT new_entry INTO TABLE node_edge_graph.
      READ TABLE node_edge_graph ASSIGNING <entry>
           WITH KEY source_node_id = from_node_id.
    ENDIF.

    DATA(edge) = VALUE zif_ai_types=>ts_edge(
                   target_node_id  = to_node_id
                   " target_node initial
                   condition       = zif_ai_types=>gc_cond_on_control
                   condition_value = control_value
                   priority        =
                     COND #( WHEN priority IS INITIAL THEN 1 ELSE priority ) ).

    APPEND edge TO <entry>-next_nodes.

    builder = me.
  ENDMETHOD.

  METHOD set_start_node.
    start_node_id = node_id.
    builder       = me.
  ENDMETHOD.

  METHOD add_tool.
    DATA entry TYPE zif_ai_types=>ts_tool_registry.

    IF alias IS INITIAL.
      entry-tool_name = tool->get_name( ).
    ELSE.
      entry-tool_name        = alias.
    ENDIF.

    IF description IS INITIAL.
      entry-tool_description = tool->get_description( ).
    ELSE.
      entry-tool_description = description.
    ENDIF.

    entry-tool_endpoint    = tool.

    INSERT entry INTO TABLE tools.

    builder = me.
  ENDMETHOD.

  METHOD attach_tool_to_node.
    READ TABLE tools INTO DATA(tool_entry)
         WITH KEY tool_name = tool_name.

    IF sy-subrc <> 0.
      builder = me.
      RETURN.
    ENDIF.

    DATA tool_aware TYPE REF TO zif_ai_node_tool_aware.

    TRY.
        tool_aware ?= node.
      CATCH cx_sy_move_cast_error.
        builder = me.
        RETURN.
    ENDTRY.

    tool_aware->add_tool(
      tool_name   = tool_entry-tool_name
      tool        = tool_entry-tool_endpoint
      description = tool_entry-tool_description ).

    builder = me.
  ENDMETHOD.

  METHOD build.
    " 1) Must have at least one node
    IF node_edge_graph IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ai_agent_error
        EXPORTING
          message = 'Agent must contain at least one node.'.
    ENDIF.

    " 2) Check that start node exists in graph
    READ TABLE node_edge_graph TRANSPORTING NO FIELDS
         WITH KEY source_node_id = start_node_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ai_agent_error
        EXPORTING
          message =
                    |Start node ID "{ start_node_id }" does not exist in graph|.
    ENDIF.

    " 3) Validate that every graph entry has a node ref
    LOOP AT node_edge_graph ASSIGNING FIELD-SYMBOL(<entry>).
      IF <entry>-source_node IS INITIAL.
        RAISE EXCEPTION TYPE zcx_ai_agent_error
          EXPORTING
            message =
                      |Graph entry for node "{ <entry>-source_node_id }" has no node instance (did you forget add_node?)|.
      ENDIF.
    ENDLOOP.

    " 4) Resolve each edge's target_node reference
    LOOP AT node_edge_graph ASSIGNING <entry>.

      LOOP AT <entry>-next_nodes ASSIGNING FIELD-SYMBOL(<edge>).

        READ TABLE node_edge_graph ASSIGNING FIELD-SYMBOL(<target_entry>)
             WITH KEY source_node_id = <edge>-target_node_id.

        IF sy-subrc <> 0 OR <target_entry>-source_node IS INITIAL.
          RAISE EXCEPTION TYPE zcx_ai_agent_error
            EXPORTING
              message =
                        |Edge from "{ <entry>-source_node_id }" to "{ <edge>-target_node_id }" has no valid target node instance|.
        ENDIF.

        <edge>-target_node = <target_entry>-source_node.

      ENDLOOP.
    ENDLOOP.

    " 5) add all TOOLS to catalog aware nodes (ZCL_AI_NODE_TOOL specifically)
    LOOP AT node_edge_graph ASSIGNING FIELD-SYMBOL(<my_entry>).

      " 1) Inject tool catalog (for LLM planner nodes)
      DATA catalog_aware TYPE REF TO zif_ai_tool_catalog_aware.
      TRY.
          catalog_aware ?= <my_entry>-source_node.
          catalog_aware->set_tool_catalog( tool_catalog = tools ).
        CATCH cx_sy_move_cast_error.
          " not catalog-aware
      ENDTRY.

      " 2) Inject executable tools (for Tool executor nodes)
      DATA tool_aware  TYPE REF TO zif_ai_node_tool_aware.
      TRY.
          tool_aware ?= <my_entry>-source_node.

          LOOP AT tools INTO DATA(tool_entry).
            " Use add_tool; the node's internal hashed table will ensure uniqueness by tool_name
            tool_aware->add_tool(
              tool_name   = tool_entry-tool_name
              tool        = tool_entry-tool_endpoint
              description = tool_entry-tool_description ).
          ENDLOOP.

        CATCH cx_sy_move_cast_error.
          " not tool-aware
      ENDTRY.
    ENDLOOP.

    " 6) Create agent
    agent = zcl_ai_agent=>create(
      agent_id        = agent_id
      agent_name      = agent_name
      node_edge_graph = node_edge_graph
      start_node_id   = start_node_id
      tools           = tools ).

    " 7) save the agent definition into the agent def table
    " In case the agent need to be restroed after pause, the definition must be stored
    TRY.
        zcl_ai_agent_repository=>save_agent_blueprint(
          agent_blueprint = agent->get_agent_blueprint( ) ).
      CATCH cx_static_check.
        zcl_abapchain_logger=>get_instance( )->log_error(
          message = 'Failed to save agent blueprint after build.' ).
    ENDTRY.

  ENDMETHOD.

  METHOD build_from_blueprint.
    DATA(agent_id) = agent_blueprint-agent_id.
    DATA(agent_name) = agent_blueprint-agent_name.
    DATA(start_node_id) = agent_blueprint-start_node_id.
    DATA(node_edge_graph) = restore_graph_from_blueprint(
      graph_blueprint = agent_blueprint-graph_blueprint
      agent_id        = agent_id ).
    DATA(tools) = restore_tools_from_blueprint(
      tool_registry_blueprint = agent_blueprint-tool_registry_blueprint
      agent_id                = agent_id ).
    agent = zcl_ai_agent=>create(
      agent_id        = agent_id
      agent_name      = agent_name
      node_edge_graph = node_edge_graph
      start_node_id   = start_node_id
      tools           = tools ).

    " assign the tool endpoint back to the tool node tool registry
    LOOP AT agent_blueprint-graph_blueprint INTO DATA(node_blueprint).
      IF node_blueprint-class_name = 'ZCL_AI_NODE_TOOL'.
        DATA(tool_node) = node_edge_graph[ source_node_id = node_blueprint-node_id ]-source_node.
        DATA tool_node_config TYPE zcl_ai_node_tool=>ts_tool_node_config.
        DATA tool_blueprints TYPE zif_ai_types=>tt_tool_blueprints.
        xco_cp_json=>data->from_string( node_blueprint-config )->write_to( REF #( tool_node_config ) ).
        tool_blueprints = tool_node_config-tools.
        DATA tool_aware TYPE REF TO zif_ai_node_tool_aware.
        tool_aware ?= tool_node.

        LOOP AT tool_blueprints INTO DATA(tool_blueprint).
          READ TABLE tools INTO DATA(tool_entry)
               WITH KEY tool_name = tool_blueprint-tool_name.
          IF sy-subrc = 0.
            tool_aware->add_tool(
              tool_name   = tool_entry-tool_name
              tool        = tool_entry-tool_endpoint
              description = tool_entry-tool_description ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    " set the agent id of the agent definition explicitly
    agent->agent_id = agent_id.
  ENDMETHOD.

  METHOD restore_graph_from_blueprint.
    DATA node_blueprint TYPE zif_ai_types=>ts_node_blueprint.
    " first loop create nodes instances
    LOOP AT graph_blueprint INTO node_blueprint.
      DATA node TYPE REF TO zif_ai_node.
      DATA graph_entry TYPE zif_ai_types=>ts_graph_entry.
      CLEAR graph_entry.
      CREATE OBJECT node TYPE (node_blueprint-class_name)
               EXPORTING
                 name = node_blueprint-node_name.
      node->set_configuration( node_blueprint-config ).
      DATA(node_in_class) = CAST zcl_ai_node_base( node ).
      node_in_class->agent_id = agent_id.
      node_in_class->node_id = node_blueprint-node_id.
      graph_entry-source_node_id = node->get_node_id( ).
      graph_entry-source_node = node.
      INSERT graph_entry INTO TABLE node_edge_graph.
    ENDLOOP.

    " second loop connect edges
    LOOP AT graph_blueprint INTO node_blueprint.
      LOOP AT node_blueprint-next_nodes INTO DATA(edge_blueprint).
        DATA edge TYPE zif_ai_types=>ts_edge.
        CLEAR edge.
        edge-target_node_id = edge_blueprint-target_node_id.
        edge-condition = edge_blueprint-condition.
        edge-condition_value = edge_blueprint-condition_value.
        edge-priority = edge_blueprint-priority.
        edge-target_node = node_edge_graph[ source_node_id = edge-target_node_id ]-source_node.
        ASSIGN node_edge_graph[ source_node_id = node_blueprint-node_id ]-next_nodes TO FIELD-SYMBOL(<edge_list>).
        INSERT edge INTO TABLE <edge_list>.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD restore_tools_from_blueprint.
    DATA tool_blueprint TYPE zif_ai_types=>ts_tool_blueprint.
    LOOP AT tool_registry_blueprint INTO tool_blueprint.
      DATA tool TYPE REF TO zif_ai_tool.
      IF tool_blueprint-tool_class = to_upper( 'zcl_ai_tool_fake_table_info' ) OR tool_blueprint-tool_class = to_upper( 'zcl_ai_tool_fake_risk_check' ).
        CREATE OBJECT tool TYPE (tool_blueprint-tool_class).
      ELSE.

        CREATE OBJECT tool TYPE (tool_blueprint-tool_class)
                 EXPORTING
                   name   = tool_blueprint-tool_name
                   description = tool_blueprint-tool_description.
      ENDIF.
      INSERT VALUE #(
      tool_name        = tool_blueprint-tool_name
      tool_endpoint    = tool
      tool_description = tool_blueprint-tool_description
      ) INTO TABLE tools.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

